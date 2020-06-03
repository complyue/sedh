
module Language.Edh.Swarm.Worker where

import           Prelude
-- import           Debug.Trace

import           System.Posix

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as Map
import           Data.Dynamic

import           Network.Socket
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )

import qualified Data.Lossless.Decimal         as D
import           Language.Edh.EHI
import           Language.Edh.Net


waitAnyWorkerDoneProc :: EdhProcedure
waitAnyWorkerDoneProc _ !exit = ask >>= \pgs ->
  contEdhSTM $ edhPerformIO pgs waitAnyWorker $ \exitInfo ->
    exitEdhProc exit exitInfo
 where
  waitAnyWorker :: IO EdhValue
  waitAnyWorker = getAnyProcessStatus True False >>= \case
    Nothing            -> return nil
    Just (pid, status) -> case status of
      Exited !exitCode ->
        return
          $ EdhPair
              (EdhPair (EdhDecimal $ fromIntegral pid) (EdhString "exited"))
          $ EdhString
          $ T.pack
          $ show exitCode
      Terminated !sig !coreDumped ->
        return
          $  EdhPair
               (EdhPair (EdhDecimal $ fromIntegral pid) (EdhString "killed"))
          $  EdhString
          $  T.pack
          $  "by "
          <> show sig
          <> if coreDumped then " with" else " without" <> " core dumped"
      Stopped !sig ->
        return
          $  EdhPair
               (EdhPair (EdhDecimal $ fromIntegral pid) (EdhString "stopped"))
          $  EdhString
          $  T.pack
          $  "by "
          <> show sig


wscStartWorkerProc :: EdhProcedure
wscStartWorkerProc (ArgsPack [EdhObject !wsAddrObj, EdhString !workDir, EdhString !executable, EdhString !workModu] !kwargs) !exit
  | Map.null kwargs
  = ask >>= \pgs ->
    contEdhSTM
      $   fromDynamic
      <$> readTVar (entity'store $ objEntity wsAddrObj)
      >>= \case
            Just addr@AddrInfo{} -> do
              wkrPidVar <- newTVar (0 :: Int)
              flip
                  (edhPerformIO pgs)
                  (\() -> contEdhSTM $ readTVar wkrPidVar >>= \pid ->
                    exitEdhSTM pgs exit $ EdhDecimal $ fromIntegral pid
                  )
                $ bracket
                    (socket (addrFamily addr)
                            (addrSocketType addr)
                            (addrProtocol addr)
                    )
                    close
                $ \sock -> do
                    connect sock (addrAddress addr)
                    bracket (socketToFd sock) (closeFd . Fd) $ \wscFd -> do
                      -- clear FD_CLOEXEC flag so it can be passed to subprocess
                      setFdOption (Fd wscFd) CloseOnExec False
                      wkrPid <- forkProcess $ do
                        changeWorkingDirectory $ T.unpack workDir
                        executeFile
                          "/usr/bin/env"
                          False
                          [T.unpack executable, T.unpack workModu, show wscFd]
                          Nothing
                      atomically $ writeTVar wkrPidVar $ fromIntegral wkrPid
            _ -> throwEdhSTM pgs EvalError "Invalid worksource addr object"
wscStartWorkerProc _ _ = throwEdh UsageError "Invalid args"


wscTakeProc :: EdhProcedure
wscTakeProc (ArgsPack [EdhDecimal !wscFd, EdhObject !peerObj] !kwargs) !exit
  | Map.null kwargs = do
    pgs <- ask
    case D.decimalToInteger wscFd of
      Nothing -> throwEdh UsageError $ "bad wsc fd: " <> T.pack (show wscFd)
      Just !wscFdInt -> contEdhSTM $ do
        let !peerId = "<wsc#" <> T.pack (show wscFdInt) <> ">"
        pktSink <- newEmptyTMVar
        poq     <- newEmptyTMVar
        chdVar  <- newTVar mempty
        wkrEoL  <- newEmptyTMVar

        let !peer = Peer { edh'peer'ident    = peerId
                         , edh'peer'eol      = wkrEoL
                         , edh'peer'posting  = putTMVar poq
                         , edh'peer'hosting  = takeTMVar pktSink
                         , edh'peer'channels = chdVar
                         }
        writeTVar (entity'store $ objEntity peerObj) $ toDyn peer

        edhPerformIO
            pgs
            ( forkFinally
                (workerThread (fromInteger wscFdInt) peerId pktSink poq wkrEoL)
            $ atomically
            . void
            . tryPutTMVar wkrEoL
            )
          $ \_ -> exitEdhProc exit $ EdhObject peerObj
wscTakeProc _ _ = throwEdh UsageError "Invalid args"


workerThread
  :: Int
  -> Text
  -> TMVar Packet
  -> TMVar Packet
  -> TMVar (Either SomeException ())
  -> IO ()
workerThread !wscFd !peerId !pktSink !poq !wkrEoL =
  bracket (mkSocket $ fromIntegral wscFd) close $ \sock ->
    try (netComm sock)
      >>= (gracefulClose sock 5000 <*)
      .   atomically
      .   tryPutTMVar wkrEoL
 where
  netComm :: Socket -> IO ()
  netComm !sock = do

    -- pump commands in, 
    -- make this thread the only one reading the handle
    -- note this won't return, will be asynchronously killed on eol
    void $ forkIO $ receivePacketStream peerId (recv sock) pktSink wkrEoL

    let
      serializeCmdsOut :: IO ()
      serializeCmdsOut =
        atomically
            ((Right <$> takeTMVar poq) `orElse` (Left <$> readTMVar wkrEoL))
          >>= \case
                Left _ -> return ()
                Right !pkt ->
                  catch
                      (sendPacket peerId (sendAll sock) pkt >> serializeCmdsOut)
                    $ \(e :: SomeException) -> -- mark eol on error
                        atomically $ void $ tryPutTMVar wkrEoL $ Left e
    -- pump commands out,
    -- make this thread the only one writing the handle
    serializeCmdsOut

