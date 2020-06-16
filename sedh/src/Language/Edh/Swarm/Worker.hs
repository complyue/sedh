
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
wscStartWorkerProc (ArgsPack [EdhObject !wsAddrObj, EdhString !workDir, !jobExecutable, EdhString !workModu] !kwargs) !exit
  | Map.null kwargs
  = ask >>= \pgs -> contEdhSTM $ prepCmdl pgs $ \wkrCmdl ->
    serviceAddressFrom pgs wsAddrObj $ \(servAddr, servPort) -> do
      wkrPidVar <- newTVar (0 :: Int)
      flip
          (edhPerformIO pgs)
          (\() -> contEdhSTM $ readTVar wkrPidVar >>= \pid ->
            exitEdhSTM pgs exit $ EdhDecimal $ fromIntegral pid
          )
        $ do
            addr <- do
              let
                hints = defaultHints { addrFlags      = [AI_PASSIVE]
                                     , addrSocketType = Stream
                                     }
              addr : _ <- getAddrInfo (Just hints)
                                      (Just $ T.unpack servAddr)
                                      (Just (show servPort))
              return addr
            bracket
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
                      executeFile "/usr/bin/env"
                                  False
                                  (wkrCmdl ++ [T.unpack workModu, show wscFd])
                                  Nothing
                    atomically $ writeTVar wkrPidVar $ fromIntegral wkrPid
 where
  strSeq
    :: EdhProgState -> [EdhValue] -> [String] -> ([String] -> STM ()) -> STM ()
  strSeq _   []       !sl exit' = exit' $ reverse sl
  strSeq pgs (v : vs) !sl exit' = case edhUltimate v of
    EdhString !s -> strSeq pgs vs (T.unpack s : sl) exit'
    _ -> throwEdhSTM pgs UsageError $ "In exec cmdl, not a string: " <> T.pack
      (show v)
  prepCmdl :: EdhProgState -> ([String] -> STM ()) -> STM ()
  prepCmdl !pgs !exit' = case jobExecutable of
    EdhString !executable -> exit' [T.unpack executable]
    EdhArgsPack (ArgsPack !vs _) -> strSeq pgs vs [] exit'
    EdhList (List _ !lv) -> readTVar lv >>= \vs -> strSeq pgs vs [] exit'
    _ -> throwEdhSTM pgs UsageError "Invalid jobExecutable"
wscStartWorkerProc _ _ = throwEdh UsageError "Invalid args"


killWorkerProc :: EdhProcedure
killWorkerProc (ArgsPack [EdhDecimal !wkrPid] !kwargs) !exit | Map.null kwargs =
  case D.decimalToInteger wkrPid of
    Nothing   -> throwEdh UsageError $ "Invalid pid: " <> T.pack (show wkrPid)
    Just !pid -> ask >>= \pgs ->
      contEdhSTM $ edhPerformIO pgs (confirmKill $ fromIntegral pid) $ \() ->
        exitEdhProc exit nil
 where
  confirmKill :: ProcessID -> IO ()
  -- assuming failure means the process by this pid doesn't exist (anymore)
  -- todo improve such confirmation criteria
  confirmKill !pid = handle (\(_ :: SomeException) -> return ()) $ do
    signalProcess killProcess pid
    threadDelay 100000  -- wait 0.1 second before checking it's actually killed
    signalProcess nullSignal pid
    threadDelay 3000000  -- wait 3 seconds before try another round
    confirmKill pid
killWorkerProc _ _ = throwEdh UsageError "Invalid args"


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

