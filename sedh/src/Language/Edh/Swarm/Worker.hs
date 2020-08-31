
module Language.Edh.Swarm.Worker where

import           Prelude
-- import           Debug.Trace

import           System.Posix

import           Control.Exception
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Dynamic

import           Network.Socket
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )

import qualified Data.Lossless.Decimal         as D
import           Language.Edh.EHI
import           Language.Edh.Net


waitAnyWorkerDoneProc :: EdhHostProc
waitAnyWorkerDoneProc _ !exit !ets = if edh'in'tx ets
  then throwEdh ets UsageError "you don't wait within a transaction"
  else
    runEdhTx ets $ edhContIO $ waitAnyWorker >>= atomically . exitEdh ets exit
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


wscStartWorkerProc :: EdhHostProc
wscStartWorkerProc (ArgsPack [EdhObject !wsAddrObj, EdhString !workDir, !jobExecutable, EdhString !workModu] !kwargs) !exit
  | odNull kwargs
  = \ !ets -> if edh'in'tx ets
    then throwEdh ets UsageError "you don't start worker within a transaction"
    else prepCmdl ets $ \ !wkrCmdl ->
      serviceAddressFrom ets wsAddrObj $ \(!servAddr, !servPort) ->
        runEdhTx ets $ edhContIO $ do
          let
            !hints =
              defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
          addr : _ <- getAddrInfo (Just hints)
                                  (Just $ T.unpack servAddr)
                                  (Just (show servPort))
          bracket
              (socket (addrFamily addr)
                      (addrSocketType addr)
                      (addrProtocol addr)
              )
              close
            $ \ !sock -> do
                connect sock (addrAddress addr)
                bracket (socketToFd sock) (closeFd . Fd) $ \wscFd -> do
                  -- clear FD_CLOEXEC flag so it can be passed to subprocess
                  setFdOption (Fd wscFd) CloseOnExec False
                  !wkrPid <- forkProcess $ do
                    changeWorkingDirectory $ T.unpack workDir
                    executeFile "/usr/bin/env"
                                False
                                (wkrCmdl ++ [T.unpack workModu, show wscFd])
                                Nothing
                  atomically $ exitEdh ets exit $ EdhDecimal $ fromIntegral
                    wkrPid
 where
  strSeq
    :: EdhThreadState
    -> [EdhValue]
    -> [String]
    -> ([String] -> STM ())
    -> STM ()
  strSeq _   []       !sl exit' = exit' $ reverse sl
  strSeq ets (v : vs) !sl exit' = case edhUltimate v of
    EdhString !s -> strSeq ets vs (T.unpack s : sl) exit'
    _ -> throwEdh ets UsageError $ "In exec cmdl, not a string: " <> T.pack
      (show v)
  prepCmdl :: EdhThreadState -> ([String] -> STM ()) -> STM ()
  prepCmdl !ets !exit' = case jobExecutable of
    EdhString !executable -> exit' [T.unpack executable]
    EdhArgsPack (ArgsPack !vs _) -> strSeq ets vs [] exit'
    EdhList (List _ !lv) -> readTVar lv >>= \vs -> strSeq ets vs [] exit'
    _ -> throwEdh ets UsageError "invalid jobExecutable"
wscStartWorkerProc _ _ = throwEdhTx UsageError "invalid args"


killWorkerProc :: EdhHostProc
killWorkerProc (ArgsPack [EdhDecimal !wkrPid] !kwargs) !exit | odNull kwargs =
  case D.decimalToInteger wkrPid of
    Nothing   -> throwEdhTx UsageError $ "invalid pid: " <> T.pack (show wkrPid)
    Just !pid -> \ !ets -> runEdhTx ets $ edhContIO $ do
      confirmKill $ fromIntegral pid
      atomically $ exitEdh ets exit nil
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
killWorkerProc _ _ = throwEdhTx UsageError "invalid args"


wscTakeProc :: Object -> EdhHostProc
wscTakeProc !peerClass (ArgsPack [EdhDecimal !wscFd] !kwargs) !exit
  | odNull kwargs = \ !ets -> case D.decimalToInteger wscFd of
    Nothing -> throwEdh ets UsageError $ "bad wsc fd: " <> T.pack (show wscFd)
    Just !wscFdInt -> do
      let !peerId = "<wsc#" <> T.pack (show wscFdInt) <> ">"
      !pktSink <- newEmptyTMVar
      !poq     <- newEmptyTMVar
      !chdVar  <- newTVar mempty
      !wkrEoL  <- newEmptyTMVar
      let !peer = Peer { edh'peer'ident    = peerId
                       , edh'peer'eol      = wkrEoL
                       , edh'peer'posting  = putTMVar poq
                       , edh'peer'hosting  = takeTMVar pktSink
                       , edh'peer'channels = chdVar
                       }
      !peerObj <- edhCreateHostObj peerClass (toDyn peer) []

      runEdhTx ets $ edhContIO $ do
        void
          $ forkFinally
              (workerThread (fromInteger wscFdInt) peerId pktSink poq wkrEoL)
          $ atomically
          . void
          . tryPutTMVar wkrEoL
        atomically $ exitEdh ets exit $ EdhObject peerObj
wscTakeProc _ _ _ = throwEdhTx UsageError "invalid args"


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

