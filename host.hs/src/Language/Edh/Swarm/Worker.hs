module Language.Edh.Swarm.Worker where

-- import           Debug.Trace

import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.HashSet as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Edh.EHI
import Language.Edh.Net
import Network.Socket
import Network.Socket.ByteString
import System.Posix
import Prelude

waitAnyWorkerDoneProc :: EdhHostProc
waitAnyWorkerDoneProc !exit !ets =
  runEdhTx ets $
    edhContIO $
      waitAnyWorker
        >>= atomically . exitEdh ets exit
  where
    waitAnyWorker :: IO EdhValue
    waitAnyWorker =
      getAnyProcessStatus True False >>= \case
        Nothing -> return nil
        Just (pid, status) -> case status of
          Exited !exitCode ->
            return $
              EdhPair
                (EdhPair (EdhDecimal $ fromIntegral pid) (EdhString "exited"))
                $ EdhString $
                  T.pack $
                    show exitCode
          Terminated !sig !coreDumped ->
            return $
              EdhPair
                (EdhPair (EdhDecimal $ fromIntegral pid) (EdhString "killed"))
                $ EdhString $
                  T.pack $
                    "by "
                      <> show sig
                      <> if coreDumped
                        then " with"
                        else " without" <> " core dumped"
          Stopped !sig ->
            return $
              EdhPair
                (EdhPair (EdhDecimal $ fromIntegral pid) (EdhString "stopped"))
                $ EdhString $
                  T.pack $
                    "by "
                      <> show sig

wscStartWorkerProc ::
  "wsAddr" !: Object ->
  "workDir" !: Text ->
  "jobExecutable" !: EdhValue ->
  "workModu" !: Text ->
  EdhHostProc
wscStartWorkerProc
  (mandatoryArg -> !wsAddrObj)
  (mandatoryArg -> !workDir)
  (mandatoryArg -> !jobExecutable)
  (mandatoryArg -> workModu)
  !exit
  !ets = prepCmdl $ \ !wkrCmdl ->
    serviceAddressFrom ets wsAddrObj $ \(!servAddr, !servPort) ->
      runEdhTx ets $
        edhContIO $ do
          let !hints =
                defaultHints
                  { addrFlags = [AI_PASSIVE],
                    addrSocketType = Stream
                  }
          addr : _ <-
            getAddrInfo
              (Just hints)
              (Just $ T.unpack servAddr)
              (Just (show servPort))
          bracket
            ( socket
                (addrFamily addr)
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
                  executeFile
                    "/usr/bin/env"
                    False
                    (wkrCmdl ++ [T.unpack workModu, show wscFd])
                    Nothing
                atomically $
                  exitEdh ets exit $
                    EdhDecimal $ fromIntegral wkrPid
    where
      strSeq :: [EdhValue] -> [String] -> ([String] -> STM ()) -> STM ()
      strSeq [] !sl exit' = exit' $ reverse sl
      strSeq (v : vs) !sl exit' = case edhUltimate v of
        EdhString !s -> strSeq vs (T.unpack s : sl) exit'
        _ ->
          throwEdh ets UsageError $
            "In exec cmdl, not a string: "
              <> T.pack
                (show v)
      prepCmdl :: ([String] -> STM ()) -> STM ()
      prepCmdl !exit' = case jobExecutable of
        EdhString !executable -> exit' [T.unpack executable]
        EdhArgsPack (ArgsPack !vs _) -> strSeq vs [] exit'
        EdhList (List _ !lv) -> readTVar lv >>= \vs -> strSeq vs [] exit'
        _ -> throwEdh ets UsageError "invalid jobExecutable"

killWorkerProc :: "workerPid" !: Int -> EdhHostProc
killWorkerProc (mandatoryArg -> !wkrPid) !exit !ets =
  runEdhTx ets $
    edhContIO $ do
      confirmKill $ fromIntegral wkrPid
      atomically $ exitEdh ets exit nil
  where
    confirmKill :: ProcessID -> IO ()
    -- assuming failure means the process by this pid doesn't exist (anymore)
    -- todo improve such confirmation criteria
    confirmKill !pid = handle (\(_ :: SomeException) -> return ()) $ do
      signalProcess killProcess pid
      threadDelay 100000 -- wait 0.1 second before checking it's actually killed
      signalProcess nullSignal pid
      threadDelay 3000000 -- wait 3 seconds before try another round
      confirmKill pid

wscTakeProc :: Object -> "wscFd" !: Int -> EdhHostProc
wscTakeProc !peerClass (mandatoryArg -> !wscFd) !exit !ets =
  mkObjSandbox ets caller'this $ \ !sandboxScope -> do
    let !peerId = "<wsc#" <> T.pack (show wscFd) <> ">"
    !pktSink <- newEmptyTMVar
    !poq <- newEmptyTMVar
    !disposalsVar <- newTVar mempty
    !chdVar <- newTVar mempty
    !wkrEoL <- newEmptyTMVar
    let !peer =
          Peer
            { edh'peer'ident = peerId,
              edh'peer'sandbox = Just sandboxScope,
              edh'peer'eol = wkrEoL,
              edh'peer'posting = putTMVar poq,
              edh'peer'hosting = takeTMVar pktSink,
              edh'peer'disposals = disposalsVar,
              edh'peer'channels = chdVar
            }
    !peerObj <- edhCreateHostObj peerClass peer

    runEdhTx ets $
      edhContIO $ do
        void $
          forkFinally (workerThread wscFd peerId pktSink poq wkrEoL) $
            \ !result -> atomically $ do
              !sinks2Dispose <- readTVar disposalsVar
              sequence_ $ flip postEvent EdhNil <$> Set.toList sinks2Dispose
              void $ tryPutTMVar wkrEoL result
        atomically $ exitEdh ets exit $ EdhObject peerObj
  where
    !ctx = edh'context ets
    !caller'this = edh'scope'this $ callingScope ctx

workerThread ::
  Int ->
  Text ->
  TMVar Packet ->
  TMVar Packet ->
  TMVar (Either SomeException ()) ->
  IO ()
workerThread !wscFd !peerId !pktSink !poq !wkrEoL =
  bracket (mkSocket $ fromIntegral wscFd) close $ \ !sock ->
    try (netComm sock)
      >>= (gracefulClose sock 5000 <*)
        . atomically
        . tryPutTMVar wkrEoL
  where
    netComm :: Socket -> IO ()
    netComm !sock = do
      -- pump commands in,
      -- make this thread the only one reading the handle
      -- note this won't return, will be asynchronously killed on eol
      void $ forkIO $ receivePacketStream peerId (recv sock) pktSink wkrEoL

      let serializeCmdsOut :: IO ()
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
