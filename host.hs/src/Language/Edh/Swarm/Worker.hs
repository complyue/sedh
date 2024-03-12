module Language.Edh.Swarm.Worker where

-- import           Debug.Trace

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Language.Edh.EHI
import Language.Edh.Net
import Network.Socket
import System.Posix
import Prelude

waitAnyWorkerDoneProc :: Edh EdhValue
waitAnyWorkerDoneProc =
  liftIO $
    getAnyProcessStatus True False >>= \case
      Nothing -> return nil
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
            $ EdhPair
              (EdhPair (EdhDecimal $ fromIntegral pid) (EdhString "killed"))
            $ EdhString
            $ T.pack
            $ "by "
              <> show sig
              <> if coreDumped
                then " with"
                else " without" <> " core dumped"
        Stopped !sig ->
          return
            $ EdhPair
              (EdhPair (EdhDecimal $ fromIntegral pid) (EdhString "stopped"))
            $ EdhString
            $ T.pack
            $ "by "
              <> show sig

wscStartWorkerProc ::
  "wsAddr" !: Object ->
  "workDir" !: Text ->
  "jobExecutable" !: EdhValue ->
  "workModu" !: Text ->
  Edh EdhValue
wscStartWorkerProc
  (mandatoryArg -> !wsAddrObj)
  (mandatoryArg -> !workDir)
  (mandatoryArg -> !jobExecutable)
  (mandatoryArg -> workModu) = do
    !wkrCmdl <- prepCmdl
    (!servAddr, !servPort) <- serviceAddressFrom wsAddrObj
    liftIO $ do
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
            return $ EdhDecimal $ fromIntegral wkrPid
    where
      strSeq :: [EdhValue] -> [String] -> Edh [String]
      strSeq [] !sl = return $ reverse sl
      strSeq (v : vs) !sl = case edhUltimate v of
        EdhString !s -> strSeq vs (T.unpack s : sl)
        _ ->
          throwEdhM UsageError $
            "In exec cmdl, not a string: "
              <> T.pack
                (show v)
      prepCmdl :: Edh [String]
      prepCmdl = case jobExecutable of
        EdhString !executable -> return [T.unpack executable]
        EdhArgsPack (ArgsPack !vs _) -> strSeq vs []
        EdhList (List _ !lv) -> readTVarEdh lv >>= \vs -> strSeq vs []
        _ -> throwEdhM UsageError "invalid jobExecutable"

killWorkerProc :: "workerPid" !: Int -> Edh EdhValue
killWorkerProc (mandatoryArg -> !wkrPid) = liftIO $ do
  confirmKill $ fromIntegral wkrPid
  return nil
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
