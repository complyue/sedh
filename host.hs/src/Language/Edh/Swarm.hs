module Language.Edh.Swarm where

-- import           Debug.Trace

import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Language.Edh.EHI
import Language.Edh.Net
import Language.Edh.Repl
import Language.Edh.Run
import Language.Edh.Swarm.NodeCfg
import Language.Edh.Swarm.Starter
import Language.Edh.Swarm.Worker
import System.Exit
import System.Process
import Prelude

systemProc :: "cmd" !: Text -> Edh EdhValue
systemProc (mandatoryArg -> !cmd) =
  liftIO $
    system (T.unpack cmd) >>= \case
      ExitSuccess -> return nil
      ExitFailure !errCode ->
        return $ EdhDecimal $ fromIntegral errCode

installSwarmCtrlBatteries :: EdhWorld -> IO ()
installSwarmCtrlBatteries !world = runProgramM_ world $ do
  installModuleM_ "swarm/CTRL" $ do
    !moduScope <- contextScope . edh'context <$> edhThreadState

    !nregClass <- createNodeRegClass

    !moduMths <-
      sequence $
        [ (AttrByName nm,) <$> mkEdhProc mc nm hp
          | (nm, mc, hp) <-
              [("system", EdhMethod, wrapEdhProc systemProc)]
        ]

    let !moduArts = (AttrByName "NodeReg", EdhObject nregClass) : moduMths

    iopdUpdateEdh moduArts $ edh'scope'entity moduScope
    !esExps <- prepareExpStoreM (edh'scope'this moduScope)
    iopdUpdateEdh moduArts esExps

installSwarmBatteries :: SwarmWorkStarter -> EdhWorld -> IO ()
installSwarmBatteries
  ( SwarmWorkStarter
      !executable
      !workDir
      !workSpec
      !managerPid
      !workerPid
      !wscFd
    )
  !world = runProgramM_ world $ do
    installModuleM_ "swarm/ENV" $ do
      !moduScope <- contextScope . edh'context <$> edhThreadState

      iopdUpdateEdh
        [ (AttrByName "jobExecutable", EdhString executable),
          (AttrByName "jobWorkDir", EdhString workDir),
          (AttrByName "jobWorkSpec", EdhString workSpec),
          ( AttrByName "swarmManagerPid",
            EdhDecimal $ fromIntegral managerPid
          ),
          ( AttrByName "swarmWorkerPid",
            EdhDecimal $ fromIntegral workerPid
          ),
          (AttrByName "wscFd", EdhDecimal $ fromIntegral wscFd)
        ]
        (edh'scope'entity moduScope)

    installModuleM_ "swarm/RT" $ do
      !moduScope <- contextScope . edh'context <$> edhThreadState

      -- loosely depend on the @net@ runtime from nedh project
      !peerClass <- getPeerClass
      !moduArts <-
        sequence $
          [ (AttrByName nm,) <$> mkEdhProc mc nm hp
            | (nm, mc, hp) <-
                [ ( "killWorker",
                    EdhMethod,
                    wrapEdhProc killWorkerProc
                  ),
                  ( "wscTake",
                    EdhMethod,
                    wrapEdhProc $ wscTakeProc peerClass
                  ),
                  ( "waitAnyWorkerDone",
                    EdhMethod,
                    wrapEdhProc waitAnyWorkerDoneProc
                  ),
                  ( "wscStartWorker",
                    EdhMethod,
                    wrapEdhProc wscStartWorkerProc
                  )
                ]
          ]

      iopdUpdateEdh moduArts $ edh'scope'entity moduScope
      !esExps <- prepareExpStoreM (edh'scope'this moduScope)
      iopdUpdateEdh moduArts esExps

startSwarmWork :: (EdhWorld -> IO ()) -> IO ()
startSwarmWork !worldCustomization = do
  starter@( SwarmWorkStarter
              _executable
              _workDir
              !workSpec
              !managerPid
              !workerPid
              !wscFd
            ) <-
    determineSwarmWorkStarter
  let initWorld !world = do
        installEdhBatteries world
        installNetBatteries world
        installSwarmBatteries starter world

        worldCustomization world

  if T.null workSpec
    then flip (edhRepl defaultEdhConsoleSettings) "swarm" $ \ !world -> do
      let consoleOut = consoleIO (edh'world'console world) . ConsoleOut
      initWorld world
      consoleOut $
        ">> Get Work Done - by a swarm <<\n"
          <> "* Blank Screen Syndrome ? Take the Tour as your companion, checkout:\n"
          <> "  https://github.com/e-wrks/tour\n"
    else case workSpec of
      -- run in headhunter mode
      !workScript | wscFd == 0 ->
        edhRunFile defaultEdhConsoleSettings (T.unpack workScript) $
          \ !world -> do
            initWorld world
            let !consoleOut = consoleIO (edh'world'console world) . ConsoleOut
            consoleOut $
              ">> Hunting working heads for "
                <> workScript
                <> " from swarm, HH pid="
                <> T.pack (show managerPid)
                <> " <<\n"
      -- run in swarm worker mode
      !workModu ->
        edhRunModule defaultEdhConsoleSettings "swarm/worker" $ \ !world -> do
          initWorld world
          let !consoleOut = consoleIO (edh'world'console world) . ConsoleOut
          consoleOut $
            ">> Working out "
              <> workModu
              <> " for swarm by worker pid="
              <> T.pack (show workerPid)
              <> " forager pid="
              <> T.pack (show managerPid)
              <> " <<\n"
