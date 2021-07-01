module Language.Edh.Swarm where

-- import           Debug.Trace

import Control.Monad.Reader
import qualified Data.Text as T
import Language.Edh.EHI
import Language.Edh.Net
import Language.Edh.Repl
import Language.Edh.Run
import Language.Edh.Swarm.NodeCfg
import Language.Edh.Swarm.Starter
import Language.Edh.Swarm.Worker
import Prelude

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
  !world =
    do
      void $
        installEdhModule world "swarm/ENV" $ \ !ets !exit -> do
          let !moduScope = contextScope $ edh'context ets
          iopdUpdate
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
          exit

      void $
        installEdhModule world "swarm/RT" $ \ !ets !exit ->
          -- loosely depend on the @net@ runtime from nedh project
          runEdhTx ets $
            withPeerClass $ \ !peerClass _ets -> do
              let !moduScope = contextScope $ edh'context ets

              !moduArts0 <-
                sequence $
                  [ (AttrByName nm,) <$> mkHostProc moduScope mc nm hp
                    | (nm, mc, hp) <-
                        [ ( "killWorker",
                            EdhMethod,
                            wrapHostProc killWorkerProc
                          ),
                          ( "wscTake",
                            EdhMethod,
                            wrapHostProc $ wscTakeProc peerClass
                          ),
                          ( "waitAnyWorkerDone",
                            EdhMethod,
                            wrapHostProc waitAnyWorkerDoneProc
                          ),
                          ( "wscStartWorker",
                            EdhMethod,
                            wrapHostProc wscStartWorkerProc
                          )
                        ]
                  ]
              !nregClass <- createNodeRegClass moduScope
              let moduArts =
                    (AttrByName "NodeReg", EdhObject nregClass) : moduArts0

              iopdUpdate moduArts $ edh'scope'entity moduScope
              prepareExpStore ets (edh'scope'this moduScope) $ \ !esExps ->
                iopdUpdate moduArts esExps

              exit

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
