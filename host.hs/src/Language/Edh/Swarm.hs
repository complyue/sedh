module Language.Edh.Swarm
  ( installSwarmBatteries,
    startSwarmWork,
    SwarmWorkStarter (..),
    determineSwarmWorkStarter,
  )
where

-- import           Debug.Trace

import Control.Concurrent (forkFinally)
import Control.Exception (SomeException)
import Control.Monad.Reader
import qualified Data.Text as T
import Language.Edh.EHI
import Language.Edh.Net
import Language.Edh.Repl
import Language.Edh.Swarm.Starter
import Language.Edh.Swarm.Worker
import System.IO (hPutStrLn, stderr)
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

              !moduArts <-
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
    else do
      !console <- defaultEdhConsole defaultEdhConsoleSettings
      let consoleOut = consoleIO console . ConsoleOut
          consoleShutdown = consoleIO console ConsoleShutdown

          workProg :: IO ()
          workProg = do
            -- create the world, we always work with this world no matter how
            -- many times the Edh programs crash
            !world <- createEdhWorld console
            initWorld world

            case workSpec of
              -- run in headhunter mode
              !workScript
                | wscFd == 0 ->
                  runEdhFile world (T.unpack workScript) >>= \case
                    Left !err -> do
                      -- program crash on error
                      consoleOut "Swarm headhunter crashed with an error:\n"
                      consoleOut $ T.pack $ show err <> "\n"
                    Right !phv -> case edhUltimate phv of
                      EdhNil ->
                        -- clean program halt, all done
                        consoleOut $
                          "Swarm work script " <> workScript <> " done right.\n"
                      _ -> do
                        -- unclean program exit
                        consoleOut "Swarm headhunter halted with a result:\n"
                        consoleOut $
                          (<> "\n") $ case phv of
                            EdhString msg -> msg
                            _ -> T.pack $ show phv

              -- run in swarm worker mode
              _workModu ->
                runEdhModule world "swarm/worker" edhModuleAsIs >>= \case
                  Left !err -> do
                    -- program crash on error
                    consoleOut "Swarm worker crashed with an error:\n"
                    consoleOut $ T.pack $ show err <> "\n"
                  Right !phv -> case edhUltimate phv of
                    -- clean program halt, all done
                    EdhNil -> consoleOut "Swarm worker right retired.\n"
                    -- unclean program exit
                    _ -> do
                      consoleOut "Swarm worker halted with a result:\n"
                      consoleOut $
                        (<> "\n") $ case phv of
                          EdhString msg -> msg
                          _ -> T.pack $ show phv

            consoleShutdown

      void $
        forkFinally workProg $ \result -> do
          case result of
            Left (e :: SomeException) ->
              hPutStrLn stderr $ "💥 " <> show e
            Right _ -> pure ()
          -- shutdown console IO anyway
          consoleIO console ConsoleShutdown

      case workSpec of
        -- run in headhunter mode
        !workScript
          | wscFd == 0 ->
            consoleOut $
              ">> Hunting working heads for "
                <> workScript
                <> " from swarm, HH pid="
                <> T.pack (show managerPid)
                <> " <<\n"
        -- run in swarm worker mode
        !workModu ->
          consoleOut $
            ">> Working out "
              <> workModu
              <> " for swarm by worker pid="
              <> T.pack (show workerPid)
              <> " forager pid="
              <> T.pack (show managerPid)
              <> " <<\n"

      consoleIOLoop console
