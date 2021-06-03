module Language.Edh.Swarm
  ( installSwarmBatteries,
    startSwarmWork,
    startSwarmWork',
    swarmRepl,
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
import Language.Edh.Swarm.Starter
  ( SwarmWorkStarter (..),
    determineSwarmWorkStarter,
  )
import Language.Edh.Swarm.Worker
  ( killWorkerProc,
    waitAnyWorkerDoneProc,
    wscStartWorkerProc,
    wscTakeProc,
  )
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
                  [ (nm,) <$> mkHostProc moduScope mc nm hp
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

              !artsDict <-
                EdhDict
                  <$> createEdhDict
                    [(EdhString k, v) | (k, v) <- moduArts]
              flip iopdUpdate (edh'scope'entity moduScope) $
                [(AttrByName k, v) | (k, v) <- moduArts]
                  ++ [(AttrByName "__exports__", artsDict)]

              exit

startSwarmWork :: (EdhWorld -> IO ()) -> IO ()
startSwarmWork !worldCustomization =
  startSwarmWork' swarmRepl worldCustomization

startSwarmWork' ::
  (EdhConsole -> EdhWorld -> IO ()) -> (EdhWorld -> IO ()) -> IO ()
startSwarmWork' !runRepl !worldCustomization = do
  starter@( SwarmWorkStarter
              _executable
              _workDir
              !workSpec
              !managerPid
              !workerPid
              !wscFd
            ) <-
    determineSwarmWorkStarter

  !console <- defaultEdhConsole defaultEdhConsoleSettings
  let consoleOut = consoleIO console . ConsoleOut
      consoleShutdown = consoleIO console ConsoleShutdown

      workProg :: IO ()
      workProg = do
        -- create the world, we always work with this world no matter how
        -- many times the Edh programs crash
        !world <- createEdhWorld console
        installEdhBatteries world

        -- install batteries provided by nedh
        installNetBatteries world

        -- install batteries provided by sedh
        installSwarmBatteries starter world

        -- call custom preparation
        worldCustomization world

        case workSpec of
          -- run in repl mode
          "" -> runRepl console world
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
          consoleOut $ "💥 " <> T.pack (show e)
        Right _ -> pure ()
      -- shutdown console IO anyway
      consoleIO console ConsoleShutdown

  case workSpec of
    -- run in repl mode
    "" -> return ()
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

-- | Manage lifecycle of Edh programs during the repl session
swarmRepl :: EdhConsole -> EdhWorld -> IO ()
swarmRepl !console !world = do
  consoleOut ">> Get Work Done - by a swarm <<\n"
  consoleOut
    "* Blank Screen Syndrome ? Take the Tour as your companion, checkout:\n"
  consoleOut "  https://github.com/e-wrks/sedh/tree/master/Tour\n"

  -- here being the host interpreter, we loop infinite runs of the Edh
  -- console REPL program, unless cleanly shutdown, for resilience
  let doneRightOrRebirth =
        runEdhModule world "swarm" edhModuleAsIs >>= \case
          -- to run a module is to seek its `__main__.edh` and execute the
          -- code there in a volatile module context, it can import itself
          -- (i.e. `__init__.edh`) during the run. all imported modules can
          -- survive program crashes.
          Left !err -> do
            -- program crash on error
            consoleOut "Your program crashed with an error:\n"
            consoleOut $ T.pack $ show err <> "\n"
            -- the world with all modules ever imported, is still
            -- there, repeat another repl session with this world.
            -- it may not be a good idea, but just so so ...
            consoleOut "🐴🐴🐯🐯\n"
            doneRightOrRebirth
          Right !phv -> case edhUltimate phv of
            -- clean program halt, all done
            EdhNil -> consoleOut "Well done, bye.\n"
            _ -> do
              -- unclean program exit
              consoleOut "Your program halted with a result:\n"
              consoleOut $
                (<> "\n") $ case phv of
                  EdhString msg -> msg
                  _ -> T.pack $ show phv
              -- the world with all modules ever imported, is still
              -- there, repeat another repl session with this world.
              -- it may not be a good idea, but just so so ...
              consoleOut "🐴🐴🐯🐯\n"
              doneRightOrRebirth
  doneRightOrRebirth
  where
    !consoleOut = consoleIO console . ConsoleOut
