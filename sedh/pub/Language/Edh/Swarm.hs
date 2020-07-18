
module Language.Edh.Swarm
  ( installSwarmBatteries
  , startSwarmWork
  , startSwarmWork'
  , swarmRepl
  , SwarmWorkStarter(..)
  , determineSwarmWorkStarter
  )
where

import           Prelude
-- import           Debug.Trace

import           Control.Exception
import           Control.Monad.Reader
import           Control.Concurrent
import           Control.Concurrent.STM

import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as Map

import           Language.Edh.EHI
import           Language.Edh.Net

import           Language.Edh.Swarm.Starter
import           Language.Edh.Swarm.Worker


installSwarmBatteries :: SwarmWorkStarter -> EdhWorld -> IO ()
installSwarmBatteries (SwarmWorkStarter executable workDir workModu managerPid workerPid wscFd) !world
  = do

    void $ installEdhModule world "swarm/ENV" $ \pgs exit -> do

      let moduScope = contextScope $ edh'context pgs
          modu      = thisObject moduScope
      updateEntityAttrs
        pgs
        (objEntity modu)
        [ (AttrByName "jobExecutable"  , EdhString executable)
        , (AttrByName "jobWorkDir"     , EdhString workDir)
        , (AttrByName "jobWorkModu"    , EdhString workModu)
        , (AttrByName "swarmManagerPid", EdhDecimal $ fromIntegral managerPid)
        , (AttrByName "swarmWorkerPid" , EdhDecimal $ fromIntegral workerPid)
        , (AttrByName "wscFd"          , EdhDecimal $ fromIntegral wscFd)
        ]
      exit

    void $ installEdhModule world "swarm/RT" $ \pgs exit -> do

      let moduScope = contextScope $ edh'context pgs
          modu      = thisObject moduScope

      moduArts <-
        sequence
          $ [ (nm, ) <$> mkHostProc moduScope mc nm hp args
            | (mc, nm, hp, args) <-
              [ ( EdhMethod
                , "killWorker"
                , killWorkerProc
                , PackReceiver [mandatoryArg "pid"]
                )
              , ( EdhMethod
                , "wscTake"
                , wscTakeProc
                , PackReceiver [mandatoryArg "wscFd", mandatoryArg "peerObj"]
                )
              , ( EdhMethod
                , "waitAnyWorkerDone"
                , waitAnyWorkerDoneProc
                , PackReceiver []
                )
              , ( EdhMethod
                , "wscStartWorker"
                , wscStartWorkerProc
                , PackReceiver
                  [ mandatoryArg "wsAddr"
                  , mandatoryArg "workDir"
                  , mandatoryArg "executable"
                  , mandatoryArg "workModu"
                  ]
                )
              ]
            ]

      artsDict <- createEdhDict [ (EdhString k, v) | (k, v) <- moduArts ]
      updateEntityAttrs pgs (objEntity modu)
        $  [ (AttrByName k, v) | (k, v) <- moduArts ]
        ++ [(AttrByName "__exports__", artsDict)]

      exit


startSwarmWork :: (EdhWorld -> IO ()) -> IO ()
startSwarmWork !worldCustomization =
  startSwarmWork' swarmRepl worldCustomization

startSwarmWork'
  :: (EdhConsole -> EdhWorld -> IO ()) -> (EdhWorld -> IO ()) -> IO ()
startSwarmWork' !doRepl !worldCustomization = do
  starter@(SwarmWorkStarter _executable _workDir workModu managerPid workerPid wscFd) <-
    determineSwarmWorkStarter

  console <- defaultEdhConsole defaultEdhConsoleSettings
  let
    consoleOut      = writeTBQueue (consoleIO console) . ConsoleOut
    consoleShutdown = writeTBQueue (consoleIO console) ConsoleShutdown

    workProg :: IO ()
    workProg = do

      -- create the world, we always work with this world no matter how
      -- many times the Edh programs crash
      world <- createEdhWorld console
      installEdhBatteries world

      -- install batteries provided by nedh
      installNetBatteries world

      -- install batteries provided by sedh
      installSwarmBatteries starter world

      -- call custom preparation
      worldCustomization world

      if "" == workModu
        then doRepl console world
        else if wscFd == 0
          then runEdhModule world (T.unpack workModu) edhModuleAsIs >>= \case
            Left !err -> atomically $ do
              -- program crash on error
              consoleOut "Swarm headhunter crashed with an error:\n"
              consoleOut $ T.pack $ show err <> "\n"
            Right !phv -> case edhUltimate phv of
              -- clean program halt, all done
              EdhNil ->
                atomically
                  $  consoleOut
                  $  "Swarm work "
                  <> workModu
                  <> " done right.\n"
              -- unclean program exit
              _ -> atomically $ do
                consoleOut "Swarm headhunter halted with a result:\n"
                consoleOut $ (<> "\n") $ case phv of
                  EdhString msg -> msg
                  _             -> T.pack $ show phv
          else runEdhModule world "swarm/worker" edhModuleAsIs >>= \case
            Left !err -> atomically $ do
              -- program crash on error
              consoleOut "Swarm worker crashed with an error:\n"
              consoleOut $ T.pack $ show err <> "\n"
            Right !phv -> case edhUltimate phv of
              -- clean program halt, all done
              EdhNil -> atomically $ consoleOut "Swarm worker right retired.\n"
              -- unclean program exit
              _      -> atomically $ do
                consoleOut "Swarm worker halted with a result:\n"
                consoleOut $ (<> "\n") $ case phv of
                  EdhString msg -> msg
                  _             -> T.pack $ show phv

      atomically consoleShutdown

  void $ forkFinally workProg $ \result -> do
    case result of
      Left (e :: SomeException) ->
        atomically $ consoleOut $ "💥 " <> T.pack (show e)
      Right _ -> pure ()
    -- shutdown console IO anyway
    atomically $ writeTBQueue (consoleIO console) ConsoleShutdown

  atomically $ if "" == workModu
    then consoleOut ">> Get Work Done - by a swarm <<\n"
    else if wscFd == 0
      then
        consoleOut
        $  ">> Hunting working heads for "
        <> workModu
        <> " from swarm, HH pid="
        <> T.pack (show managerPid)
        <> " <<\n"
      else
        consoleOut
        $  ">> Working out "
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
  atomically $ do
    consoleOut
      "* Blank Screen Syndrome ? Take the Tour as your companion, checkout:\n"
    consoleOut "  https://github.com/e-wrks/sedh/tree/master/Tour\n"

  -- here being the host interpreter, we loop infinite runs of the Edh
  -- console REPL program, unless cleanly shutdown, for resilience
  let doneRightOrRebirth = runEdhModule world "swarm" edhModuleAsIs >>= \case
    -- to run a module is to seek its `__main__.edh` and execute the
    -- code there in a volatile module context, it can import itself
    -- (i.e. `__init__.edh`) during the run. all imported modules can
    -- survive program crashes.
        Left !err -> do -- program crash on error
          atomically $ do
            consoleOut "Your program crashed with an error:\n"
            consoleOut $ T.pack $ show err <> "\n"
            -- the world with all modules ever imported, is still
            -- there, repeat another repl session with this world.
            -- it may not be a good idea, but just so so ...
            consoleOut "🐴🐴🐯🐯\n"
          doneRightOrRebirth
        Right !phv -> case edhUltimate phv of
            -- clean program halt, all done
          EdhNil -> atomically $ consoleOut "Well done, bye.\n"
          _      -> do -- unclean program exit
            atomically $ do
              consoleOut "Your program halted with a result:\n"
              consoleOut $ (<> "\n") $ case phv of
                EdhString msg -> msg
                _             -> T.pack $ show phv
            -- the world with all modules ever imported, is still
            -- there, repeat another repl session with this world.
            -- it may not be a good idea, but just so so ...
              consoleOut "🐴🐴🐯🐯\n"
            doneRightOrRebirth
  doneRightOrRebirth
  where consoleOut = writeTBQueue (consoleIO console) . ConsoleOut
