
module Language.Edh.Swarm
  ( installSwarmBatteries
  , startSwarmWork
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
installSwarmBatteries (SwarmWorkStarter executable workDir workModu managerPid workerPid _wscFd) !world
  = void $ installEdhModule world "swarm/RT" $ \pgs exit -> do

    let moduScope = contextScope $ edh'context pgs
        modu      = thisObject moduScope

    let !moduArts =
          [ ("jobExecutable"  , EdhString executable)
          , ("jobWorkDir"     , EdhString workDir)
          , ("jobWorkModu"    , EdhString workModu)
          , ("swarmManagerPid", EdhDecimal $ fromIntegral managerPid)
          , ("swarmWorkerPid" , EdhDecimal $ fromIntegral workerPid)
          ]
    artsDict <- createEdhDict
      $ Map.fromList [ (EdhString k, v) | (k, v) <- moduArts ]
    updateEntityAttrs pgs (objEntity modu)
      $  [ (AttrByName k, v) | (k, v) <- moduArts ]
      ++ [(AttrByName "__exports__", artsDict)]

    exit


startSwarmWork :: IO ()
startSwarmWork = do
  starter@(SwarmWorkStarter _executable _workDir workModu managerPid workerPid wscFd) <-
    determineSwarmWorkStarter

  console <- defaultEdhConsole defaultEdhConsoleSettings
  let
    consoleOut      = writeTQueue (consoleIO console) . ConsoleOut
    consoleShutdown = writeTQueue (consoleIO console) ConsoleShutdown

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

      if wscFd == 0
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
        else
          runEdhModule world "swarm/worker" (prepareSwarmWorkerModu starter)
            >>= \case
                  Left !err -> atomically $ do
                    -- program crash on error
                    consoleOut "Swarm worker crashed with an error:\n"
                    consoleOut $ T.pack $ show err <> "\n"
                  Right !phv -> case edhUltimate phv of
                    -- clean program halt, all done
                    EdhNil ->
                      atomically $ consoleOut "Swarm worker right retired.\n"
                    -- unclean program exit
                    _ -> atomically $ do
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
    atomically $ writeTQueue (consoleIO console) ConsoleShutdown

  atomically $ if wscFd == 0
    then
      consoleOut
      $  ">> Hunting working heads for "
      <> workModu
      <> " from Edh swarm ["
      <> T.pack (show managerPid)
      <> "] <<\n"
    else
      consoleOut
      $  ">> Working out "
      <> workModu
      <> " for Edh swarm by worker ["
      <> T.pack (show workerPid)
      <> "] of forager ["
      <> T.pack (show managerPid)
      <> "] <<\n"

  consoleIOLoop console
