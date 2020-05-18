
module Language.Edh.Swarm
  ( installSwarmBatteries
  , runSwarmWorker
  -- TODO organize and doc the re-exports
  , module Language.Edh.Swarm.Forager
  , module Language.Edh.Swarm.HeadHunter
  , module Language.Edh.Swarm.Worker
  , module Language.Edh.Solo.BrewHere
  )
where

import           Prelude
-- import           Debug.Trace

import           Control.Exception
import           Control.Monad.Reader
import           Control.Concurrent
import           Control.Concurrent.STM

import qualified Data.Text                     as T

import           Language.Edh.EHI
import           Language.Edh.Net

import           Language.Edh.Swarm.Forager
import           Language.Edh.Swarm.HeadHunter
import           Language.Edh.Swarm.Worker
import           Language.Edh.Solo.BrewHere


installSwarmBatteries :: EdhWorld -> IO ()
installSwarmBatteries !world =

  void $ installEdhModule world "swarm/RT" $ \pgs exit -> do

    let moduScope = contextScope $ edh'context pgs
        modu      = thisObject moduScope

    exit


runSwarmWorker :: IO ()
runSwarmWorker = do

  console <- defaultEdhConsole defaultEdhConsoleSettings
  let consoleOut = writeTQueue (consoleIO console) . ConsoleOut

  void $ forkFinally (workProg console) $ \result -> do
    case result of
      Left (e :: SomeException) ->
        atomically $ consoleOut $ "💥 " <> T.pack (show e)
      Right _ -> pure ()
    -- shutdown console IO anyway
    atomically $ writeTQueue (consoleIO console) ConsoleShutdown

  atomically $ do
    consoleOut ">> Swarming Edh Worker <<\n"

  consoleIOLoop console

 where
  workProg :: EdhConsole -> IO ()
  workProg !console = do

    -- create the world, we always work with this world no matter how
    -- many times the Edh programs crash
    world <- createEdhWorld console
    installEdhBatteries world

    -- install batteries provided by nedh
    installNetBatteries world

    -- install batteries provided by sedh
    installSwarmBatteries world

    runEdhModule world "swarm/worker" edhModuleAsIs >>= \case
      Left !err -> -- program crash on error
                   atomically $ do
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
   where
    consoleOut      = writeTQueue (consoleIO console) . ConsoleOut
    consoleShutdown = writeTQueue (consoleIO console) ConsoleShutdown
