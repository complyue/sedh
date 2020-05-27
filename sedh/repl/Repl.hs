
module Repl where

import           Prelude
-- import           Debug.Trace

import           Control.Concurrent.STM

import qualified Data.Text                     as T

import           Language.Edh.EHI
import           Language.Edh.Net

import           Language.Edh.Swarm


-- the Edh module name to be run as the console
consoleModule :: String
consoleModule = "swarm"


-- | Manage lifecycle of Edh programs during the repl session
edhProgLoop :: EdhConsole -> IO ()
edhProgLoop !console = do
  starter <- determineSwarmWorkStarter

  -- create the world, we always work with this world no matter how
  -- many times the Edh programs crash
  world   <- createEdhWorld console
  installEdhBatteries world

  -- install batteries provided by nedh
  installNetBatteries world

  -- install batteries provided by sedh
  installSwarmBatteries starter world

  -- here being the host interpreter, we loop infinite runs of the Edh
  -- console REPL program, unless cleanly shutdown, for resilience
  let doneRightOrRebirth =
        runEdhModule world consoleModule edhModuleAsIs >>= \case
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
            EdhNil -> atomically $ do
              -- clean program halt, all done
              consoleOut "Well done, bye.\n"
              consoleShutdown
            _ -> do -- unclean program exit
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
 where
  consoleOut      = writeTQueue (consoleIO console) . ConsoleOut
  consoleShutdown = writeTQueue (consoleIO console) ConsoleShutdown
