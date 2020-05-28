
module Repl where

import           Prelude
-- import           Debug.Trace

import           Control.Concurrent.STM

import qualified Data.Text                     as T

import           Language.Edh.EHI


-- the Edh module name to be run as the console
consoleModule :: String
consoleModule = "swarm"


-- | Manage lifecycle of Edh programs during the repl session
swarmRepl :: EdhConsole -> EdhWorld -> IO ()
swarmRepl !console !world = do
  atomically $ do
    consoleOut ">> Get Work Done - by an Edh swarm <<\n"
    consoleOut
      "* Blank Screen Syndrome ? Take the Tour as your companion, checkout:\n"
    consoleOut "  https://github.com/e-wrks/sedh/tree/master/Tour\n"

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
