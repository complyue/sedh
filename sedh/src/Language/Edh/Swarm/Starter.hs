
module Language.Edh.Swarm.Starter where

import           Prelude
-- import           Debug.Trace

import           System.Environment
import           System.Directory
import           System.Posix.Process

import           Data.Text                      ( Text )
import qualified Data.Text                     as T


data SwarmWorkStarter = SwarmWorkStarter {
      swarm'executable :: !Text
    , swarm'work'dir :: !Text
    , swarm'work'module :: !Text
    , swarm'manager'pid :: !Int
    , swarm'worker'pid :: !Int
    , swarm'wsc'fd :: !Int
  } deriving (Eq, Show)

determineSwarmWorkStarter :: IO SwarmWorkStarter
determineSwarmWorkStarter = do
  !execPath <- getExecutablePath
  !pwd      <- getCurrentDirectory
  !ppid     <- getParentProcessID
  !pid      <- getProcessID
  getArgs >>= \case
    -- no arg, will run as forager or repl
    [] -> return SwarmWorkStarter { swarm'executable  = T.pack execPath
                                  , swarm'work'dir    = T.pack pwd
                                  , swarm'work'module = ""
                                  , swarm'manager'pid = fromIntegral pid
                                  , swarm'worker'pid  = 0
                                  , swarm'wsc'fd      = 0
                                  }
    -- single arg, will run as headhunter
    [workModule] -> return SwarmWorkStarter
      { swarm'executable  = T.pack execPath
      , swarm'work'dir    = T.pack pwd
      , swarm'work'module = T.pack workModule
      , swarm'manager'pid = fromIntegral pid
      , swarm'worker'pid  = 0
      , swarm'wsc'fd      = 0
      }
    -- double args, will run as swarm worker
    [workModule, wscFd] -> return SwarmWorkStarter
      { swarm'executable  = T.pack execPath
      , swarm'work'dir    = T.pack pwd
      , swarm'work'module = T.pack workModule
      , swarm'manager'pid = fromIntegral ppid
      , swarm'worker'pid  = fromIntegral pid
      , swarm'wsc'fd      = read wscFd
      }
    _ -> error "Invalid command line args"

