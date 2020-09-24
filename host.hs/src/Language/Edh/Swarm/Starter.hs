
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
    , swarm'work'spec :: !Text
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
                                  , swarm'work'spec   = ""
                                  , swarm'manager'pid = fromIntegral pid
                                  , swarm'worker'pid  = 0
                                  , swarm'wsc'fd      = 0
                                  }

    -- single arg, will run as headhunter
    [!workScript] -> return SwarmWorkStarter
      { swarm'executable  = T.pack execPath
      , swarm'work'dir    = T.pack pwd
      , swarm'work'spec   = T.pack workScript
      , swarm'manager'pid = fromIntegral pid
      , swarm'worker'pid  = 0
      , swarm'wsc'fd      = 0
      }

    -- double args, will run as swarm worker
    [!workModu, !wscFd] -> return SwarmWorkStarter
      { swarm'executable  = T.pack execPath
      , swarm'work'dir    = T.pack pwd
      , swarm'work'spec   = T.pack workModu
      , swarm'manager'pid = fromIntegral ppid
      , swarm'worker'pid  = fromIntegral pid
      , swarm'wsc'fd      = read wscFd
      }

    _ -> error "Invalid command line args"

