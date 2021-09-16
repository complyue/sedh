module Main where

-- import           Debug.Trace

import Language.Edh.CHI
import Language.Edh.Net
import Language.Edh.Run
import Language.Edh.Swarm
import Prelude

main :: IO ()
main = edhRunModule defaultEdhConsoleSettings "swarm/cc" $
  \ !world -> do
    -- install all necessary batteries
    installEdhBatteries world
    installNetBatteries world
    installSwarmCtrlBatteries world
