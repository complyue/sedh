module Main where

-- import           Debug.Trace

import Language.Edh.CHI
import Language.Edh.Net
import Language.Edh.Run
import Language.Edh.Swarm
import Language.Edh.Swarm.Starter
import Prelude

main :: IO ()
main = do
  !starter <- determineSwarmWorkStarter
  edhRunModule defaultEdhConsoleSettings "forage" $ \ !world -> do
    -- install all necessary batteries
    installEdhBatteries world
    installNetBatteries world
    installSwarmBatteries starter world

    let !consoleOut = consoleIO (edh'world'console world) . ConsoleOut
    consoleOut ">> Đ (Edh) Swarming Forager <<\n"
