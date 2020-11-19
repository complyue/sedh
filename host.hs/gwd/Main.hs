module Main where

import Language.Edh.Swarm (startSwarmWork)
import Prelude

main :: IO ()
main = startSwarmWork $ const $ return ()
