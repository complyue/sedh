
module Language.Edh.Swarm
  ( installSwarmBatteries
  -- TODO organize and doc the re-exports
  , module Language.Edh.Swarm.Forager
  , module Language.Edh.Swarm.HeadHunter
  , module Language.Edh.Swarm.Worker
  , module Language.Edh.Solo.BrewHere
  )
where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader

import           Language.Edh.EHI

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

