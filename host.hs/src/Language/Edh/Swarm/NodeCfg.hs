module Language.Edh.Swarm.NodeCfg where

-- import           Debug.Trace

import Control.Concurrent.STM
import Control.Exception
import Data.ByteString
import Data.Dynamic
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Language.Edh.EHI
import System.Directory
import System.FilePath
import System.IO
import System.Posix
import Prelude

data NodeReg = NodeReg
  { node'reg'dir :: !FilePath,
    node'cfg'default :: !Text,
    node'cfg'reg :: !(IOPD NodeKey NodeCfg)
  }

type NodeKey = Text -- MAC address with colon (:) as separator

data NodeCfg = NodeCfg
  { -- | human editable Edh source text of the config, the content is persisted
    -- as separate .edh files within the registry directory
    node'cfg'src :: !Text,
    -- | last modification time of the file used to persist this node's config
    node'cfg'persist'ts :: !EpochTime,
    -- | structured fields (a backing entity) populated by evaluating the src,
    -- then continuously updated according to heartbeats of respective node
    node'cfg'attrs :: !EntityStore,
    -- | json payload conforming to pixiecore API for node booting
    node'cfg'boot'json :: !(Maybe ByteString)
  }

createNodeRegClass :: Scope -> STM Object
createNodeRegClass !clsOuterScope =
  mkHostClass clsOuterScope "NodeReg" (allocEdhObj nregAllocator) [] $
    \ !clsScope -> do
      !mths <-
        sequence
          [ (AttrByName nm,) <$> mkHostProc clsScope vc nm hp
            | (nm, vc, hp) <-
                [ ("__repr__", EdhMethod, wrapHostProc nregReprProc),
                  ("cfgOf", EdhMethod, wrapHostProc nregCfgOfProc)
                ]
          ]
      iopdUpdate mths $ edh'scope'entity clsScope
  where
    nregAllocator ::
      "cfgDefault" !: EdhValue -> "regDir" ?: Text -> EdhObjectAllocator
    nregAllocator
      (mandatoryArg -> !cfgDefault)
      (defaultArg "./etc" -> !regDir)
      !ctorExit
      !etsCtor = edhValueStr etsCtor cfgDefault $ \ !cfgDefaultText -> do
        runEdhTx etsCtor $
          edhContIO $ do
            !regDirPath <- canonicalizePath $ T.unpack regDir
            doesDirectoryExist regDirPath >>= \case
              False ->
                atomically $
                  throwEdh etsCtor UsageError $
                    "node registry dir not existing: " <> T.pack regDirPath
              True -> do
                !reg <- iopdEmptyIO
                atomically $
                  ctorExit Nothing $
                    HostStore $
                      toDyn $ NodeReg regDirPath cfgDefaultText reg

    nregReprProc :: EdhHostProc
    nregReprProc !exit !ets =
      withThisHostObj ets $
        \(nreg :: NodeReg) ->
          exitEdh ets exit $
            EdhString $ "NodeReg<" <> T.pack (node'reg'dir nreg) <> ">"

    nregCfgOfProc :: "mac" !: Text -> EdhHostProc
    nregCfgOfProc (mandatoryArg -> !mac) !exit !ets =
      withThisHostObj ets $ \(nreg :: NodeReg) -> do
        !cfgLoaded <- iopdLookup mac $ node'cfg'reg nreg
        runEdhTx ets $
          edhContIO $ do
            let !pfp = node'reg'dir nreg </> T.unpack cfgFileName
            try (modificationTime <$> getFileStatus pfp) >>= \case
              Left (_errRead :: IOError) ->
                -- TODO attempt write fresh
                undefined
              Right !tsFile -> case cfgLoaded of
                Just (NodeCfg !src !tsCfg !attrs !boot) | tsCfg < tsFile ->
                  -- in-mem loaded cfg is up-to-date
                  atomically $ do
                    !attrsWrapper <-
                      mkScopeWrapper ets $
                        (edh'world'sandbox world) {edh'scope'entity = attrs}
                    exitEdh ets exit $
                      EdhArgsPack $
                        ArgsPack [] $
                          odFromList
                            [ (AttrByName "src", EdhString src),
                              (AttrByName "attrs", EdhObject attrsWrapper),
                              (AttrByName "boot", maybe edhNone EdhBlob boot)
                            ]
                _ ->
                  -- TODO reload from file, reuse attrs if already loaded
                  undefined
      where
        world = edh'prog'world $ edh'thread'prog ets

        cfgFileName = T.map fsMapChar mac <> ".edh"
        fsMapChar = \case
          ':' -> '-'
          '/' -> '-'
          '<' -> '-'
          '>' -> '-'
          '$' -> '-'
          '\'' -> '-'
          '\"' -> '-'
          '?' -> '-'
          '!' -> '-'
          c -> c
