module Language.Edh.Swarm.NodeCfg where

-- import           Debug.Trace

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString
import qualified Data.ByteString as B
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
    -- | structured fields (in a sandbox) populated by evaluating the src,
    -- then continuously updated according to heartbeats of respective node
    node'cfg'attrs :: !Scope,
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
                srcName = T.pack pfp
                createSandbox = do
                  !sb <- newSandbox ets
                  let !sbp =
                        (edh'scope'proc sb)
                          { edh'procedure'decl =
                              ProcDecl
                                { edh'procedure'addr = AttrAddrSrc (NamedAttr srcName) noSrcRange,
                                  edh'procedure'args = WildReceiver,
                                  edh'procedure'body = StmtSrc VoidStmt noSrcRange,
                                  edh'procedure'loc = SrcLoc (SrcDoc srcName) zeroSrcRange
                                }
                          }
                  return sb {edh'scope'proc = sbp}
            try (modificationTime <$> getFileStatus pfp) >>= \case
              Left (_errRead :: IOError) -> do
                let !src = node'cfg'default nreg
                -- assuming non-existing, try write fresh. the write should fail
                -- similarly to stat, due to other IO problems, e.g. permission,
                -- fs corruption etc. and such errors in writting the file will
                -- propagate to Edh code this time.
                B.writeFile pfp $ TE.encodeUtf8 src
                -- get file mod time after written
                !tsFile <- modificationTime <$> getFileStatus pfp
                -- load into a new attrs sandbox
                !attrs <- atomically createSandbox
                atomically $
                  iopdInsert
                    (AttrByName "mac")
                    (EdhString mac)
                    (edh'scope'entity attrs)
                !boot <- loadNodeCfg world srcName src attrs
                -- record & return to Edh
                atomically $ do
                  let !ncfg = NodeCfg src tsFile attrs boot
                  iopdInsert mac ncfg $ node'cfg'reg nreg
                  exitWithCfg ncfg
              Right !tsFile -> case cfgLoaded of
                Just !ncfg
                  | node'cfg'persist'ts ncfg >= tsFile ->
                    -- in-mem loaded cfg is up-to-date
                    atomically $ exitWithCfg ncfg
                _ -> do
                  !src <- TE.decodeUtf8 <$> B.readFile pfp
                  !attrs <- case cfgLoaded of
                    Nothing -> atomically createSandbox
                    Just !ncfg -> return $ node'cfg'attrs ncfg
                  atomically $
                    iopdInsert
                      (AttrByName "mac")
                      (EdhString mac)
                      (edh'scope'entity attrs)
                  !boot <- loadNodeCfg world srcName src attrs
                  -- record & return to Edh
                  atomically $ do
                    let !ncfg = NodeCfg src tsFile attrs boot
                    iopdInsert mac ncfg $ node'cfg'reg nreg
                    exitWithCfg ncfg
      where
        world = edh'prog'world $ edh'thread'prog ets

        exitWithCfg (NodeCfg !src _tsCfg !attrs !boot) = do
          !attrsWrapper <- mkScopeWrapper ets attrs
          exitEdh ets exit $
            EdhArgsPack $
              ArgsPack [] $
                odFromList
                  [ (AttrByName "src", EdhString src),
                    (AttrByName "attrs", EdhObject attrsWrapper),
                    (AttrByName "boot", maybe edhNone EdhBlob boot)
                  ]

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

loadNodeCfg :: EdhWorld -> Text -> Text -> Scope -> IO (Maybe ByteString)
loadNodeCfg !world !srcName !src !attrs = do
  !result <- newTVarIO Nothing
  void $
    runEdhProgram' world $
      pushEdhStack $ \ !etsEffs -> do
        let effsScope = contextScope $ edh'context etsEffs
            bootProc :: ArgsPack -> EdhHostProc
            bootProc !apk !exit !ets =
              edhValueJson ets (EdhArgsPack apk) $ \ !jsonStr -> do
                writeTVar result $ Just $ TE.encodeUtf8 jsonStr
                exitEdh ets exit nil
        !effMths <-
          sequence
            [ (AttrByName nm,)
                <$> mkHostProc effsScope vc nm hp
              | (nm, vc, hp) <-
                  [ -- record boot parameters conforming to pixie api
                    ("boot", EdhMethod, (bootProc, WildReceiver))
                  ]
            ]
        let !effArts = effMths
        prepareEffStore etsEffs (edh'scope'entity effsScope)
          >>= iopdUpdate effArts
        runEdhTx etsEffs $ pushEdhStack' attrs $ evalEdh srcName src endOfEdh
  readTVarIO result
