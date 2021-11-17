module Language.Edh.Swarm.NodeCfg where

-- import           Debug.Trace

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
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
    node'cfg'default :: !(Text, TVar (EpochTime, Text)),
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

wrapNodeCfg :: NodeCfg -> Edh EdhValue
wrapNodeCfg (NodeCfg !src _tsCfg !attrs !boot) =
  wrapScopeM attrs >>= \ !attrsWrapper ->
    return $
      EdhArgsPack $
        ArgsPack [] $
          odFromList
            [ (AttrByName "src", EdhString src),
              (AttrByName "attrs", EdhObject attrsWrapper),
              (AttrByName "boot", maybe edhNone EdhBlob boot)
            ]

createNodeRegClass :: Edh Object
createNodeRegClass =
  mkEdhClass "NodeReg" (allocObjM nregAllocator) [] $ do
    !mths <-
      sequence
        [ (AttrByName nm,) <$> mkEdhProc vc nm hp
          | (nm, vc, hp) <-
              [ ("__repr__", EdhMethod, wrapEdhProc nregReprProc),
                ("cfgOf", EdhMethod, wrapEdhProc nregCfgOfProc),
                ("saveCfgSrc", EdhMethod, wrapEdhProc saveCfgSrcProc),
                ("knownNodes", EdhGnrtor, wrapEdhProc nregKnownNodesProc)
              ]
        ]

    !clsScope <- contextScope . edh'context <$> edhThreadState
    iopdUpdateEdh mths $ edh'scope'entity clsScope
  where
    nregAllocator ::
      "cfgDefault" !: EdhValue ->
      "regDir" ?: Text ->
      Edh (Maybe Unique, ObjectStore)
    nregAllocator
      (mandatoryArg -> !cfgDefault)
      (defaultArg "./etc" -> !regDir) =
        edhValueStrM cfgDefault >>= \ !cfgDefaultText -> do
          !regDirPath <- liftIO $ canonicalizePath $ T.unpack regDir
          liftIO (doesDirectoryExist regDirPath) >>= \case
            False ->
              throwEdhM UsageError $
                "node registry dir not existing: " <> T.pack regDirPath
            True -> do
              !defVar <- newTVarEdh (0, "")
              !reg <- iopdEmptyEdh
              let !nreg = NodeReg regDirPath (cfgDefaultText, defVar) reg
              void $ liftIO $ prepareDefaultCfg nreg
              return (Nothing, HostStore $ toDyn nreg)

    withThisReg :: forall r. (Object -> NodeReg -> Edh r) -> Edh r
    withThisReg withCfg = do
      !this <- edh'scope'this . contextScope . edh'context <$> edhThreadState
      case fromDynamic =<< dynamicHostData this of
        Nothing -> throwEdhM EvalError "bug: this is not an NodeReg"
        Just !col -> withCfg this col

    nregReprProc :: Edh EdhValue
    nregReprProc = withThisReg $ \_this !nreg ->
      return $ EdhString $ "NodeReg<" <> T.pack (node'reg'dir nreg) <> ">"

    saveCfgSrcProc :: "mac" !: Text -> "src" !: Text -> Edh EdhValue
    saveCfgSrcProc (mandatoryArg -> !mac) (mandatoryArg -> !src) =
      withThisReg $ \_this !nreg -> do
        !world <- edh'prog'world <$> edhProgramState
        (pfp, attrs, _) <- prepareNodeCfg nreg mac
        !ncfg <- liftIO $ do
          atomically $
            iopdInsert
              (AttrByName "mac")
              (EdhString mac)
              (edh'scope'entity attrs)
          !boot <- saveNodeCfg pfp world (T.pack pfp) src attrs
          -- get file mod time after written
          !tsFile <- modificationTime <$> getFileStatus pfp
          -- record & return to Edh
          let !ncfg = NodeCfg src tsFile attrs boot
          atomically $ do
            iopdInsert mac ncfg $ node'cfg'reg nreg
          return ncfg
        wrapNodeCfg ncfg

    nregCfgOfProc :: "mac" !: Text -> Edh EdhValue
    nregCfgOfProc (mandatoryArg -> !mac) = withThisReg $ \_this !nreg -> do
      !world <- edh'prog'world <$> edhProgramState
      (pfp, attrs, cfgLoaded) <- prepareNodeCfg nreg mac
      !ncfg <- liftIO $ do
        try (modificationTime <$> getFileStatus pfp) >>= \case
          Left (_errRead :: IOError) -> do
            -- assuming non-existing, try write fresh. the write should fail
            -- similarly to stat, due to other IO problems, e.g. permission,
            -- fs corruption etc. and such errors in writting the file will
            -- propagate to Edh code this time.
            atomically $
              iopdInsert
                (AttrByName "mac")
                (EdhString mac)
                (edh'scope'entity attrs)
            !src <- prepareDefaultCfg nreg
            !boot <- saveNodeCfg pfp world (T.pack pfp) src attrs
            -- get file mod time after written
            !tsFile <- modificationTime <$> getFileStatus pfp
            -- record & return to Edh
            atomically $ do
              let !ncfg = NodeCfg src tsFile attrs boot
              iopdInsert mac ncfg $ node'cfg'reg nreg
              return ncfg
          Right !tsFile -> case cfgLoaded of
            Just !ncfg
              | node'cfg'persist'ts ncfg >= tsFile ->
                -- in-mem loaded cfg is up-to-date
                atomically $ return ncfg
            _ -> do
              !src <- TE.decodeUtf8 <$> B.readFile pfp
              -- load disk file for updated cfg
              atomically $
                iopdInsert
                  (AttrByName "mac")
                  (EdhString mac)
                  (edh'scope'entity attrs)
              !boot <- loadNodeCfg world (T.pack pfp) src attrs
              -- record & return to Edh
              atomically $ do
                let !ncfg = NodeCfg src tsFile attrs boot
                iopdInsert mac ncfg $ node'cfg'reg nreg
                return ncfg
      wrapNodeCfg ncfg

    nregKnownNodesProc :: Edh EdhValue
    nregKnownNodesProc = withThisReg $ \_this !nreg ->
      iopdToListEdh (node'cfg'reg nreg) >>= yieldNext
      where
        yieldNext :: [(NodeKey, NodeCfg)] -> Edh EdhValue
        yieldNext [] = return nil
        yieldNext ((_mac, !cfg) : rest) = do
          !cfgVal <- wrapNodeCfg cfg
          yieldM cfgVal (const $ yieldNext rest)

prepareNodeCfg :: NodeReg -> Text -> Edh (FilePath, Scope, Maybe NodeCfg)
prepareNodeCfg !nreg !mac = do
  let !pfp = cfgFilePathOf (node'reg'dir nreg) mac
      srcName = T.pack pfp
  iopdLookupEdh mac (node'cfg'reg nreg) >>= \case
    Just !ncfg -> return (pfp, node'cfg'attrs ncfg, Just ncfg)
    Nothing -> do
      !sb <- newSandboxM
      let !sbp =
            (edh'scope'proc sb)
              { edh'procedure'decl =
                  ProcDecl
                    { edh'procedure'addr =
                        AttrAddrSrc (NamedAttr srcName) noSrcRange,
                      edh'procedure'args =
                        NullaryReceiver,
                      edh'procedure'anno = Nothing,
                      edh'procedure'body =
                        StmtSrc VoidStmt noSrcRange,
                      edh'procedure'loc =
                        SrcLoc (SrcDoc srcName) zeroSrcRange
                    }
              }
      let !attrs = sb {edh'scope'proc = sbp}
      return (pfp, attrs, Nothing)

prepareDefaultCfg :: NodeReg -> IO Text
prepareDefaultCfg !nreg = do
  let (!defSrc, !defVar) = node'cfg'default nreg
      !pfpDefault = cfgFilePathOf (node'reg'dir nreg) "default"
  try (modificationTime <$> getFileStatus pfpDefault) >>= \case
    Left (_errRead :: IOError) -> do
      -- write default node cfg file, assuming non-existence,
      -- other problems will err out in our attempt in writting
      B.writeFile pfpDefault $ TE.encodeUtf8 defSrc
      -- get file mod time after written
      !tsFile <- modificationTime <$> getFileStatus pfpDefault
      atomically $ writeTVar defVar (tsFile, defSrc)
      return defSrc
    Right !tsFile -> do
      (tsLoaded, srcLoaded) <- readTVarIO defVar
      if tsLoaded >= tsFile
        then return srcLoaded
        else do
          !src <- TE.decodeUtf8 <$> B.readFile pfpDefault
          atomically $ writeTVar defVar (tsFile, src)
          return src

saveNodeCfg ::
  FilePath -> EdhWorld -> Text -> Text -> Scope -> IO (Maybe ByteString)
saveNodeCfg !pfp !world !srcName !src !attrs = do
  B.writeFile pfp $ TE.encodeUtf8 src
  loadNodeCfg world srcName src attrs

loadNodeCfg :: EdhWorld -> Text -> Text -> Scope -> IO (Maybe ByteString)
loadNodeCfg !world !srcName !src !attrs = do
  !result <- newTVarIO Nothing
  void $
    runProgramM' world $
      runNested $ do
        let bootProc :: ArgsPack -> Edh EdhValue
            bootProc !apk =
              edhValueJsonM (EdhArgsPack apk) >>= \ !jsonStr -> do
                writeTVarEdh result $ Just $ TE.encodeUtf8 jsonStr
                return nil
        !effMths <-
          sequence
            [ (AttrByName nm,)
                <$> mkEdhProc vc nm hp
              | (nm, vc, hp) <-
                  [ -- record boot parameters conforming to pixie api
                    ("boot", EdhMethod, (bootProc, NullaryReceiver))
                  ]
            ]
        let !effArts = effMths
        prepareEffStoreM >>= iopdUpdateEdh effArts
        runNestedIn attrs $ evalSrcM srcName src
  readTVarIO result

cfgFilePathOf :: FilePath -> Text -> FilePath
cfgFilePathOf !regDir !mac = regDir </> cfgFileName
  where
    cfgFileName = fsMapChar <$> T.unpack mac <> ".edh"
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
