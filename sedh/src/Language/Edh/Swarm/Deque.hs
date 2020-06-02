
module Language.Edh.Swarm.Deque where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader
import           Control.Concurrent.STM

import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as Map
import           Data.Dynamic

import qualified Data.Dequeue                  as Q

import           Language.Edh.EHI


type Deque = Q.BankersDequeue EdhValue


-- | host constructor Deque()
dequeHostCtor
  :: EdhProgState
  -> ArgsPack  -- ctor args, if __init__() is provided, will go there too
  -> TVar (Map.HashMap AttrKey EdhValue)  -- out-of-band attr store 
  -> (Dynamic -> STM ())  -- in-band data to be written to entity store
  -> STM ()
dequeHostCtor !pgsCtor _ !obs !ctorExit = do
  let !scope = contextScope $ edh'context pgsCtor
  !qv     <- newTVar (Q.empty :: Deque)
  methods <- sequence
    [ (AttrByName nm, ) <$> mkHostProc scope vc nm hp mthArgs
    | (nm, vc, hp, mthArgs) <-
      [ ("__null__" , EdhMethod, qNullProc     , PackReceiver [])
      , ("length"   , EdhMethod, qLengthProc   , PackReceiver [])
      , ("front"    , EdhMethod, qFrontProc    , PackReceiver [])
      , ("pushFront", EdhMethod, qPushFrontProc, PackReceiver [])
      , ("popFront" , EdhMethod, qPopFrontProc , PackReceiver [])
      , ("back"     , EdhMethod, qBackProc     , PackReceiver [])
      , ("pushBack" , EdhMethod, qPushBackProc , PackReceiver [])
      , ("popBack"  , EdhMethod, qPopBackProc  , PackReceiver [])
      ]
    ]
  modifyTVar' obs
    $  Map.union
    $  Map.fromList
    $  methods
    ++ [(AttrByName "__repr__", EdhString "Deque()")]
  ctorExit $ toDyn qv

 where

  qNullProc :: EdhProcedure
  qNullProc _ !exit = do
    pgs <- ask
    let this = thisObject $ contextScope $ edh'context pgs
        es   = entity'store $ objEntity this
    contEdhSTM $ do
      esd <- readTVar es
      case fromDynamic esd of
        Nothing ->
          throwEdhSTM pgs UsageError $ "bug: this is not a deque : " <> T.pack
            (show esd)
        Just !(qv :: TVar Deque) ->
          readTVar qv >>= \q -> exitEdhSTM pgs exit $ EdhBool $ Q.null q

  qLengthProc :: EdhProcedure
  qLengthProc _ !exit = do
    pgs <- ask
    let this = thisObject $ contextScope $ edh'context pgs
        es   = entity'store $ objEntity this
    contEdhSTM $ do
      esd <- readTVar es
      case fromDynamic esd of
        Nothing ->
          throwEdhSTM pgs UsageError $ "bug: this is not a deque : " <> T.pack
            (show esd)
        Just !(qv :: TVar Deque) -> readTVar qv
          >>= \q -> exitEdhSTM pgs exit $ EdhDecimal $ fromIntegral $ length q

  qFrontProc :: EdhProcedure
  qFrontProc _ !exit = do
    pgs <- ask
    let this = thisObject $ contextScope $ edh'context pgs
        es   = entity'store $ objEntity this
    contEdhSTM $ do
      esd <- readTVar es
      case fromDynamic esd of
        Nothing ->
          throwEdhSTM pgs UsageError $ "bug: this is not a deque : " <> T.pack
            (show esd)
        Just !(qv :: TVar Deque) ->
          readTVar qv >>= \q -> exitEdhSTM pgs exit $ fromMaybe nil $ Q.first q

  qBackProc :: EdhProcedure
  qBackProc _ !exit = do
    pgs <- ask
    let this = thisObject $ contextScope $ edh'context pgs
        es   = entity'store $ objEntity this
    contEdhSTM $ do
      esd <- readTVar es
      case fromDynamic esd of
        Nothing ->
          throwEdhSTM pgs UsageError $ "bug: this is not a deque : " <> T.pack
            (show esd)
        Just !(qv :: TVar Deque) ->
          readTVar qv >>= \q -> exitEdhSTM pgs exit $ fromMaybe nil $ Q.last q

  qPushFrontProc :: EdhProcedure
  qPushFrontProc (ArgsPack [val] !kwargs) !exit | Map.null kwargs = do
    pgs <- ask
    let this = thisObject $ contextScope $ edh'context pgs
        es   = entity'store $ objEntity this
    contEdhSTM $ do
      esd <- readTVar es
      case fromDynamic esd of
        Nothing ->
          throwEdhSTM pgs UsageError $ "bug: this is not a deque : " <> T.pack
            (show esd)
        Just !(qv :: TVar Deque) -> do
          modifyTVar' qv $ flip Q.pushFront val
          exitEdhSTM pgs exit val
  qPushFrontProc _ _ = throwEdh UsageError $ "Invalid args"

  qPopFrontProc :: EdhProcedure
  qPopFrontProc _ !exit = do
    pgs <- ask
    let this = thisObject $ contextScope $ edh'context pgs
        es   = entity'store $ objEntity this
    contEdhSTM $ do
      esd <- readTVar es
      case fromDynamic esd of
        Nothing ->
          throwEdhSTM pgs UsageError $ "bug: this is not a deque : " <> T.pack
            (show esd)
        Just !(qv :: TVar Deque) -> readTVar qv >>= \q -> case Q.popFront q of
          Nothing        -> exitEdhSTM pgs exit nil
          Just (val, q') -> do
            writeTVar qv q'
            exitEdhSTM pgs exit val

  qPushBackProc :: EdhProcedure
  qPushBackProc (ArgsPack [val] !kwargs) !exit | Map.null kwargs = do
    pgs <- ask
    let this = thisObject $ contextScope $ edh'context pgs
        es   = entity'store $ objEntity this
    contEdhSTM $ do
      esd <- readTVar es
      case fromDynamic esd of
        Nothing ->
          throwEdhSTM pgs UsageError $ "bug: this is not a deque : " <> T.pack
            (show esd)
        Just !(qv :: TVar Deque) -> do
          modifyTVar' qv $ flip Q.pushBack val
          exitEdhSTM pgs exit val
  qPushBackProc _ _ = throwEdh UsageError $ "Invalid args"

  qPopBackProc :: EdhProcedure
  qPopBackProc _ !exit = do
    pgs <- ask
    let this = thisObject $ contextScope $ edh'context pgs
        es   = entity'store $ objEntity this
    contEdhSTM $ do
      esd <- readTVar es
      case fromDynamic esd of
        Nothing ->
          throwEdhSTM pgs UsageError $ "bug: this is not a deque : " <> T.pack
            (show esd)
        Just !(qv :: TVar Deque) -> readTVar qv >>= \q -> case Q.popBack q of
          Nothing        -> exitEdhSTM pgs exit nil
          Just (val, q') -> do
            writeTVar qv q'
            exitEdhSTM pgs exit val

