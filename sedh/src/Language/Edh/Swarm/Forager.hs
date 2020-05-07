
module Language.Edh.Swarm.Forager where

import           Prelude
-- import           Debug.Trace

import           System.IO

import           Control.Exception
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM

import           Control.Monad.Reader

import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as Map
import           Data.Dynamic

import           Network.Socket
import           Network.Socket.ByteString

import qualified Data.Lossless.Decimal         as D

import           Language.Edh.EHI

import           Language.Edh.Net


isMultiCastAddr :: AddrInfo -> Bool
isMultiCastAddr (AddrInfo _ _ _ _ (SockAddrInet _ !hostAddr) _) =
  case hostAddressToTuple hostAddr of
    (n, _, _, _) -> 224 <= n && n <= 239
isMultiCastAddr (AddrInfo _ _ _ _ SockAddrInet6{} _) =
  error "IPv6 not supported yet"
isMultiCastAddr _ = False


type SwarmAddr = Text
type SwarmPort = Int

data EdhForager = EdhForager {
    -- the import spec of the module to run as the forager
      edh'forage'modu :: !Text
    -- local network interface to bind
    , edh'swarm'addr :: !SwarmAddr
    -- local network port to bind
    , edh'swarm'port :: !SwarmPort
    -- actually listened network addresses
    , edh'forage'addrs :: !(TMVar [AddrInfo])
    -- end-of-life status
    , edh'forage'eol :: !(TMVar (Either SomeException ()))
    -- forage module initializer, must callable if not nil
    , edh'forage'init :: !EdhValue
    -- each consulted worksource is sunk into this
    , edh'worksources :: !EventSink
  }


-- | host constructor Forager()
foragerCtor
  :: Class
  -> EdhProgState
  -> ArgsPack  -- ctor args, if __init__() is provided, will go there too
  -> TVar (Map.HashMap AttrKey EdhValue)  -- out-of-band attr store
  -> (Dynamic -> STM ())  -- in-band data to be written to entity store
  -> STM ()
foragerCtor !peerClass !pgsCtor !apk !obs !ctorExit =
  case
      parseArgsPack
        (Nothing, "237.3.7.21" :: SwarmAddr, 3722 :: SwarmPort, nil, Nothing)
        parseCtorArgs
        apk
    of
      Left err -> throwEdhSTM pgsCtor UsageError err
      Right (Nothing, _, _, _, _) ->
        throwEdhSTM pgsCtor UsageError "missing forage module"
      Right (Just forage, addr, port, __peer_init__, maybeClients) -> do
        forageAddrs <- newEmptyTMVar
        forageEoL   <- newEmptyTMVar
        worksources <- maybe newEventSink return maybeClients
        let !forager = EdhForager { edh'forage'modu  = forage
                                  , edh'swarm'addr   = addr
                                  , edh'swarm'port   = port
                                  , edh'forage'addrs = forageAddrs
                                  , edh'forage'eol   = forageEoL
                                  , edh'forage'init  = __peer_init__
                                  , edh'worksources  = worksources
                                  }
            !scope = contextScope $ edh'context pgsCtor
        methods <- sequence
          [ (AttrByName nm, ) <$> mkHostProc scope vc nm hp args
          | (nm, vc, hp, args) <-
            [ ("addrs", EdhMethod, addrsProc, PackReceiver [])
            , ("eol"  , EdhMethod, eolProc  , PackReceiver [])
            , ("join" , EdhMethod, joinProc , PackReceiver [])
            , ("stop" , EdhMethod, stopProc , PackReceiver [])
            ]
          ]
        modifyTVar' obs
          $  Map.union
          $  Map.fromList
          $  methods
          ++ [ (AttrByName "worksources", EdhSink worksources)
             , ( AttrByName "__repr__"
               , EdhString
               $  "Forager("
               <> T.pack (show forage)
               <> ", "
               <> T.pack (show addr)
               <> ", "
               <> T.pack (show port)
               <> ")"
               )
             ]
        edhPerformIO
            pgsCtor
            (forkFinally
              (forageThread forager)
              -- mark forage end-of-life, worksource sink end-of-stream, anyway after
              ( atomically
              . (publishEvent worksources nil <*)
              . tryPutTMVar forageEoL
              )
            )
          $ \_ -> ctorExit $ toDyn forager
 where
  parseCtorArgs =
    ArgsPackParser
        [ \arg (_, addr', port', init', worksources') -> case edhUltimate arg of
          EdhString !forage ->
            Right (Just forage, addr', port', init', worksources')
          _ -> Left "Invalid forage"
        , \arg (forage', _, port', init', worksources') ->
          case edhUltimate arg of
            EdhString addr -> Right (forage', addr, port', init', worksources')
            _              -> Left "Invalid addr"
        , \arg (forage', addr', _, init', worksources') ->
          case edhUltimate arg of
            EdhDecimal d -> case D.decimalToInteger d of
              Just port ->
                Right (forage', addr', fromIntegral port, init', worksources')
              Nothing -> Left "port must be integer"
            _ -> Left "Invalid port"
        ]
      $ Map.fromList
          [ ( "addr"
            , \arg (forage', _, port', init', worksources') ->
              case edhUltimate arg of
                EdhString addr ->
                  Right (forage', addr, port', init', worksources')
                _ -> Left "Invalid addr"
            )
          , ( "port"
            , \arg (forage', addr', _, init', worksources') ->
              case edhUltimate arg of
                EdhDecimal d -> case D.decimalToInteger d of
                  Just port -> Right
                    (forage', addr', fromIntegral port, init', worksources')
                  Nothing -> Left "port must be integer"
                _ -> Left "Invalid port"
            )
          , ( "init"
            , \arg (forage', addr', port', _, worksources') ->
              case edhUltimate arg of
                EdhNil -> Right (forage', addr', port', nil, worksources')
                mth@EdhMethod{} ->
                  Right (forage', addr', port', mth, worksources')
                mth@EdhIntrpr{} ->
                  Right (forage', addr', port', mth, worksources')
                _ -> Left "Invalid init"
            )
          , ( "worksources"
            , \arg (forage', addr', port', init', _) -> case edhUltimate arg of
              EdhSink sink -> Right (forage', addr', port', init', Just sink)
              _            -> Left "Invalid worksources"
            )
          ]

  addrsProc :: EdhProcedure
  addrsProc _ !exit = do
    pgs <- ask
    let ctx  = edh'context pgs
        this = thisObject $ contextScope ctx
        es   = entity'store $ objEntity this
    contEdhSTM $ do
      esd <- readTVar es
      case fromDynamic esd :: Maybe EdhForager of
        Nothing ->
          throwEdhSTM pgs UsageError $ "bug: this is not a forager : " <> T.pack
            (show esd)
        Just !forager ->
          edhPerformIO pgs (atomically $ readTMVar (edh'forage'addrs forager))
            $ \addrs ->
                exitEdhSTM pgs exit
                  $   EdhTuple
                  $   EdhString
                  .   T.pack
                  .   show
                  .   addrAddress
                  <$> addrs

  eolProc :: EdhProcedure
  eolProc _ !exit = do
    pgs <- ask
    let ctx  = edh'context pgs
        this = thisObject $ contextScope ctx
        es   = entity'store $ objEntity this
    contEdhSTM $ do
      esd <- readTVar es
      case fromDynamic esd :: Maybe EdhForager of
        Nothing ->
          throwEdhSTM pgs UsageError $ "bug: this is not a forager : " <> T.pack
            (show esd)
        Just !forager -> tryReadTMVar (edh'forage'eol forager) >>= \case
          Nothing         -> exitEdhSTM pgs exit $ EdhBool False
          Just (Left  e ) -> toEdhError pgs e $ \exv -> exitEdhSTM pgs exit exv
          Just (Right ()) -> exitEdhSTM pgs exit $ EdhBool True

  joinProc :: EdhProcedure
  joinProc _ !exit = do
    pgs <- ask
    let ctx  = edh'context pgs
        this = thisObject $ contextScope ctx
        es   = entity'store $ objEntity this
    contEdhSTM $ do
      esd <- readTVar es
      case fromDynamic esd :: Maybe EdhForager of
        Nothing ->
          throwEdhSTM pgs UsageError $ "bug: this is not a forager : " <> T.pack
            (show esd)
        Just !forager ->
          edhPerformIO pgs (atomically $ readTMVar (edh'forage'eol forager))
            $ \case
                Left  e  -> toEdhError pgs e $ \exv -> edhThrowSTM pgs exv
                Right () -> exitEdhSTM pgs exit nil

  stopProc :: EdhProcedure
  stopProc _ !exit = do
    pgs <- ask
    let ctx  = edh'context pgs
        this = thisObject $ contextScope ctx
        es   = entity'store $ objEntity this
    contEdhSTM $ do
      esd <- readTVar es
      case fromDynamic esd :: Maybe EdhForager of
        Nothing ->
          throwEdhSTM pgs UsageError $ "bug: this is not a forager : " <> T.pack
            (show esd)
        Just !forager -> do
          stopped <- tryPutTMVar (edh'forage'eol forager) $ Right ()
          exitEdhSTM pgs exit $ EdhBool stopped


  forageThread :: EdhForager -> IO ()
  forageThread (EdhForager !forageModu !forageAddr !foragePort !forageAddrs !forageEoL !__peer_init__ !worksources)
    = do
      forageThId <- myThreadId
      void $ forkIO $ do -- async terminate the foraging thread on stop signal
        _ <- atomically $ readTMVar forageEoL
        killThread forageThId
      addr <- resolveServAddr
      bracket (open addr) close forageFrom
   where
    ctx             = edh'context pgsCtor
    world           = contextWorld ctx

    resolveServAddr = do
      let hints = defaultHints { addrSocketType = Datagram }
      addr : _ <- getAddrInfo (Just hints)
                              (Just $ T.unpack forageAddr)
                              (Just (show foragePort))
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      if isMultiCastAddr addr
        then do
          setSocketOption sock ReuseAddr 1
          -- setSocketOption sock ReusePort 1
          -- todo support mcast 
          --   for now `setSockOpt` is not released yet, 
          --   HIE ceases to render a project with .hsc files,
          --   not a good time to get it straight.
          -- setSockOpt sock (SockOpt _IPPROTO_IP _IP_ADD_MEMBERSHIP) xxx
          -- bind sock (addrAddress addr)
          error "mcast not supported yet"
        else -- receiving unicast to the specified addr
             bind sock (addrAddress addr)
      atomically
        $   fromMaybe []
        <$> tryTakeTMVar forageAddrs
        >>= putTMVar forageAddrs
        .   (addr :)
      return sock
    forageFrom :: Socket -> IO ()
    forageFrom !sock = do
      (payload, wsAddr) <- recvFrom sock
      -- don't expect too large a call-for-workers announcement
                                    1500
      (conn, addr) <- accept sock

      hndl         <- socketToHandle conn ReadWriteMode
      clientEoL    <- newEmptyTMVarIO
      void
        $ forkFinally (servClient clientEoL (T.pack $ show addr) hndl)
        $ (hClose hndl <*) -- close the socket anyway after the thread done
        . atomically
        . tryPutTMVar clientEoL

      forageFrom sock -- tail recursion

    servClient :: TMVar (Either SomeException ()) -> Text -> Handle -> IO ()
    servClient !clientEoL !clientId !hndl = do
      pktSink <- newEmptyTMVarIO
      poq     <- newTQueueIO
      chdVar  <- newTVarIO mempty

      let
        !peer = Peer { edh'peer'ident    = clientId
                     , edh'peer'eol      = clientEoL
                     , edh'peer'posting  = writeTQueue poq
                     , edh'peer'hosting  = takeTMVar pktSink
                     , edh'peer'channels = chdVar
                     }
        prepPeer :: IO EdhValue
        prepPeer = do
          peerVar <- newTVarIO undefined
          void
            $ runEdhProgram' ctx
            $ createEdhObject peerClass (ArgsPack [] mempty)
            $ \(OriginalValue !peerVal _ _) -> case peerVal of
                EdhObject !peerObj -> contEdhSTM $ do
                  -- actually fill in the in-band entity storage here
                  writeTVar (entity'store $ objEntity peerObj) $ toDyn peer
                  -- announce this new peer to the event sink
                  publishEvent worksources peerVal
                  -- save it and this part done
                  writeTVar peerVar peerVal
                _ -> error "bug: Peer ctor returned non-object"
          readTVarIO peerVar
        prepService :: EdhValue -> EdhModulePreparation
        prepService !peerVal !pgs !exit = do
          let !modu = thisObject $ contextScope $ edh'context pgs
          -- implant to the module being prepared
          changeEntityAttr pgs (objEntity modu) (AttrByName "peer") peerVal
          -- insert a tick here, for serving to start in next stm tx
          flip (exitEdhSTM pgs) nil $ \_ -> contEdhSTM $ if __peer_init__ == nil
            then exit
            else
              -- call the per-connection peer module initialization method,
              -- with the module object as `that`
              edhMakeCall pgs
                          __peer_init__
                          (thisObject $ contextScope $ edh'context pgs)
                          []
                $ \mkCall -> runEdhProc pgs $ mkCall $ \_ -> contEdhSTM exit

      try prepPeer >>= \case
        Left err -> atomically $ do
          -- mark the client eol with this error
          void $ tryPutTMVar clientEoL (Left err)
          -- failure in preparation for a peer object is considered so fatal
          -- that the forager should terminate as well
          void $ tryPutTMVar forageEoL (Left err)
        Right !peerVal -> do

          void
            -- run the forage module on a separate thread as another program
            $ forkFinally
                (runEdhModule' world (T.unpack forageModu) (prepService peerVal)
                )
            -- mark client end-of-life with the result anyway
            $ void
            . atomically
            . tryPutTMVar clientEoL
            . void

          -- pump commands in, 
          -- make this thread the only one reading the handle
          -- note this won't return, will be asynchronously killed on eol
          void $ forkIO $ receivePacketStream clientId hndl pktSink clientEoL

          let
            serializeCmdsOut :: IO ()
            serializeCmdsOut =
              atomically
                  (        (Right <$> readTQueue poq)
                  `orElse` (Left <$> readTMVar clientEoL)
                  )
                >>= \case
                      Left _ -> return () -- stop on eol any way
                      Right !pkt ->
                        catch (sendPacket clientId hndl pkt >> serializeCmdsOut)
                          $ \(e :: SomeException) -> -- mark eol on error
                              atomically $ void $ tryPutTMVar clientEoL $ Left e
          -- pump commands out,
          -- make this thread the only one writing the handle
          serializeCmdsOut

