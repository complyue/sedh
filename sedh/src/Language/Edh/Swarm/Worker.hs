
module Language.Edh.Swarm.Worker where

import           Prelude
-- import           Debug.Trace

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as Map
import           Data.Dynamic

import           Network.Socket
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )

import qualified Data.Lossless.Decimal         as D

import           Language.Edh.EHI
import           Language.Edh.Net

import           Language.Edh.Swarm.Starter


prepareSwarmWorkerModu :: SwarmWorkStarter -> EdhModulePreparation
prepareSwarmWorkerModu (SwarmWorkStarter _ _ _ _ _ !wscFd) !pgs !exit = do

  let moduScope = contextScope $ edh'context pgs
      modu      = thisObject moduScope


  return ()




-- | host constructor Client()
clientCtor
  :: Class
  -> Class
  -> EdhProgState
  -> ArgsPack  -- ctor args, if __init__() is provided, will go there too
  -> TVar (Map.HashMap AttrKey EdhValue)  -- out-of-band attr store
  -> (Dynamic -> STM ())  -- in-band data to be written to entity store
  -> STM ()
clientCtor !addrClass !peerClass !pgsCtor !apk !obs !ctorExit =
  case
      parseArgsPack
        (Nothing, Left ("127.0.0.1" :: ServiceAddr, 3721 :: ServicePort), nil)
        parseCtorArgs
        apk
    of
      Left err -> throwEdhSTM pgsCtor UsageError err
      Right (Nothing, _, _) ->
        throwEdhSTM pgsCtor UsageError "missing consumer module"
      Right (Just consumer, Right addrObj, __peer_init__) -> do
        esd <- readTVar $ entity'store $ objEntity addrObj
        case fromDynamic esd :: Maybe AddrInfo of
          Nothing -> throwEdhSTM pgsCtor UsageError "bogus addr object"
          Just (AddrInfo _ _ _ _ (SockAddrInet port host) _) ->
            case hostAddressToTuple host of
              (n1, n2, n3, n4) -> go
                consumer
                (  T.pack
                $  "'"
                <> show n1
                <> "."
                <> show n2
                <> "."
                <> show n3
                <> "."
                <> show n4
                <> "'"
                )
                (fromIntegral port)
                __peer_init__
          Just (AddrInfo _ _ _ _ (SockAddrInet6 port _ (n1, n2, n3, n4) _) _)
            -> go
              consumer
              (  T.pack
              $  "'"
              <> show n1
              <> ":"
              <> show n2
              <> ":"
              <> show n3
              <> "::"
              <> show n4
              <> "'"
              )
              (fromIntegral port)
              __peer_init__
          _ -> throwEdhSTM pgsCtor UsageError "unsupported addr object"

      Right (Just consumer, Left (addr, port), __peer_init__) ->
        go consumer addr port __peer_init__
 where
  go consumer addr port __peer_init__ = do
    serviceAddrs <- newEmptyTMVar
    cnsmrEoL     <- newEmptyTMVar
    let !client = EdhClient { edh'consumer'modu = consumer
                            , edh'service'addr  = addr
                            , edh'service'port  = port
                            , edh'service'addrs = serviceAddrs
                            , edh'consumer'eol  = cnsmrEoL
                            , edh'consumer'init = __peer_init__
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
      ++ [ ( AttrByName "__repr__"
           , EdhString
           $  "Client("
           <> T.pack (show consumer)
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
          (consumerThread client)
          ( void
          . atomically
            -- fill empty addrs if the connection has ever failed
          . (tryPutTMVar serviceAddrs [] <*)
            -- mark consumer end-of-life anyway finally
          . tryPutTMVar cnsmrEoL
          )
        )
      $ \_ -> ctorExit $ toDyn client
  parseCtorArgs =
    ArgsPackParser
        [ \arg (_, addr', init') -> case edhUltimate arg of
          EdhString !consumer -> Right (Just consumer, addr', init')
          _                   -> Left "Invalid consumer"
        , \arg (consumer', addr', init') -> case edhUltimate arg of
          EdhString host -> case addr' of
            Left  (_, port') -> Right (consumer', Left (host, port'), init')
            Right _addrObj   -> Right (consumer', Left (host, 3721), init')
          EdhObject addrObj -> Right (consumer', Right addrObj, init')
          _                 -> Left "Invalid addr"
        , \arg (consumer', addr', init') -> case edhUltimate arg of
          EdhDecimal d -> case D.decimalToInteger d of
            Just port -> case addr' of
              Left (host', _) ->
                Right (consumer', Left (host', fromIntegral port), init')
              Right _addrObj ->
                Left "Can not specify both addr object and port"
            Nothing -> Left "port must be integer"
          _ -> Left "Invalid port"
        ]
      $ Map.fromList
          [ ( "addr"
            , \arg (consumer', addr', init') -> case edhUltimate arg of
              EdhString host -> case addr' of
                Left  (_, port') -> Right (consumer', Left (host, port'), init')
                Right _addrObj   -> Right (consumer', Left (host, 3721), init')
              EdhObject addrObj -> Right (consumer', Right addrObj, init')
              _                 -> Left "Invalid addr"
            )
          , ( "port"
            , \arg (consumer', addr', init') -> case edhUltimate arg of
              EdhDecimal d -> case D.decimalToInteger d of
                Just port -> case addr' of
                  Left (host', _) ->
                    Right (consumer', Left (host', fromIntegral port), init')
                  Right _addrObj ->
                    Left "Can not specify both addr object and port"
                Nothing -> Left "port must be integer"
              _ -> Left "Invalid port"
            )
          , ( "init"
            , \arg (consumer', addr', _) -> case edhUltimate arg of
              EdhNil          -> Right (consumer', addr', nil)
              mth@EdhMethod{} -> Right (consumer', addr', mth)
              mth@EdhIntrpr{} -> Right (consumer', addr', mth)
              _               -> Left "Invalid init"
            )
          ]

  addrsProc :: EdhProcedure
  addrsProc _ !exit = do
    pgs <- ask
    let ctx  = edh'context pgs
        this = thisObject $ contextScope ctx
        es   = entity'store $ objEntity this
        wrapAddrs :: [EdhValue] -> [AddrInfo] -> STM ()
        wrapAddrs addrs [] = exitEdhSTM pgs exit $ EdhTuple addrs
        wrapAddrs !addrs (addr : rest) =
          runEdhProc pgs
            $ createEdhObject addrClass (ArgsPack [] mempty)
            $ \(OriginalValue !addrVal _ _) -> case addrVal of
                EdhObject !addrObj -> contEdhSTM $ do
                  -- actually fill in the in-band entity storage here
                  writeTVar (entity'store $ objEntity addrObj) $ toDyn addr
                  wrapAddrs (addrVal : addrs) rest
                _ -> error "bug: addr ctor returned non-object"
    contEdhSTM $ do
      esd <- readTVar es
      case fromDynamic esd :: Maybe EdhClient of
        Nothing ->
          throwEdhSTM pgs UsageError $ "bug: this is not a client : " <> T.pack
            (show esd)
        Just !client ->
          waitEdhSTM pgs (readTMVar $ edh'service'addrs client) $ wrapAddrs []

  eolProc :: EdhProcedure
  eolProc _ !exit = do
    pgs <- ask
    let ctx  = edh'context pgs
        this = thisObject $ contextScope ctx
        es   = entity'store $ objEntity this
    contEdhSTM $ do
      esd <- readTVar es
      case fromDynamic esd :: Maybe EdhClient of
        Nothing ->
          throwEdhSTM pgs UsageError $ "bug: this is not a client : " <> T.pack
            (show esd)
        Just !client -> tryReadTMVar (edh'consumer'eol client) >>= \case
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
      case fromDynamic esd :: Maybe EdhClient of
        Nothing ->
          throwEdhSTM pgs UsageError $ "bug: this is not a client : " <> T.pack
            (show esd)
        Just !client ->
          edhPerformIO pgs (atomically $ readTMVar (edh'consumer'eol client))
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
      case fromDynamic esd :: Maybe EdhClient of
        Nothing ->
          throwEdhSTM pgs UsageError $ "bug: this is not a client : " <> T.pack
            (show esd)
        Just !client -> do
          stopped <- tryPutTMVar (edh'consumer'eol client) $ Right ()
          exitEdhSTM pgs exit $ EdhBool stopped


  consumerThread :: EdhClient -> IO ()
  consumerThread (EdhClient !cnsmrModu !servAddr !servPort !serviceAddrs !cnsmrEoL !__peer_init__)
    = do
      addr <- resolveServAddr
      bracket
          (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
          close
        $ \sock -> do
            connect sock $ addrAddress addr
            atomically
              $   fromMaybe []
              <$> tryTakeTMVar serviceAddrs
              >>= putTMVar serviceAddrs
              .   (addr :)
            try (consumeService (T.pack $ show $ addrAddress addr) sock)
              >>= (gracefulClose sock 5000 <*)
              .   atomically
              .   tryPutTMVar cnsmrEoL

   where
    ctx             = edh'context pgsCtor
    world           = contextWorld ctx

    resolveServAddr = do
      let hints =
            defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      addr : _ <- getAddrInfo (Just hints)
                              (Just $ T.unpack servAddr)
                              (Just (show servPort))
      return addr

    consumeService :: Text -> Socket -> IO ()
    consumeService !clientId !sock = do
      pktSink <- newEmptyTMVarIO
      poq     <- newEmptyTMVarIO
      chdVar  <- newTVarIO mempty

      let
        !peer = Peer { edh'peer'ident    = clientId
                     , edh'peer'eol      = cnsmrEoL
                     , edh'peer'posting  = putTMVar poq
                     , edh'peer'hosting  = takeTMVar pktSink
                     , edh'peer'channels = chdVar
                     }
        prepConsumer :: EdhModulePreparation
        prepConsumer !pgs !exit = do
          let !modu = thisObject $ contextScope $ edh'context pgs
          runEdhProc pgs
            $ createEdhObject peerClass (ArgsPack [] mempty)
            $ \(OriginalValue !peerVal _ _) -> case peerVal of
                EdhObject !peerObj -> contEdhSTM $ do
                  -- actually fill in the in-band entity storage here
                  writeTVar (entity'store $ objEntity peerObj) $ toDyn peer
                  -- implant to the module being prepared
                  changeEntityAttr pgs
                                   (objEntity modu)
                                   (AttrByName "peer")
                                   peerVal
                  -- insert a tick here, for Consumer to start in next stm tx
                  flip (exitEdhSTM pgs) nil $ \_ ->
                    contEdhSTM $ if __peer_init__ == nil
                      then exit
                      else
                        -- call the per-connection peer module initialization method,
                        -- with the module object as `that`
                        edhMakeCall
                            pgs
                            __peer_init__
                            (thisObject $ contextScope $ edh'context pgs)
                            []
                            id
                          $ \mkCall -> runEdhProc pgs $ mkCall $ \_ ->
                              contEdhSTM exit
                _ -> error "bug: Peer ctor returned non-object"

      void
        -- run the consumer module as another program
        $ forkFinally (runEdhModule' world (T.unpack cnsmrModu) prepConsumer)
        -- mark client end-of-life with the result anyway
        $ void
        . atomically
        . tryPutTMVar cnsmrEoL
        . void

      -- pump commands in, 
      -- make this thread the only one reading the handle
      -- note this won't return, will be asynchronously killed on eol
      void $ forkIO $ receivePacketStream clientId (recv sock) pktSink cnsmrEoL

      let
        serializeCmdsOut :: IO ()
        serializeCmdsOut =
          atomically
              ((Right <$> takeTMVar poq) `orElse` (Left <$> readTMVar cnsmrEoL))
            >>= \case
                  Left _ -> return ()
                  Right !pkt ->
                    catch
                        (  sendPacket clientId (sendAll sock) pkt
                        >> serializeCmdsOut
                        )
                      $ \(e :: SomeException) -> -- mark eol on error
                          atomically $ void $ tryPutTMVar cnsmrEoL $ Left e
      -- pump commands out,
      -- make this thread the only one writing the handle
      serializeCmdsOut

