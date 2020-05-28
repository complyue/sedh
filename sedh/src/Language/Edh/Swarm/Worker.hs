
module Language.Edh.Swarm.Worker where

import           Prelude
-- import           Debug.Trace

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Control.Concurrent
import           Control.Concurrent.STM

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


wscTakeProc :: EdhProcedure
wscTakeProc (ArgsPack [EdhDecimal !wscFd, EdhObject !peerObj] !kwargs) !exit
  | Map.null kwargs = do
    pgs <- ask
    case D.decimalToInteger wscFd of
      Nothing -> throwEdh UsageError $ "bad wsc fd: " <> T.pack (show wscFd)
      Just !wscFdInt -> contEdhSTM $ do
        let !peerId = "<wsc#" <> T.pack (show wscFdInt) <> ">"
        pktSink <- newEmptyTMVar
        poq     <- newEmptyTMVar
        chdVar  <- newTVar mempty
        wkrEoL  <- newEmptyTMVar

        let !peer = Peer { edh'peer'ident    = peerId
                         , edh'peer'eol      = wkrEoL
                         , edh'peer'posting  = putTMVar poq
                         , edh'peer'hosting  = takeTMVar pktSink
                         , edh'peer'channels = chdVar
                         }
        writeTVar (entity'store $ objEntity peerObj) $ toDyn peer

        edhPerformIO
            pgs
            ( forkFinally
                (workerThread (fromInteger wscFdInt) peerId pktSink poq wkrEoL)
            $ atomically
            . void
            . tryPutTMVar wkrEoL
            )
          $ \_ -> exitEdhProc exit $ EdhObject peerObj

wscTakeProc _ _ = throwEdh UsageError "Invalid args"


workerThread
  :: Int
  -> Text
  -> TMVar Packet
  -> TMVar Packet
  -> TMVar (Either SomeException ())
  -> IO ()
workerThread !wscFd !peerId !pktSink !poq !wkrEoL =
  bracket (mkSocket $ fromIntegral wscFd) close $ \sock ->
    try (netComm sock)
      >>= (gracefulClose sock 5000 <*)
      .   atomically
      .   tryPutTMVar wkrEoL

 where
  netComm :: Socket -> IO ()
  netComm !sock = do
    -- pump commands in, 
    -- make this thread the only one reading the handle
    -- note this won't return, will be asynchronously killed on eol
    void $ forkIO $ receivePacketStream peerId (recv sock) pktSink wkrEoL

    let
      serializeCmdsOut :: IO ()
      serializeCmdsOut =
        atomically
            ((Right <$> takeTMVar poq) `orElse` (Left <$> readTMVar wkrEoL))
          >>= \case
                Left _ -> return ()
                Right !pkt ->
                  catch
                      (sendPacket peerId (sendAll sock) pkt >> serializeCmdsOut)
                    $ \(e :: SomeException) -> -- mark eol on error
                        atomically $ void $ tryPutTMVar wkrEoL $ Left e
    -- pump commands out,
    -- make this thread the only one writing the handle
    serializeCmdsOut

