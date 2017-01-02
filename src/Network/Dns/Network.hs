module Network.Dns.Network
    (

    -- * Types
      Config(..)

    -- * Operations
    , query

    ) where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Network.Dns.Types
import Network.Dns.Serialization

data Config = Config  {
    server :: HostAddress
  , port :: PortNumber
  }

query :: Config -> Message -> IO Message
query cfg msg = do
  s <- socket AF_INET Datagram defaultProtocol
  _ <- connect s (SockAddrInet (port cfg) (server cfg))
  _ <- sendAll s (serializeMessage msg)
  parseMessage <$> recv s 1024

