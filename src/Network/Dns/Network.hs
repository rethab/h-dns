module Network where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Types

defaultConfig :: Config
defaultConfig = Config { server = googleDns, port =  53 }

data Config = Config  {
    server :: HostAddress
  , port :: PortNumber
  }

query :: Config -> Message -> IO Message
query (Config server port) msg = do
  s <- socket AF_INET Datagram defaultProtocol
  _ <- connect s (SockAddrInet port server)
  _ <- sendAll s (serializeMessage msg)
  parseMessage <$> recv s 1024

googleDns :: HostAddress
googleDns = tupleToHostAddress (8, 8, 8, 8)

google2Dns :: HostAddress
google2Dns = tupleToHostAddress (216, 239, 34, 10)

cccDns :: HostAddress
cccDns = tupleToHostAddress (94, 45, 228, 23)

yandexDns :: HostAddress
yandexDns = tupleToHostAddress (77, 88, 8, 88)
