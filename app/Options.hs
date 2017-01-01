module Options
    (
      Options(..)
    , getOptions
    )where

import Control.Monad (join)
import Data.Text (Text, pack)

import Options.Applicative
import Network.Dns hiding (header)

import qualified Network.Socket as S

data Options = Options {
    domain :: Text
  , dnsServer :: S.HostAddress
  }

getOptions :: IO Options
getOptions =  execParser opts
  where
    opts = info (helper <*> parser)
      (fullDesc
      <> header "h-dns: DNS Query Tool"
      <> progDesc "Queries a DNS Server for entries of the DOMAIN")

parser :: Parser Options
parser = 
  Options
    <$> (pack <$> argument str (metavar "DOMAIN"))
    <*> (option hostParser
         ( long "server"
         <> short 's'
         <> help "The DNS Server to Query"
         <> value googleDns
         <> metavar "DNS Server"
         ))


hostParser :: ReadM S.HostAddress
hostParser = eitherReader toIp
  where
    toIp :: String -> Either String S.HostAddress
    toIp s = split4 s >>= toHostAddr
    
    split4 :: String -> Either String (String, String, String, String)
    split4 s =
      case split '.' s of
        (a:b:c:d:[]) -> Right (a, b, c, d)
        _            -> Left "Host must be IPv4"

    toHostAddr :: (String, String, String, String) -> Either String S.HostAddress
    toHostAddr (a, b, c, d) = Right $ S.tupleToHostAddress (read a, read b, read c, read d)


googleDns :: S.HostAddress
googleDns = S.tupleToHostAddress (8, 8, 8, 8)

google2Dns :: S.HostAddress
google2Dns = S.tupleToHostAddress (216, 239, 34, 10)

cccDns :: S.HostAddress
cccDns = S.tupleToHostAddress (94, 45, 228, 23)

yandexDns :: S.HostAddress
yandexDns = S.tupleToHostAddress (77, 88, 8, 88)

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s
