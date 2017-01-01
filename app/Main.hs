{--
 - DNS Parameters: http://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml
 -
 -}
module Main where

import qualified Network.Socket as S

import Network.Dns
import Options

main :: IO ()
main = do

  opts <- getOptions

  let config = Config {
      server = dnsServer opts
    , port = 53
  }

  let message = defaultMessage (domain opts)

  msg <- query config message

  print msg
