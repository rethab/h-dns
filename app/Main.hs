{--
 - DNS Parameters: http://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml
 -
 -}
module Main where

import qualified Data.Text.IO as T

import Network.Dns
import Network.Dns.Printer
import Options

main :: IO ()
main = do

  opts <- getOptions

  let config = Config {
      server = dnsServer opts
    , port = 53
  }

  let message = defaultMessage (domain opts)

  resp <- query config message

  T.putStr (printTech resp)
