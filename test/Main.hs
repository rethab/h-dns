module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Network.Dns.Serialization.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [  Network.Dns.Serialization.Tests.tests
  ]
