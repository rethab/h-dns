module Network.Dns.Serialization.Tests (tests) where

import Data.Text (Text)
import qualified Data.Text as T

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import qualified Test.QuickCheck as QC

import Network.Dns
import Network.Dns.Serialization

tests :: TestTree
tests = testGroup "Network.Dns.Serialization"
  [ testProperty "parseMessageInverse" parseMessageInverse
  ]

parseMessageInverse :: Message -> Bool
parseMessageInverse msg =
  parseMessage (serializeMessage msg) == msg


instance QC.Arbitrary Message where
  arbitrary = defaultMessage <$> QC.arbitrary

instance QC.Arbitrary Text where
  arbitrary = T.pack <$> QC.arbitrary
