module Network.Dns
    (

    -- * Types
      Message(..)
    , RecordType(..)
    , RecordClass(..)
    , Config(..)

    -- * Default Values
    , defaultMessage

    -- * Operations
    , query
  ) where

import Network.Dns.Types
import Network.Dns.Network
