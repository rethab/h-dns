module Network.Dns.Types
    (

    -- * Types
      Message(..)
    , Header(..)
    , Question(..)
    , RecordType(..)
    , RecordClass(..)
    , ResponseCode(..)
    , OpCode(..)
    , RequestID(..)
    , AuthoritativeAnswer(..)
    , QuestionCount(..)
    , AnswerCount(..)
    , NameserverCount(..)
    , AdditionalCount(..)
    , RecursionDesired(..)
    , RecursionAvailable(..)
    , Truncation(..)
    , RData(..)
    , Z(..)
    , ResourceRecord(..)
    , QR(..)
    , Ttl(..)

    -- * Default Values
    , defaultMessage
    ) where

import Data.Word (Word8, Word16, Word32)
import Data.Text (Text)

import qualified Data.ByteString      as BS


data Message = Message {
    header :: Header
  , questions :: [Question]
  , answers :: [ResourceRecord]
  } deriving Show

data RecordType = A | NS | MD | MF | CNAME | SOA | MB | MG | MR | NULL
                | WKS | PTR | HINFO | MINFO | MX | TXT | RP | AFSDB
                | X25 | ISDN | RT | NSAP | NSAPPTR | SIG | KEY | PX
                | GPOS | AAAA | LOC | NXT | EID | NIMLOC | SRV | ATMA
                | NAPTR | KX | CERT | A6 | DNAME | SINK | OPT | APL | DS
                | SSHFP | IPSECKEY | RRSIG | NSEC | DNSKEY | DHCID
                | NSEC3 | NSEC3PARAM | TLSA | SMIMEA | HIP | NINFO
                | RKEY | TALINK | CDS | CDNSKEY | OPENPGPKEY | CSYNC
                | SPF | UINFO | UID | GID | UNSPEC | NID | L32 | L64
                | LP | EUI48 | EUI64 | TKEY | TSIG | IXFR | AXFR
                | MAILB | MAILA | Asterisk | URI | CAA | AVC | TA | DLV
                | UnassignedType Int | PrivateUse Int | Reserved deriving Show

data RecordClass = ReservedClass Int | Internet | Chaos | Hesiod
                 | QClassNone | QClassAny | ReservedPrivate Int | UnassignedClass Int deriving Show

newtype RequestID = RequestID Word16 deriving Show
data QR = Query | Response deriving (Show, Eq)
data OpCode = StdQuery
            | InvQuery
            | Status
            | NotUsed
            | Notify
            | Update
            | ReservedOp Word8 deriving Show
newtype AuthoritativeAnswer = AuthoritativeAnswer Bool deriving Show
newtype Truncation = Truncation Bool deriving Show
newtype RecursionDesired = RecursionDesired Bool deriving Show
newtype RecursionAvailable = RecursionAvailable Bool deriving Show
data Z = Z deriving Show
data ResponseCode = NoError
                  | FormatError
                  | ServerFailure
                  | NameError
                  | NotImplemented
                  | Refused
                  | YxDomain
                  | YxRrSet
                  | NxRrSet
                  | NotAuth
                  | NotZone
                  | ReservedCode deriving Show
newtype QuestionCount = QuestionCount { qCnt :: Int } deriving Show
newtype AnswerCount = AnswerCount { aCnt :: Int } deriving Show
newtype NameserverCount = NameserverCount Int deriving Show
newtype AdditionalCount = AdditionalCount Int deriving Show

data Header = Header {
    reqID :: RequestID
  , qr :: QR
  , opCode :: OpCode
  , auth :: AuthoritativeAnswer
  , tc :: Truncation
  , rd :: RecursionDesired
  , ra :: RecursionAvailable
  , z :: Z
  , respCode :: ResponseCode
  , qCount :: QuestionCount
  , aCount :: AnswerCount
  , nsCount :: NameserverCount
  , addCount :: AdditionalCount
} deriving Show


data Question = Question Text RecordType RecordClass deriving Show

newtype Ttl = Ttl { seconds :: Word32 } deriving Show
data RData = AddressResource { ipv4 :: BS.ByteString }
           | Ipv6AddressResource { ipv6 :: BS.ByteString }
           | NameServerResource { authNs :: Text }
           | CNameResource { name :: Text }
           | SOAResource { masterName :: Text, responsibleName :: Text, serial :: Int, refresh :: Int, retry :: Int, expire :: Int, minimum :: Int }
           | PointerResource { ptrDomainName :: Text }
           | MailExchangeResource { preference :: Int, exchange :: Text }
           | TextResource { txtData :: Text }
           | Raw BS.ByteString deriving Show
data ResourceRecord = ResourceRecord Text RecordType RecordClass Ttl RData deriving Show



defaultMessage :: Text -> Message
defaultMessage qname = Message {
    header = defaultHeader
  , questions = [defaultQuestion qname]
  , answers = []
}

defaultHeader :: Header
defaultHeader = Header (RequestID 1337) Query StdQuery (AuthoritativeAnswer False)
                       (Truncation False) (RecursionDesired True) (RecursionAvailable False)
                       Z NoError (QuestionCount 1) (AnswerCount 0) (NameserverCount 0)
                       (AdditionalCount 0)

defaultQuestion :: Text -> Question
defaultQuestion qname = Question qname Asterisk Internet

