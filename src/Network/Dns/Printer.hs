module Network.Dns.Printer
    (
      printTech
    ) where

import Data.Bits ((.&.), shiftR)
import Data.Char (chr, ord)
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Word (Word8)

import qualified Data.Text as T
import qualified Data.ByteString as BS

import Network.Dns.Types


printTech :: Message -> Text
printTech (Message h _ as) = 
  case respCode h of
    NoError        -> printTechResourceRecords as
    FormatError    -> "Error: FormatError"
    ServerFailure  -> "Error: ServerFailure"
    NameError      -> "Error: NameError"
    NotImplemented -> "Error: NotImplemented"
    Refused        -> "Error: Refused"
    YxDomain       -> "Error: YxDomain"
    YxRrSet        -> "Error: YxRrSet"
    NxRrSet        -> "Error: NxRrSet"
    NotAuth        -> "Error: NotAuth"
    NotZone        -> "Error: NotZone"
    ReservedCode   -> "Error: ReservedCode"

printTechResourceRecords :: [ResourceRecord] -> Text
printTechResourceRecords [] = ""
printTechResourceRecords (r: rs) = 
       printTechResourceRecord r
    <> "\n"
    <> printTechResourceRecords rs
  where
    printTechResourceRecord :: ResourceRecord -> Text
    printTechResourceRecord (ResourceRecord name rType rClass ttl rData) =
      prettyType rType <> " "
                       <> "\"" <> name <> "\""
                       <> " (ttl=" <> prettyTtl ttl <> ")"
                       <> " = " <> printTechResourceData rData

    printTechResourceData :: RData -> Text
    printTechResourceData (AddressResource ipv4) = prettyIpv4 ipv4
    printTechResourceData (Ipv6AddressResource ipv6) = prettyIpv6 ipv6
    printTechResourceData (NameServerResource authNs) = authNs
    printTechResourceData (CNameResource name) = name
    printTechResourceData (SOAResource masterName responsibleName serial refresh retry expire minimum) =
      masterName <> " " <> responsibleName
    printTechResourceData (PointerResource ptrDomainName) = ptrDomainName
    printTechResourceData (MailExchangeResource pref exchange) =
      T.pack (show pref) <> " " <> T.pack (show exchange)
    printTechResourceData (TextResource txtData) = txtData
    printTechResourceData (Raw bs) = T.pack (show $ BS.unpack bs)

    prettyType :: RecordType -> Text
    prettyType A     = "Address"
    prettyType AAAA  = "Ipv6Address"
    prettyType NS    = "NameServer"
    prettyType CNAME = "CName"
    prettyType SOA   = "SOA"
    prettyType PTR   = "Pointer"
    prettyType MX    = "MailExchange"
    prettyType TXT   = "Text"
    prettyType _     = "Raw"

    prettyTtl :: Ttl -> Text
    prettyTtl (Ttl ttl) =
      let (m', s) = ttl `divMod` 60
          m = m' `mod` 60
          h = m `div` 60
          hTxt = if h == 0 then "" else T.pack (show h) <> "h "
      in  hTxt <> T.pack (show m) <> "m " <> T.pack (show s) <> "s"

    prettyIpv4 :: BS.ByteString -> Text
    prettyIpv4 bs = T.intercalate "." (map (T.pack . show) $ BS.unpack bs)

    prettyIpv6 :: BS.ByteString -> Text
    prettyIpv6 = printHex . group4 . concatMap toWord4 . BS.unpack 
      where
        toWord4 :: Word8 -> [Word8]
        toWord4 x = [shiftR (x .&. 0xF0) 4, x .&. 0x0F]

        group4 :: [Word8] -> [[Word8]]
        group4 [] = []
        group4 (a:b:c:d:rest) = [a, b, c, d] : group4 rest

        printHex :: [[Word8]] -> Text
        printHex [] = ""
        printHex (x:xs) = T.pack (map toHex x) <> if null xs then "" else ":" <> printHex xs
          where
            toHex :: Word8 -> Char
            toHex x | x <= 9 = head (show x)
            toHex x          = chr $ ord 'A' + fromIntegral (x `mod` 10)
