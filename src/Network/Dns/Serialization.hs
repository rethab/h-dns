module Network.Dns.Serialization where

-- the original input string, passed around for label pointers
newtype GetCtx = GetCtx LBS.ByteString

mkCtx :: BS.ByteString -> GetCtx
mkCtx = GetCtx . LBS.fromStrict

parseMessage :: BS.ByteString -> Message
parseMessage bs = runGet (runBitGet getMessage) (LBS.fromStrict bs)
  where
    getMessage :: BitGet Message
    getMessage = do
      header <- getHeader
      questions <- loopM (getQuestion (mkCtx bs)) (qCnt $ qCount header)
      answers <- loopM (getResourceRecord (mkCtx bs)) (aCnt $ aCount header)
      return (Message header questions answers)

-- replace with mapM ??? 
loopM :: (Monad m) => m a -> Int -> m [a]
loopM _ 0 = return []
loopM m n = do { a <- m; bs <- loopM m (n-1); return (a : bs) }

serializeMessage :: Message -> BS.ByteString
serializeMessage (Message h qs _) = LBS.toStrict $ runPut $ runBitPut $ do
  _ <- putHeader h
  _ <- mapM_ putQuestion qs
  return ()

getResourceRecord :: GetCtx -> BitGet ResourceRecord
getResourceRecord ctx = do
  name <- getName ctx
  rType <- toRecordType <$> getWord16be 16
  rClass <- toRecordClass <$> getWord16be 16
  ttl <- Ttl . fromIntegral <$> getWord32be 32
  rData <- getRData ctx rType
  return (ResourceRecord name rType rClass ttl rData)

getRData :: GetCtx -> RecordType -> BitGet RData
getRData ctx rType = do
  len <- getWord16be 16
  case rType of
    A -> AddressResource <$> getByteString (fromIntegral len)
    AAAA -> Ipv6AddressResource <$> getByteString (fromIntegral len)
    NS -> NameServerResource <$> getName ctx
    CNAME -> CNameResource <$> getName ctx
    SOA -> getSOAResource ctx
    PTR -> PointerResource <$> getName ctx
    MX -> MailExchangeResource <$> (fromIntegral <$> getWord16be 16) <*> getName ctx
    TXT -> TextResource <$> (decodeUtf8 <$> getByteString (fromIntegral len))
    _ -> Raw <$> getByteString (fromIntegral len)

getSOAResource :: GetCtx -> BitGet RData
getSOAResource ctx =
  SOAResource <$> getName ctx
              <*> getEmail ctx
              <*> (fromIntegral <$> getWord32be 32)
              <*> (fromIntegral <$> getWord32be 32)
              <*> (fromIntegral <$> getWord32be 32)
              <*> (fromIntegral <$> getWord32be 32)
              <*> (fromIntegral <$> getWord32be 32)

getName :: GetCtx -> BitGet Text
getName ctx = (T.intercalate ".") <$> getLabels ctx

getEmail :: GetCtx -> BitGet Text
getEmail ctx = mkEmail <$> getLabels ctx
  where
    mkEmail :: [Text] -> Text
    mkEmail (name:domain:ts) = name <> "@" <> domain <> "." <> T.intercalate "." ts
      

getLabels :: GetCtx -> BitGet [Text]
getLabels ctx@(GetCtx bs) = labelType >>= \lt ->
  case lt of
    (SEQ 0)   -> return []
    (SEQ len) -> do a <- decodeUtf8 <$> getByteString (fromIntegral len)
                    bs <- getLabels ctx
                    return (a : bs)
    (PNT off) -> return $ runGet (skip off >> runBitGet (getLabels ctx)) bs

-- a domain name is either a sequence of labels or a pointer
data NT = SEQ { length :: Int }
        | PNT { offset :: Int }

labelType :: BitGet NT
labelType = do
  a <- getBool 
  b <- getBool
  case (a, b) of
    (True, True) -> PNT . fromIntegral <$> getWord16be 14
    (False, False) -> SEQ . fromIntegral <$> getWord8 6
    _ -> fail "label must start with 00 or 11"

getQuestion :: GetCtx -> BitGet Question
getQuestion ctx =
  Question <$> getName ctx
           <*> (toRecordType <$> getWord16be 16)
           <*> (toRecordClass <$> getWord16be 16)

putQuestion :: Question -> BitPut ()
putQuestion (Question qname qtype qclass) = do
  _ <- putQname qname
  _ <- putWord16be 16 (fromRecordType qtype)
  _ <- putWord16be 16 (fromRecordClass qclass)
  return ()

  where
    putQname :: Text -> BitPut ()
    putQname name = do
      _ <- mapM_ (\l -> putWord8 8 (fst l) >> putByteString (snd l)) (mkLabels qname)
      putWord8 8 0
    mkLabels :: Text -> [(Word8, BS.ByteString)]
    mkLabels name = map (\ws -> (fromIntegral (T.length ws), encodeUtf8 ws)) (T.splitOn "." name)

putHeader :: Header -> BitPut ()
putHeader (Header (RequestID reqId) qr opcode (AuthoritativeAnswer aa)
                  (Truncation tc) (RecursionDesired rd) (RecursionAvailable ra)
                  _ rc (QuestionCount qc) (AnswerCount ac) (NameserverCount nc)
                  (AdditionalCount addc)) = do
  _ <- putWord16be 16 reqId
  _ <- putBool (qr == Response) 
  _ <- putWord8 4 (case opcode of
                     StdQuery     -> 0
                     InvQuery     -> 1
                     Status       -> 2
                     NotUsed      -> 3
                     Notify       -> 4
                     Update       -> 5
                     ReservedOp x -> x) 
  _ <- putBool aa 
  _ <- putBool tc
  _ <- putBool rd
  _ <- putBool ra
  _ <- putWord8 3 0
  _ <- putWord8 4 (case rc of
                     NoError        ->  0  
                     FormatError    ->  1  
                     ServerFailure  ->  2  
                     NameError      ->  3  
                     NotImplemented ->  4  
                     Refused        ->  5  
                     YxDomain       ->  6  
                     YxRrSet        ->  7  
                     NxRrSet        ->  8  
                     NotAuth        ->  9  
                     NotZone        -> 10 
                     ReservedCode   -> 11 ) 
  _ <- putWord16be 16 (fromIntegral qc)
  _ <- putWord16be 16 (fromIntegral ac)
  _ <- putWord16be 16 (fromIntegral nc)
  _ <- putWord16be 16 (fromIntegral addc)
  return ()
    

getHeader :: BitGet Header
getHeader = block $ do
  Header <$> (RequestID <$> word16be 16)
         <*> ((\b -> if b then Response else Query) <$> bool)
         <*> ((\w -> case w of 
                       0 -> StdQuery
                       1 -> InvQuery
                       2 -> Status
                       3 -> NotUsed
                       4 -> Notify
                       5 -> Update
                       x -> ReservedOp x) <$> word8 4)
         <*> (AuthoritativeAnswer <$> bool)
         <*> (Truncation <$> bool)
         <*> (RecursionDesired <$> bool)
         <*> (RecursionAvailable <$> bool)
         <*> ((\b -> Z) <$> word8 3)
         <*> ((\w -> case w of
                       0 -> NoError
                       1 -> FormatError
                       2 -> ServerFailure
                       3 -> NameError
                       4 -> NotImplemented
                       5 -> Refused
                       6 -> YxDomain
                       7 -> YxRrSet
                       8 -> NxRrSet
                       9 -> NotAuth
                       10 -> NotZone
                       _ -> ReservedCode) <$> word8 4)
         <*> (QuestionCount . fromIntegral <$> word16be 16)
         <*> (AnswerCount . fromIntegral <$> word16be 16)
         <*> (NameserverCount . fromIntegral <$> word16be 16)
         <*> (AdditionalCount . fromIntegral <$> word16be 16)


toRecordClass :: Word16 -> RecordClass
toRecordClass   1 = Internet
toRecordClass   3 = Chaos
toRecordClass   4 = Hesiod
toRecordClass 254 = QClassNone
toRecordClass 255 = QClassAny
toRecordClass x | x == 0 || x == 65535     = ReservedClass (fromIntegral x)
                | x == 2                   = UnassignedClass (fromIntegral x)
                | x >= 5   && x <= 253     = UnassignedClass (fromIntegral x)
                | x >= 256 && x <= 65279   = UnassignedClass (fromIntegral x)
                | x >= 65280 && x <= 65534 = ReservedPrivate (fromIntegral x)

fromRecordClass :: RecordClass -> Word16
fromRecordClass Internet = 1
fromRecordClass Chaos  = 3 
fromRecordClass Hesiod = 4
fromRecordClass QClassNone = 254
fromRecordClass QClassAny = 255
fromRecordClass (ReservedClass x) = fromIntegral x
fromRecordClass (UnassignedClass x) = fromIntegral x
fromRecordClass (ReservedPrivate x) = fromIntegral x

toRecordType :: Word16 -> RecordType
toRecordType 1 = A
toRecordType 2 = NS
toRecordType 3 = MD
toRecordType 4 = MF
toRecordType 5 = CNAME
toRecordType 6 = SOA
toRecordType 7 = MB
toRecordType 8 = MG
toRecordType 9 = MR
toRecordType 10 = NULL
toRecordType 11 = WKS
toRecordType 12 = PTR
toRecordType 13 = HINFO
toRecordType 14 = MINFO
toRecordType 15 = MX
toRecordType 16 = TXT
toRecordType 17 = RP
toRecordType 18 = AFSDB
toRecordType 19 = X25
toRecordType 20 = ISDN
toRecordType 21 = RT
toRecordType 22 = NSAP
toRecordType 23 = NSAPPTR
toRecordType 24 = SIG
toRecordType 25 = KEY
toRecordType 26 = PX
toRecordType 27 = GPOS
toRecordType 28 = AAAA
toRecordType 29 = LOC
toRecordType 30 = NXT
toRecordType 31 = EID
toRecordType 32 = NIMLOC
toRecordType 33 = SRV
toRecordType 34 = ATMA
toRecordType 35 = NAPTR
toRecordType 36 = KX
toRecordType 37 = CERT
toRecordType 38 = A6
toRecordType 39 = DNAME
toRecordType 40 = SINK
toRecordType 41 = OPT
toRecordType 42 = APL
toRecordType 43 = DS
toRecordType 44 = SSHFP
toRecordType 45 = IPSECKEY
toRecordType 46 = RRSIG
toRecordType 47 = NSEC
toRecordType 48 = DNSKEY
toRecordType 49 = DHCID
toRecordType 50 = NSEC3
toRecordType 51 = NSEC3PARAM
toRecordType 52 = TLSA
toRecordType 53 = SMIMEA
toRecordType 55 = HIP
toRecordType 56 = NINFO
toRecordType 57 = RKEY
toRecordType 58 = TALINK
toRecordType 59 = CDS
toRecordType 60 = CDNSKEY
toRecordType 61 = OPENPGPKEY
toRecordType 62 = CSYNC
toRecordType 99 = SPF
toRecordType 100 = UINFO
toRecordType 101 = UID
toRecordType 102 = GID
toRecordType 103 = UNSPEC
toRecordType 104 = NID
toRecordType 105 = L32
toRecordType 106 = L64
toRecordType 107 = LP
toRecordType 108 = EUI48
toRecordType 109 = EUI64
toRecordType 249 = TKEY
toRecordType 250 = TSIG
toRecordType 251 = IXFR
toRecordType 252 = AXFR
toRecordType 253 = MAILB
toRecordType 254 = MAILA
toRecordType 255 = Asterisk
toRecordType 256 = URI
toRecordType 257 = CAA
toRecordType 258 = AVC
toRecordType 32768 = TA
toRecordType 32769 = DLV
toRecordType 65535= Reserved
toRecordType x | x == 54                  = UnassignedType (fromIntegral x)
               | x >= 63    && x <= 98    = UnassignedType (fromIntegral x)
               | x >= 110   && x <= 248   = UnassignedType (fromIntegral x)
               | x >= 259   && x <= 32767 = UnassignedType (fromIntegral x)
               | x >= 32770 && x <= 65279 = UnassignedType (fromIntegral x)
               | x >= 65280 && x <= 65534 = PrivateUse (fromIntegral x)

fromRecordType :: RecordType -> Word16
fromRecordType A = 1  
fromRecordType NS = 2  
fromRecordType MD = 3  
fromRecordType MF = 4  
fromRecordType CNAME = 5  
fromRecordType SOA = 6  
fromRecordType MB = 7  
fromRecordType MG = 8  
fromRecordType MR = 9  
fromRecordType NULL = 10 
fromRecordType WKS = 11 
fromRecordType PTR = 12 
fromRecordType HINFO = 13 
fromRecordType MINFO = 14 
fromRecordType MX = 15 
fromRecordType TXT = 16 
fromRecordType RP = 17 
fromRecordType AFSDB = 18 
fromRecordType X25 = 19 
fromRecordType ISDN = 20 
fromRecordType RT = 21 
fromRecordType NSAP = 22 
fromRecordType NSAPPTR = 23 
fromRecordType SIG = 24 
fromRecordType KEY = 25 
fromRecordType PX = 26 
fromRecordType GPOS = 27 
fromRecordType AAAA = 28 
fromRecordType LOC = 29 
fromRecordType NXT = 30 
fromRecordType EID = 31 
fromRecordType NIMLOC = 32 
fromRecordType SRV = 33 
fromRecordType ATMA = 34 
fromRecordType NAPTR = 35 
fromRecordType KX = 36 
fromRecordType CERT = 37 
fromRecordType A6 = 38 
fromRecordType DNAME = 39 
fromRecordType SINK = 40 
fromRecordType OPT = 41 
fromRecordType APL = 42 
fromRecordType DS = 43 
fromRecordType SSHFP = 44 
fromRecordType IPSECKEY = 45 
fromRecordType RRSIG = 46 
fromRecordType NSEC = 47 
fromRecordType DNSKEY = 48 
fromRecordType DHCID = 49 
fromRecordType NSEC3 = 50 
fromRecordType NSEC3PARAM = 51 
fromRecordType TLSA = 52 
fromRecordType SMIMEA = 53 
fromRecordType HIP = 55 
fromRecordType NINFO = 56 
fromRecordType RKEY = 57 
fromRecordType TALINK = 58 
fromRecordType CDS = 59 
fromRecordType CDNSKEY = 60 
fromRecordType OPENPGPKEY = 61 
fromRecordType CSYNC = 62 
fromRecordType SPF = 99 
fromRecordType UINFO = 100  
fromRecordType UID = 101  
fromRecordType GID = 102  
fromRecordType UNSPEC = 103  
fromRecordType NID = 104  
fromRecordType L32 = 105  
fromRecordType L64 = 106  
fromRecordType LP = 107  
fromRecordType EUI48 = 108  
fromRecordType EUI64 = 109  
fromRecordType TKEY = 249  
fromRecordType TSIG = 250  
fromRecordType IXFR = 251  
fromRecordType AXFR = 252  
fromRecordType MAILB = 253  
fromRecordType MAILA = 254  
fromRecordType Asterisk = 255  
fromRecordType URI = 256  
fromRecordType CAA = 257  
fromRecordType AVC = 258  
fromRecordType TA = 32768 
fromRecordType DLV = 32769 
fromRecordType Reserved = 65535
fromRecordType (UnassignedType x) = fromIntegral x
fromRecordType (PrivateUse x) = fromIntegral x
