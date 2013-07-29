module Network.HaskellNet.IMAP
    ( connectIMAP, connectIMAPPort, connectStream
      -- * IMAP commands
      -- ** any state commands
    , noop, capability, logout
      -- ** not authenticated state commands
    , login, authenticate
      -- ** autenticated state commands
    , select, examine, create, delete, rename
    , subscribe, unsubscribe
    , list, lsub, status, append
      -- ** selected state commands
    , check, close, expunge
    , search, store, copy
      -- * fetch commands
    , fetch, fetchHeader, fetchSize, fetchHeaderFields, fetchHeaderFieldsNot
    , fetchFlags, fetchR, fetchByString, fetchByStringR
      -- * other types
    , Flag(..), Attribute(..), MailboxStatus(..)
    , SearchQuery(..), FlagsQuery(..)
    )
where

import Network
import Network.HaskellNet.BSStream
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.Types
import Network.HaskellNet.IMAP.Parsers
import qualified Network.HaskellNet.Auth as A

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Control.Monad
import Control.Monad.Trans (MonadIO(..))

import System.Time

import Data.Maybe
import Data.List hiding (delete)
import Data.Char

import Text.Packrat.Parse (Result)

-- suffixed by `s'
data SearchQuery = ALLs
                 | FLAG Flag
                 | UNFLAG Flag
                 | BCCs String
                 | BEFOREs CalendarTime
                 | BODYs String
                 | CCs String
                 | FROMs String
                 | HEADERs String String
                 | LARGERs Integer
                 | NEWs
                 | NOTs SearchQuery
                 | OLDs
                 | ONs CalendarTime
                 | ORs SearchQuery SearchQuery
                 | SENTBEFOREs CalendarTime
                 | SENTONs CalendarTime
                 | SENTSINCEs CalendarTime
                 | SINCEs CalendarTime
                 | SMALLERs Integer
                 | SUBJECTs String
                 | TEXTs String
                 | TOs String
                 | UIDs [UID]


instance Show SearchQuery where
    showsPrec d q = showParen (d>app_prec) $ showString $ showQuery q
        where app_prec = 10
              showQuery ALLs            = "ALL"
              showQuery (FLAG f)        = showFlag f
              showQuery (UNFLAG f)      = "UN" ++ showFlag f
              showQuery (BCCs addr)     = "BCC " ++ addr
              showQuery (BEFOREs t)     = "BEFORE " ++ dateToStringIMAP t
              showQuery (BODYs s)       = "BODY " ++ s
              showQuery (CCs addr)      = "CC " ++ addr
              showQuery (FROMs addr)    = "FROM " ++ addr
              showQuery (HEADERs f v)   = "HEADER " ++ f ++ " " ++ v
              showQuery (LARGERs siz)   = "LARGER {" ++ show siz ++ "}"
              showQuery NEWs            = "NEW"
              showQuery (NOTs qry)      = "NOT " ++ show qry
              showQuery OLDs            = "OLD"
              showQuery (ONs t)         = "ON " ++ dateToStringIMAP t
              showQuery (ORs q1 q2)     = "OR " ++ show q1 ++ " " ++ show q2 
              showQuery (SENTBEFOREs t) = "SENTBEFORE " ++ dateToStringIMAP t
              showQuery (SENTONs t)     = "SENTON " ++ dateToStringIMAP t
              showQuery (SENTSINCEs t)  = "SENTSINCE " ++ dateToStringIMAP t
              showQuery (SINCEs t)      = "SINCE " ++ dateToStringIMAP t
              showQuery (SMALLERs siz)  = "SMALLER {" ++ show siz ++ "}"
              showQuery (SUBJECTs s)    = "SUBJECT " ++ s
              showQuery (TEXTs s)       = "TEXT " ++ s
              showQuery (TOs addr)      = "TO " ++ addr
              showQuery (UIDs uids)     = concat $ intersperse "," $
                                          map show uids
              showFlag Seen        = "SEEN"
              showFlag Answered    = "ANSWERED"
              showFlag Flagged     = "FLAGGED"
              showFlag Deleted     = "DELETED"
              showFlag Draft       = "DRAFT"
              showFlag Recent      = "RECENT"
              showFlag (Keyword s) = "KEYWORD " ++ s

data FlagsQuery = ReplaceFlags [Flag]
                | PlusFlags [Flag]
                | MinusFlags [Flag]

----------------------------------------------------------------------
-- establish connection

connectIMAPPort :: (MonadIO m) => String -> PortNumber -> m (IMAPConnectionM m)
connectIMAPPort hostname port =
    handleToStream `liftM` liftIO (connectTo hostname (PortNumber port))
    >>= connectStream

connectIMAP :: (MonadIO m) => String -> m (IMAPConnectionM m)
connectIMAP hostname = connectIMAPPort hostname 143

connectStream :: (MonadIO m) => BSStreamM m -> m (IMAPConnectionM m)
connectStream s =
    do msg <- bsGetLine s
       unless (and $ BS.zipWith (==) msg (BS.pack "* OK")) $
              fail "cannot connect to the server"
       newConnection s

----------------------------------------------------------------------
-- normal send commands
sendCommand' :: (MonadIO m) => IMAPConnectionM m -> String -> m (ByteString, Int)
sendCommand' c cmdstr = do
  (_, num) <- withNextCommandNum c $ \num -> bsPutCrLf (stream c) $
              BS.pack $ show6 num ++ " " ++ cmdstr
  resp <- getResponse (stream c)
  return (resp, num)

show6 :: (Ord a, Num a, Show a) => a -> String
show6 n | n > 100000 = show n
        | n > 10000  = '0' : show n
        | n > 1000   = "00" ++ show n
        | n > 100    = "000" ++ show n
        | n > 10     = "0000" ++ show n
        | otherwise  = "00000" ++ show n

sendCommand :: (MonadIO m)
            => IMAPConnectionM m -> String
            -> (RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, v))
            -> m v
sendCommand imapc cmdstr pFunc =
    do (buf, num) <- sendCommand' imapc cmdstr
       let (resp, mboxUp, value) = eval pFunc (show6 num) buf
       case resp of
         OK _ _        -> do mboxUpdate imapc mboxUp
                             return value
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)

getResponse :: (MonadIO m) => BSStreamM m -> m ByteString
getResponse s = unlinesCRLF `liftM` getLs
    where unlinesCRLF = BS.concat . concatMap (:[crlfStr])
          getLs =
              do l <- strip `liftM` bsGetLine s
                 case () of
                   _ | isLiteral l ->  do l' <- getLiteral l (getLitLen l)
                                          ls <- getLs
                                          return (l' : ls)
                     | isTagged l -> (l:) `liftM` getLs
                     | otherwise -> return [l]
          getLiteral l len = 
              do lit <- bsGet s len
                 l2 <- strip `liftM` bsGetLine s
                 let l' = BS.concat [l, crlfStr, lit, l2]
                 if isLiteral l2
                   then getLiteral l' (getLitLen l2)
                   else return l'
          crlfStr = BS.pack "\r\n"
          isLiteral l = BS.last l == '}' &&
                        BS.last (fst (BS.spanEnd isDigit (BS.init l))) == '{'
          getLitLen = read . BS.unpack . snd . BS.spanEnd isDigit . BS.init
          isTagged l = BS.head l == '*' && BS.head (BS.tail l) == ' '

mboxUpdate :: (MonadIO m) => IMAPConnectionM m -> MboxUpdate -> m ()
mboxUpdate conn (MboxUpdate exists' recent') = do
  when (isJust exists') $
       modifyMailboxInfo conn $ \mbox -> mbox { _exists = fromJust exists' }

  when (isJust recent') $
       modifyMailboxInfo conn $ \mbox -> mbox { _recent = fromJust recent' }

----------------------------------------------------------------------
-- IMAP commands
--

noop :: (MonadIO m) => IMAPConnectionM m -> m ()
noop conn = sendCommand conn "NOOP" pNone

capability :: (MonadIO m) => IMAPConnectionM m -> m [String]
capability conn = sendCommand conn "CAPABILITY" pCapability

logout :: (MonadIO m) => IMAPConnectionM m -> m ()
logout c = do bsPutCrLf (stream c) $ BS.pack "a0001 LOGOUT"
              bsClose (stream c)

login :: (MonadIO m) => IMAPConnectionM m -> A.UserName -> A.Password -> m ()
login conn username password = sendCommand conn ("LOGIN " ++ username ++ " " ++ password)
                               pNone

authenticate :: (MonadIO m)
             => IMAPConnectionM m -> A.AuthType
             -> A.UserName -> A.Password -> m ()
authenticate conn A.LOGIN username password =
    do (_, num) <- sendCommand' conn "AUTHENTICATE LOGIN"
       bsPutCrLf (stream conn) $ BS.pack userB64
       bsGetLine (stream conn)
       bsPutCrLf (stream conn) $ BS.pack passB64
       buf <- getResponse $ stream conn
       let (resp, mboxUp, value) = eval pNone (show6 num) buf
       case resp of
         OK _ _        -> do mboxUpdate conn $ mboxUp
                             return value
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)
    where (userB64, passB64) = A.login username password
authenticate conn at username password =
    do (c, num) <- sendCommand' conn $ "AUTHENTICATE " ++ show at
       let challenge =
               if BS.take 2 c == BS.pack "+ "
               then A.b64Decode $ BS.unpack $ head $
                    dropWhile (isSpace . BS.last) $ BS.inits $ BS.drop 2 c
               else ""
       bsPutCrLf (stream conn) $ BS.pack $
                 A.auth at challenge username password
       buf <- getResponse $ stream conn
       let (resp, mboxUp, value) = eval pNone (show6 num) buf
       case resp of
         OK _ _        -> do mboxUpdate conn $ mboxUp
                             return value
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)

_select :: (MonadIO m) => String -> IMAPConnectionM m -> String -> m ()
_select cmd conn mboxName =
    do mbox' <- sendCommand conn (cmd ++ mboxName) pSelect
       setMailboxInfo conn $ mbox' { _mailbox = mboxName }

select :: (MonadIO m) => IMAPConnectionM m -> MailboxName -> m ()
select = _select "SELECT "

examine :: (MonadIO m) => IMAPConnectionM m -> MailboxName -> m ()
examine = _select "EXAMINE "

create :: (MonadIO m) => IMAPConnectionM m -> MailboxName -> m ()
create conn mboxname = sendCommand conn ("CREATE " ++ mboxname) pNone

delete :: (MonadIO m) => IMAPConnectionM m -> MailboxName -> m ()
delete conn mboxname = sendCommand conn ("DELETE " ++ mboxname) pNone

rename :: (MonadIO m) => IMAPConnectionM m -> MailboxName -> MailboxName -> m ()
rename conn mboxorg mboxnew =
    sendCommand conn ("RENAME " ++ mboxorg ++ " " ++ mboxnew) pNone

subscribe :: (MonadIO m) => IMAPConnectionM m -> MailboxName -> m ()
subscribe conn mboxname = sendCommand conn ("SUBSCRIBE " ++ mboxname) pNone

unsubscribe :: (MonadIO m) => IMAPConnectionM m -> MailboxName -> m ()
unsubscribe conn mboxname = sendCommand conn ("UNSUBSCRIBE " ++ mboxname) pNone

list :: (MonadIO m) => IMAPConnectionM m -> m [([Attribute], MailboxName)]
list conn = (map (\(a, _, m) -> (a, m))) `liftM` listFull conn "\"\"" "*"

lsub :: (MonadIO m) => IMAPConnectionM m -> m [([Attribute], MailboxName)]
lsub conn = (map (\(a, _, m) -> (a, m))) `liftM` lsubFull conn "\"\"" "*"

listFull :: (MonadIO m)
         => IMAPConnectionM m -> String -> String
         -> m [([Attribute], String, MailboxName)]
listFull conn ref pat = sendCommand conn (unwords ["LIST", ref, pat]) pList

lsubFull :: (MonadIO m)
         => IMAPConnectionM m -> String -> String
         -> m [([Attribute], String, MailboxName)]
lsubFull conn ref pat = sendCommand conn (unwords ["LSUB", ref, pat]) pLsub

status :: (MonadIO m)
       => IMAPConnectionM m -> MailboxName -> [MailboxStatus]
       -> m [(MailboxStatus, Integer)]
status conn mbox stats =
    let cmd = "STATUS " ++ mbox ++ " (" ++ (unwords $ map show stats) ++ ")"
    in sendCommand conn cmd pStatus

append :: (MonadIO m) => IMAPConnectionM m -> MailboxName -> ByteString -> m ()
append conn mbox mailData = appendFull conn mbox mailData [] Nothing

appendFull :: (MonadIO m)
           => IMAPConnectionM m -> MailboxName -> ByteString
           -> [Flag] -> Maybe CalendarTime -> m ()
appendFull conn mbox mailData flags' time =
    do (buf, num) <- sendCommand' conn
                (unwords ["APPEND", mbox
                         , fstr, tstr,  "{" ++ show len ++ "}"])
       unless (BS.null buf || (BS.head buf /= '+')) $
              fail "illegal server response"
       mapM_ (bsPutCrLf $ stream conn) mailLines
       buf2 <- getResponse $ stream conn
       let (resp, mboxUp, ()) = eval pNone (show6 num) buf2
       case resp of
         OK _ _ -> mboxUpdate conn mboxUp
         NO _ msg -> fail ("NO: "++msg)
         BAD _ msg -> fail ("BAD: "++msg)
         PREAUTH _ msg -> fail ("PREAUTH: "++msg)
    where mailLines = BS.lines mailData
          len       = sum $ map ((2+) . BS.length) mailLines
          tstr      = maybe "" show time
          fstr      = unwords $ map show flags'

check :: (MonadIO m) => IMAPConnectionM m -> m ()
check conn = sendCommand conn "CHECK" pNone

close :: (MonadIO m) => IMAPConnectionM m -> m ()
close conn =
    do sendCommand conn "CLOSE" pNone
       setMailboxInfo conn emptyMboxInfo

expunge :: (MonadIO m) => IMAPConnectionM m -> m [Integer]
expunge conn = sendCommand conn "EXPUNGE" pExpunge

search :: (MonadIO m) => IMAPConnectionM m -> [SearchQuery] -> m [UID]
search conn queries = searchCharset conn "" queries

searchCharset :: (MonadIO m)
              => IMAPConnectionM m -> Charset -> [SearchQuery]
              -> m [UID]
searchCharset conn charset queries =
    sendCommand conn ("UID SEARCH "
                    ++ (if not . null $ charset
                           then charset ++ " "
                           else "")
                    ++ unwords (map show queries)) pSearch

fetch :: (MonadIO m) => IMAPConnectionM m -> UID -> m ByteString
fetch conn uid =
    do lst <- fetchByString conn uid "BODY[]"
       return $ maybe BS.empty BS.pack $ lookup "BODY[]" lst

fetchHeader :: (MonadIO m) => IMAPConnectionM m -> UID -> m ByteString
fetchHeader conn uid =
    do lst <- fetchByString conn uid "BODY[HEADER]"
       return $ maybe BS.empty BS.pack $ lookup "BODY[HEADER]" lst

fetchSize :: (MonadIO m) => IMAPConnectionM m -> UID -> m Int
fetchSize conn uid =
    do lst <- fetchByString conn uid "RFC822.SIZE"
       return $ maybe 0 read $ lookup "RFC822.SIZE" lst

fetchHeaderFields :: (MonadIO m)
                  => IMAPConnectionM m
                  -> UID -> [String] -> m ByteString
fetchHeaderFields conn uid hs =
    do lst <- fetchByString conn uid ("BODY[HEADER.FIELDS "++unwords hs++"]")
       return $ maybe BS.empty BS.pack $
              lookup ("BODY[HEADER.FIELDS "++unwords hs++"]") lst

fetchHeaderFieldsNot :: (MonadIO m)
                     => IMAPConnectionM m
                     -> UID -> [String] -> m ByteString
fetchHeaderFieldsNot conn uid hs =
    do let fetchCmd = "BODY[HEADER.FIELDS.NOT "++unwords hs++"]"
       lst <- fetchByString conn uid fetchCmd
       return $ maybe BS.empty BS.pack $ lookup fetchCmd lst

fetchFlags :: (MonadIO m) => IMAPConnectionM m -> UID -> m [Flag]
fetchFlags conn uid =
    do lst <- fetchByString conn uid "FLAGS"
       return $ getFlags $ lookup "FLAGS" lst
    where getFlags Nothing  = []
          getFlags (Just s) = eval' dvFlags "" s

fetchR :: (MonadIO m)
       => IMAPConnectionM m -> (UID, UID)
       -> m [(UID, ByteString)]
fetchR conn r =
    do lst <- fetchByStringR conn r "BODY[]"
       return $ map (\(uid, vs) -> (uid, maybe BS.empty BS.pack $
                                       lookup "BODY[]" vs)) lst
fetchByString :: (MonadIO m)
              => IMAPConnectionM m -> UID -> String
              -> m [(String, String)]
fetchByString conn uid command =
    do lst <- fetchCommand conn ("UID FETCH "++show uid++" "++command) id
       return $ snd $ head lst

fetchByStringR :: (MonadIO m)
               => IMAPConnectionM m -> (UID, UID) -> String
               -> m [(UID, [(String, String)])]
fetchByStringR conn (s, e) command =
    fetchCommand conn ("UID FETCH "++show s++":"++show e++" "++command) proc
    where proc (n, ps) =
              (maybe (toEnum (fromIntegral n)) read (lookup "UID" ps), ps)

fetchCommand :: (MonadIO m)
             => IMAPConnectionM m -> String
             -> ((Integer, [(String, String)]) -> b) -> m [b]
fetchCommand conn command proc =
    (map proc) `liftM` sendCommand conn command pFetch

storeFull :: (MonadIO m)
          => IMAPConnectionM m -> String -> FlagsQuery -> Bool
          -> m [(UID, [Flag])]
storeFull conn uidstr query isSilent =
    fetchCommand conn ("UID STORE " ++ uidstr ++ flgs query) procStore
    where fstrs fs = "(" ++ (concat $ intersperse " " $ map show fs) ++ ")"
          toFStr s fstrs' =
              s ++ (if isSilent then ".SILENT" else "") ++ " " ++ fstrs'
          flgs (ReplaceFlags fs) = toFStr "FLAGS" $ fstrs fs
          flgs (PlusFlags fs)    = toFStr "+FLAGS" $ fstrs fs
          flgs (MinusFlags fs)   = toFStr "-FLAGS" $ fstrs fs
          procStore (n, ps) = (maybe (toEnum (fromIntegral n)) read
                                         (lookup "UID" ps)
                              ,maybe [] (eval' dvFlags "") (lookup "FLAG" ps))


store :: (MonadIO m) => IMAPConnectionM m -> UID -> FlagsQuery -> m ()
store conn i q = storeFull conn (show i) q True >> return ()

copyFull :: (MonadIO m) => IMAPConnectionM m -> String -> String -> m ()
copyFull conn uidStr mbox =
    sendCommand conn ("UID COPY " ++ uidStr ++ " " ++ mbox) pNone

copy :: (MonadIO m) => IMAPConnectionM m -> UID -> MailboxName -> m ()
copy conn uid mbox     = copyFull conn (show uid) mbox

----------------------------------------------------------------------
-- auxialiary functions

dateToStringIMAP :: CalendarTime -> String
dateToStringIMAP date = concat $ intersperse "-" [show2 $ ctDay date
                                                 , showMonth $ ctMonth date
                                                 , show $ ctYear date]
    where show2 n | n < 10    = '0' : show n
                  | otherwise = show n
          showMonth January   = "Jan"
          showMonth February  = "Feb"
          showMonth March     = "Mar"
          showMonth April     = "Apr"
          showMonth May       = "May"
          showMonth June      = "Jun"
          showMonth July      = "Jul"
          showMonth August    = "Aug"
          showMonth September = "Sep"
          showMonth October   = "Oct"
          showMonth November  = "Nov"
          showMonth December  = "Dec"

strip :: ByteString -> ByteString
strip = fst . BS.spanEnd isSpace . BS.dropWhile isSpace

crlf :: BS.ByteString
crlf = BS.pack "\r\n"

bsPutCrLf :: (MonadIO m) => BSStreamM m -> ByteString -> m ()
bsPutCrLf h s = bsPut h s >> bsPut h crlf >> bsFlush h
