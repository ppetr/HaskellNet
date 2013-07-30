{-# LANGUAGE ScopedTypeVariables #-}
module Network.HaskellNet.SMTP
    ( -- * Types
      Command(..)
    , Response(..)
    , SMTPConnection
    , SMTPConnectionM
      -- * Establishing Connection
    , connectSMTPPort
    , connectSMTP
    , connectStream
      -- * Operation to a Connection
    , sendCommand
    , closeSMTP
      -- * Other Useful Operations 
    , sendMail
    , doSMTPPort
    , doSMTP
    , doSMTPStream
    , sendMimeMail
    )
    where

import Network.HaskellNet.BSStream
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.BSD (getHostName)
import Network

import Control.Exception
import Control.Monad (liftM, unless)
import Control.Monad.Trans (MonadIO(..))

import Data.Char (isDigit)

import Network.HaskellNet.Auth

import System.IO

import Network.Mail.Mime
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

import Prelude hiding (catch)

type SMTPConnection = SMTPConnectionM IO
-- The response field seems to be unused. It's saved at one place, but never
-- retrieved.
data SMTPConnectionM m = SMTPC { bsstream :: !(BSStreamM m), _response :: ![ByteString] }

data Command = HELO String
             | EHLO String
             | MAIL String
             | RCPT String
             | DATA ByteString
             | EXPN String
             | VRFY String
             | HELP String
             | AUTH AuthType UserName Password
             | NOOP
             | RSET
             | QUIT
               deriving (Show, Eq)

type ReplyCode = Int

data Response = Ok
              | SystemStatus
              | HelpMessage
              | ServiceReady
              | ServiceClosing
              | UserNotLocal
              | CannotVerify
              | StartMailInput
              | ServiceNotAvailable
              | MailboxUnavailable
              | ErrorInProcessing
              | InsufficientSystemStorage
              | SyntaxError
              | ParameterError
              | CommandNotImplemented
              | BadSequence
              | ParameterNotImplemented
              | MailboxUnavailableError
              | UserNotLocalError
              | ExceededStorage
              | MailboxNotAllowed
              | TransactionFailed
                deriving (Show, Eq)

-- | connecting SMTP server with the specified name and port number.
connectSMTPPort :: (MonadIO m)
                => String     -- ^ name of the server
                -> PortNumber -- ^ port number
                -> m (SMTPConnectionM m)
connectSMTPPort hostname port =
    (handleToStream `liftM` liftIO (connectTo hostname (PortNumber port)))
    >>= connectStream

-- | connecting SMTP server with the specified name and port 25.
connectSMTP :: (MonadIO m)
            => String     -- ^ name of the server
            -> m (SMTPConnectionM m)
connectSMTP = flip connectSMTPPort 25

tryCommand :: (MonadIO m)
           => SMTPConnectionM m -> Command -> Int -> ReplyCode
           -> m ByteString
tryCommand conn cmd tries expectedReply = do
  (code, msg) <- sendCommand conn cmd
  case () of
    _ | code == expectedReply   -> return msg
    _ | tries > 1               ->
          tryCommand conn cmd (tries - 1) expectedReply
    _ | otherwise               -> do
          bsClose (bsstream conn)
          fail $ "cannot execute command " ++ show cmd ++
                 ", expected reply code " ++ show expectedReply ++
                 ", but received " ++ show code ++ " " ++ BS.unpack msg

-- | create SMTPConnectionM m from already connected Stream
connectStream :: (MonadIO m) => BSStreamM m -> m (SMTPConnectionM m)
connectStream st =
    do (code1, _) <- parseResponse st
       unless (code1 == 220) $
              do bsClose st
                 fail "cannot connect to the server"
       senderHost <- liftIO getHostName
       msg <- tryCommand (SMTPC st []) (EHLO senderHost) 3 250
       return (SMTPC st (tail $ BS.lines msg))

parseResponse :: (MonadIO m) => BSStreamM m -> m (ReplyCode, ByteString)
parseResponse st =
    do (code, bdy) <- readLines
       return (read $ BS.unpack code, BS.unlines bdy)
    where readLines =
              do l <- bsGetLine st
                 let (c, bdy) = BS.span isDigit l
                 if not (BS.null bdy) && BS.head bdy == '-'
                    then do (c2, ls) <- readLines
                            return (c2, (BS.tail bdy:ls))
                    else return (c, [BS.tail bdy])


-- | send a method to a server
sendCommand :: (MonadIO m) => SMTPConnectionM m -> Command -> m (ReplyCode, ByteString)
sendCommand (SMTPC conn _) (DATA dat) =
    do bsPutCrLf conn $ BS.pack "DATA"
       (code, _) <- parseResponse conn
       unless (code == 354) $ fail "this server cannot accept any data."
       mapM_ sendLine $ BS.lines dat ++ [BS.pack "."]
       parseResponse conn
    where sendLine l = bsPutCrLf conn l
sendCommand (SMTPC conn _) (AUTH LOGIN username password) =
    do bsPutCrLf conn command
       (_, _) <- parseResponse conn
       bsPutCrLf conn $ BS.pack userB64
       (_, _) <- parseResponse conn
       bsPutCrLf conn $ BS.pack passB64
       parseResponse conn
    where command = BS.pack $ "AUTH LOGIN"
          (userB64, passB64) = login username password
sendCommand (SMTPC conn _) (AUTH at username password) =
    do bsPutCrLf conn command
       (code, msg) <- parseResponse conn
       unless (code == 334) $ fail "authentication failed."
       bsPutCrLf conn $ BS.pack $ auth at (BS.unpack msg) username password
       parseResponse conn
    where command = BS.pack $ unwords ["AUTH", show at]
sendCommand (SMTPC conn _) meth =
    do bsPutCrLf conn $ BS.pack command
       parseResponse conn
    where command = case meth of
                      (HELO param) -> "HELO " ++ param
                      (EHLO param) -> "EHLO " ++ param
                      (MAIL param) -> "MAIL FROM:<" ++ param ++ ">"
                      (RCPT param) -> "RCPT TO:<" ++ param ++ ">"
                      (EXPN param) -> "EXPN " ++ param
                      (VRFY param) -> "VRFY " ++ param
                      (HELP msg)   -> if null msg
                                        then "HELP\r\n"
                                        else "HELP " ++ msg
                      NOOP         -> "NOOP"
                      RSET         -> "RSET"
                      QUIT         -> "QUIT"
                      (DATA _)     ->
                          error "BUG: DATA pattern should be matched by sendCommand patterns"
                      (AUTH _ _ _)     ->
                          error "BUG: AUTH pattern should be matched by sendCommand patterns"

-- | close the connection.  This function send the QUIT method, so you
-- do not have to QUIT method explicitly.
closeSMTP :: (MonadIO m) => SMTPConnectionM m -> m ()
closeSMTP (SMTPC conn _) = bsClose conn

{-
I must be being stupid here

I can't seem to be able to catch the exception arising from the
connection already being closed this would be the correct way to do it
but instead we're being naughty above by just closes the connection
without first sending QUIT

closeSMTP c@(SMTPC conn _) =
    do sendCommand c QUIT
       bsClose conn `catch` \(_ :: (MonadIO m) => IOException) -> return ()
-}

-- | sending a mail to a server. This is achieved by sendMessage.  If
-- something is wrong, it raises an IOexception.
sendMail :: (MonadIO m) => String     -- ^ sender mail
         -> [String]   -- ^ receivers
         -> ByteString -- ^ data
         -> SMTPConnectionM m
         -> m ()
sendMail sender receivers dat conn = do
                 sendAndCheck (MAIL sender)
                 mapM_ (sendAndCheck . RCPT) receivers
                 sendAndCheck (DATA dat)
                 return ()
  where
    -- Try the command once and @fail@ if the response isn't 250.
    sendAndCheck cmd = tryCommand conn cmd 1 250

-- | doSMTPPort open a connection, and do an m action with the
-- connection, and then close it.
doSMTPPort :: String -> PortNumber -> (SMTPConnection -> IO a) -> IO a
doSMTPPort host port execution =
    bracket (connectSMTPPort host port) closeSMTP execution

-- | doSMTP is similar to doSMTPPort, except that it does not require
-- port number but connects to the server with port 25.
doSMTP :: String -> (SMTPConnection -> IO a) -> IO a
doSMTP host execution = doSMTPPort host 25 execution

-- | doSMTPStream is similar to doSMTPPort, except that its argument
-- is a Stream data instead of hostname and port number.
doSMTPStream :: BSStream -> (SMTPConnection -> IO a) -> IO a
doSMTPStream s execution = bracket (connectStream s) closeSMTP execution

sendMimeMail :: (MonadIO m) => String -> String -> String -> LT.Text
             -> LT.Text -> [(T.Text, FilePath)] -> SMTPConnectionM m -> m ()
sendMimeMail to from subject plainBody htmlBody attachments con = do
  myMail <- liftIO $ simpleMail (address to) (address from) (T.pack subject)
            plainBody htmlBody attachments
  renderedMail <- liftIO $ renderMail' myMail
  sendMail from [to] (lazyToStrict renderedMail) con
  closeSMTP con
  where
    address = Address Nothing . T.pack

-- haskellNet uses strict bytestrings
-- TODO: look at making haskellnet lazy
lazyToStrict :: B.ByteString -> S.ByteString
lazyToStrict = S.concat . B.toChunks

crlf :: BS.ByteString
crlf = BS.pack "\r\n"

bsPutCrLf :: (MonadIO m) => BSStreamM m -> ByteString -> m ()
bsPutCrLf h s = bsPut h s >> bsPut h crlf >> bsFlush h
