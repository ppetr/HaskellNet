module Network.HaskellNet.IMAP.Connection
    ( IMAPConnection
    , IMAPConnectionM
    , withNextCommandNum
    , setMailboxInfo
    , modifyMailboxInfo
    , newConnection
    , mailbox
    , exists
    , recent
    , flags
    , permanentFlags
    , isWritable
    , isFlagWritable
    , uidNext
    , uidValidity
    , stream
    )
where

import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    , modifyIORef
    )
import Control.Applicative
    ( (<$>)
    , (<*>)
    )
import Control.Monad
    ( liftM
    )
import Control.Monad.Trans
    ( MonadIO(..)
    )

import Network.HaskellNet.BSStream
import Network.HaskellNet.IMAP.Types
    ( MailboxInfo(..)
    , emptyMboxInfo
    , MailboxName
    , Flag
    , UID
    )

type IMAPConnection = IMAPConnectionM IO
data IMAPConnectionM m =
    IMAPC { stream :: BSStreamM m
          , mboxInfo :: IORef MailboxInfo
          , nextCommandNum :: IORef Int
          }

newConnection :: (MonadIO m) => BSStreamM m -> m (IMAPConnectionM m)
newConnection s = liftIO $ IMAPC s <$> (newIORef emptyMboxInfo) <*> (newIORef 0)

getMailboxInfo :: (MonadIO m) => IMAPConnectionM m -> m MailboxInfo
getMailboxInfo c = liftIO $ readIORef $ mboxInfo c

mailbox :: (MonadIO m) => IMAPConnectionM m -> m MailboxName
mailbox c = _mailbox `liftM` getMailboxInfo c

exists :: (MonadIO m) => IMAPConnectionM m -> m Integer
exists c = _exists `liftM` getMailboxInfo c

recent :: (MonadIO m) => IMAPConnectionM m -> m Integer
recent c = _recent `liftM` getMailboxInfo c

flags :: (MonadIO m) => IMAPConnectionM m -> m [Flag]
flags c = _flags `liftM` getMailboxInfo c

permanentFlags :: (MonadIO m) => IMAPConnectionM m -> m [Flag]
permanentFlags c = _permanentFlags `liftM` getMailboxInfo c

isWritable :: (MonadIO m) => IMAPConnectionM m -> m Bool
isWritable c = _isWritable `liftM` getMailboxInfo c

isFlagWritable :: (MonadIO m) => IMAPConnectionM m -> m Bool
isFlagWritable c = _isFlagWritable `liftM` getMailboxInfo c

uidNext :: (MonadIO m) => IMAPConnectionM m -> m UID
uidNext c = _uidNext `liftM` getMailboxInfo c

uidValidity :: (MonadIO m) => IMAPConnectionM m -> m UID
uidValidity c = _uidValidity `liftM` getMailboxInfo c

withNextCommandNum :: (MonadIO m) => IMAPConnectionM m -> (Int -> m a) -> m (a, Int)
withNextCommandNum c act = do
  let ref = nextCommandNum c
  num <- liftIO $ readIORef ref
  result <- act num
  liftIO $ modifyIORef ref (+1)
  return (result, num)

setMailboxInfo :: (MonadIO m) => IMAPConnectionM m -> MailboxInfo -> m ()
setMailboxInfo c = liftIO . writeIORef (mboxInfo c)

modifyMailboxInfo :: (MonadIO m) => IMAPConnectionM m -> (MailboxInfo -> MailboxInfo) -> m ()
modifyMailboxInfo c f = liftIO $ modifyIORef (mboxInfo c) f
