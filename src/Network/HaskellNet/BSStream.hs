-- |This module provides a byte string \"stream\" interface.  This
-- interface provides some common operations on a value which
-- supports reading and writing byte strings.
module Network.HaskellNet.BSStream
    ( BSStream
    , BSStreamM(..)
    , handleToStream
    )
where

import Control.Monad.Trans (MonadIO(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.IO

-- |A byte string stream.
type BSStream = BSStreamM IO
-- |A byte string stream that uses a given @MonadIO@ monad.
data BSStreamM m =
    BSStream { bsGetLine :: m ByteString
             -- ^Read a line from the stream.  Should return the line
             -- which was read, including the newline.
             , bsGet :: Int -> m ByteString
             -- ^Read the specified number of bytes from the stream.
             -- Should block until the requested bytes can be read.
             , bsPut :: ByteString -> m ()
             -- ^Write the specified byte string to the stream.
             -- Should flush the stream after writing.
             , bsFlush :: m ()
             -- ^Flush the stream.
             , bsClose :: m ()
             -- ^Close the stream.
             , bsIsOpen :: m Bool
             -- ^Is the stream open?
             }

-- |Build a byte string stream which operates on a 'Handle'.
handleToStream :: (MonadIO m) => Handle -> BSStreamM m
handleToStream h =
    BSStream { bsGetLine = liftIO $ BS.hGetLine h
             , bsGet = liftIO . BS.hGet h
             , bsPut = liftIO . (\s -> BS.hPut h s >> hFlush h)
             , bsFlush = liftIO $ hFlush h
             , bsClose = liftIO $ do
                 op <- hIsOpen h
                 if op then (hClose h) else return ()
             , bsIsOpen = liftIO $ hIsOpen h
             }
