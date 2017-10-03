module Compact
  ( encodeCompact
  , unsafeDecodeCompact
  ) where

import Control.Exception
import qualified Data.ByteString as BS
import Data.Compact
import Data.Compact.Serialize
import System.Directory
import System.IO
import Type.Reflection

withBinaryTempFile :: (FilePath -> Handle -> IO a) -> IO a
withBinaryTempFile f =
  bracket
    (do tmpdir <- getTemporaryDirectory
        openBinaryTempFile tmpdir "")
    (\(p, _) -> removeFile p)
    (uncurry f)

encodeCompact :: Typeable a => Compact a -> IO BS.ByteString
encodeCompact c =
  withBinaryTempFile $ \p h -> do
    hPutCompact h c
    hClose h
    BS.readFile p

unsafeDecodeCompact :: Typeable a => BS.ByteString -> IO (Compact a)
unsafeDecodeCompact bs =
  withBinaryTempFile $ \p h -> do
    BS.hPut h bs
    hClose h
    r <- unsafeReadCompact p
    case r of
      Left err -> fail err
      Right c -> pure c
