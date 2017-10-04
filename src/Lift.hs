{-# LANGUAGE TemplateHaskell #-}

module Lift
  ( liftByteString
  , liftAnyWithCompact
  ) where

import Compact
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Compact
import Language.Haskell.TH.Syntax
import System.IO.Unsafe
import Type.Reflection

liftByteString :: BS.ByteString -> Q Exp
liftByteString bs =
  [|unsafePerformIO $
    BS.unsafePackAddressLen
      $(lift $ BS.length bs)
      $(pure $ LitE $ StringPrimL $ BS.unpack bs)|]

liftAnyWithCompact :: Typeable a => a -> Q Exp
liftAnyWithCompact a =
  [|unsafePerformIO $ do
      c <-
        unsafeDecodeCompact
          $(do bs <-
                 runIO $ do
                   c <- compactWithSharing a
                   encodeCompact c
               liftByteString bs)
      pure $ getCompact c|]
