{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tuple
  ( Tuple
  , HasIndex
  , readTuple
  , writeTuple
  ) where

import Data.Kind
import Data.Primitive.SmallArray
import Data.Proxy
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Maybe
import GHC.Exts
import GHC.TypeLits
import System.IO.Unsafe
import Unsafe.Coerce

newtype Tuple (ts :: [Type]) =
  Tuple (SmallArray Any)

type Index t ts = FromJust (ElemIndex t ts)

type HasIndex t ts = KnownNat (Index t ts)

toIndex :: HasIndex t ts => p t -> p0 ts -> Sing (Index t ts)
{-# INLINE toIndex #-}
toIndex _ _ = sing

toIntIndex :: HasIndex t ts => p t -> p0 ts -> Int
{-# INLINE toIntIndex #-}
toIntIndex t ts = fromIntegral (fromSing (toIndex t ts))

toProxy :: t -> Proxy t
{-# INLINE toProxy #-}
toProxy _ = Proxy

readTuple :: HasIndex t ts => p t -> Tuple ts -> t
{-# INLINE readTuple #-}
readTuple p tup@(Tuple arr) =
  unsafeCoerce (indexSmallArray arr (toIntIndex p tup))

writeTuple :: HasIndex t ts => t -> Tuple ts -> Tuple ts
{-# INLINE writeTuple #-}
writeTuple t tup@(Tuple arr) =
  unsafeDupablePerformIO $ do
    buf <- thawSmallArray arr 0 (sizeofSmallArray arr)
    writeSmallArray buf (toIntIndex (toProxy t) tup) (unsafeCoerce t)
    arr' <- unsafeFreezeSmallArray buf
    pure (Tuple arr')
