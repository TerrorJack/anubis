{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Vec where

import qualified Data.Foldable as F
import Data.Kind
import Data.Singletons
import Data.Singletons.TH
import GHC.Exts

data N
  = Z
  | S N

$(genSingletons [''N])

$(singDecideInstance ''N)

data Vec :: N -> Type -> Type where
  VNil :: Vec 'Z t
  VCons :: t -> Vec n t -> Vec ('S n) t

deriving instance Functor (Vec n)

deriving instance Foldable (Vec n)

deriving instance Traversable (Vec n)

deriving instance Eq t => Eq (Vec n t)

deriving instance Ord t => Ord (Vec n t)

deriving instance Show t => Show (Vec n t)

instance SingI n => Applicative (Vec n) where
  pure x =
    case sing :: Sing n of
      SZ -> VNil
      SS n1 ->
        case singInstance n1 of
          SingInstance -> VCons x (pure x)
  (<*>) =
    case sing :: Sing n of
      SZ -> \VNil VNil -> VNil
      SS n1 ->
        withSingI n1 $ \(VCons f fs) (VCons x xs) -> VCons (f x) (fs <*> xs)

data SomeVec t =
  forall n. SingI n =>
            SomeVec (Vec n t)

instance IsList (SomeVec t) where
  type Item (SomeVec t) = t
  fromList [] = SomeVec VNil
  fromList (x:xs) =
    case fromList xs of
      SomeVec v -> SomeVec (VCons x v)
  toList (SomeVec v) = toList v

instance SingI n => IsList (Vec n t) where
  type Item (Vec n t) = t
  fromList l =
    case fromList l of
      SomeVec (v :: Vec n1 t) ->
        case (sing :: Sing n1) %~ (sing :: Sing n) of
          Proved Refl -> v
          Disproved _ -> error "Length mismatch"
  toList = F.toList
