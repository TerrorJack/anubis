{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Vec where

import Data.Kind
import Data.Singletons
import Data.Singletons.TH

data N
  = Z
  | S N

$(genSingletons [''N])

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
