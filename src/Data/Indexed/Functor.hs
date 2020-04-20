{-# language KindSignatures #-}
{-# language RankNTypes #-}

-- | Functors from indexed-types to types.

module Data.Indexed.Functor ( HFunctor( hmap ) ) where

-- base
import Data.Functor.Compose ( Compose( Compose ), getCompose )
import Data.Kind ( Type )


class HFunctor (f :: (Type -> Type) -> Type) where
  hmap :: (forall x. g x -> h x) -> f g -> f h


instance (Functor f, HFunctor g) => HFunctor (Compose f g) where
  hmap f =
    Compose . fmap (hmap f) . getCompose
