{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.HTable.Nullify
  ( HNullify( HNullify )
  , hnulls, hnullify, hunnullify
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude hiding ( null )

-- rel8
import Rel8.Kind.Nullability
  ( Nullability( Nullable )
  , SNullability( SNullable )
  )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable
  ( HTable, HConstrainTable, HField
  , hfield, htabulate, htabulateA, htraverse, hdicts, hspecs
  )
import Rel8.Schema.HTable.Context ( H, HKTable )
import Rel8.Schema.HTable.Functor ( HFunctor, hmap )
import Rel8.Schema.Spec ( Context, Spec( Spec ), SSpec( SSpec ) )


type HNullify :: HKTable -> HKTable
data HNullify table context where
  HNullify :: table (H (NullifySpec context)) -> HNullify table (H context)


instance HFunctor HNullify where
  hmap f (HNullify table) = HNullify (f table)


type HNullifyField :: HKTable -> Context
data HNullifyField table spec where
  HNullifyField
    :: HField table ('Spec necessity nullability blueprint)
    -> HNullifyField table ('Spec necessity 'Nullable blueprint)


instance HTable table => HTable (HNullify table) where
  type HField (HNullify table) = HNullifyField table
  type HConstrainTable (HNullify table) c =
    HConstrainTable table (NullifySpecC c)

  hfield (HNullify table) (HNullifyField field) =
    getNullifySpec (hfield table field)

  htabulate f = HNullify $ htabulate $ \field -> case hfield hspecs field of
    SSpec {} -> NullifySpec (f (HNullifyField field))
  htraverse f (HNullify t) = HNullify <$> htraverse (traverseNullifySpec f) t

  hdicts :: forall c. HConstrainTable table (NullifySpecC c)
    => HNullify table (H (Dict c))
  hdicts = HNullify $ htabulate $ \field -> case hfield hspecs field of
    SSpec {} -> case hfield (hdicts @_ @(NullifySpecC c)) field of
      Dict -> NullifySpec Dict

  hspecs = HNullify $ htabulate $ \field -> case hfield hspecs field of
    SSpec necessity  _ blueprint info ->
      NullifySpec (SSpec necessity SNullable blueprint info)


type NullifyingSpec :: Type -> Type
type NullifyingSpec r = (Spec -> r) -> Spec -> r


type NullifySpec :: NullifyingSpec Type
data NullifySpec context spec where
  NullifySpec
    :: { getNullifySpec :: context ('Spec necessity 'Nullable blueprint) }
    -> NullifySpec context ('Spec necessity nullability blueprint)


type NullifySpecC :: NullifyingSpec Constraint
class
  ( forall necessity nullability blueprint.
    ( spec ~ 'Spec necessity nullability blueprint =>
       constraint ('Spec necessity 'Nullable blueprint)
    )
  ) => NullifySpecC constraint spec
instance
  ( spec ~ 'Spec necessity nullability blueprint
  , constraint ('Spec necessity 'Nullable blueprint)
  ) => NullifySpecC constraint spec


traverseNullifySpec :: forall context context' spec m. Functor m
  => (forall x. context x -> m (context' x))
  -> NullifySpec context spec -> m (NullifySpec context' spec)
traverseNullifySpec f (NullifySpec a) = NullifySpec <$> f a


hnulls :: HTable t
  => (forall necessity blueprint. ()
    => context ('Spec necessity 'Nullable blueprint))
  -> HNullify t (H context)
hnulls null = HNullify $ htabulate $ \field -> case hfield hspecs field of
  SSpec {} -> NullifySpec null


hnullify :: HTable t
  => (forall necessity nullability blueprint. ()
    => SSpec ('Spec necessity nullability blueprint)
    -> context ('Spec necessity nullability blueprint)
    -> context ('Spec necessity 'Nullable blueprint))
  -> t (H context)
  -> HNullify t (H context)
hnullify nullifier a = HNullify $ htabulate $ \field ->
  case hfield hspecs field of
    spec@SSpec {} -> NullifySpec (nullifier spec (hfield a field))


hunnullify :: (HTable t, Applicative m)
  => (forall necessity nullability blueprint. ()
    => SSpec ('Spec necessity nullability blueprint)
    -> context ('Spec necessity 'Nullable blueprint)
    -> m (context ('Spec necessity nullability blueprint)))
  -> HNullify t (H context)
  -> m (t (H context))
hunnullify unnullifier (HNullify as) =
  htabulateA $ \field -> case hfield hspecs field of
    spec@SSpec {} -> case hfield as field of
      NullifySpec a -> unnullifier spec a