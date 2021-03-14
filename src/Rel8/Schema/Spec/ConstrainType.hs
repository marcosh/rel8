{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Spec.ConstrainType
  ( ConstrainType
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Kind.Blueprint ( ToType )
import Rel8.Schema.Spec ( Spec( Spec ) )


type ConstrainType :: (Type -> Constraint) -> Spec -> Constraint
class
  ( forall necessity nullability blueprint a. ()
     => (spec ~ 'Spec necessity nullability blueprint, a ~ ToType blueprint)
     => constraint a
  ) =>
  ConstrainType constraint spec
instance
  ( spec ~ 'Spec necessity nullability blueprint
  , a ~ ToType blueprint
  , constraint a
  ) =>
  ConstrainType constraint spec