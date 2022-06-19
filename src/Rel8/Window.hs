{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Window
  ( Window(..)
  , Partition
  , over
  , partitionBy
  , orderPartitionBy
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.Window as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( toColumn, toPrimExpr )
import Rel8.Order( Order( Order ) )
import Rel8.Schema.HTable ( hfoldMap )
import Rel8.Table ( Table, toColumns )

-- semigroupoids
import Data.Functor.Apply ( Apply, WrappedApplicative(..) )


-- | 'Window' is an applicative functor that represents expressions that
-- contain
-- [window functions](https://www.postgresql.org/docs/current/tutorial-window.html).
-- 'Rel8.Query.Window.window' can be used to
-- evaluate these expressions over a particular query.
type Window :: Type -> Type
newtype Window a = Window (Opaleye.Window a)
  deriving newtype (Functor, Applicative)
  deriving (Apply) via (WrappedApplicative Window)


-- | In PostgreSQL, window functions must specify the \"window\" or
-- \"partition\" over which they operate. The syntax for this looks like:
-- @SUM(salary) OVER (PARTITION BY department)@. The Rel8 type 'Partition'
-- represents everything that comes after @OVER@.
--
-- 'Partition' is a 'Monoid', so 'Partition's created with 'partitionBy' and
-- 'orderPartitionBy' can be combined using '<>'.
type Partition :: Type
newtype Partition = Partition Opaleye.Partition
  deriving newtype (Semigroup, Monoid)


-- | 'over' adds a 'Partition' to a 'Window' expression.
--
-- @@@
-- 'Rel8.Table.Window.cumulative' ('Rel8.Expr.Aggregate.sum' salary) `over` 'partitionBy' department <> 'orderPartitionBy' salary 'Rel8.desc'
-- @@@
over :: Window a -> Partition -> Window a
over (Window w) (Partition p) = Window (w `Opaleye.over` p)
infixl 1 `over`


-- | Restricts a window function to operate only the group of rows that share
-- the same value(s) for the given expression(s).
partitionBy :: Table Expr a => a -> Partition
partitionBy = Partition . hfoldMap opartitionBy . toColumns
  where
    opartitionBy = Opaleye.partitionBy . toColumn . toPrimExpr


-- | Controls the order in which rows are processed by window functions. This
-- does not need to match the ordering of the overall query.
orderPartitionBy :: a -> Order a -> Partition
orderPartitionBy a (Order ordering) = Partition $ Opaleye.orderPartitionBy a ordering
