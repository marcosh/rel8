{-# language MonoLocalBinds #-}

module Rel8.Table.Window
  ( cumulative
  )
where

-- base
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregates )
import qualified Rel8.Expr.Window as Expr
import Rel8.Schema.HTable ( htraverse )
import Rel8.Table ( fromColumns, toColumns )
import Rel8.Window ( Window )


-- | 'cumulative' allows the use of aggregation functions in 'Window'
-- expressions. In particular, @'cumulative' . 'Rel8.sum'@
-- (when combined with 'Rel8.Window.orderPartitionBy') gives a running total,
-- also known as a \"cumulative sum\", hence the name @cumulative@.
cumulative :: Aggregates aggregates exprs => aggregates -> Window exprs
cumulative = fmap fromColumns . htraverse Expr.cumulative . toColumns
