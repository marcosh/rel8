module Rel8.Expr.Window
  ( cumulative
  , rowNumber
  , rank
  , denseRank
  , percentRank
  , cumeDist
  , ntile
  , lag
  , lead
  , firstValue
  , lastValue
  , nthValue
  )
where

-- base
import Data.Int ( Int32, Int64 )
import Prelude

-- opaleye
import qualified Opaleye.Window as Opaleye

-- rel8
import Rel8.Aggregate ( Aggregate( Aggregate ) )
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( fromColumn, fromPrimExpr, toColumn, toPrimExpr )
import Rel8.Schema.Null ( Nullify )
import Rel8.Window ( Window( Window ) )


cumulative :: Aggregate a -> Window (Expr a)
cumulative (Aggregate aggregator) = Window $ Opaleye.cumulative aggregator ()


-- | [@row_number()@](https://www.postgresql.org/docs/current/functions-window.html)
rowNumber :: Window (Expr Int64)
rowNumber = Window $ fromPrimExpr . fromColumn <$> Opaleye.rowNumber


-- | [@rank()@](https://www.postgresql.org/docs/current/functions-window.html)
rank :: Window (Expr Int64)
rank = Window $ fromPrimExpr . fromColumn <$> Opaleye.rank


-- | [@dense_rank()@](https://www.postgresql.org/docs/current/functions-window.html)
denseRank :: Window (Expr Int64)
denseRank = Window $ fromPrimExpr . fromColumn <$> Opaleye.denseRank


-- | [@percent_rank()@](https://www.postgresql.org/docs/current/functions-window.html)
percentRank :: Window (Expr Double)
percentRank = Window $ fromPrimExpr . fromColumn <$> Opaleye.percentRank


-- | [@cume_dist()@](https://www.postgresql.org/docs/current/functions-window.html)
cumeDist :: Window (Expr Double)
cumeDist = Window $ fromPrimExpr . fromColumn <$> Opaleye.cumeDist


-- | [@ntile(num_buckets)@](https://www.postgresql.org/docs/current/functions-window.html)
ntile :: Expr Int32 -> Window (Expr Int32)
ntile buckets = Window $ fromPrimExpr . fromColumn <$>
  Opaleye.ntile (toColumn (toPrimExpr buckets))


-- | [@lag(value, offset, default)@](https://www.postgresql.org/docs/current/functions-window.html)
lag :: Expr a -> Expr Int32 -> Expr a -> Window (Expr a)
lag a offset def = Window $ fromPrimExpr . fromColumn <$>
  Opaleye.lag (toColumn (toPrimExpr a)) (toColumn (toPrimExpr offset))
    (toColumn (toPrimExpr def))


-- | [@lead(value, offset, default)@](https://www.postgresql.org/docs/current/functions-window.html)
lead :: Expr a -> Expr Int32 -> Expr a -> Window (Expr a)
lead a offset def = Window $ fromPrimExpr . fromColumn <$>
  Opaleye.lead (toColumn (toPrimExpr a)) (toColumn (toPrimExpr offset))
    (toColumn (toPrimExpr def))


-- | [@first_value(value)@](https://www.postgresql.org/docs/current/functions-window.html)
firstValue :: Expr a -> Window (Expr a)
firstValue a = Window $ fromPrimExpr . fromColumn <$>
  Opaleye.firstValue (toColumn (toPrimExpr a))


-- | [@last_value(value)@](https://www.postgresql.org/docs/current/functions-window.html)
lastValue :: Expr a -> Window (Expr a)
lastValue a = Window $ fromPrimExpr . fromColumn <$>
  Opaleye.lastValue (toColumn (toPrimExpr a))


-- | [@nth_value(value, n)@](https://www.postgresql.org/docs/current/functions-window.html)
nthValue :: Expr a -> Expr Int32 -> Window (Expr (Nullify a))
nthValue a n = Window $ fromPrimExpr . fromColumn <$>
  Opaleye.nthValue (toColumn (toPrimExpr a)) (toColumn (toPrimExpr n))
