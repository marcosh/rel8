{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Rel8.Query.SQL
  ( showQuery
  , sqlForQuery, sqlForQueryWithNames
  )
where

-- base
import Data.Foldable ( fold )
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Void ( Void )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.Sql as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.Print as Opaleye
import qualified Opaleye.Internal.Optimize as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye hiding ( Select )
import qualified Opaleye.Internal.Sql as Opaleye

-- rel8
import Rel8.Expr.Opaleye ( toPrimExpr )
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( toOpaleye )
import qualified Rel8.Query.Optimize as Rel8 ( optimize )
import Rel8.Schema.Context ( DB( DB ), Name( Name ) )
import Rel8.Schema.HTable ( htabulateA, hfield )
import Rel8.Table ( Table, toColumns )
import Rel8.Table.Name ( namesFromLabels )
import Rel8.Table.Recontextualize ( Selects )


-- | Convert a query to a 'String' containing the query as a @SELECT@
-- statement.
showQuery :: Table DB a => Query a -> String
showQuery = fold . sqlForQuery


sqlForQuery :: Table DB a
  => Query a -> Maybe String
sqlForQuery = sqlForQueryWithNames namesFromLabels . fmap toColumns


sqlForQueryWithNames :: Selects names exprs
  => names -> Query exprs -> Maybe String
sqlForQueryWithNames names query =
  show . Opaleye.ppSql . selectFrom names exprs <$> optimize primQuery
  where
    (exprs, primQuery, _) =
      Opaleye.runSimpleQueryArrStart (toOpaleye query) ()


optimize :: Opaleye.PrimQuery' a -> Maybe (Opaleye.PrimQuery' Void)
optimize = Opaleye.removeEmpty . Rel8.optimize . Opaleye.optimize


selectFrom :: Selects names exprs
  => names -> exprs -> Opaleye.PrimQuery' Void -> Opaleye.Select
selectFrom (toColumns -> names) (toColumns -> exprs) query =
  Opaleye.SelectFrom $ Opaleye.newSelect
    { Opaleye.attrs = Opaleye.SelectAttrs attributes
    , Opaleye.tables = Opaleye.oneTable select
    }
  where
    select = Opaleye.foldPrimQuery Opaleye.sqlQueryGenerator query
    attributes = getConst $ htabulateA $ \field -> case hfield names field of
      Name name -> case hfield exprs field of
        DB (toPrimExpr -> expr) -> Const (pure (makeAttr name expr))
    makeAttr label expr =
      (Opaleye.sqlExpr expr, Just (Opaleye.SqlColumn label))