{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Rel8.Table.Opaleye
  ( aggregator
  , binaryspec
  , distinctspec
  , table
  , tableFields
  , unpackspec
  , valuesspec
  )
where

-- base
import Data.Functor ( (<&>) )
import Prelude hiding ( undefined )

-- opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.Binary as Opaleye
import qualified Opaleye.Internal.Distinct as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import qualified Opaleye.Internal.Values as Opaleye
import qualified Opaleye.Internal.Table as Opaleye

-- profunctors
import Data.Profunctor ( dimap, lmap )

-- rel8
import Rel8.Expr.Aggregate ( Aggregate( Aggregate ), Aggregator( Aggregator ) )
import qualified Rel8.Expr.Aggregate
import Rel8.Expr.Opaleye
  ( scastExpr
  , unsafeFromPrimExpr, unsafeToPrimExpr
  , unsafeTraversePrimExpr
  , columnToExpr, exprToColumn
  )
import Rel8.Kind.Blueprint ( typeInformationFromBlueprint )
import Rel8.Kind.Necessity ( SNecessity( SRequired, SOptional ) )
import Rel8.Schema.Context ( Aggregation(..), DB(..), Insert(..), Name(..) )
import Rel8.Schema.HTable ( htabulateA, hfield, htraverse, hspecs )
import Rel8.Schema.Spec ( SSpec( SSpec ) )
import Rel8.Schema.Table ( TableSchema(..) )
import Rel8.Table ( Table, fromColumns, toColumns )
import Rel8.Table.Map ( MapTable )
import Rel8.Table.Undefined ( undefined )


aggregator :: MapTable Aggregation DB aggregates exprs
  => Opaleye.Aggregator aggregates exprs
aggregator = Opaleye.Aggregator $ Opaleye.PackMap $ \f ->
  fmap fromColumns .
  htraverse (\(Aggregation aggregate) -> case aggregate of
    Aggregate {aggregator = maggregator, input, output} ->
      let
        aggregator' = maggregator <&>
          \Aggregator {operation, ordering, distinction} ->
            (operation, ordering, distinction)
      in
        DB . output <$> f (aggregator', input)) .
  toColumns


binaryspec :: Table DB a => Opaleye.Binaryspec a a
binaryspec = Opaleye.Binaryspec $ Opaleye.PackMap $ \f (as, bs) ->
  fmap fromColumns $ htabulateA $ \field ->
    case (hfield (toColumns as) field, hfield (toColumns bs) field) of
      (DB a, DB b) -> DB . unsafeFromPrimExpr <$>
        f (unsafeToPrimExpr a, unsafeToPrimExpr b)


distinctspec :: Table DB a => Opaleye.Distinctspec a a
distinctspec =
  Opaleye.Distinctspec $ Opaleye.Aggregator $ Opaleye.PackMap $ \f ->
    fmap fromColumns .
    htraverse (\(DB a) -> DB . unsafeFromPrimExpr <$> f (Nothing, unsafeToPrimExpr a)) .
    toColumns


table ::
  ( MapTable Name DB names exprs
  , MapTable Name Insert names inserts
  )
  => TableSchema names -> Opaleye.Table inserts exprs
table (TableSchema name schema columns) =
  Opaleye.Table qualified (tableFields columns)
  where
    qualified = case schema of
      Nothing -> name
      Just qualifier -> qualifier <> "." <> name


tableFields ::
  ( MapTable Name DB names exprs
  , MapTable Name Insert names inserts
  )
  => names -> Opaleye.TableFields inserts exprs
tableFields (toColumns -> names) = dimap toColumns fromColumns $
  htabulateA $ \field -> case hfield hspecs field of
    specs -> case hfield names field of
      name -> lmap (`hfield` field) (go specs name)
  where
    go :: SSpec spec -> Name spec -> Opaleye.TableFields (Insert spec) (DB spec)
    go (SSpec _ necessity _ blueprint) (Name name) = case necessity of
      SRequired ->
        lmap (\(RequiredInsert a) -> exprToColumn a) $
        DB . scastExpr info . columnToExpr <$> Opaleye.requiredTableField name
      SOptional -> lmap (\(OptionalInsert ma) -> exprToColumn <$> ma) $
        DB . scastExpr info . columnToExpr <$> Opaleye.optionalTableField name
      where
        info = typeInformationFromBlueprint blueprint


unpackspec :: Table DB a => Opaleye.Unpackspec a a
unpackspec = Opaleye.Unpackspec $ Opaleye.PackMap $ \f ->
  fmap fromColumns .
  htraverse (\(DB a) -> DB <$> unsafeTraversePrimExpr f a) .
  toColumns


valuesspec :: Table DB a => Opaleye.ValuesspecSafe a a
valuesspec = Opaleye.ValuesspecSafe (toPackMap undefined) unpackspec


toPackMap :: Table DB a
  => a -> Opaleye.PackMap Opaleye.PrimExpr Opaleye.PrimExpr () a
toPackMap as = Opaleye.PackMap $ \f () ->
  fmap fromColumns $
  htraverse (\(DB a) -> DB <$> unsafeTraversePrimExpr f a) $
  toColumns as