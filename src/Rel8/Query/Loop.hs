{-# language FlexibleContexts #-}

module Rel8.Query.Loop
  ( loop
  ) where

-- base
import Prelude

-- opaleye
import Opaleye.WithRecursive ( withRecursiveExplicit )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( fromOpaleye, toOpaleye )
import Rel8.Table ( Table )
import Rel8.Table.Opaleye ( binaryspec )


loop :: Table Expr a => Query a -> (a -> Query a) -> Query a
loop base recurse =
  fromOpaleye $ withRecursiveExplicit binaryspec base' recurse'
  where
    base' = toOpaleye base
    recurse' = toOpaleye . recurse
