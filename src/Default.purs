module Default where

import Control.Category (identity, (<<<))
import Data.Function (const)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Heterogeneous.Mapping (class HMap, class Mapping)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))
import Type.Row (RProxy)


data Justify = Justify

instance anyJustify ∷ Mapping Justify n (Maybe n) where
  mapping Justify = Just


data Nothingify = Nothingify

instance anyNothingify ∷ Mapping Nothingify n (Maybe n) where
  mapping Nothingify = const Nothing


class NothingFields
  (rl :: RowList) (from :: # Type) (to :: # Type)
  | rl -> from to where
  nothingFields :: forall g. g rl -> Builder (Record from) (Record to)

instance nothingFieldsNil ::
  NothingFields Nil () () where
    nothingFields _ = identity

instance nothingFieldsCons ::
  ( IsSymbol name
  , NothingFields tail from from'
  , Row.Lacks name from'
  , Row.Cons name (Maybe a) from' to
  ) => NothingFields (Cons name (Maybe a) tail) from to where
  nothingFields _ =
    let
      first :: Builder (Record from') (Record to)
      first = Builder.insert (SProxy :: SProxy name) Nothing
    in
      first <<< nothingFields (RLProxy :: RLProxy tail)


buildDefault
  ∷ ∀ r nr nrl
  . HMap Nothingify { | r } { | nr }
  ⇒ RowToList nr nrl
  ⇒ NothingFields nrl () nr
  ⇒ RProxy r -> { | nr }
buildDefault _ = Builder.build (nothingFields (RLProxy :: RLProxy nrl)) {}


-- makeOptionalDefault
--   ∷ ∀ provided providedJustified missing  allNothingified ret retNubbed nrl all
--   . HMap Justify { | provided } { | providedJustified }
--   ⇒ HMap Nothingify { | all } { | allNothingified }
--   ⇒ Union providedJustified allNothingified ret
--   ⇒ Union provided missing all
--   ⇒ RowToList allNothingified nrl
--   ⇒ NothingFields nrl () allNothingified
--   ⇒ Nub ret retNubbed
--   ⇒ { | provided} → { | retNubbed}
-- makeOptionalDefault prov = merge (hmap Justify prov) (buildDefault (RProxy :: RProxy all))
