-- Depends on packages numeric-prelude and groups
-- https://hackage.haskell.org/package/numeric-prelude-0.4.3.1
-- https://hackage.haskell.org/package/groups-0.4.1.0
module GroupsOfUnitsOfFiniteRings
  ( GroupOfUnits
  , UnitGroupElem
  , groupOfUnits
  ) where

import qualified Algebra.Ring as Ring
import Data.Group
import qualified Data.Map as Map
import qualified Data.Set as Set

-- a unit is never separate from the group, to avoid recomputing groupOfUnits
data UnitGroupElem a =
  UnitGroupElem (GroupOfUnits a) a

data GroupOfUnits a =
  GroupOfUnits
    { _elems :: Set.Set a
    , _inverses :: Map.Map a a
    }

-- The group of units of an arbitrary finite ring. A unit of a ring is an element of the ring, a,
-- which has an inverse, b, such that a * b = b * a = Ring.one.
groupOfUnits :: (Ring.C a, Bounded a, Enum a, Ord a) => GroupOfUnits a
groupOfUnits =
  let ringElems = [minBound .. maxBound]
      inversePairs = filter areInverses $ getAllPairs ringElems
      units = Set.fromList $ concatMap (\(a, b) -> [a, b]) inversePairs
      inversesMap =
        Map.fromList $ inversePairs ++ map (\(a, b) -> (b, a)) inversePairs
   in GroupOfUnits {_elems = units, _inverses = inversesMap}
  where
    areInverses (a, b) = a Ring .* b == b Ring .* a && a Ring .* b == Ring.one
    getAllPairs [] = []
    getAllPairs [_] = []
    getAllPairs (x:xs) = [(x, y) | y <- xs] ++ getAllPairs xs

-- instance ( Ring.C a, Ord a ) => Monoid (UnitGroupElem a) where
--  mappend = (Ring.*)
--  mempty = Ring.one
-- Why is (UnitGroupElem a) a group, defining the monoid operation as the multiplication of the ring a?
instance (Ring.C a, Ord a) => Group (UnitGroupElem a) where
  invert (UnitGroupElem groups a) = UnitGroupElem groups inverse
    where
      inverse =
        case Map.lookup a $ _inverses groups of
          Nothing -> error "No inverse"
          Just val -> val
-- assume that GroupOfUnits is a value of groupOfUnits;
-- that works because we don’t export the constructor of GroupOfUnits,
-- so that type only gets built by groupOfUnits
