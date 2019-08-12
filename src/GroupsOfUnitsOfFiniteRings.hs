-- Depends on packages numeric-prelude and groups
-- https://hackage.haskell.org/package/numeric-prelude-0.4.3.1
-- https://hackage.haskell.org/package/groups-0.4.1.0

module GroupsOfUnitsOfFiniteRings ( GroupOfUnits, UnitGroupElem, groupOfUnits ) where

import Data.Group
import qualified Algebra.Ring as Ring
import qualified Data.Set as Set
import qualified Data.Map as Map

-- a unit is never separate from the group, to avoid recomputing groupOfUnits
data UnitGroupElem a = UnitGroupElem (GroupOfUnits a) a

data GroupOfUnits a = GroupOfUnits { _elems :: Set.Set a, _inverses :: Map.Map a a }

getAllPairs :: [a] -> [(a,a)]
getAllPairs [] = []
getAllPairs [_] = []
getAllPairs (x:xs) = [(x, y) | y <- xs] ++ getAllPairs xs

-- The group of units of an arbitrary finite ring. A unit of a ring is an element of the ring, a,
-- which has an inverse, b, such that a * b = b * a = Ring.one.
groupOfUnits :: ( Ring.C a, Bounded a, Enum a ) => ([a], [(a,a)])
groupOfUnits = (range, pairs)
  where range = [minBound .. maxBound]
        pairs = getAllPairs range

-- Why is (UnitGroupElem a) a group, defining the monoid operation as the multiplication of the ring a?
-- instance ( Ring.C a ) => Group (UnitGroupElem a) where
--   TODO
-- assume that GroupOfUnits is a value of groupOfUnits;
-- that works because we don’t export the constructor of GroupOfUnits,
-- so that type only gets built by groupOfUnits
