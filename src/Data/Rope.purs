module Data.Rope where

import Data.String (length)
import Data.Newtype
import Data.Sequence.Internal
import Prelude

import Data.FingerTree as FT

-- Character offset to a specific location
newtype Delta = Columns Int

derive instance newtypeDelta :: Newtype Delta _
derive newtype instance showDelta :: Show Delta
derive newtype instance eqDelta :: Eq Delta
derive newtype instance ordDelta :: Ord Delta

instance semigroupDelta :: Semigroup Delta where
  append (Columns x) (Columns y) = Columns (x + y)

instance monoidDelta :: Monoid Delta where
  mempty = Columns 0

instance hasDeltaDelta :: HasDelta Delta where
  delta = identity

instance measuredDeltaRope :: Measured Rope Delta where
  measure (Rope s _) = s

instance measuredDeltaStrand :: Measured Strand Delta where
  measure (Strand _ d) = delta d
  measure (Skipping s) = s

class HasDelta t where
  delta :: t -> Delta

data Strand =
    Strand String Delta   -- Data of a certain length
  | Skipping Delta -- Absence of data of a certain length

instance showStrand :: Show Strand where
  show (Strand s _) = "Strand " <> s
  show (Skipping d) = "Skipping " <> show d

instance hasDeltaStrand :: HasDelta Strand where
  delta (Strand _ d) = delta d
  delta (Skipping d) = delta d

strand :: String -> Strand
strand s = Strand s (Columns $ length s)

strands :: Rope -> FT.FingerTree Delta Strand
strands (Rope _ r) = r

data Rope = Rope Delta (FT.FingerTree Delta Strand)

instance showRope :: Show Rope where
  show (Rope delta tree) = "Rope { " <> show delta <> " " <> show tree <> " }"

instance semigroupRope :: Semigroup Rope where
  append (Rope d1 t1) (Rope d2 t2) = Rope (d1 <> d2) (t1 <> t2)

instance monoidRope :: Monoid Rope where
  mempty = Rope mempty FT.Empty

rope :: FT.FingerTree Delta Strand -> Rope
rope r = Rope (measure r) r

-- | Construct a 'Rope' out of a single 'String' strand.
ropeStr :: String -> Rope
ropeStr = rope <<< FT.Single <<< strand
