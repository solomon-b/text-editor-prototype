module Data.Rope where

import Prelude

import Data.Array (range)
import Data.FingerTree as FT
import Data.FingerTree.Search as FT
import Data.Foldable (class Foldable, foldr, foldl)
import Data.Identity (Identity)
import Data.Lens (Lens, set, lens, over)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Sequence.Internal (class Measured, measure)
import Data.String as STR
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..), fst)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

import Data.Rope.Chunk

newtype FingerTree v a = FingerTree (FT.FingerTree v a)

derive instance newtypeFingerTree :: Newtype (FingerTree v a) _
derive newtype instance showFingerTree :: (Show v, Show a) => Show (FingerTree v a)
derive newtype instance semigroupFingerTree :: (Monoid v, Measured a v) => Semigroup (FingerTree v a)
derive newtype instance functorFingerTree :: Functor (FingerTree v)
derive newtype instance foldableFingerTree :: Foldable (FingerTree v)
derive newtype instance traversableFingerTree :: Traversable (FingerTree v)
--derive newtype instance measuredFingerTree :: (Monoid v, Measured a v) => Measured (FingerTree v a) v

instance measuredFingerTree :: (Monoid v, Measured a v) => Measured (FingerTree v a) v where
  measure = measure <<< unwrap

instance monoidFingerTree :: (Monoid v, Measured a v) => Monoid (FingerTree v a) where
  mempty = wrap FT.Empty

instance measuredDeltaRope :: Measured (Rope anno) Delta where
  measure (Rope tree) = measure tree

newtype Rope anno = Rope (FingerTree Delta (Chunk anno))

instance semigroupRope :: Semigroup (Rope anno) where
  append (Rope a) (Rope b) = Rope $ a <> b

instance monoidRope :: Monoid (Rope anno) where
  mempty = Rope mempty

instance showRope :: Show (Rope anno) where
  show text = "\"" <> (fromRope text) <> "\""

fromRope :: forall anno. Rope anno -> String
fromRope (Rope tree) = foldr (append <<< show) mempty $ tree

intoRope :: forall anno. Monoid anno => String -> Rope anno
intoRope = Rope <<< wrap <<< FT.Single <<< flip mkChunk mempty

replicateRope :: forall anno. Int -> Rope anno -> Rope anno
replicateRope i (Rope tree) =
  Rope $ foldr (\_ acc -> tree <> acc) mempty (range 1 i)

width :: forall anno. Rope anno -> Int
width rope = let (Delta x) = measure rope in x

split :: forall anno. Int -> Rope anno -> Tuple (Rope anno) (Rope anno)
split i (Rope (FingerTree tree)) =
  case unsafePartial FT.search (\(w1) _ -> w1 >= Delta i) tree of
    FT.Position before a after ->
      let Delta w = measure before
          Tuple one two = splitAtChunk (i - w) a
      in Tuple (Rope $ wrap $ FT.snoc before one) (Rope $ wrap $ FT.cons two after)
    FT.OnLeft -> Tuple (Rope $ wrap FT.Empty) (Rope $ wrap tree)
    FT.OnRight -> Tuple (Rope $ wrap tree) (Rope $ wrap FT.Empty)
    FT.Nowhere -> unsafeCrashWith "Out of bounds index"

insert :: forall anno. Partial => Int -> Rope anno -> Rope anno -> Rope anno
insert i (Rope new) old =
  let (Tuple (Rope before) (Rope after)) = split i old
  in Rope $ before <> new <> after

indexOf :: forall anno. STR.Pattern -> Rope anno -> Maybe Int
indexOf p (Rope tree) = fst $ foldl f (Tuple Nothing 0) tree
  where
    f :: Tuple (Maybe Int) Int -> Chunk anno -> Tuple (Maybe Int) Int
    f acc (Chunk next) =
      case acc of
        Tuple (Just i) _ -> Tuple (Just i) 0
        Tuple Nothing i ->
          case STR.indexOf p next._fromChunk of
            Nothing -> Tuple Nothing (i + STR.length next._fromChunk)
            Just j -> Tuple (Just (i + j)) 0

cons :: forall anno. Chunk anno -> Rope anno -> Rope anno
cons t (Rope (FingerTree tree)) = Rope $ wrap $ FT.cons t tree

snoc :: forall anno. Rope anno -> Chunk anno -> Rope anno
snoc (Rope (FingerTree tree)) t = Rope $ wrap $ FT.snoc tree t

splitRange :: forall anno. Int -> Int -> Rope anno -> FT.Split Identity (Rope anno)
splitRange i j rope =
  let Tuple before inter = split i rope
      Tuple target after = split (j - i) inter
  in FT.Split (wrap before) target (wrap after)

applyAnnoToRange :: forall anno. anno -> Int -> Int -> Rope anno -> Rope anno
applyAnnoToRange a i j rope =
  let FT.Split before (Rope x) after = splitRange i j rope
      x' = map (setAnno a) x
  in unwrap before <> Rope x' <> unwrap after

overAnno :: forall anno anno'. (anno -> anno') -> Chunk anno -> Chunk anno'
overAnno f chunk = over anno f chunk

setAnno :: forall anno anno'. anno'-> Chunk anno -> Chunk anno'
setAnno a chunk = set anno a chunk

