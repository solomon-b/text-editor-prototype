module Data.Rope where

import Prelude

import Data.Array (range)
import Data.FingerTree as FT
import Data.FingerTree.Search as FT
import Data.Foldable (class Foldable, foldr, foldl)
import Data.Identity
import Data.Lens (set, lens)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Sequence.Internal (class Measured, measure)
import Data.String as STR
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..), fst)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

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

-- Character offset to a specific column
newtype Delta = Delta Int

derive instance newtypeDelta :: Newtype Delta _
derive newtype instance showDelta :: Show Delta
derive newtype instance eqDelta :: Eq Delta
derive newtype instance ordDelta :: Ord Delta

instance semigroupDelta :: Semigroup Delta where
  append (Delta x) (Delta y) = Delta (x + y)

instance monoidDelta :: Monoid Delta where
  mempty = Delta 0

instance measuredDeltaRope :: Measured Rope Delta where
  measure (Rope tree) = measure tree

-- .. | Italic | H1 | H2 | H3 | H4 | H5 | H6
data Annotation = Bold

data Chunk = Chunk { chunkLength :: Int, fromChunk :: String, anno :: Maybe Annotation }

chunkAnno = lens get set
  where
    get (Chunk { anno }) = anno
    set (Chunk c) anno' = Chunk $ c { anno = anno' }

instance showChunk :: Show Chunk where
  show (Chunk {fromChunk}) = fromChunk

instance measuredChunk :: Measured Chunk Delta where
  measure (Chunk {chunkLength}) = Delta chunkLength

mkChunk :: String -> Maybe Annotation -> Chunk
mkChunk txt anno = Chunk { chunkLength: (STR.length txt), fromChunk: txt, anno }

splitAtChunk :: Int -> Chunk -> Tuple Chunk Chunk
splitAtChunk i (Chunk {fromChunk, anno}) =
  let {before, after} = STR.splitAt i fromChunk
  in Tuple (mkChunk before anno) (mkChunk after anno)

newtype Rope = Rope (FingerTree Delta Chunk)

instance semigroupRope :: Semigroup Rope where
  append (Rope a) (Rope b) = Rope $ a <> b

instance monoidRope :: Monoid Rope where
  mempty = Rope mempty

instance showRope :: Show Rope where
  show text = "\"" <> (fromRope text) <> "\""

fromRope :: Rope -> String
fromRope (Rope tree) = foldr (append <<< show) mempty $ tree

intoRope :: String -> Rope
intoRope = Rope <<< wrap <<< FT.Single <<< flip mkChunk Nothing

replicateRope :: Int -> Rope -> Rope
replicateRope i (Rope tree) =
  Rope $ foldr (\_ acc -> tree <> acc) mempty (range 1 i)

width :: Rope -> Int
width rope = let (Delta x) = measure rope in x

split :: Int -> Rope -> Tuple Rope Rope
split i (Rope (FingerTree tree)) =
  case unsafePartial FT.search (\(w1) _ -> w1 >= Delta i) tree of
    FT.Position before a after ->
      let Delta w = measure before
          Tuple one two = splitAtChunk (i - w) a
      in Tuple (Rope $ wrap $ FT.snoc before one) (Rope $ wrap $ FT.cons two after)
    FT.OnLeft -> Tuple (Rope $ wrap FT.Empty) (Rope $ wrap tree)
    FT.OnRight -> Tuple (Rope $ wrap tree) (Rope $ wrap FT.Empty)
    FT.Nowhere -> unsafeCrashWith "Out of bounds index"

insert :: Partial => Int -> Rope -> Rope -> Rope
insert i (Rope new) old =
  let (Tuple (Rope before) (Rope after)) = split i old
  in Rope $ before <> new <> after

indexOf :: STR.Pattern -> Rope -> Maybe Int
indexOf p (Rope tree) = fst $ foldl f (Tuple Nothing 0) tree
  where
    f :: Tuple (Maybe Int) Int -> Chunk -> Tuple (Maybe Int) Int
    f acc (Chunk next) =
      case acc of
        Tuple (Just i) _ -> Tuple (Just i) 0
        Tuple Nothing i ->
          case STR.indexOf p next.fromChunk of
            Nothing -> Tuple Nothing (i + STR.length next.fromChunk)
            Just j -> Tuple (Just (i + j)) 0

cons :: Chunk -> Rope -> Rope
cons t (Rope (FingerTree tree)) = Rope $ wrap $ FT.cons t tree

snoc :: Rope -> Chunk -> Rope
snoc (Rope (FingerTree tree)) t = Rope $ wrap $ FT.snoc tree t

splitRange :: Int -> Int -> Rope -> FT.Split Identity Rope
splitRange i j rope =
  let Tuple before inter = split i rope
      Tuple target after = split (j - i) inter
  in FT.Split (wrap before) target (wrap after)

applyAnnoToRange :: Maybe Annotation -> Int -> Int -> Rope -> Rope
applyAnnoToRange anno i j rope =
  let FT.Split before (Rope x) after = splitRange i j rope
      x' = map (set chunkAnno anno) x
  in unwrap before <> Rope x' <> unwrap after
