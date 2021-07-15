module Data.FingerTree.Search where

import Prelude

import Data.FingerTree as FT
import Data.Lazy (force, defer)
import Data.Sequence.Internal (class Measured, measure)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

data SearchResult v a =
    Position (FT.FingerTree v a) a (FT.FingerTree v a)
  | OnLeft
  | OnRight
  | Nowhere

instance showSearchResult :: (Show v, Show a) => Show (SearchResult v a) where
  show (Position left a right) = "Position " <> show left <> " " <> show a <> " " <> show right
  show OnLeft = "OnLeft"
  show OnRight = "OnRight"
  show Nowhere = "Nowhere"

derive instance eqSearchResult :: (Eq a, Eq (FT.FingerTree v a)) => Eq (SearchResult v a)
derive instance ordSearchResult :: (Ord a, Ord (FT.FingerTree v a)) => Ord (SearchResult v a)

search :: forall a v. Monoid v => Measured a v => (v -> v -> Boolean) -> FT.FingerTree v a -> SearchResult v a
search p t =
  case unit of
  _ | pLeft && pRight -> OnLeft
  _ | not pLeft && pRight -> let FT.Split l x r = unsafePartial searchTree p mempty t mempty in Position l x r
  _ | not pLeft && not pRight -> OnRight
  _ -> Nowhere
  where
    pLeft = p mempty (measure t)
    pRight = p (measure t) mempty

searchTree :: forall a v. Partial => Monoid v => Measured a v =>
    (v -> v -> Boolean) -> v -> FT.FingerTree v a -> v -> FT.Split (FT.FingerTree v) a
searchTree _ _l FT.Empty _ = unsafeCrashWith "Cannot search an empty tree"
searchTree _ _ (FT.Single a) _ = FT.Split FT.Empty a FT.Empty
searchTree p vl (FT.Deep _ prefix deeper suffix) vr =
  case unit of
  _ | p vlPrefix deepVrSuf -> let FT.Split l x r = searchDigit p vl prefix deepVrSuf
                              in FT.Split (FT.toFingerTree l) x (FT.deepL r deeper suffix)
  _ | p vlPreDeep vrSuffix -> let FT.Split ml x mr = searchTree p vlPrefix (force deeper) vrSuffix
                                  FT.Split l x' r = searchNode p (vlPrefix <> measure ml) x (measure mr <> vrSuffix)
                              in FT.Split (FT.deepR prefix (defer (const ml)) l) x' (FT.deepL r (defer (const mr)) suffix)
  _ -> let FT.Split l x r = searchDigit p vlPreDeep suffix vr
       in FT.Split (FT.deepR prefix deeper l) x (FT.toFingerTree r)
  where
    vlPrefix = vl <> measure prefix
    vlPreDeep = vlPrefix <> measure deeper
    vrSuffix = measure suffix <> vr
    deepVrSuf = measure deeper <> vrSuffix

searchNode :: forall a v. Semigroup v => Measured a v => (v -> v -> Boolean) -> v -> FT.Node v a -> v -> FT.Split Array a
searchNode p vl (FT.Node3 _ a b c) vr
  | p (vl <> measure a) (measure b <> measure c <> vr) = FT.Split mempty a [b, c]
  | p (vl <> measure a <> measure b) (measure c <> vr) = FT.Split [a] b [c]
  | otherwise = FT.Split [a, b] c mempty
searchNode p vl (FT.Node2 _ a b) vr
  | p (vl <> measure a) (measure b <> vr) = FT.Split mempty a [b]
  | otherwise = FT.Split [a] b mempty

searchDigit :: forall a v. Partial => Semigroup v => Measured a v => (v -> v -> Boolean) -> v -> FT.Digit a -> v -> FT.Split Array a
searchDigit p vl digit vr =
  case FT.runDigit digit of
    [a] -> FT.Split mempty a mempty
    [a, b] | p (vl <> measure a) (measure b <> vr) -> FT.Split mempty a [b]
    [a, b] -> FT.Split [a] b mempty
    [a, b, c] | p (vl <> measure a) (measure b <> measure c <> vr) -> FT.Split mempty a [b, c]
    [a, b, c] | p (vl <> measure a <> measure b) (measure c <> vr) -> FT.Split [a] b [c]
    [a, b, c] -> FT.Split [a, b] c mempty
    [a, b, c, d] | p (vl <> measure a) (measure b <> measure c <> measure d <> vr) -> FT.Split mempty a [b, c, d]
    [a, b, c, d] | p (vl <> measure a <> measure b) (measure c <> measure d <> vr) -> FT.Split [a] b [c, d]
    [a, b, c, d] | p (vl <> measure a <> measure b <> measure c) (measure d <> vr) -> FT.Split [a, b] c [d]
    [a, b, c, d] -> FT.Split [a, b, c] d mempty
