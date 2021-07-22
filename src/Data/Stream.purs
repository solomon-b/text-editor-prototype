module Data.Stream where

import Prelude

import Control.Extend (class Extend, extend)
import Control.Comonad (class Comonad, extract)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Lazy (Lazy, defer, force)

data Stream a = Stream a (Lazy (Stream a))

instance functorStream :: Functor Stream where
  map f (Stream a as) = Stream (f a) ((map <<< map) f as)

instance foldableStream :: Foldable Stream where
  foldMap f (Stream a as) = f a <> foldMap f (force as)
  foldl f e (Stream a as) = foldl f (f e a) (force as)
  foldr f e (Stream a as) = f a $ foldr f e (force as)

instance extendStream :: Extend Stream where
  extend f s@(Stream _ as) = Stream (f s) (map (extend f) as)

instance comonadStream :: Comonad Stream where
  extract (Stream a _) = a

headS :: forall a. Stream a -> a
headS  = extract

tailS :: forall a. Stream a -> Stream a
tailS (Stream _ as) = force as

mkStream :: forall a. (a -> a) -> a -> Stream a
mkStream f a = Stream a (defer $ \_ -> mkStream f (f a))
