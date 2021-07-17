module Data.Rope.Chunk where

import Prelude

import Data.Lens (Lens, Lens', lens, view)
import Data.Newtype (class Newtype, wrap)
import Data.Sequence.Internal (class Measured)
import Data.String as STR
import Data.Tuple (Tuple(..))

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

data Chunk anno = Chunk { _chunkLength :: Int, _fromChunk :: String, _anno :: anno }

chunkLength :: forall anno. Lens' (Chunk anno) Int
chunkLength =
  let get (Chunk c) = c._chunkLength
      set (Chunk c) i = Chunk $ c { _chunkLength = i }
  in lens get set

fromChunk :: forall anno. Lens' (Chunk anno) String
fromChunk =
  let get (Chunk c) = c._fromChunk
      set (Chunk c) str = Chunk $ c { _fromChunk = str }
  in lens get set

anno :: forall anno anno'. Lens (Chunk anno) (Chunk anno') anno anno'
anno = lens get set
  where
    get (Chunk { _anno }) = _anno
    set (Chunk c) anno' = Chunk $ c { _anno = anno' }

instance showChunk :: Show (Chunk anno) where
  show c = view fromChunk c

instance measuredChunk :: Measured (Chunk anno) Delta where
  measure = wrap <<< view chunkLength

mkChunk :: forall anno. String -> anno -> Chunk anno
mkChunk txt a = Chunk { _chunkLength: (STR.length txt), _fromChunk: txt, _anno: a }

splitAtChunk :: forall anno. Int -> Chunk anno -> Tuple (Chunk anno) (Chunk anno)
splitAtChunk i (Chunk {_fromChunk, _anno}) =
  let {before, after} = STR.splitAt i _fromChunk
  in Tuple (mkChunk before _anno) (mkChunk after _anno)
