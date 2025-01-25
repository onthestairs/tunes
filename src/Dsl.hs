module Dsl (Song, synth, patt, build, Note) where

import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Relude hiding (seq)
import Sound.PlSynth (PlSynthT (..))

type Song = [([Word8], [[Word8]], PlSynthT)]

newtype Note = Note {unNote :: Word8}
  deriving newtype (Ord, Eq, Show, Num)

newtype SynthIndex = SynthIndex {_unSynthIndex :: Word8}
  deriving newtype (Ord, Eq, Show)

newtype PatternIndex = PatternIndex {unPatternIndex :: Word8}
  deriving newtype (Ord, Eq, Show)

data SongState = SongState
  { synths :: Vector.Vector PlSynthT,
    patterns :: Map.Map SynthIndex (Vector.Vector [Note])
  }
  deriving (Eq, Show)

synth :: PlSynthT -> State SongState SynthIndex
synth x = do
  n <- gets $ SynthIndex . toEnum . Vector.length . synths
  modify' $ \s -> s {synths = Vector.snoc s.synths x}
  pure n

patt :: SynthIndex -> [Note] -> State SongState (SynthIndex, PatternIndex)
patt synthIndex x = do
  modify' $ \s -> do
    let v = Vector.singleton x
    let patterns' = Map.insertWith (flip (Vector.++)) synthIndex v s.patterns
    s {patterns = patterns'}
  n <- gets $ PatternIndex . toEnum . Vector.length . Map.findWithDefault Vector.empty synthIndex . (.patterns)
  pure (synthIndex, n)

build :: State SongState [[(SynthIndex, PatternIndex)]] -> [([Word8], [[Word8]], PlSynthT)]
build song = do
  let (grid, result) = runState song (SongState Vector.empty Map.empty)
  toList $ flip Vector.imap result.synths $ \synthIndex s -> do
    let si = SynthIndex $ toEnum synthIndex
    let patterns = map (map (.unNote)) $ toList $ Map.findWithDefault Vector.empty si result.patterns
    let findPattern row = maybe 0 ((.unPatternIndex) . snd) $ find (\(si', _pi') -> si == si') row
    let seq = map findPattern grid
    (seq, patterns, s)
