module Dsl (Song, synth, patt, build) where

import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Relude hiding (seq)
import Sound.PlSynth (PlSynthT (..))

type Song = [([Word8], [[Word8]], PlSynthT)]

newtype SynthIndex = SynthIndex {unSynthIndex :: Word8}
  deriving newtype (Ord, Eq, Show)

newtype PatternIndex = PatternIndex {unPatternIndex :: Word8}
  deriving newtype (Ord, Eq, Show)

data S = S
  { synths :: Vector.Vector PlSynthT,
    patterns :: Map.Map SynthIndex (Vector.Vector [Word8])
  }
  deriving (Eq, Show)

synth :: PlSynthT -> State S SynthIndex
synth x = do
  n <- gets $ SynthIndex . toEnum . Vector.length . synths
  modify' $ \s -> s {synths = Vector.snoc s.synths x}
  pure n

patt :: SynthIndex -> [Word8] -> State S (SynthIndex, PatternIndex)
patt synthIndex x = do
  modify' $ \s -> do
    let v = Vector.singleton x
    let patterns' = Map.insertWith (flip (Vector.++)) synthIndex v s.patterns
    s {patterns = patterns'}
  n <- gets $ PatternIndex . toEnum . Vector.length . Map.findWithDefault Vector.empty synthIndex . (.patterns)
  pure (synthIndex, n)

build :: State S [[(SynthIndex, PatternIndex)]] -> [([Word8], [[Word8]], PlSynthT)]
build song = do
  let (grid, result) = runState song (S Vector.empty Map.empty)
  toList $ flip Vector.imap result.synths $ \synthIndex s -> do
    let si = SynthIndex $ toEnum synthIndex
    let patterns = toList $ Map.findWithDefault Vector.empty si result.patterns
    let findPattern row = maybe 0 ((.unPatternIndex) . snd) $ find (\(si', _pi') -> si == si') row
    let seq = map findPattern grid
    (seq, patterns, s)
