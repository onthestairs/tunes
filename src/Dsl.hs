module Dsl
  ( Song,
    SynthName (..),
    synth,
    patt,
    build,
    Note,
  )
where

import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Notes
import Relude hiding (seq)
import Sound.PlSynth (PlSynthT (..))
import Synths.Types (Synth, toPlSynth)

newtype SynthIndex = SynthIndex {_unSynthIndex :: Word8}
  deriving newtype (Ord, Eq, Show)

newtype SynthName = SynthName {toSynthName :: Text}
  deriving newtype (Ord, Eq, Show, IsString)

newtype PatternIndex = PatternIndex {unPatternIndex :: Word8}
  deriving newtype (Ord, Eq, Show)

data SongState = SongState
  { synths :: Vector.Vector (SynthName, Synth),
    patterns :: Map.Map SynthIndex (Vector.Vector [Note])
  }
  deriving (Eq, Show)

synth :: SynthName -> Synth -> State SongState SynthIndex
synth name x = do
  n <- gets $ SynthIndex . toEnum . Vector.length . synths
  modify' $ \s -> s {synths = Vector.snoc s.synths (name, x)}
  pure n

patt :: SynthIndex -> [Note] -> State SongState (SynthIndex, PatternIndex)
patt synthIndex x = do
  modify' $ \s -> do
    let v = Vector.singleton x
    let patterns' = Map.insertWith (flip (Vector.++)) synthIndex v s.patterns
    s {patterns = patterns'}
  n <- gets $ PatternIndex . toEnum . Vector.length . Map.findWithDefault Vector.empty synthIndex . (.patterns)
  pure (synthIndex, n)

type Song = [(SynthName, [Word8], [[Word8]], PlSynthT)]

build :: State SongState [[(SynthIndex, PatternIndex)]] -> Song
build song = do
  let (grid, result) = runState song (SongState Vector.empty Map.empty)
  toList $ flip Vector.imap result.synths $ \synthIndex (synthName, s) -> do
    let si = SynthIndex $ toEnum synthIndex
    let patterns = map (map (.unNote)) $ toList $ Map.findWithDefault Vector.empty si result.patterns
    let findPattern row = maybe 0 ((.unPatternIndex) . snd) $ find (\(si', _pi') -> si == si') row
    let seq = map findPattern grid
    (synthName, seq, patterns, toPlSynth s)
