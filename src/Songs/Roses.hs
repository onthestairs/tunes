module Songs.Roses (roses) where

import Dsl
import Relude
import Sound.PlSynth (PlSynthT (..))
import Synths qualified as Synths

leadSynth :: PlSynthT
leadSynth = PlSynthT 9 0 0 0 255 0 9 0 12 0 255 0 0 100 0 14545 70 0 0 240 2 157 3 47 0 0 0 0 0

makeRiff :: (Num b) => [b] -> [b]
makeRiff bassNotes = concatMap (\n -> bassNotes <> [n]) [66, 65, 61, 70, 65] <> [66]

riff1, riff2 :: [Word8]
riff1 = map (+ 75) (makeRiff [51, 58] <> makeRiff [51, 58])
riff2 = map (+ 75) (makeRiff [47, 54] <> makeRiff [49, 56])

replicateL :: Int -> [a] -> [a]
replicateL n = concat . replicate n

fourOnTheFloor :: [Word8]
fourOnTheFloor = replicateL 16 [147, 0]

syncopatedFour :: [Word8]
syncopatedFour = replicateL 16 [0, 147]

roses :: Song
roses = build $ do
  lead <- synth leadSynth
  bass <- synth Synths.bassDrum
  hihat <- synth Synths.hihat
  leadPattern1 <- patt lead riff1
  leadPattern2 <- patt lead riff2
  four <- patt bass fourOnTheFloor
  hihat0 <- patt hihat syncopatedFour
  pure
    [ [four, hihat0, leadPattern1],
      [four, hihat0, leadPattern2],
      [four, hihat0, leadPattern1],
      [four, hihat0, leadPattern2],
      [four, hihat0, leadPattern1],
      [four, hihat0, leadPattern2]
    ]
