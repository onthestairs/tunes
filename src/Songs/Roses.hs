module Songs.Roses (roses) where

import Dsl
import Relude
import Sound.PlSynth (PlSynthT (..))
import Synths qualified

leadSynth :: PlSynthT
leadSynth = PlSynthT 9 0 0 0 255 0 9 0 12 0 255 0 0 100 0 14545 70 0 0 240 2 157 3 47 0 0 0 0 0

makeRiff :: [Note] -> [Note]
makeRiff bassNotes = concatMap (\n -> bassNotes <> [n]) [66, 65, 61, 70, 65] <> [66]

riff1, riff2 :: [Note]
riff1 = map (+ 75) (makeRiff [51, 58] <> makeRiff [51, 58])
riff2 = map (+ 75) (makeRiff [47, 54] <> makeRiff [49, 56])

replicateL :: Int -> [a] -> [a]
replicateL n = concat . replicate n

fourOnTheFloor :: [Note]
fourOnTheFloor = replicateL 16 [147, 0]

syncopatedFour :: [Note]
syncopatedFour = replicateL 16 [0, 147]

roses :: Song
roses = build $ do
  bass <- synth Synths.bassDrum
  bass0 <- patt bass fourOnTheFloor

  hihat <- synth Synths.hihat
  hihat0 <- patt hihat syncopatedFour

  lead <- synth leadSynth
  lead0 <- patt lead riff1
  lead1 <- patt lead riff2

  pure
    [ [bass0],
      [bass0, hihat0],
      [bass0, hihat0, lead0],
      [bass0, hihat0, lead1],
      [bass0, hihat0, lead0],
      [bass0, hihat0, lead1],
      [bass0, hihat0, lead0],
      [bass0, hihat0, lead1]
    ]
