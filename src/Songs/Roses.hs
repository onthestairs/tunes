module Songs.Roses (roses) where

import Dsl
import Notes
import Relude
import Sound.PlSynth (PlSynthT (..))
import Synths qualified
import Theory

leadSynth :: PlSynthT
leadSynth = PlSynthT 9 0 0 0 255 0 9 0 12 0 255 0 0 100 0 14545 70 0 0 240 2 157 3 47 0 0 0 0 0

bassSynth :: PlSynthT
bassSynth = PlSynthT 5 0 0 0 240 0 5 0 0 0 157 2 0 100 20000 69090 218 2 3900 240 6 70 0 0 0 1 7 42 0

makeRiff :: Note -> [Note]
makeRiff root = concatMap (\n -> [root, fifth root, n]) [f'4, f4, c'4, a'4, f4] <> [f'4]

riff1, riff2 :: [Note]
riff1 = makeRiff d'3 <> makeRiff d'3
riff2 = makeRiff b2 <> makeRiff c'3

replicateL :: Int -> [a] -> [a]
replicateL n = concat . replicate n

bassRiff1, bassRiff2 :: [Note]
bassRiff1 = ([d'4] <> replicate 15 0) <> ([d'4] <> replicate 15 0)
bassRiff2 = ([b3] <> replicate 15 0) <> (replicate 5 c4 <> replicate 11 0)

roses :: Song
roses = build $ do
  bassDrum <- synth Synths.bassDrum
  fourOnTheFloor <- patt bassDrum (replicateL 16 [c5, 0])

  hihat <- synth Synths.hihat
  hihatSync <- patt hihat (replicateL 16 [0, c5])

  lead <- synth leadSynth
  lead0 <- patt lead riff1
  lead1 <- patt lead riff2

  bass <- synth bassSynth
  bass0 <- patt bass bassRiff1
  bass1 <- patt bass bassRiff2

  pure
    [ -- [fourOnTheFloor],
      --   [fourOnTheFloor, hihatSync],
      [fourOnTheFloor, hihatSync, bass0, lead0],
      [fourOnTheFloor, hihatSync, bass1, lead1],
      [fourOnTheFloor, hihatSync, bass0, lead0],
      [fourOnTheFloor, hihatSync, bass1, lead1]
    ]
