module Songs.Roses (roses) where

import Dsl
import Relude
import Sound.PlSynth (PlSynthT (..))
import Synths qualified

leadSynth :: PlSynthT
leadSynth = PlSynthT 9 0 0 0 255 0 9 0 12 0 255 0 0 100 0 14545 70 0 0 240 2 157 3 47 0 0 0 0 0

makeRiff :: [Note] -> [Note]
makeRiff bassNotes = concatMap (\n -> bassNotes <> [n]) [141, 140, 136, 145, 140] <> [141]

riff1, riff2 :: [Note]
riff1 = makeRiff [126, 133] <> makeRiff [126, 133]
riff2 = makeRiff [122, 129] <> makeRiff [124, 131]

replicateL :: Int -> [a] -> [a]
replicateL n = concat . replicate n

roses :: Song
roses = build $ do
  bass <- synth Synths.bassDrum
  fourOnTheFloor <- patt bass (replicateL 16 [147, 0])

  hihat <- synth Synths.hihat
  hihatSync <- patt hihat (replicateL 16 [0, 147])

  lead <- synth leadSynth
  lead0 <- patt lead riff1
  lead1 <- patt lead riff2

  pure
    [ [fourOnTheFloor],
      [fourOnTheFloor, hihatSync],
      [fourOnTheFloor, hihatSync, lead0],
      [fourOnTheFloor, hihatSync, lead1],
      [fourOnTheFloor, hihatSync, lead0],
      [fourOnTheFloor, hihatSync, lead1]
    ]
