module Songs.Roses (roses) where

import Dsl
import Notes
import Relude
import Sound.PlSynth (PlSynthT (..))
import Synths qualified

leadSynth :: PlSynthT
leadSynth = PlSynthT 9 0 0 0 255 0 9 0 12 0 255 0 0 100 0 14545 70 0 0 240 2 157 3 47 0 0 0 0 0

makeRiff :: [Note] -> [Note]
makeRiff bassNotes = concatMap (\n -> bassNotes <> [n]) [fs4, f4, cs4, as4, f4] <> [fs4]

riff1, riff2 :: [Note]
riff1 = makeRiff [ds3, as3] <> makeRiff [ds3, as3]
riff2 = makeRiff [b2, fs3] <> makeRiff [cs3, gs3]

replicateL :: Int -> [a] -> [a]
replicateL n = concat . replicate n

roses :: Song
roses = build $ do
  bass <- synth Synths.bassDrum
  fourOnTheFloor <- patt bass (replicateL 16 [c5, 0])

  hihat <- synth Synths.hihat
  hihatSync <- patt hihat (replicateL 16 [0, c5])

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
