module Songs.Roses (roses) where

import Dsl
import Notes
import Relude
import Sound.PlSynth (PlSynthT (..))
import Synths qualified

leadSynth :: PlSynthT
leadSynth = PlSynthT 9 0 0 0 255 0 9 0 12 0 255 0 0 100 0 14545 70 0 0 240 2 157 3 47 0 0 0 0 0

makeRiff :: [Note] -> [Note]
makeRiff bassNotes = concatMap (\n -> bassNotes <> [n]) [f'4, f4, c'4, a'4, f4] <> [f'4]

riff1, riff2 :: [Note]
riff1 = makeRiff [d'3, a'3] <> makeRiff [d'3, a'3]
riff2 = makeRiff [b2, f'3] <> makeRiff [c'3, g'3]

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
