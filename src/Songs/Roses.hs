module Songs.Roses (roses) where

import Dsl
import Notes
import Relude
import Synths qualified
import Theory

makeRiff :: Note -> [Note]
makeRiff root = concatMap (\n -> [root, fifth root, n]) [f'4, f4, c'4, a'4, f4] <> [f'4]

riff1, riff2 :: [Note]
riff1 = makeRiff d'3 <> makeRiff d'3
riff2 = makeRiff b2 <> makeRiff c'3

bassRiff1, bassRiff2 :: [Note]
bassRiff1 = ([d'4] <> replicate 15 0) <> ([d'4] <> replicate 15 0)
bassRiff2 = ([b3] <> replicate 15 0) <> (replicate 5 c4 <> replicate 11 0)

everyOther :: Note -> [Note]
everyOther n = cycle [n, 0]

everyOff :: Note -> [Note]
everyOff n = cycle [0, n]

mask :: Note -> [Char] -> [Note]
mask n = map (\c -> if c == 'x' then n else 0)

roses :: Song
roses = build $ do
  bassDrum <- synth Synths.bassDrum
  fourOnTheFloor <- patt bassDrum (take 32 (everyOther c5))
  fourOnTheFloorFill <- patt bassDrum $ take 32 $ cycle $ take 12 (cycle $ mask c5 "x.") <> mask c5 "xxxx"

  hihat <- synth Synths.hihat
  hihatSync <- patt hihat (take 32 (everyOff c5))

  lead <- synth Synths.leadSynth
  lead0 <- patt lead riff1
  lead1 <- patt lead riff2

  weird <- synth Synths.weirdSynth
  weird0 <- patt weird (take 16 (arpeggio d'3 Minor) <> take 16 (arpeggio d'3 Minor))
  weird1 <- patt weird (take 16 (arpeggio b2 Major) <> take 16 (arpeggio c'3 Major))

  spacey <- synth Synths.spaceySynth
  spacey0 <- patt spacey (take 16 (drop 1 $ arpeggio d'5 Minor) <> take 16 (drop 1 $ arpeggio d'5 Minor))
  spacey1 <- patt spacey (take 16 (drop 1 $ arpeggio b4 Major) <> take 16 (drop 1 $ arpeggio c'4 Major))

  pure
    [ [fourOnTheFloor, weird0, spacey0],
      [fourOnTheFloorFill, weird1, spacey1],
      [fourOnTheFloor, weird0, spacey0],
      [fourOnTheFloorFill, weird1, spacey1],
      [fourOnTheFloor, weird0, spacey0],
      [fourOnTheFloorFill, weird1, spacey1],
      [fourOnTheFloor, hihatSync, weird0, spacey0],
      [fourOnTheFloorFill, hihatSync, weird0, spacey1],
      [fourOnTheFloor, hihatSync, weird0, spacey0],
      [fourOnTheFloorFill, hihatSync, weird1, spacey1],
      [fourOnTheFloor, hihatSync, weird0, spacey0],
      [fourOnTheFloorFill, hihatSync, weird1, spacey1],
      [fourOnTheFloor, hihatSync, weird0, spacey0, lead0],
      [fourOnTheFloorFill, hihatSync, weird1, spacey1, lead1],
      [fourOnTheFloor, hihatSync, weird0, spacey0, lead0],
      [fourOnTheFloorFill, hihatSync, weird1, spacey1, lead1]
    ]
