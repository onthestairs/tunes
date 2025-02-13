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

weirdSynth :: PlSynthT
weirdSynth = PlSynthT 7 0 0 0 206 2 7 0 0 0 201 2 14 1111 150 3125 175 4 8589 114 6 53 4 0 0 1 6 195 3

spaceySynth :: PlSynthT
spaceySynth = PlSynthT 9 0 0 0 255 0 9 0 12 0 255 0 0 100 0 14545 70 0 0 240 2 157 0 0 0 0 0 0 0

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

  lead <- synth leadSynth
  lead0 <- patt lead riff1
  lead1 <- patt lead riff2

  bass <- synth bassSynth
  bass0 <- patt bass bassRiff1
  bass1 <- patt bass bassRiff2

  weird <- synth weirdSynth
  weird0 <- patt weird (take 16 (arpeggio d'3 Minor) <> take 16 (arpeggio d'3 Minor))
  weird1 <- patt weird (take 16 (arpeggio b2 Major) <> take 16 (arpeggio c'3 Major))

  spacey <- synth spaceySynth
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
