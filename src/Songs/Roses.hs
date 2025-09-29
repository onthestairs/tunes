module Songs.Roses (roses, sp) where

import Dsl
import Notes
import Relude
import Synths qualified
import Theory

makeRiff :: Note -> [Note]
makeRiff root = concatMap (\n -> [root, 0, fifth root, 0, n, 0]) [f'4, f4, c'4, a'4, f4] <> [f'4]

riff0, riff1, riff2 :: [Note]
riff0 = makeRiff d'3
riff1 = makeRiff b2
riff2 = makeRiff c'3

everyOther :: Note -> [Note]
everyOther n = cycle [n, 0, 0, 0]

mask :: Note -> [Char] -> [Note]
mask n = map (\c -> if c == 'x' then n else 0)

roses :: Song
roses = build $ do
  bassDrum <- synth Synths.bassDrum
  fourOnTheFloor <- patt bassDrum (take 32 (everyOther c5))
  fourOnTheFloorFill <- patt bassDrum $ take 32 $ cycle $ take 12 (cycle $ mask c5 "x.") <> mask c5 "xxxx"

  hihat <- synth Synths.hihat
  hihatSync <- patt hihat (take 32 $ cycle $ mask c5 "..x...x...x...x.")
  hihatPattern <- patt hihat (take 32 $ cycle $ mask c5 ".xx.x.xx.xxx.xx.")

  lead <- synth Synths.leadSynth
  lead0 <- patt lead riff0
  lead1 <- patt lead riff1
  lead2 <- patt lead riff2

  weird <- synth Synths.weirdSynth
  weird0 <- patt weird (take 32 (arpeggio0 d'3 Minor))
  weird1 <- patt weird (take 32 (arpeggio0 b2 Major))
  weird2 <- patt weird (take 32 (arpeggio0 c'3 Major))

  spacey <- synth Synths.spaceySynth
  spacey0 <- patt spacey (take 32 (drop 2 $ arpeggio0 d'5 Minor))
  spacey1 <- patt spacey (take 32 (drop 2 $ arpeggio0 b4 Major))
  spacey2 <- patt spacey (take 32 (drop 2 $ arpeggio0 c'4 Major))

  epm <- synth Synths.epmSynth
  epm0 <- patt epm (take 32 (arpeggio0Fast d'5 Minor))
  epm1 <- patt epm (take 32 (arpeggio0Fast b4 Major))
  epm2 <- patt epm (take 32 (arpeggio0Fast c'4 Major))

  paddy <- synth Synths.paddySynth
  paddy0 <- patt paddy (take 32 (arpeggio0 d'4 Minor))
  paddy1 <- patt paddy (take 32 (arpeggio0 b3 Major))
  paddy2 <- patt paddy (take 32 (arpeggio0 c'3 Major))

  let preIntro = [[weird0], [weird0], [weird1], [weird2]]
  let intro =
        [ [fourOnTheFloor, weird0, spacey0, paddy0],
          [fourOnTheFloor, weird0, spacey0, paddy0],
          [fourOnTheFloor, weird1, spacey1, paddy1],
          [fourOnTheFloor, weird2, spacey2, paddy2]
        ]
  let introHihat =
        [ [fourOnTheFloor, hihatSync, weird0, spacey0, paddy0],
          [fourOnTheFloor, hihatSync, weird0, spacey0, paddy0],
          [fourOnTheFloor, hihatSync, weird1, spacey1, paddy1],
          [fourOnTheFloor, hihatSync, weird2, spacey2, paddy2]
        ]
  let groove =
        [ [fourOnTheFloor, hihatPattern, weird0, spacey0, paddy0],
          [fourOnTheFloor, hihatPattern, weird0, spacey0, paddy0],
          [fourOnTheFloor, hihatPattern, weird1, spacey1, paddy1],
          [fourOnTheFloor, hihatPattern, weird2, spacey2, paddy2]
        ]
  let bridge =
        [ [fourOnTheFloor, weird0, epm0, paddy0],
          [fourOnTheFloor, weird0, epm0, paddy0],
          [fourOnTheFloor, weird1, epm1, paddy1],
          [fourOnTheFloor, weird2, epm2, paddy2]
        ]
  let quietBit = [[spacey0, spacey0, spacey1, spacey2]]
  let riff =
        [ [fourOnTheFloor, hihatPattern, weird0, spacey0, lead0, paddy0],
          [fourOnTheFloor, hihatPattern, weird0, spacey0, lead0, paddy0],
          [fourOnTheFloor, hihatPattern, weird1, spacey1, lead1, paddy1],
          [fourOnTheFloor, hihatPattern, weird2, spacey2, lead2, paddy2],
          [fourOnTheFloor, hihatPattern, weird0, spacey0, lead0, paddy0],
          [fourOnTheFloor, hihatPattern, weird0, spacey0, lead0, paddy0],
          [fourOnTheFloor, hihatPattern, weird1, spacey1, lead1, paddy1],
          [fourOnTheFloor, hihatPattern, weird2, spacey2, lead2, paddy2]
        ]

  pure
    $ concat
      [ -- parts
        preIntro,
        intro,
        introHihat,
        groove,
        riff,
        riff,
        riff,
        riff,
        quietBit,
        bridge,
        bridge,
        bridge,
        bridge
      ]

sp :: Song
sp = build $ do
  epm <- synth Synths.epmSynth
  spacey0 <- patt epm (take 32 (arpeggio0Fast d'5 Minor))
  spacey1 <- patt epm (take 32 (arpeggio0Fast b4 Major))
  spacey2 <- patt epm (take 32 (arpeggio0Fast c'4 Major))
  pure
    $ [ [spacey0],
        [spacey0],
        [spacey1],
        [spacey2],
        [spacey0],
        [spacey0],
        [spacey1],
        [spacey2]
      ]
