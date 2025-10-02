module Songs.Solaris (solaris) where

import Dsl
import Notes
import Relude
import Synths qualified
import Theory

everyOther :: Note -> [Note]
everyOther n = cycle [n, 0, 0, 0]

solaris :: Song
solaris = build $ do
  bassDrum <- synth "bass-drum" Synths.bassDrum
  fourOnTheFloor <- patt bassDrum (take 32 (everyOther c5))

  weird <- synth "weird" Synths.weirdSynth
  let makeWeird ns1 ns2 = patt weird $ (take 16 $ arpeggio3 ns1) <> (take 16 $ arpeggio3 ns2)
  let makeWeird' nss = patt weird $ concat $ map (\(l, ns) -> take l $ arpeggio3 ns) nss
  weird0 <- makeWeird [g'3, c4, f4, e4] [f4, c4, g'3, f3]
  weird1 <- makeWeird [g3, a'3, c'4, c4] [f3, g'3, c4, a'3]
  weird2 <- makeWeird [g'3, f3, g'3, c4] [f4, e4, f4, g'4, g4, f4, e4, f4]
  weird3 <-
    makeWeird'
      [ (16, [e4, c4, g3, a'3, g'3, c4, f4, g'4]),
        (8, [g4, d'4, g'4, g4]),
        (8, [g'4, d'4, f4, f'4])
      ]

  spacey <- synth "spacey" Synths.spaceySynth
  let makeSpacey ns1 ns2 = patt spacey $ take 16 (arpeggio3 (reverse $ map octave ns1)) <> take 16 (arpeggio3 (reverse $ map octave ns2))
  let makeSpacey' nss = patt spacey $ concat $ map (\(l, ns) -> take l $ arpeggio3 (reverse $ map octave ns)) nss
  spacey0 <- makeSpacey [g'3, c4, f4, e4] [f4, c4, g'3, f3]
  spacey1 <- makeSpacey [g3, a'3, c'4, c4] [f3, g'3, c4, a'3]
  spacey2 <- makeSpacey [g'3, f3, g'3, c4] [f4, e4, f4, g'4, g4, f4, e4, f4]
  spacey3 <- makeSpacey' [(16, [e4, c4, g3, a'3, g'3, c4, f4, g'4]), (8, [g4, d'4, g'4, g4]), (8, [g'4, d'4, f4, f'4])]

  pure
    $ [ [fourOnTheFloor, weird0, spacey0],
        [fourOnTheFloor, weird1, spacey1],
        [fourOnTheFloor, weird2, spacey2],
        [fourOnTheFloor, weird3, spacey3]
        -- [weird3],
        -- [weird4]
      ]
