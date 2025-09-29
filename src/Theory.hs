module Theory
  ( fifth,
    majorThird,
    minorThird,
    octave,
    Chord (..),
    arpeggio0,
    arpeggio0Fast,
    arpeggio1,
    arpeggio2,
    arpeggio3,
  )
where

import Notes (Note (Note))
import Relude

fifth :: Note -> Note
fifth n = n + 7

majorThird :: Note -> Note
majorThird n = n + 4

minorThird :: Note -> Note
minorThird n = n + 3

seventh :: Note -> Note
seventh n = n + 10

octave :: Note -> Note
octave n = n + 12

data Chord = Major | Minor | Seventh

-- e.g. mask "1.2.3.." picks out the first note then a rest
-- then the second note
mask :: [Char] -> [Note] -> [Note]
mask m ns = map (fromMaybe 0 . f) m
  where
    f i = do
      i' <- readMaybe [i]
      ns !!? (i' - 1)

chord :: Note -> Chord -> [Note]
chord root Major = [root, majorThird root, fifth root]
chord root Minor = [root, minorThird root, fifth root]
chord root Seventh = [root, majorThird root, fifth root, seventh root]

infiniteChord :: Note -> Chord -> [Note]
infiniteChord root c = chord root c <> infiniteChord (octave root) c

arpeggio :: Note -> Chord -> [Char] -> [Note]
arpeggio root scale m = cycle $ mask m (infiniteChord root scale)

arpeggio0 :: Note -> Chord -> [Note]
arpeggio0 root scale = arpeggio root scale "1.2.3."

arpeggio0Fast :: Note -> Chord -> [Note]
arpeggio0Fast root scale = arpeggio root scale "1234321"

arpeggio2 :: Note -> Chord -> [Note]
arpeggio2 root scale = arpeggio root scale "1.2.3.4."

arpeggio3 :: [Note] -> [Note]
arpeggio3 ns = cycle $ intersperse 0 ns <> [0]

arpeggio1 :: Note -> Chord -> [Note]
arpeggio1 root scale = arpeggio root scale "1.34.678"
