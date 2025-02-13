module Theory (fifth, majorThird, minorThird, Scale (..), arpeggio) where

import Notes (Note)
import Relude

fifth :: Note -> Note
fifth n = n + 7

majorThird :: Note -> Note
majorThird n = n + 4

minorThird :: Note -> Note
minorThird n = n + 3

data Scale = Major | Minor

arpeggio :: Note -> Scale -> [Note]
arpeggio root Major = cycle [root, majorThird root, fifth root]
arpeggio root Minor = cycle [root, minorThird root, fifth root]
