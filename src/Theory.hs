module Theory (fifth, majorThird, minorThird) where

import Notes (Note (Note))
import Relude

fifth :: Note -> Note
fifth n = n + 7

majorThird :: Note -> Note
majorThird n = n + 4

minorThird :: Note -> Note
minorThird n = n + 3
