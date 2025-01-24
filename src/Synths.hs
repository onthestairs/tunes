module Synths (hihat, bassDrum) where

import Sound.PlSynth (PlSynthT (..))

bassDrum :: PlSynthT
bassDrum = PlSynthT 7 0 0 1 255 0 7 0 0 1 255 0 0 100 0 3636 254 2 500 254 0 27 0 0 0 0 0 0 0

hihat :: PlSynthT
hihat = PlSynthT 8 0 0 0 0 0 8 0 0 0 0 0 60 50 419 4607 130 1 10332 120 4 16 5 108 0 0 5 187 0
