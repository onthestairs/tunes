module Songs.Roses (roses) where

import Dsl
import Relude
import Sound.PlSynth (PlSynthT (..))

leadSynth :: PlSynthT
leadSynth = PlSynthT 9 0 0 0 255 0 9 0 12 0 255 0 0 100 0 14545 70 0 0 240 2 157 3 47 0 0 0 0 0

bassDrum :: PlSynthT
bassDrum = PlSynthT 7 0 0 1 255 0 7 0 0 1 255 0 0 100 0 3636 254 2 500 254 0 27 0 0 0 0 0 0 0

makeRiff :: (Num b) => [b] -> [b]
makeRiff bassNotes = concatMap (\n -> bassNotes <> [n]) [66, 65, 61, 70, 65] <> [66]

riff1, riff2 :: [Word8]
riff1 = map (+ 75) (makeRiff [51, 58] <> makeRiff [51, 58])
riff2 = map (+ 75) (makeRiff [47, 54] <> makeRiff [49, 56])

replicateL :: Int -> [a] -> [a]
replicateL n = concat . replicate n

fourOnTheFloor :: [Word8]
fourOnTheFloor = replicateL 16 [147, 0]

roses :: [([Word8], [[Word8]], PlSynthT)]
roses = build $ do
  lead <- synth leadSynth
  bass <- synth bassDrum
  leadPattern1 <- patt lead riff1
  leadPattern2 <- patt lead riff2
  four <- patt bass fourOnTheFloor
  pure
    [ [four, leadPattern1],
      [four, leadPattern2],
      [four, leadPattern1],
      [four, leadPattern2],
      [four, leadPattern1],
      [four, leadPattern2]
    ]
