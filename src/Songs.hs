module Songs where

import Relude
import Songs.Roses (roses)
import Sound.PlSynth (withPlSynth, withSongTracks, writeSong)

bpm :: Word32 -> Word32
bpm n = 60 * 44100 `div` 4 `div` n

main :: IO ()
main = do
  withPlSynth $ withSongTracks (bpm 70) roses $ writeSong "test-song.wav"
