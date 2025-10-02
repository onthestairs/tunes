module Songs where

import Export (exportJson, writeWavs)
import Relude hiding (seq)
import Songs.Roses (roses, sp)
import Songs.Solaris (solaris)
import Sound.PlSynth (withPlSynth, withSongTracks, writeSong)

main :: IO ()
main = withPlSynth $ do
  writeWavs roses 160 "output/roses"

-- withSongTracks (bpm 160) roses $ writeSong "roses.wav"
-- exportJson (bpm 160) roses "roses.json"

--
-- withSongTracks (bpm 160) solaris $ writeSong "solaris.wav"
-- exportJson (bpm 160) solaris "solaris.json"
--
-- withSongTracks (bpm 160) sp $ writeSong "sp.wav"
-- exportJson (bpm 160) roses "sp.json"
