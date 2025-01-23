module Songs where

import Songs.Roses (roses)
import Sound.PlSynth (withPlSynth, withSongTracks, writeSong)

main = withPlSynth $ withSongTracks 8481 roses $ writeSong "test-song.wav"
