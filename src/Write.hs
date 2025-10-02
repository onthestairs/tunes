module Write (write) where

import Dsl (Song, SynthName (..))
import Relude
import Sound.PlSynth (PlSynthT (..), withPlSynth, withSongTracks, writeSong)

makeBpm :: Word32 -> Word32
makeBpm n = 60 * 44100 `div` 4 `div` n

type BPM = Word32

type PlSong = [([Word8], [[Word8]], PlSynthT)]

toPlSong :: Song -> PlSong
toPlSong = map (\(name, seq, patterns, synth) -> (seq, patterns, synth))

write :: Song -> BPM -> FilePath -> IO ()
write song bpm path = withPlSynth $ do
  let bpm' = makeBpm bpm
  -- write the whole song
  let fullPath = path <> "/full.wav"
  let plSong = toPlSong song
  withSongTracks bpm' plSong $ writeSong fullPath
  -- write the individual tracks
  forM_ song $ \(name, seq, patterns, synth) -> do
    let trackPath = path <> "/" <> toString name.toSynthName <> ".wav"
    withSongTracks bpm' [(seq, patterns, synth)] $ writeSong trackPath
