module Export (writeWavs, exportJson) where

import Data.Aeson
import Dsl (Song, SynthName (..))
import Relude
import Relude hiding (seq)
import Songs.Roses (roses)
import Sound.PlSynth (PlSynthT (..), withPlSynth, withSongTracks, writeSong)

makeBpm :: Word32 -> Word32
makeBpm n = 60 * 44100 `div` 4 `div` n

type BPM = Word32

type PlSong = [([Word8], [[Word8]], PlSynthT)]

toPlSong :: Song -> PlSong
toPlSong = map (\(name, seq, patterns, synth) -> (seq, patterns, synth))

writeWavs :: Song -> BPM -> FilePath -> IO ()
writeWavs song bpm path = withPlSynth $ do
  let bpm' = makeBpm bpm
  -- write the whole song
  let fullPath = path <> "/full.wav"
  let plSong = toPlSong song
  withSongTracks bpm' plSong $ writeSong fullPath
  -- write the individual tracks
  forM_ song $ \(name, seq, patterns, synth) -> do
    let trackPath = path <> "/" <> toString name.toSynthName <> ".wav"
    withSongTracks bpm' [(seq, patterns, synth)] $ writeSong trackPath

number :: (Enum a) => a -> Value
number = Number . fromIntegral . fromEnum

array :: [Value] -> Value
array = Array . fromList

exportJson :: Word32 -> Song -> FilePath -> IO ()
exportJson rowLen song path = do
  let trackJson (_, seq :: [Word8], patterns, synth) =
        array
          [ synthToJson synth,
            array (map number seq),
            array (map (array . map number) patterns)
          ]

  let json =
        array
          [ -- row len
            number rowLen,
            --- tracks
            array (map trackJson song)
          ]
  let jsonBytes = encode json
  writeFileLBS path jsonBytes

synthToJson :: PlSynthT -> Value
synthToJson (PlSynthT a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac) =
  array
    [ number a,
      number b,
      number c,
      number d,
      number e,
      number f,
      number g,
      number h,
      number i,
      number j,
      number k,
      number l,
      number m,
      number n,
      number o,
      number p,
      number q,
      number r,
      number s,
      number t,
      number u,
      number v,
      number w,
      number x,
      number y,
      number z,
      number aa,
      number ab,
      number ac
    ]
