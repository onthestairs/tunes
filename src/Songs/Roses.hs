module Songs.Roses (roses) where

import Relude (Word8)
import Sound.PlSynth (PlSynthT (..), emptyPlSynthT)

synth :: PlSynthT
synth =
  emptyPlSynthT
    { osc0_oct = 7,
      osc0_vol = 121,
      osc0_waveform = 1,
      osc1_oct = 7,
      osc1_vol = 91,
      osc1_waveform = 3,
      env_attack = 100,
      env_sustain = 1212,
      env_release = 5513,
      env_master = 100,
      fx_freq = 6,
      fx_resonance = 19,
      fx_delay_time = 3,
      fx_delay_amt = 121,
      fx_pan_freq = 6,
      fx_pan_amt = 21,
      lfo_fx_freq = 1,
      lfo_freq = 1,
      lfo_amt = 29
    }

synth1 :: PlSynthT
synth1 = PlSynthT 7 0 0 0 121 1 7 0 0 0 91 3 0 100 1212 5513 113 0 6 19 3 121 6 21 0 1 1 29 0

synth2 :: PlSynthT
synth2 = PlSynthT 9 0 0 0 255 0 9 0 12 0 255 0 0 100 0 14545 70 0 0 240 2 157 3 47 0 0 0 0 0

makeRiff :: (Num b) => [b] -> [b]
makeRiff bassNotes = concatMap (\n -> bassNotes <> [n]) [66, 65, 61, 70, 65] <> [66]

riff1, riff2 :: [Word8]
riff1 = map (+ 75) (makeRiff [51, 58] <> makeRiff [51, 58])
riff2 = map (+ 75) (makeRiff [47, 54] <> makeRiff [49, 56])

roses :: [([Word8], [[Word8]], PlSynthT)]
roses = [t1]
  where
    t1 =
      ( [1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2],
        [riff1, riff2],
        synth2
      )
