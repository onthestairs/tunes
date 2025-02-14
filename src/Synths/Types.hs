module Synths.Types
  ( OscOct (..),
    OscDet (..),
    OscDetune (..),
    OscXEnv (..),
    OscVol (..),
    OscWaveform (..),
    NoiseFader (..),
    EnvAttack (..),
    EnvSustain (..),
    EnvRelease (..),
    EnvMaster (..),
    FxFilter (..),
    FxFreq (..),
    FxResonance (..),
    FxDelayTime (..),
    FxDelayAmt (..),
    FxPanFreq (..),
    FxPanAmt (..),
    LfoOscFreq (..),
    LfoFxFreq (..),
    LfoFreq (..),
    LfoAmt (..),
    LfoWaveform (..),
    OscConfig (..),
    EnvConfig (..),
    FxConfig (..),
    LfoConfig (..),
    Synth (..),
    toPlSynth,
  )
where

import Relude
import Sound.PlSynth (PlSynthT (..))

-- Oscillator parameters
newtype OscOct = OscOct Word8
  deriving (Eq, Show)

newtype OscDet = OscDet Word8
  deriving (Eq, Show)

newtype OscDetune = OscDetune Word8
  deriving (Eq, Show)

newtype OscXEnv = OscXEnv Word8
  deriving (Eq, Show)

newtype OscVol = OscVol Word8
  deriving (Eq, Show)

newtype OscWaveform = OscWaveform Word8
  deriving (Eq, Show)

-- Noise parameter
newtype NoiseFader = NoiseFader Word8
  deriving (Eq, Show)

-- Envelope parameters
newtype EnvAttack = EnvAttack Word32
  deriving (Eq, Show)

newtype EnvSustain = EnvSustain Word32
  deriving (Eq, Show)

newtype EnvRelease = EnvRelease Word32
  deriving (Eq, Show)

newtype EnvMaster = EnvMaster Word32
  deriving (Eq, Show)

-- Effects parameters
newtype FxFilter = FxFilter Word8
  deriving (Eq, Show)

newtype FxFreq = FxFreq Word32
  deriving (Eq, Show)

newtype FxResonance = FxResonance Word8
  deriving (Eq, Show)

newtype FxDelayTime = FxDelayTime Word8
  deriving (Eq, Show)

newtype FxDelayAmt = FxDelayAmt Word8
  deriving (Eq, Show)

newtype FxPanFreq = FxPanFreq Word8
  deriving (Eq, Show)

newtype FxPanAmt = FxPanAmt Word8
  deriving (Eq, Show)

-- LFO parameters
newtype LfoOscFreq = LfoOscFreq Word8
  deriving (Eq, Show)

newtype LfoFxFreq = LfoFxFreq Word8
  deriving (Eq, Show)

newtype LfoFreq = LfoFreq Word8
  deriving (Eq, Show)

newtype LfoAmt = LfoAmt Word8
  deriving (Eq, Show)

newtype LfoWaveform = LfoWaveform Word8
  deriving (Eq, Show)

-- Oscillator configuration
data OscConfig = OscConfig
  { oscOct :: OscOct,
    oscDet :: OscDet,
    oscDetune :: OscDetune,
    oscXEnv :: OscXEnv,
    oscVol :: OscVol,
    oscWaveform :: OscWaveform
  }
  deriving (Eq, Show)

-- Envelope configuration
data EnvConfig = EnvConfig
  { envAttack :: EnvAttack,
    envSustain :: EnvSustain,
    envRelease :: EnvRelease,
    envMaster :: EnvMaster
  }
  deriving (Eq, Show)

-- FX configuration
data FxConfig = FxConfig
  { fxFilter :: FxFilter,
    fxFreq :: FxFreq,
    fxResonance :: FxResonance,
    fxDelayTime :: FxDelayTime,
    fxDelayAmt :: FxDelayAmt,
    fxPanFreq :: FxPanFreq,
    fxPanAmt :: FxPanAmt
  }
  deriving (Eq, Show)

-- LFO configuration
data LfoConfig = LfoConfig
  { lfoOscFreq :: LfoOscFreq,
    lfoFxFreq :: LfoFxFreq,
    lfoFreq :: LfoFreq,
    lfoAmt :: LfoAmt,
    lfoWaveform :: LfoWaveform
  }
  deriving (Eq, Show)

-- Main synth type
data Synth = Synth
  { osc0 :: OscConfig,
    osc1 :: OscConfig,
    noiseFader :: NoiseFader,
    env :: EnvConfig,
    fx :: FxConfig,
    lfo :: LfoConfig
  }
  deriving (Eq, Show)

toPlSynth :: Synth -> PlSynthT
toPlSynth synth =
  PlSynthT
    { osc0_oct = coerce synth.osc0.oscOct,
      osc0_det = coerce synth.osc0.oscDet,
      osc0_detune = coerce synth.osc0.oscDetune,
      osc0_xenv = coerce synth.osc0.oscXEnv,
      osc0_vol = coerce synth.osc0.oscVol,
      osc0_waveform = coerce synth.osc0.oscWaveform,
      osc1_oct = coerce synth.osc1.oscOct,
      osc1_det = coerce synth.osc1.oscDet,
      osc1_detune = coerce synth.osc1.oscDetune,
      osc1_xenv = coerce synth.osc1.oscXEnv,
      osc1_vol = coerce synth.osc1.oscVol,
      osc1_waveform = coerce synth.osc1.oscWaveform,
      noise_fader = coerce synth.noiseFader,
      env_attack = coerce synth.env.envAttack,
      env_sustain = coerce synth.env.envSustain,
      env_release = coerce synth.env.envRelease,
      env_master = coerce synth.env.envMaster,
      fx_filter = coerce synth.fx.fxFilter,
      fx_freq = coerce synth.fx.fxFreq,
      fx_resonance = coerce synth.fx.fxResonance,
      fx_delay_time = coerce synth.fx.fxDelayTime,
      fx_delay_amt = coerce synth.fx.fxDelayAmt,
      fx_pan_freq = coerce synth.fx.fxPanFreq,
      fx_pan_amt = coerce synth.fx.fxPanAmt,
      lfo_osc_freq = coerce synth.lfo.lfoOscFreq,
      lfo_fx_freq = coerce synth.lfo.lfoFxFreq,
      lfo_freq = coerce synth.lfo.lfoFreq,
      lfo_amt = coerce synth.lfo.lfoAmt,
      lfo_waveform = coerce synth.lfo.lfoWaveform
    }
