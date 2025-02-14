module Synths
  ( hihat,
    bassDrum,
    leadSynth,
    weirdSynth,
    spaceySynth,
  )
where

import Synths.Types

bassDrum :: Synth
bassDrum =
  Synth
    { osc0 =
        OscConfig
          { oscOct = OscOct 7,
            oscDet = OscDet 0,
            oscDetune = OscDetune 0,
            oscXEnv = OscXEnv 1,
            oscVol = OscVol 255,
            oscWaveform = OscWaveform 0
          },
      osc1 =
        OscConfig
          { oscOct = OscOct 7,
            oscDet = OscDet 0,
            oscDetune = OscDetune 0,
            oscXEnv = OscXEnv 1,
            oscVol = OscVol 255,
            oscWaveform = OscWaveform 0
          },
      noiseFader = NoiseFader 0,
      env =
        EnvConfig
          { envAttack = EnvAttack 100,
            envSustain = EnvSustain 0,
            envRelease = EnvRelease 3636,
            envMaster = EnvMaster 254
          },
      fx =
        FxConfig
          { fxFilter = FxFilter 2,
            fxFreq = FxFreq 500,
            fxResonance = FxResonance 254,
            fxDelayTime = FxDelayTime 0,
            fxDelayAmt = FxDelayAmt 27,
            fxPanFreq = FxPanFreq 0,
            fxPanAmt = FxPanAmt 0
          },
      lfo =
        LfoConfig
          { lfoOscFreq = LfoOscFreq 0,
            lfoFxFreq = LfoFxFreq 0,
            lfoFreq = LfoFreq 0,
            lfoAmt = LfoAmt 0,
            lfoWaveform = LfoWaveform 0
          }
    }

hihat :: Synth
hihat =
  Synth
    { osc0 =
        OscConfig
          { oscOct = OscOct 8,
            oscDet = OscDet 0,
            oscDetune = OscDetune 0,
            oscXEnv = OscXEnv 0,
            oscVol = OscVol 0,
            oscWaveform = OscWaveform 0
          },
      osc1 =
        OscConfig
          { oscOct = OscOct 8,
            oscDet = OscDet 0,
            oscDetune = OscDetune 0,
            oscXEnv = OscXEnv 0,
            oscVol = OscVol 0,
            oscWaveform = OscWaveform 0
          },
      noiseFader = NoiseFader 60,
      env =
        EnvConfig
          { envAttack = EnvAttack 50,
            envSustain = EnvSustain 419,
            envRelease = EnvRelease 4607,
            envMaster = EnvMaster 130
          },
      fx =
        FxConfig
          { fxFilter = FxFilter 1,
            fxFreq = FxFreq 10332,
            fxResonance = FxResonance 120,
            fxDelayTime = FxDelayTime 4,
            fxDelayAmt = FxDelayAmt 16,
            fxPanFreq = FxPanFreq 5,
            fxPanAmt = FxPanAmt 108
          },
      lfo =
        LfoConfig
          { lfoOscFreq = LfoOscFreq 0,
            lfoFxFreq = LfoFxFreq 0,
            lfoFreq = LfoFreq 5,
            lfoAmt = LfoAmt 187,
            lfoWaveform = LfoWaveform 0
          }
    }

leadSynth :: Synth
leadSynth =
  Synth
    { osc0 =
        OscConfig
          { oscOct = OscOct 9,
            oscDet = OscDet 0,
            oscDetune = OscDetune 0,
            oscXEnv = OscXEnv 0,
            oscVol = OscVol 255,
            oscWaveform = OscWaveform 0
          },
      osc1 =
        OscConfig
          { oscOct = OscOct 9,
            oscDet = OscDet 0,
            oscDetune = OscDetune 12,
            oscXEnv = OscXEnv 0,
            oscVol = OscVol 255,
            oscWaveform = OscWaveform 0
          },
      noiseFader = NoiseFader 0,
      env =
        EnvConfig
          { envAttack = EnvAttack 100,
            envSustain = EnvSustain 0,
            envRelease = EnvRelease 14545,
            envMaster = EnvMaster 70
          },
      fx =
        FxConfig
          { fxFilter = FxFilter 0,
            fxFreq = FxFreq 0,
            fxResonance = FxResonance 240,
            fxDelayTime = FxDelayTime 2,
            fxDelayAmt = FxDelayAmt 157,
            fxPanFreq = FxPanFreq 3,
            fxPanAmt = FxPanAmt 47
          },
      lfo =
        LfoConfig
          { lfoOscFreq = LfoOscFreq 0,
            lfoFxFreq = LfoFxFreq 0,
            lfoFreq = LfoFreq 0,
            lfoAmt = LfoAmt 0,
            lfoWaveform = LfoWaveform 0
          }
    }

weirdSynth :: Synth
weirdSynth =
  Synth
    { osc0 =
        OscConfig
          { oscOct = OscOct 7,
            oscDet = OscDet 0,
            oscDetune = OscDetune 0,
            oscXEnv = OscXEnv 0,
            oscVol = OscVol 206,
            oscWaveform = OscWaveform 2
          },
      osc1 =
        OscConfig
          { oscOct = OscOct 7,
            oscDet = OscDet 0,
            oscDetune = OscDetune 0,
            oscXEnv = OscXEnv 0,
            oscVol = OscVol 201,
            oscWaveform = OscWaveform 2
          },
      noiseFader = NoiseFader 14,
      env =
        EnvConfig
          { envAttack = EnvAttack 1111,
            envSustain = EnvSustain 150,
            envRelease = EnvRelease 3125,
            envMaster = EnvMaster 175
          },
      fx =
        FxConfig
          { fxFilter = FxFilter 4,
            fxFreq = FxFreq 8589,
            fxResonance = FxResonance 114,
            fxDelayTime = FxDelayTime 6,
            fxDelayAmt = FxDelayAmt 53,
            fxPanFreq = FxPanFreq 4,
            fxPanAmt = FxPanAmt 0
          },
      lfo =
        LfoConfig
          { lfoOscFreq = LfoOscFreq 0,
            lfoFxFreq = LfoFxFreq 1,
            lfoFreq = LfoFreq 6,
            lfoAmt = LfoAmt 195,
            lfoWaveform = LfoWaveform 3
          }
    }

spaceySynth :: Synth
spaceySynth =
  Synth
    { osc0 =
        OscConfig
          { oscOct = OscOct 9,
            oscDet = OscDet 0,
            oscDetune = OscDetune 0,
            oscXEnv = OscXEnv 0,
            oscVol = OscVol 255,
            oscWaveform = OscWaveform 0
          },
      osc1 =
        OscConfig
          { oscOct = OscOct 9,
            oscDet = OscDet 0,
            oscDetune = OscDetune 12,
            oscXEnv = OscXEnv 0,
            oscVol = OscVol 255,
            oscWaveform = OscWaveform 0
          },
      noiseFader = NoiseFader 0,
      env =
        EnvConfig
          { envAttack = EnvAttack 100,
            envSustain = EnvSustain 0,
            envRelease = EnvRelease 14545,
            envMaster = EnvMaster 70
          },
      fx =
        FxConfig
          { fxFilter = FxFilter 0,
            fxFreq = FxFreq 0,
            fxResonance = FxResonance 240,
            fxDelayTime = FxDelayTime 2,
            fxDelayAmt = FxDelayAmt 157,
            fxPanFreq = FxPanFreq 0,
            fxPanAmt = FxPanAmt 0
          },
      lfo =
        LfoConfig
          { lfoOscFreq = LfoOscFreq 0,
            lfoFxFreq = LfoFxFreq 0,
            lfoFreq = LfoFreq 0,
            lfoAmt = LfoAmt 0,
            lfoWaveform = LfoWaveform 0
          }
    }
