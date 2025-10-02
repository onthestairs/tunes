module Songs where

import Export (exportSong)
import Relude hiding (seq)
import Songs.Roses (roses)
import Songs.Solaris (solaris)
import Sound.PlSynth (withPlSynth)

main :: IO ()
main = withPlSynth $ do
  exportSong roses 160 "output/roses"
  exportSong solaris 160 "output/solaris"
