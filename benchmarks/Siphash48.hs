module Siphash48
       ( siphash48bench
       ) where
import Criterion.Main
import Control.DeepSeq
import Crypto.MAC.Siphash48

import qualified Data.ByteString as B

import Util

--------------------------------------------------------------------------------

instance NFData Auth where
  rnf (Auth x) = rnf x

--------------------------------------------------------------------------------

siphash48bench :: [Benchmark]
siphash48bench =
  let dummy = B.pack [1..512]
      k     = maybe (error "impossible") id (key $ B.pack [0..15])
      msg   = authenticate k dummy
  in [ bench "authenticate" $ nf (authenticate k) dummy
     , bench "verify"       $ nf (verify k)       msg
     , bench "roundtrip"    $ nf (roundtrip k)    dummy
     ]

roundtrip :: Key -> B.ByteString -> Bool
roundtrip k xs = verify k (authenticate k xs) xs
