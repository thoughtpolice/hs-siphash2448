module Main
       ( main -- :: IO ()
       ) where
import Criterion.Main
import Siphash24
import Siphash48

main :: IO ()
main = defaultMain [ bgroup "siphash24" siphash24bench
                   , bgroup "siphash48" siphash48bench
                   ]
