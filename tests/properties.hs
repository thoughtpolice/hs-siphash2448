{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Main
       ( main  -- :: IO ()
       ) where
import           Control.Monad
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as S

import qualified Crypto.MAC.Siphash24     as Siphash24
import qualified Crypto.MAC.Siphash48     as Siphash48

import           System.Environment       (getArgs)
import           Test.QuickCheck
import           Test.QuickCheck.Property (morallyDubiousIOProperty)
import           Text.Printf

--------------------------------------------------------------------------------
-- Orphans

instance Arbitrary ByteString where
  arbitrary = S.pack `liftM` arbitrary

newtype K2 = K2 ByteString deriving Show
instance Arbitrary K2 where
  arbitrary = K2 `liftM` (arbitrary `suchThat` (\x -> S.length x == 16))

--------------------------------------------------------------------------------
-- Tests

roundtrip24 :: K2 -> ByteString -> Bool
roundtrip24 (K2 k) xs = Siphash24.verify k' (Siphash24.authenticate k' xs) xs
  where k' = maybe (error "impossible") id (Siphash24.key k)

roundtrip48 :: K2 -> ByteString -> Bool
roundtrip48 (K2 k) xs = Siphash48.verify k' (Siphash48.authenticate k' xs) xs
  where k' = maybe (error "impossible") id (Siphash48.key k)

--------------------------------------------------------------------------------
-- Driver

main :: IO ()
main = do
  args <- fmap (drop 1) getArgs
  let n = if null args then 100 else read (head args) :: Int
  (results, passed) <- runTests n
  printf "Passed %d tests!\n" (sum passed)
  unless (and results) (fail "Not all tests passed!")

runTests :: Int -> IO ([Bool], [Int])
runTests ntests = fmap unzip . forM (tests ntests) $ \(s, a) ->
  printf "%-40s: " s >> a

tests :: Int -> [(String, IO (Bool,Int))]
tests ntests =
  [ ("siphash24 roundtrip", wrap roundtrip24)
  , ("siphash48 roundtrip", wrap roundtrip48)
  ]
  where
    wrap :: Testable prop => prop -> IO (Bool, Int)
    wrap prop = do
      r <- quickCheckWithResult stdArgs{maxSize=ntests} prop
      case r of
        Success n _ _           -> return (True, n)
        GaveUp  n _ _           -> return (True, n)
#if MIN_VERSION_QuickCheck(2,6,0)
        Failure n _ _ _ _ _ _ _ -> return (False, n)
#else
        Failure n _ _ _ _ _ _   -> return (False, n)
#endif
        _                       -> return (False, 0)
