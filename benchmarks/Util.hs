{-# LANGUAGE CPP #-}
module Util where
import Control.DeepSeq
import qualified Data.ByteString as B

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData B.ByteString
#endif
