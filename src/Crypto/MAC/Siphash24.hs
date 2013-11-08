{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Crypto.MAC.Siphash24
-- Copyright   : (c) Austin Seipp 2013
-- License     : MIT
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides @siphash24@ as a message-authentication code
-- (MAC.) The underlying implementation is the @little@ code of
-- @siphash24@ from SUPERCOP, and should be relatively fast.
--
-- For more information visit <https://131002.net/siphash/>.
--
module Crypto.MAC.Siphash24
       ( Key          -- :: *
       , key          -- :: *
       , Auth(..)     -- :: *
       , authenticate -- :: ByteString -> ByteString -> Maybe Auth
       , verify       -- :: ByteString -> Auth -> ByteString -> Maybe Bool
       ) where
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr

import           System.IO.Unsafe         (unsafePerformIO)

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as S
import           Data.ByteString.Internal (create)
import           Data.ByteString.Unsafe

newtype Key = Key ByteString
  deriving (Eq, Show, Ord)

key :: ByteString -> Maybe Key
key xs | S.length xs /= siphashKEYBYTES = Nothing
       | otherwise = Just (Key xs)

-- | An authenticator.
newtype Auth = Auth { unAuth :: ByteString }
  deriving (Eq, Show, Ord)

authenticate :: Key
             -- ^ Secret key
             -> ByteString
             -- ^ Message
             -> Auth
             -- ^ Authenticator
authenticate (Key k) msg = Auth $
  unsafePerformIO . create siphashBYTES $ \out ->
    unsafeUseAsCStringLen msg $ \(cstr, clen) ->
      unsafeUseAsCString k $ \pk ->
        c_crypto_siphash24 out cstr (fromIntegral clen) pk >> return ()
{-# INLINEABLE authenticate #-}

verify :: Key
       -- ^ Secret key
       -> Auth
       -- ^ Authenticator returned via 'authenticateOnce'
       -> ByteString
       -- ^ Message
       -> Bool
       -- ^ Result: @True@ if verified, @False@ otherwise
verify (Key k) (Auth auth) msg =
  unsafePerformIO . unsafeUseAsCString auth $ \pauth ->
    unsafeUseAsCStringLen msg $ \(cstr, clen) ->
      unsafeUseAsCString k $ \pk -> do
        b <- c_crypto_siphash24_verify pauth cstr (fromIntegral clen) pk
        return (b == 0)
{-# INLINE verify #-}

--
-- FFI mac binding
--

siphashKEYBYTES :: Int
siphashKEYBYTES = 16

siphashBYTES :: Int
siphashBYTES = 8

foreign import ccall unsafe "siphash24_mac"
  c_crypto_siphash24 :: Ptr Word8 -> Ptr CChar -> CULLong ->
                        Ptr CChar -> IO Int

foreign import ccall unsafe "siphash24_mac_verify"
  c_crypto_siphash24_verify :: Ptr CChar -> Ptr CChar -> CULLong ->
                               Ptr CChar -> IO Int
