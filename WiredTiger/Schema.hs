{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
module WiredTiger.Schema where

import Data.Int
import Data.Serialize
import Data.Word
import Data.String (IsString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as CL
import WiredTiger.IntPack


newtype B = B Int8
  deriving Show

instance Serialize B where
  get = B . fromIntegral <$> getInt
  put (B x) = putInt (fromIntegral x)

newtype BB = BB Word8
  deriving Show

instance Serialize BB where
  get = BB . fromIntegral <$> getWord
  put (BB x) = putInt (fromIntegral x)

newtype H  = H Int16
  deriving Show

instance Serialize H where
  get = H . fromIntegral <$> getInt
  put (H x) = putInt (fromIntegral x)

newtype HH = HH Word16
  deriving Show

instance Serialize HH where
  get = HH . fromIntegral <$> getWord
  put (HH x) = putInt (fromIntegral x)

newtype I = I Int32
  deriving Show

instance Serialize I where
  get = I . fromIntegral <$> getInt
  put (I x) = putInt (fromIntegral x)

newtype II = II Word32
  deriving Show

instance Serialize II where
  get = II . fromIntegral <$> getWord
  put (II x) = putInt (fromIntegral x)

newtype L = L Int32
  deriving Show

instance Serialize L where
  get = L . fromIntegral <$> getInt
  put (L x) = putInt (fromIntegral x)

newtype LL = LL Word32
  deriving Show

instance Serialize LL where
  get = LL . fromIntegral <$> getWord
  put (LL x) = putWord (fromIntegral x)

newtype Q  = Q Int64
  deriving Show

instance Serialize Q where
  get = Q . fromIntegral <$> getInt
  put (Q x) = putWord (fromIntegral x)

newtype QQ = QQ Word64
  deriving Show

instance Serialize QQ where
  get = QQ . fromIntegral <$> getWord
  put (QQ x) = putWord (fromIntegral x)

newtype S = S String
  deriving Show
  deriving IsString via String

newtype SS = SS String
  deriving Show
  deriving IsString via String

instance Serialize SS where
  put (SS str) = putLazyByteString $ B.toLazyByteString $ B.stringUtf8 str <> B.word8 0
  get = SS <$> getNullTerminatedString

getNullTerminatedLazyByteString :: Get BL.ByteString
getNullTerminatedLazyByteString = go mempty
  where
    go acc = do
      c <- getWord8
      case c of
        0 -> pure $ B.toLazyByteString acc
        _ -> go (acc <> B.word8 c)

getNullTerminatedByteString :: Get BS.ByteString
getNullTerminatedByteString = BS.toStrict <$> getNullTerminatedLazyByteString

getNullTerminatedString :: Get String
getNullTerminatedString = CL.unpack <$> getNullTerminatedLazyByteString

