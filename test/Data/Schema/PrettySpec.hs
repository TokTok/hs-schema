{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
module Data.Schema.PrettySpec (spec) where

import           GHC.Generics        (Generic)
import           Test.Hspec

import qualified Data.ByteString     as BS
import           Data.Proxy          (Proxy (..))
import           Data.Schema
import           Data.Schema.Builder (atom)
import           Data.Schema.Pretty  (ppSchema)
import           Data.Word           (Word16, Word32, Word64)
import           GHC.TypeNats        (KnownNat, Nat, natVal)


newtype FixedByteString (size :: Nat) = FixedByteString BS.ByteString
    deriving (Ord, Eq, Read, Show, Generic)
instance KnownNat size => ToSchema (FixedByteString (size :: Nat)) where
    toSchema = atom (TyFixedBin (fromIntegral $ natVal (Proxy :: Proxy size))) (FixedByteString BS.empty)

instance ToSchema BS.ByteString where
    toSchema = atom TyBin BS.empty
instance ToSchema Word16 where
    toSchema = atom TyWord16 0
instance ToSchema Word32 where
    toSchema = atom TyWord32 0
instance ToSchema Word64 where
    toSchema = atom TyWord64 0
instance ToSchema Bool where
    toSchema = atom TyBool False

instance ToSchema Int where
    toSchema = atom TyWord32 0
instance ToSchema Double where
    toSchema = atom (TyName "double") 0
instance ToSchema String where
    toSchema = atom (TyName "string") ""

--------------------------------------------------------------------------------
-- Version 1 of the protocol.
--
data MyRecordV1 = MyRecordV1
  { recordField1V1 :: Int
  , recordField2V1 :: Double
  , recordField3V1 :: String
  , someWord32V1   :: Word32
  , publicKeyV1    :: FixedByteString 32
  , messageV1      :: BS.ByteString
  }
  deriving (Eq, Show, Read, Generic)

instance ToSchema MyRecordV1

myRecordV1 :: Schema
myRecordV1 = getSchema (toSchema :: Builder MyRecordV1)


spec :: Spec
spec =
    describe "ppSchema" $ do
        it "prints a human-readable form" $ do
            show (ppSchema myRecordV1) `shouldBe` unlines
                [ "type Data.Schema.PrettySpec.MyRecordV1 {"
                , "  MyRecordV1 = {"
                , "    recordField1V1 :: TyWord32"
                , "    recordField2V1 :: TyName \"double\""
                , "    recordField3V1 :: TyName \"string\""
                , "    someWord32V1 :: TyWord32"
                , "    publicKeyV1 :: TyFixedBin 32"
                , "    messageV1 :: TyBin"
                , "  }"
                , "}"
                ]
