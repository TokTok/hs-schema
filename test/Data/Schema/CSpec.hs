{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
module Data.Schema.CSpec (spec) where

import           GHC.Generics        (Generic)
import           Test.Hspec

import           Control.Monad       (forM_)
import qualified Data.ByteString     as BS
import           Data.Proxy          (Proxy (..))
import           Data.Schema
import           Data.Schema.Builder (atom)
import           Data.Schema.C       (genC)
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
    describe "genC" $ do
        it "generates valid C code" $ do
            let actual = lines (show (genC myRecordV1))
            let expected =
                    [ "// events.h"
                    , "typedef struct My_Record_V1 My_Record_V1;"
                    , "uint32_t my_record_v1_get_record_field1_v1("
                    , "    const My_Record_V1 *event);"
                    , "double my_record_v1_get_record_field2_v1("
                    , "    const My_Record_V1 *event);"
                    , "string my_record_v1_get_record_field3_v1("
                    , "    const My_Record_V1 *event);"
                    , "uint32_t my_record_v1_get_some_word32_v1("
                    , "    const My_Record_V1 *event);"
                    , "const uint8_t *my_record_v1_get_public_key_v1("
                    , "    const My_Record_V1 *event);"
                    , "const uint8_t *my_record_v1_get_message_v1("
                    , "    const My_Record_V1 *event);"
                    , "uint32_t my_record_v1_get_message_v1_size("
                    , "    const My_Record_V1 *event);"
                    , "// events.c"
                    , "struct My_Record_V1 {"
                    , "    uint32_t record_field1_v1;"
                    , "    double record_field2_v1;"
                    , "    string record_field3_v1;"
                    , "    uint32_t some_word32_v1;"
                    , "    uint8_t public_key_v1[32];"
                    , "    uint8_t *message_v1;"
                    , "    uint32_t message_v1_size;"
                    , "};"
                    , "uint32_t my_record_v1_get_record_field1_v1("
                    , "    const My_Record_V1 *event) {"
                    , "    return event->record_field1_v1;"
                    , "}"
                    , ""
                    , "static void my_record_v1_set_record_field1_v1("
                    , "    My_Record_V1 *event, uint32_t value) {"
                    , "    event->record_field1_v1 = value;"
                    , "}"
                    , ""
                    , "double my_record_v1_get_record_field2_v1("
                    , "    const My_Record_V1 *event) {"
                    , "    return event->record_field2_v1;"
                    , "}"
                    , ""
                    , "static void my_record_v1_set_record_field2_v1("
                    , "    My_Record_V1 *event, double value) {"
                    , "    event->record_field2_v1 = value;"
                    , "}"
                    , ""
                    , "string my_record_v1_get_record_field3_v1("
                    , "    const My_Record_V1 *event) {"
                    , "    return event->record_field3_v1;"
                    , "}"
                    , ""
                    , "static void my_record_v1_set_record_field3_v1("
                    , "    My_Record_V1 *event, string value) {"
                    , "    event->record_field3_v1 = value;"
                    , "}"
                    , ""
                    , "uint32_t my_record_v1_get_some_word32_v1("
                    , "    const My_Record_V1 *event) {"
                    , "    return event->some_word32_v1;"
                    , "}"
                    , ""
                    , "static void my_record_v1_set_some_word32_v1("
                    , "    My_Record_V1 *event, uint32_t value) {"
                    , "    event->some_word32_v1 = value;"
                    , "}"
                    , ""
                    , "const uint8_t *my_record_v1_get_public_key_v1("
                    , "    const My_Record_V1 *event) {"
                    , "    return event->public_key_v1;"
                    , "}"
                    , ""
                    , "static void my_record_v1_set_public_key_v1("
                    , "    My_Record_V1 *event, const uint8_t *value) {"
                    , "    memcpy(event->public_key_v1, value, 32);"
                    , "}"
                    , ""
                    , "const uint8_t *my_record_v1_get_message_v1("
                    , "    const My_Record_V1 *event) {"
                    , "    return event->message_v1;"
                    , "}"
                    , ""
                    , "uint32_t my_record_v1_get_message_v1_size("
                    , "    const My_Record_V1 *event) {"
                    , "    return event->message_v1_size;"
                    , "}"
                    , ""
                    , "static void my_record_v1_set_message_v1("
                    , "    My_Record_V1 *event, const uint8_t *value, uint32_t size) {"
                    , "    assert(event != nullptr);"
                    , "    if (event->message_v1 != nullptr) {"
                    , "        free(event->message_v1);"
                    , "        event->message_v1 = nullptr;"
                    , "        event->message_v1_size = 0;"
                    , "    }"
                    , "    event->message_v1 = (uint8_t *)malloc(size);"
                    , "    if (event->message_v1 == nullptr) {"
                    , "        return false;"
                    , "    }"
                    , "    memcpy(event->message_v1, value, size);"
                    , "    event->message_v1_size = size;"
                    , "    return true;"
                    , "}"
                    ]
            forM_ (zip (addLines actual) (addLines expected)) $ uncurry shouldBe

addLines :: [String] -> [String]
addLines = zipWith addLine [1..]

addLine :: Int -> String -> String
addLine i s = show i <> ". " <> s
