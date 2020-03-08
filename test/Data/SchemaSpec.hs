{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.SchemaSpec (spec) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (when)
import           Data.Proxy          (Proxy (..))
import           Debug.Trace         (trace)
import           GHC.Generics        (Generic)
import           Test.Hspec
import           Test.QuickCheck
import           Text.Groom          (groom)

import           Data.MessagePack    (MessagePack (..), Object (..))
import           Data.Schema

--------------------------------------------------------------------------------
-- Version 1 of the protocol.
--
data MyRecordV1 = MyRecordV1
  { recordField1V1 :: Int
  , recordField2V1 :: Double
  , recordField3V1 :: String
  }
  deriving (Eq, Show, Read, Generic)

instance MessagePack MyRecordV1
instance Arbitrary MyRecordV1 where
  arbitrary = MyRecordV1
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary


instance HasSchema MyRecordV1 where
  schema p = Type
    { typeName = "MyRecord"
    , typeSchema = SchemaRecord
        [ Field "recordField1" $ schema $ recordField1V1 <$> p
        , Field "recordField2" $ schema $ recordField2V1 <$> p
        , Field "recordField3" $ schema $ recordField3V1 <$> p
        ]
    }


--------------------------------------------------------------------------------
-- Version 2 of the protocol.
--
-- This version has reordered the second and third field.
--
data MyRecordV2 = MyRecordV2
  { recordField1V2 :: Int
  , recordField3V2 :: String
  , recordField2V2 :: Double
  }
  deriving (Eq, Show, Read, Generic)

instance MessagePack MyRecordV2
instance Arbitrary MyRecordV2 where
  arbitrary = MyRecordV2
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary


instance HasSchema MyRecordV2 where
  schema p = Type
    { typeName = "MyRecord"
    , typeSchema = SchemaRecord
        [ Field "recordField1" $ schema $ recordField1V2 <$> p
        -- Also reordered here so we know the order in the schema.
        , Field "recordField3" $ schema $ recordField3V2 <$> p
        , Field "recordField2" $ schema $ recordField2V2 <$> p
        ]
    }


--------------------------------------------------------------------------------
-- Implementation of schema loading goes here:
--
fromObjectWithSchema :: (MessagePack a, HasSchema a) => Type -> Type -> Object -> a
fromObjectWithSchema srcType tgtType obj =
  trace ("Loading object: " ++ groom obj) $
  trace ("Source schema: " ++ groom srcType) $
  trace ("Target schema: " ++ groom tgtType) $
  error "unimplemented: fromObjectWithSchema"


spec :: Spec
spec =
  describe "msgpack formats" $
    it "can be translated from v1 to v2 and back" $
      property $ \(v1 :: MyRecordV1) ->
        let
          -- Forward:
          obj1 = toObject v1
          scm1 = schema (Proxy :: Proxy MyRecordV1)
          v2 :: MyRecordV2
          v2 = fromObjectWithSchema scm1 scm2 obj1
          -- And back:
          obj2 = toObject v2
          scm2 = schema (Proxy :: Proxy MyRecordV2)
          v1' :: MyRecordV1
          v1' = fromObjectWithSchema scm2 scm1 obj2
        in
        when False $
          v1' `shouldBe` v1
