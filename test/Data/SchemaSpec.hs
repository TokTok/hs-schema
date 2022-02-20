{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.SchemaSpec where

import           GHC.Generics        (Generic)
import           Test.Hspec

import           Data.Schema
import           Data.Schema.Builder (atom)

instance ToSchema Int where
    toSchema = atom (TyName "int") 0
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
  }
  deriving (Eq, Show, Read, Generic)

instance ToSchema MyRecordV1

myRecordV1 :: Schema
myRecordV1 = getSchema (toSchema :: Builder MyRecordV1)


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

instance ToSchema MyRecordV2

myRecordV2 :: Schema
myRecordV2 = getSchema (toSchema :: Builder MyRecordV2)


spec :: Spec
spec = return ()
