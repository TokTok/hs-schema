{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Schema where

import           Data.Proxy (Proxy)


data Field = Field
  { fieldName :: String
  , fieldType :: Schema
  }
  deriving (Eq, Show, Read)

data Schema
  = SchemaRecord [Field]
  | SchemaInt
  | SchemaDouble
  | SchemaString
  deriving (Eq, Show, Read)


class HasSchema a where
  schema :: Proxy a -> Schema


instance HasSchema Int where
  schema _ = SchemaInt

instance HasSchema Double where
  schema _ = SchemaDouble

instance HasSchema String where
  schema _ = SchemaString
