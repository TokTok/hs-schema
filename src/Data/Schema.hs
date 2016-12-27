{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Schema where

import           Data.Proxy (Proxy)


data Field = Field
  { fieldName :: String
  , fieldType :: Type
  }
  deriving (Eq, Show, Read)

data Option = Option
  { optionName  :: String
  , optionValue :: Int
  }
  deriving (Eq, Show, Read)

data Schema
  = SchemaRecord [Field]
  | SchemaEnum [Option]
  | SchemaInt
  | SchemaDouble
  | SchemaString
  deriving (Eq, Show, Read)


data Type = Type
  { typeName   :: String
  , typeSchema :: Schema
  }
  deriving (Eq, Show, Read)


class HasSchema a where
  schema :: Proxy a -> Type


instance HasSchema Schema where
  schema = error "TODO(robinlinden): define schema for Schema"


instance HasSchema Int where
  schema _ = Type "Int" SchemaInt

instance HasSchema Double where
  schema _ = Type "Double" SchemaDouble

instance HasSchema String where
  schema _ = Type "String" SchemaString
