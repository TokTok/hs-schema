{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
module Data.Schema.Type
    ( SchemaF (..)
    , Schema
    , Type (..)
    , sumType
    , prodType
    ) where

import           Control.Applicative          ((<|>))
import           Data.Fix                     (Fix (..))
import           Data.Functor.Classes         (Eq1, Read1, Show1)
import           Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import           GHC.Generics                 (Generic, Generic1)


data Type
    = TyBool
    | TyWord8
    | TyWord16
    | TyWord32
    | TyWord64
    | TyBin
    | TyFixedBin Int
    | TyName String
    deriving (Show, Read, Eq)

type DatatypeName = (String, String)

data SchemaF a
    = Empty
    | Atom Type
    | List a

    | Prod [a]
    | Field String a

    | Sum (Maybe DatatypeName) [a]
    | Con String a

    | Module String [a]
    | Schema [a]
    deriving (Show, Read, Eq, Generic, Generic1, Functor, Foldable, Traversable)
    deriving (Show1, Read1, Eq1) via FunctorClassesDefault SchemaF

type Schema = Fix SchemaF

instance Semigroup Schema where
    (<>) = sumType

sumType :: Schema -> Schema -> Schema
sumType (Fix (Sum da a)) (Fix (Sum db b)) = Fix $ Sum (da <|> db) (a ++ b)
sumType (Fix (Sum da a)) b                = Fix $ Sum da (a ++ [b])
sumType a (Fix (Sum db b))                = Fix $ Sum db (a : b)
sumType a b                               = Fix $ Sum Nothing [a, b]

prodType :: Schema -> Schema -> Schema
prodType (Fix (Prod a)) (Fix (Prod b)) = Fix $ Prod (a ++ b)
prodType (Fix (Prod a)) b              = Fix $ Prod (a ++ [b])
prodType a (Fix (Prod b))              = Fix $ Prod (a : b)
prodType a b                           = Fix $ Prod [a, b]
