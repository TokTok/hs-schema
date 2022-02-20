{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Data.Schema.Builder
    ( Builder (..)
    , ToSchema (..)
    , atom
    ) where

import           Control.Applicative (liftA2)
import           Data.Fix            (Fix (..))
import           Data.Schema.Type    (Schema, SchemaF (..), Type (..), prodType,
                                      sumType)
import           GHC.Generics


data Builder a = Builder
    { getSchema :: Schema
    , zero      :: a
    }
    deriving (Read, Show)

atom :: Type -> a -> Builder a
atom ty z = Builder (Fix (Atom ty)) z

instance Semigroup (Builder a) where
    Builder l x <> Builder r _ = Builder (sumType l r) x
instance Functor Builder where
    fmap f (Builder s x) = Builder s (f x)
instance Applicative Builder where
    pure = Builder (Fix Empty)
    Builder l f <*> Builder r x = Builder (prodType l r) (f x)


class GToSchema f where
    gToSchema :: Builder (f a)


class ToSchema a where
    toSchema :: Builder a

    default toSchema :: (Generic a, GToSchema (Rep a)) => Builder a
    toSchema = to <$> gToSchema

instance GToSchema U1 where
    gToSchema = Builder (Fix Empty) U1

instance (GToSchema a, GToSchema b) => GToSchema (a :*: b) where
    gToSchema = liftA2 (:*:) gToSchema gToSchema

instance (GToSchema a, GToSchema b) => GToSchema (a :+: b) where
    gToSchema = (L1 <$> gToSchema) <> (R1 <$> gToSchema)

instance (Datatype d, GToSchema f) => GToSchema (D1 d f) where
    gToSchema =
        M1 <$> addDatatypeName
            (moduleName (undefined :: D1 d f a))
            (datatypeName (undefined :: D1 d f a))
            gToSchema
      where
        addDatatypeName :: String -> String -> Builder a -> Builder a
        addDatatypeName mName dName (Builder (Fix (Sum Nothing cs)) x) = Builder (Fix (Sum (Just (mName, dName)) cs)) x
        addDatatypeName mName dName (Builder (Fix (Prod Nothing cs)) x) = Builder (Fix (Prod (Just (mName, dName)) cs)) x
        addDatatypeName _ _ s = s

instance (Constructor c, GToSchema f) => GToSchema (C1 c f) where
    gToSchema = M1 <$> addConName (conName (undefined :: C1 c f a)) gToSchema
      where
        addConName :: String -> Builder a -> Builder a
        addConName name (Builder (Fix Empty) x) = Builder (Fix (Atom (TyName name))) x
        addConName name (Builder s x)           = Builder (Fix (Con name s)) x

instance (Selector s, GToSchema f) => GToSchema (S1 s f) where
    gToSchema = M1 <$> addSelName (selName (undefined :: S1 s f a)) gToSchema
      where
        addSelName :: String -> Builder a -> Builder a
        addSelName name (Builder s x) = Builder (Fix (Field name s)) x

instance GToSchema a => GToSchema (M1 t c a) where
    gToSchema = M1 <$> gToSchema

instance ToSchema a => GToSchema (K1 i a) where
    gToSchema = K1 <$> toSchema
