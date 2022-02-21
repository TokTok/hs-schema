{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TypeOperators       #-}
module Data.Schema.Builder
    ( Builder (..)
    , ToSchema (..)
    , atom
    ) where

import           Control.Applicative (liftA2)
import           Data.Fix            (Fix (..))
import           Data.Schema.Type    (Schema, SchemaF (..), Type (..), prodType)
import           GHC.Generics


data Builder a = Builder
    { zero      :: a
    , getSchema :: Schema
    }
    deriving (Read, Show)

atom :: Type -> a -> Builder a
atom ty z = Builder z (Fix (Atom ty))

instance Semigroup (Builder a) where
    Builder x l <> Builder _ r = Builder x (l <> r)
instance Functor Builder where
    fmap f (Builder x s) = Builder (f x) s
instance Applicative Builder where
    pure x = Builder x (Fix Empty)
    Builder f l <*> Builder x r = Builder (f x) (prodType l r)


class GToSchema f where
    gToSchema :: Builder (f a)


class ToSchema a where
    toSchema :: Builder a

    default toSchema :: (Generic a, GToSchema (Rep a)) => Builder a
    toSchema = to <$> gToSchema

instance ToSchema a => ToSchema [a] where
    toSchema = Builder [v] (Fix (List s))
      where
        Builder v s = toSchema

instance GToSchema U1 where
    gToSchema = Builder U1 (Fix Empty)

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
        addDatatypeName mName dName (Builder x (Fix (Sum Nothing cs))) =
            Builder x . Fix . Sum (Just (mName, dName)) $ cs
        addDatatypeName _ _ s = s

instance (Constructor c, GToSchema f) => GToSchema (C1 c f) where
    gToSchema = M1 <$> addConName (conName (undefined :: C1 c f a)) gToSchema
      where
        addConName :: String -> Builder a -> Builder a
        addConName name (Builder x (Fix Empty)) =
            Builder x . Fix . Atom . TyName $ name
        addConName name (Builder x s) =
            Builder x . Fix . Sum Nothing . (:[]) . Fix . Con name $ s

instance (Selector s, GToSchema f) => GToSchema (S1 s f) where
    gToSchema = M1 <$> addSelName (selName (undefined :: S1 s f a)) gToSchema
      where
        addSelName :: String -> Builder a -> Builder a
        addSelName "" b               = b
        addSelName name (Builder x s) = Builder x . Fix . Field name $ s

instance GToSchema a => GToSchema (M1 t c a) where
    gToSchema = M1 <$> gToSchema

instance ToSchema a => GToSchema (K1 i a) where
    gToSchema = K1 <$> toSchema
