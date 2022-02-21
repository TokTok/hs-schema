module Data.Schema.Deinline (deinline) where

import           Control.Arrow (first)
import           Data.Fix      (Fix (..), foldFix)
import           Data.List     (nub)
import           Data.Schema   (Schema, SchemaF (..), Type (..))

deinline :: Schema -> Schema
deinline = fst . foldFix getTypes

getTypes :: SchemaF (Schema, [Schema]) -> (Schema, [Schema])
getTypes (Atom ty)                    = (Fix . Atom $ ty, [])
getTypes (Sum dt@(Just (_, ty)) cons) = (Fix . Atom . TyName $ ty, Fix (Sum dt (map fst cons)) : concatMap snd cons)
getTypes (Prod fields)                = (Fix . Prod . map fst $ fields, concatMap snd fields)
getTypes (Field name ty)              = first (Fix . Field name) ty
getTypes (Con name ty)                = first (Fix . Con name) ty
getTypes (List ty)                    = first (Fix . List) ty
getTypes (Schema mods)                = (Fix . Schema . nub . concatMap snd $ mods, [])
getTypes x                            = error $ "unhandled: " <> show x
