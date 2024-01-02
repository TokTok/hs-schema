{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wwarn #-}
module Data.Schema.OpenAPI.Haskell where

import qualified Data.HashMap.Strict       as HashMap
import           Data.Maybe                (fromMaybe, mapMaybe)
import           Data.Schema.OpenAPI.Types (Content (contentSchema),
                                            Method (methodRequestBody), Path,
                                            Request (requestContent),
                                            SchemaObject (SchemaObject, schemaObjectProperties, schemaObjectRequired),
                                            SchemaType (..), Spec (specPaths))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Casing          (pascal)

mkFieldType :: SchemaType -> Maybe Text
mkFieldType SchemaTypeObject{}  = Nothing  -- TODO
mkFieldType SchemaTypeString{}  = Just "String"
mkFieldType SchemaTypeBoolean{} = Just "Bool"
mkFieldType SchemaTypeInteger{} = Just "Int"
mkFieldType SchemaTypeNumber{}  = Just "Float"
mkFieldType x                   = error $ show x

mkField :: Text -> [Text] -> Text -> SchemaType -> Maybe Text
mkField prefix req name ty = do
    innerTy <- mkFieldType ty
    let fieldTy = if name `elem` req
        then innerTy
        else "Maybe " <> innerTy
    return $ prefix <> pascal name <> " :: " <> fieldTy

mkTypeName :: Text
mkTypeName = "data EditRepo = EditRepo"

mkType :: SchemaType -> Maybe Text
mkType (SchemaTypeObject SchemaObject{..}) = do
    props <- schemaObjectProperties
    let req = fromMaybe [] schemaObjectRequired
    let fields = mapMaybe (uncurry (mkField "editRepo" req)) . HashMap.toList $ props
    return $ mkTypeName
        <> "\n    { "
        <> Text.intercalate "\n    , " fields
        <> "\n    } deriving (Eq)"
        <> "\n$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop (length \"EditRepo\")} ''EditRepo)"
mkType ty = error $ show ty

toHaskell :: Spec -> Text -> (Path -> Maybe Method) -> Maybe Text
toHaskell spec pathName getMethod =
    HashMap.lookup pathName (specPaths spec)
        >>= getMethod
        >>= methodRequestBody
        >>= HashMap.lookup "application/json" . requestContent
        >>= mkType . contentSchema
