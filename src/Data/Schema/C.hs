module Data.Schema.C where

import           Control.Arrow                ((&&&))
import qualified Data.Char                    as Char
import           Data.Fix                     (Fix (..), foldFix)
import           Data.List.Split              as List
import           Data.Schema                  (Schema, SchemaF (..), Type (..))
import           Prelude                      hiding ((<$>))
import qualified Text.Casing                  as Casing
import           Text.PrettyPrint.ANSI.Leijen

genC :: Schema -> Doc
genC (Fix (Sum (Just (mName, dName)) cons)) =
    vsep
    . punctuate line
    . map (uncurry genDatatype)
    . map (flattenFields ns prefix)
    $ cons
  where
    ns = namespaceFor (List.splitOn "." mName)
    prefix = dName <> "_"
genC s = error $ show s

namespaceFor :: [String] -> String
namespaceFor []               = ""
namespaceFor [ns, "Types", _] = ns <> "_"
namespaceFor (_:parts)        = namespaceFor parts

data StructField = StructField
    { sfField :: Doc
    , sfType  :: Type
    }
    deriving (Show)

data StructName = StructName
    { snType :: Doc
    , snFun  :: Doc
    }
    deriving (Show)

flattenFields :: String -> String -> Schema -> (StructName, [StructField])
flattenFields ns prefix = (conName . unFix &&& foldFix go)
  where
    go (Atom ty)               = [StructField empty ty]
    go (Prod (Just (_, ty)) _) = [StructField empty $ tyName ns ty]
    go (Sum (Just (_, ty)) _)  = [StructField empty $ tyName ns ty]

    go (Field name tys)        = map (\ty -> ty{sfField = fieldName name}) tys
    go (Con _ tys)             = tys
    go (Prod Nothing fields)   = concat fields
    go (Sum Nothing _)         = []
    go Empty                   = []

    conName (Con name _) = StructName
        { snType = text $ toCName name
        , snFun = text $ map Char.toLower $ toCName name <> "_"
        }
    conName _ = StructName empty empty

    -- | @TypeName -> Namespace_Prefix_Type_Name@
    toCName :: String -> String
    toCName name = ns <> prefix <> toCamelSnake name


genDatatype :: StructName -> [StructField] -> Doc
genDatatype sName fields =
    text "// Event?" <$>
    genTypedef sName <$>
    genStruct sName fields <$> line <>
    genGetters sName fields

genTypedef :: StructName -> Doc
genTypedef (StructName sName _) =
    text "typedef struct" <+> sName <+> sName <> semi

-- | Generate the C struct definition.
genStruct :: StructName -> [StructField] -> Doc
genStruct (StructName sName _) fields =
    text "struct" <+> sName <+> braces (line <>
        indent 4 (vcat (map ((<> semi) . go) fields))
        <> line) <> semi
  where
    go (StructField f TyBool)     = text "bool"     <+> f
    go (StructField f TyWord8)    = text "uint8_t"  <+> f
    go (StructField f TyWord16)   = text "uint16_t" <+> f
    go (StructField f TyWord32)   = text "uint32_t" <+> f
    go (StructField f TyWord64)   = text "uint64_t" <+> f
    go (StructField f (TyName s)) = text s <+> f
    go (StructField f (TyFixedBin s)) =
        text "uint8_t" <+> f <> (brackets . text . show $ s)
    go (StructField f TyBin) =
        text "uint8_t *" <> f <> semi <$>
        text "uint32_t " <> f <> text "_size"

genGetters :: StructName -> [StructField] -> Doc
genGetters (StructName sName fName) fields =
    vcat (map ((<> semi) . go) fields)
  where
    getter ty f = text ty <> fName <> text "get_" <> f <> eventParam (text "const ")
    eventParam qual = parens (line <> indent 4 (qual <> sName <+> text "*event"))

    go (StructField f TyBool)       = getter "bool " f
    go (StructField f TyWord8)      = getter "uint8_t " f
    go (StructField f TyWord16)     = getter "uint16_t " f
    go (StructField f TyWord32)     = getter "uint32_t " f
    go (StructField f TyWord64)     = getter "uint64_t " f
    go (StructField f (TyName s))   = getter (s <> " ") f
    go (StructField f TyFixedBin{}) = getter "const uint8_t *" f
    go (StructField f TyBin) =
        getter "const uint8_t *" f <> semi <$>
        getter "uint32_t " (f <> text "_size")

-- | @fieldName' -> field_name@
fieldName :: String -> Doc
fieldName = text . filter (/= '\'') . Casing.toQuietSnake . Casing.fromHumps

tyName :: String -> String -> Type
tyName ns = TyName . (ns <>) . toCamelSnake

-- | @TypeName -> Type_Name@
toCamelSnake :: String -> String
toCamelSnake = Casing.toSnake . Casing.fromHumps
