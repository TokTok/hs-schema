{-# LANGUAGE LambdaCase #-}
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
    go Module{}                = []

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
    genHeader sName fields </> line <>
    genSource sName fields

genHeader :: StructName -> [StructField] -> Doc
genHeader sName fields =
    text "// events.h" </>
    genTypedef sName </>
    genGetterDecls sName fields

genSource :: StructName -> [StructField] -> Doc
genSource sName fields =
    text "// events.c" </>
    genStruct sName fields </> line <>
    (vcat . punctuate line . map (\field ->
        genGetterDefn sName field </>
        genSetterDefn sName field) $ fields)

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
        text "uint8_t" <+> f <> (brackets $ int s)
    go (StructField f TyBin) =
        text "uint8_t *" <> f <> semi </>
        text "uint32_t " <> f <> text "_size"

eventParam :: Doc -> Doc -> Doc -> Doc
eventParam qual sName params =
    parens (line <> indent 4 (qual <> sName <+> text "*event" <> params))

getterDecl :: Doc -> Doc -> String -> Doc -> Doc
getterDecl sName fName ty f =
    text ty <> fName <> text "get_" <> f <> eventParam (text "const ") sName empty

setterDecl :: Doc -> Doc -> String -> Doc -> Doc -> Doc
setterDecl sName fName ty f params =
    text "static void" <+> fName <> text "set_" <> f <> eventParam empty sName (text (", " <> ty <> "value") <> params)

getterDeclFor :: (String -> Doc -> Doc) -> StructField -> Doc
getterDeclFor decl = \case
    (StructField f TyBool)       -> decl "bool "     f
    (StructField f TyWord8)      -> decl "uint8_t "  f
    (StructField f TyWord16)     -> decl "uint16_t " f
    (StructField f TyWord32)     -> decl "uint32_t " f
    (StructField f TyWord64)     -> decl "uint64_t " f
    (StructField f (TyName s))   -> decl (s <> " ")  f
    (StructField f TyFixedBin{}) -> decl "const uint8_t *" f
    (StructField f TyBin) ->
        decl "const uint8_t *" f <> semi </>
        decl "uint32_t " (f <> text "_size")

genGetterDecls :: StructName -> [StructField] -> Doc
genGetterDecls (StructName sName fName) fields =
    vcat (map ((<> semi) . getterDeclFor decl) fields)
  where
    decl = getterDecl sName fName

genGetterDefn :: StructName -> StructField -> Doc
genGetterDefn (StructName sName fName) = getterDeclFor defn
  where
    defn ty f = getterDecl sName fName ty f </> braces
        (line <> indent 4 (text "return event->" <> f <> semi) <> line)

genSetterDefn :: StructName -> StructField -> Doc
genSetterDefn (StructName sName fName) = go
  where
    defn params body ty field = setterDecl sName fName ty field params </> braces
        (line <> indent 4 (body field) <> line)

    setValue field = text "event->" <> field <> text " = value" <> semi
    simpleDefn = defn empty setValue

    setFixedBin n field =
        text "memcpy" <> parens (text "event->" <> field <> text ", value, " <> int n) <> semi
    setBin field =
        text "assert(event != nullptr);" </>
        line </>
        text "if (event->" <> field <> text " != nullptr)" <+> braces (line <>
            indent 4 (
                text "free(event->" <> field <> text ");" </>
                text "event->" <> field <> text " = nullptr;" </>
                text "event->" <> field <> text "_size = 0;"
            ) <> line) </>
        line </>
        text "event->" <> field <> text " = (uint8_t *)malloc(size);" </>
        line <>
        text "if (event->" <> field <> text " == nullptr)" <+> braces (line <>
            indent 4 (
                text "return false;"
            ) <> line) </>
        line </>
        text "memcpy" <> parens (text "event->" <> field <> text ", value, size") <> semi </>
        text "event->" <> field <> text "_size = size;" </>
        text "return true;"

    go (StructField field TyBool)       = simpleDefn "bool "     field
    go (StructField field TyWord8)      = simpleDefn "uint8_t "  field
    go (StructField field TyWord16)     = simpleDefn "uint16_t " field
    go (StructField field TyWord32)     = simpleDefn "uint32_t " field
    go (StructField field TyWord64)     = simpleDefn "uint64_t " field
    go (StructField field (TyName s))   = simpleDefn (s <> " ")  field
    go (StructField field (TyFixedBin n)) =
        defn empty (setFixedBin n) "const uint8_t *" field
    go (StructField field TyBin) =
        defn (text ", uint32_t size") setBin "const uint8_t *" field

-- | @fieldName' -> field_name@
fieldName :: String -> Doc
fieldName = text . filter (/= '\'') . Casing.toQuietSnake . Casing.fromHumps

tyName :: String -> String -> Type
tyName ns = TyName . (ns <>) . toCamelSnake

-- | @TypeName -> Type_Name@
toCamelSnake :: String -> String
toCamelSnake = Casing.toSnake . Casing.fromHumps
