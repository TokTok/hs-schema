{-# LANGUAGE LambdaCase #-}
module Data.Schema.C (genC) where

import           Control.Arrow   ((&&&))
import qualified Data.Char       as Char
import           Data.Fix        (Fix (..), foldFix)
import           Data.List.Split as List
import           Data.Schema     (Schema, SchemaF (..), Type (..))
import           Prettyprinter
import qualified Text.Casing     as Casing

genC :: Schema -> Doc ()
genC (Fix (Sum (Just (mName, dName)) cons)) =
    vsep
    . punctuate line
    . map (uncurry genDatatype . flattenFields ns prefix)
    $ cons
  where
    ns = namespaceFor . List.splitOn "." $ mName
    prefix = toCamelSnake dName
genC s = error $ show s

namespaceFor :: [String] -> String
namespaceFor []               = ""
namespaceFor [ns, "Types", _] = ns <> "_"
namespaceFor (_:parts)        = namespaceFor parts

data StructField = StructField
    { sfField :: Doc ()
    , sfType  :: Type
    }
    deriving (Show)

data StructName = StructName
    { snType :: Doc ()
    , snFun  :: Doc ()
    }
    deriving (Show)

flattenFields :: String -> String -> Schema -> (StructName, [StructField])
flattenFields ns prefix = conName . unFix &&& foldFix go
  where
    go (Atom ty)              = [StructField mempty ty]
    go (Sum (Just (_, ty)) _) = [StructField mempty $ tyName ns ty]

    go (Field name tys)       = map (\ty -> ty{sfField = fieldName name}) tys
    go (Con _ tys)            = tys
    go (Prod fields)          = concat fields
    go (Sum Nothing _)        = []
    go Empty                  = []
    go Module{}               = []
    go Schema{}               = []
    go List{}                 = []

    conName (Con name _) = StructName
        { snType = pretty $ toCName name
        , snFun = pretty $ map Char.toLower $ toCName name <> "_"
        }
    conName _ = StructName mempty mempty

    -- | @TypeName -> Namespace_Prefix_Type_Name@
    toCName :: String -> String
    toCName name =
        let cName = toCamelSnake name in
        if prefix == cName
            then ns <>           cName
            else ns <> prefix <> cName

genDatatype :: StructName -> [StructField] -> Doc ()
genDatatype sName fields =
    genHeader sName fields <$$>
    genSource sName fields

genHeader :: StructName -> [StructField] -> Doc ()
genHeader sName fields =
    pretty "// events.h" <$$>
    genTypedef sName <$$>
    genGetterDecls sName fields

genSource :: StructName -> [StructField] -> Doc ()
genSource sName fields =
    pretty "// events.c" <$$>
    genStruct sName fields <$$>
    (vcat . punctuate line . concatMap (\field ->
        genGetterDefn sName field ++
        [genSetterDefn sName field]) $ fields)

genTypedef :: StructName -> Doc ()
genTypedef (StructName sName _) =
    pretty "typedef struct" <+> sName <+> sName <> semi

-- | Generate the C struct definition.
genStruct :: StructName -> [StructField] -> Doc ()
genStruct (StructName sName _) fields =
    pretty "struct" <+> sName <+> braces (line <>
        indent 4 (vsep (map (<> semi) $ concatMap go fields))
        <> line) <> semi
  where
    go (StructField f TyBool)     = [pretty "bool"     <+> f]
    go (StructField f TyWord8)    = [pretty "uint8_t"  <+> f]
    go (StructField f TyWord16)   = [pretty "uint16_t" <+> f]
    go (StructField f TyWord32)   = [pretty "uint32_t" <+> f]
    go (StructField f TyWord64)   = [pretty "uint64_t" <+> f]
    go (StructField f (TyName s)) = [pretty s <+> f]
    go (StructField f (TyFixedBin s)) =
        [pretty "uint8_t" <+> f <> brackets (pretty s)]
    go (StructField f TyBin) =
        [ pretty "uint8_t *" <> f
        , pretty "uint32_t " <> f <> pretty "_size"
        ]

eventParam :: Doc () -> Doc () -> Doc () -> Doc ()
eventParam qual sName params =
    parens (line <> indent 4 (qual <> sName <+> pretty "*event" <> params))

getterDecl :: Doc () -> Doc () -> String -> Doc () -> Doc ()
getterDecl sName fName ty f =
    pretty ty <> fName <> pretty "get_" <> f <> eventParam (pretty "const ") sName mempty

setterDecl :: Doc () -> Doc () -> String -> Doc () -> Doc () -> Doc ()
setterDecl sName fName ty f params =
    pretty "static void" <+> fName <> pretty "set_" <> f <> eventParam mempty sName (pretty (", " <> ty <> "value") <> params)

getterDeclFor :: (String -> Doc () -> Doc ()) -> StructField -> [Doc ()]
getterDeclFor decl = \case
    (StructField f TyBool)       -> [decl "bool "     f]
    (StructField f TyWord8)      -> [decl "uint8_t "  f]
    (StructField f TyWord16)     -> [decl "uint16_t " f]
    (StructField f TyWord32)     -> [decl "uint32_t " f]
    (StructField f TyWord64)     -> [decl "uint64_t " f]
    (StructField f (TyName s))   -> [decl (s <> " ")  f]
    (StructField f TyFixedBin{}) -> [decl "const uint8_t *" f]
    (StructField f TyBin) ->
        [ decl "const uint8_t *" f
        , decl "uint32_t " (f <> pretty "_size")
        ]

genGetterDecls :: StructName -> [StructField] -> Doc ()
genGetterDecls (StructName sName fName) fields =
    vcat (map (<> semi) $ concatMap (getterDeclFor decl) fields)
  where
    decl = getterDecl sName fName

genGetterDefn :: StructName -> StructField -> [Doc ()]
genGetterDefn (StructName sName fName) = getterDeclFor defn
  where
    defn ty f = getterDecl sName fName ty f <+> braces
        (line <> indent 4 (pretty "return event->" <> f <> semi) <> line)

genSetterDefn :: StructName -> StructField -> Doc ()
genSetterDefn (StructName sName fName) = go
  where
    defn params body ty field = setterDecl sName fName ty field params <+> braces
        (line <> indent 4 (body field) <> line)

    setValue field = pretty "event->" <> field <> pretty " = value" <> semi
    simpleDefn = defn mempty setValue

    setFixedBin n field =
        pretty "memcpy" <> parens (pretty "event->" <> field <> pretty ", value, " <> pretty n) <> semi
    setBin field =
        pretty "assert(event != nullptr);" <$$>
        pretty "if (event->" <> field <> pretty " != nullptr)" <+> braces (line <>
            indent 4 (
                pretty "free(event->" <> field <> pretty ");" <$$>
                pretty "event->" <> field <> pretty " = nullptr;" <$$>
                pretty "event->" <> field <> pretty "_size = 0;"
            ) <> line) <$$>
        pretty "event->" <> field <> pretty " = (uint8_t *)malloc(size);" <$$>
        pretty "if (event->" <> field <> pretty " == nullptr)" <+> braces (line <>
            indent 4 (
                pretty "return false;"
            ) <> line) <$$>
        pretty "memcpy" <> parens (pretty "event->" <> field <> pretty ", value, size") <> semi <$$>
        pretty "event->" <> field <> pretty "_size = size;" <$$>
        pretty "return true;"

    go (StructField field TyBool)       = simpleDefn "bool "     field
    go (StructField field TyWord8)      = simpleDefn "uint8_t "  field
    go (StructField field TyWord16)     = simpleDefn "uint16_t " field
    go (StructField field TyWord32)     = simpleDefn "uint32_t " field
    go (StructField field TyWord64)     = simpleDefn "uint64_t " field
    go (StructField field (TyName s))   = simpleDefn (s <> " ")  field
    go (StructField field (TyFixedBin n)) =
        defn mempty (setFixedBin n) "const uint8_t *" field
    go (StructField field TyBin) =
        defn (pretty ", uint32_t size") setBin "const uint8_t *" field

-- | @fieldName' -> field_name@
fieldName :: String -> Doc ()
fieldName = pretty . filter (/= '\'') . Casing.toQuietSnake . Casing.fromHumps

tyName :: String -> String -> Type
tyName ns = TyName . (ns <>) . toCamelSnake

-- | @TypeName -> Type_Name@
toCamelSnake :: String -> String
toCamelSnake = Casing.toSnake . Casing.fromHumps

infixr 5 <$$>
(<$$>) :: Doc () -> Doc () -> Doc ()
x <$$> y = x <> line <> y
