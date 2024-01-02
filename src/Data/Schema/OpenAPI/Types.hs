{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.Schema.OpenAPI.Types where

import           Control.Applicative ((<|>))
import           Data.Aeson          (FromJSON (..), ToJSON (toJSON),
                                      Value (Array, Object, String), object,
                                      (.:), (.:?), (.=))
import qualified Data.Aeson.Key      as Key
import           Data.Aeson.KeyMap   (KeyMap)
import qualified Data.Aeson.KeyMap   as KeyMap
import           Data.Aeson.TH       (Options (..), defaultOptions, deriveJSON)
import           Data.Aeson.Types    (Pair, Parser, Value (Bool, Null, Number),
                                      parseEither, parseFail)
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe          (catMaybes)
import           Data.Scientific     (Scientific)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Vector         as V
import           Data.Void           (Void)
import           Text.Casing         (camel)

data Tag = Tag
    { tagName        :: Text
    , tagDescription :: Text
    }
    deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (Text.length "Tag")} ''Tag)

data Variable = Variable
    { variableDescription :: Text
    , variableDefault     :: Text
    }
    deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (Text.length "Variable")} ''Variable)

data Server = Server
    { serverUrl       :: Text
    , serverVariables :: Maybe (HashMap Text Variable)
    }
    deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (Text.length "Server")} ''Server)

data Url = Url
    { urlDescription :: Text
    , urlUrl         :: Text
    }
    deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (Text.length "Url")} ''Url)

data NameUrl = NameUrl
    { nameUrlName :: Text
    , nameUrlUrl  :: Text
    }
    deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (Text.length "NameUrl")} ''NameUrl)

data Info = Info
    { infoVersion        :: Text
    , infoTitle          :: Text
    , infoDescription    :: Text
    , infoLicense        :: NameUrl
    , infoTermsOfService :: Text
    , infoContact        :: NameUrl
    }
    deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (Text.length "Info")} ''Info)

data SchemaType
    = SchemaTypeEmpty SchemaEmpty
    | SchemaTypeObject SchemaObject
    | SchemaTypeArray SchemaArray
    | SchemaTypeString SchemaString
    | SchemaTypeBoolean SchemaBoolean
    | SchemaTypeInteger (SchemaNumber Int)
    | SchemaTypeNumber (SchemaNumber Scientific)
    | SchemaTypeAllOf SchemaQuantifier
    | SchemaTypeOneOf SchemaQuantifier
    | SchemaTypeAnyOf SchemaQuantifier
    deriving (Show)

parseSingleSchema :: KeyMap.KeyMap Value -> Value -> Parser SchemaType
parseSingleSchema mems = \case
    String "array" -> fmap SchemaTypeArray $
        SchemaArray
            <$> decodeCommon mems
            <*> mems .: "items"
            <*> mems .:? "minItems"
            <*> mems .:? "maxItems"
            <*> mems .:? "required"
    String "object" -> fmap SchemaTypeObject $
        SchemaObject
            <$> decodeCommon mems
            <*> mems .:? "properties"
            <*> mems .:? "additionalProperties"
            <*> mems .:? "maxProperties"
            <*> mems .:? "required"
            <*> mems .:? "oneOf"
            <*> mems .:? "allOf"
            <*> mems .:? "anyOf"
    String "string" -> fmap SchemaTypeString $
        SchemaString
            <$> (SchemaCommon
                <$> mems .:? "description"
                <*> mems .:? "title"
                <*> (mems .:? "default" >>= traverse toString)  -- sometimes a bool is used as default for string
                <*> mems .:? "nullable"
                <*> mems .:? "deprecated"
                <*> mems .:? "example"
                <*> mems .:? "readOnly")
            <*> (fmap catMaybes <$> mems .:? "enum")  -- we're skipping null and rely on nullable
            <*> mems .:? "format"
            <*> mems .:? "pattern"
            <*> mems .:? "minLength"
            <*> mems .:? "maxLength"
            <*> mems .:? "additionalProperties"
            <*> mems .:? "minItems"
    String "boolean" -> fmap SchemaTypeBoolean $
        SchemaBoolean
            <$> decodeCommon mems
    String "integer" -> fmap SchemaTypeInteger $
        SchemaNumber
            <$> decodeCommon mems
            <*> mems .:? "format"
            <*> mems .:? "minimum"
            <*> mems .:? "maximum"
    String "number" -> fmap SchemaTypeNumber $
        SchemaNumber
            <$> decodeCommon mems
            <*> mems .:? "format"
            <*> mems .:? "minimum"
            <*> mems .:? "maximum"
    Null -> fmap SchemaTypeEmpty $
        SchemaEmpty
            <$> mems .:? "nullable"
            <*> mems .:? "required"
    ty ->
        parseFail $ show ty

  where
    toString :: Value -> Parser Text
    toString (String str) = pure str
    toString (Bool True)  = pure "true"
    toString (Bool False) = pure "false"
    toString val          = parseFail $ "invalid String value: " <> show val

parseInferredSchema :: KeyMap Value -> Maybe (Parser SchemaType)
parseInferredSchema mems
    -- Try one more time. Sometimes, there's only "properties", but the type is missing.
    | KeyMap.member "properties" mems = Just $ parseSingleSchema mems (String "object")
    -- If there's "enum", maybe it's a string.
    | KeyMap.member "enum" mems = Just $ parseSingleSchema mems (String "string")
    | otherwise = Nothing


instance FromJSON SchemaType where
    parseJSON ob@(Object mems)
        | KeyMap.null mems = pure . SchemaTypeEmpty $ SchemaEmpty Nothing Nothing
        | otherwise =
            case (parseSingleSchema mems <$> KeyMap.lookup "type" mems)
                <|> parseInferredSchema mems
                <|> parseQuantifier mems of
                Just tys -> tys
                Nothing
                    | KeyMap.member "nullable" mems || KeyMap.member "required" mems ->
                        -- Type missing, nothing distinctive there, maybe empty?
                        parseSingleSchema mems Null
                    | otherwise ->
                        parseFail $ "invalid schema: " <> show ob
    parseJSON x = parseFail $ show x

instance ToJSON SchemaType where
    toJSON (SchemaTypeEmpty SchemaEmpty{..}) = object
        [ "nullable"                .= schemaEmptyNullable
        , "required"                .= schemaEmptyRequired
        ]
    toJSON (SchemaTypeObject SchemaObject{..}) =
        object $ encodeCommon schemaObjectCommon ++
        [ "type"                    .= String "object"
        , "properties"              .= schemaObjectProperties
        , "additionalProperties"    .= schemaObjectAdditionalProperties
        , "maxProperties"           .= schemaObjectMaxProperties
        , "required"                .= schemaObjectRequired
        , "oneOf"                   .= schemaObjectOneOf
        , "allOf"                   .= schemaObjectAllOf
        , "anyOf"                   .= schemaObjectAnyOf
        ]
    toJSON (SchemaTypeArray SchemaArray{..}) =
        object $ encodeCommon schemaArrayCommon ++
        [ "type"                    .= String "array"
        , "items"                   .= schemaArrayItems
        , "minItems"                .= schemaArrayMinItems
        , "maxItems"                .= schemaArrayMaxItems
        , "required"                .= schemaArrayRequired
        ]
    toJSON (SchemaTypeString SchemaString{..}) =
        object $ encodeCommon schemaStringCommon ++
        [ "type"                    .= String "string"
        , "enum"                    .= schemaStringEnum
        , "format"                  .= schemaStringFormat
        , "pattern"                 .= schemaStringPattern
        , "minLength"               .= schemaStringMinLength
        , "maxLength"               .= schemaStringMaxLength
        , "additionalProperties"    .= schemaStringAdditionalProperties
        , "minItems"                .= schemaStringMinItems
        ]
    toJSON (SchemaTypeBoolean SchemaBoolean{..}) =
        object $ encodeCommon schemaBooleanCommon ++
        [ "type"                    .= String "boolean"
        ]
    toJSON (SchemaTypeInteger SchemaNumber{..}) =
        object $ encodeCommon schemaNumberCommon ++
        [ "type"                    .= String "integer"
        , "format"                  .= schemaNumberFormat
        , "maximum"                 .= schemaNumberMaximum
        , "minimum"                 .= schemaNumberMinimum
        ]
    toJSON (SchemaTypeNumber SchemaNumber{..}) =
        object $ encodeCommon schemaNumberCommon ++
        [ "type"                    .= String "number"
        , "format"                  .= schemaNumberFormat
        , "maximum"                 .= schemaNumberMaximum
        , "minimum"                 .= schemaNumberMinimum
        ]
    toJSON (SchemaTypeAllOf SchemaQuantifier{..}) =
        object $ encodeCommon schemaQuantifierCommon ++
        [ "allOf"                   .= schemaQuantifierSchemas
        ]
    toJSON (SchemaTypeAnyOf SchemaQuantifier{..}) =
        object $ encodeCommon schemaQuantifierCommon ++
        [ "anyOf"                   .= schemaQuantifierSchemas
        ]
    toJSON (SchemaTypeOneOf SchemaQuantifier{..}) =
        object $ encodeCommon schemaQuantifierCommon ++
        [ "oneOf"                   .= schemaQuantifierSchemas
        ]

data SchemaCommon a example = SchemaCommon
    { schemaCommonDescription :: Maybe Text
    , schemaCommonTitle       :: Maybe Text
    , schemaCommonDefault     :: Maybe a
    , schemaCommonNullable    :: Maybe Bool
    , schemaCommonDeprecated  :: Maybe Bool
    , schemaCommonExample     :: Maybe example
    , schemaCommonReadOnly    :: Maybe Bool
    }
    deriving (Show)

encodeCommon :: (ToJSON a, ToJSON example) => SchemaCommon a example -> [Pair]
encodeCommon SchemaCommon{..} =
    [ "description" .= schemaCommonDescription
    , "title"       .= schemaCommonTitle
    , "default"     .= schemaCommonDefault
    , "nullable"    .= schemaCommonNullable
    , "deprecated"  .= schemaCommonDeprecated
    , "example"     .= schemaCommonExample
    , "readOnly"    .= schemaCommonReadOnly
    ]

decodeCommon :: (FromJSON a, FromJSON example) => KeyMap Value -> Parser (SchemaCommon a example)
decodeCommon mems =
    SchemaCommon
        <$> mems .:? "description"
        <*> mems .:? "title"
        <*> mems .:? "default"
        <*> mems .:? "nullable"
        <*> mems .:? "deprecated"
        <*> mems .:? "example"
        <*> mems .:? "readOnly"

data SchemaEmpty = SchemaEmpty
    { schemaEmptyNullable :: Maybe Bool
    , schemaEmptyRequired :: Maybe [Text]
    }
    deriving (Show)

data SchemaQuantifier = SchemaQuantifier
    { schemaQuantifierCommon  :: SchemaCommon Void Void
    , schemaQuantifierSchemas :: [SchemaType]
    }
    deriving (Show)

parseQuantifier :: KeyMap Value -> Maybe (Parser SchemaType)
parseQuantifier mems =
    (fmap SchemaTypeAllOf . parseRest . parseJSON <$> KeyMap.lookup "allOf" mems)
    <|> (fmap SchemaTypeOneOf . parseRest . parseJSON <$> KeyMap.lookup "oneOf" mems)
    <|> (fmap SchemaTypeAnyOf . parseRest . parseJSON <$> KeyMap.lookup "anyOf" mems)
  where
    parseRest :: Parser [SchemaType] -> Parser SchemaQuantifier
    parseRest = (SchemaQuantifier <$> decodeCommon mems <*>)

data AdditionalProperties
    = AdditionalPropertiesBool Bool
    | AdditionalPropertiesSchema SchemaType
    deriving (Show)

instance FromJSON AdditionalProperties where
    parseJSON :: Value -> Parser AdditionalProperties
    parseJSON val =
        (AdditionalPropertiesBool <$> parseJSON val) <|>
        (AdditionalPropertiesSchema <$> parseJSON val)

instance ToJSON AdditionalProperties where
    toJSON (AdditionalPropertiesBool str)   = toJSON str
    toJSON (AdditionalPropertiesSchema arr) = toJSON arr

data SchemaObject = SchemaObject
    { schemaObjectCommon               :: SchemaCommon Void Value
    , schemaObjectProperties           :: Maybe (HashMap Text SchemaType)
    , schemaObjectAdditionalProperties :: Maybe AdditionalProperties
    , schemaObjectMaxProperties        :: Maybe Int
    , schemaObjectRequired             :: Maybe [Text]
    , schemaObjectOneOf                :: Maybe [SchemaType]
    , schemaObjectAllOf                :: Maybe [SchemaType]
    , schemaObjectAnyOf                :: Maybe [SchemaType]
    }
    deriving (Show)

data SchemaArray = SchemaArray
    { schemaArrayCommon   :: SchemaCommon [Value] Value -- should be [Value], but ghes has many bugs
    , schemaArrayItems    :: SchemaType
    , schemaArrayMinItems :: Maybe Int
    , schemaArrayMaxItems :: Maybe Int
    , schemaArrayRequired :: Maybe [Text]
    }
    deriving (Show)

data StringExample
    = StringExampleString Text
    | StringExampleArray [Text]
    deriving (Show)

instance FromJSON StringExample where
    parseJSON val =
        (StringExampleString <$> parseJSON val) <|>
        (StringExampleArray <$> parseJSON val)

instance ToJSON StringExample where
    toJSON (StringExampleString str) = toJSON str
    toJSON (StringExampleArray arr)  = toJSON arr

data SchemaString = SchemaString
    { schemaStringCommon               :: SchemaCommon Text StringExample
    , schemaStringEnum                 :: Maybe [Text]
    , schemaStringFormat               :: Maybe Text
    , schemaStringPattern              :: Maybe Text
    , schemaStringMinLength            :: Maybe Int
    , schemaStringMaxLength            :: Maybe Int
    , schemaStringAdditionalProperties :: Maybe Bool
    , schemaStringMinItems             :: Maybe Int
    }
    deriving (Show)

newtype SchemaBoolean = SchemaBoolean
    { schemaBooleanCommon :: SchemaCommon Bool Bool
    }
    deriving (Show)

data SchemaNumber int = SchemaNumber
    { schemaNumberCommon  :: SchemaCommon int int
    , schemaNumberFormat  :: Maybe Text
    , schemaNumberMinimum :: Maybe int
    , schemaNumberMaximum :: Maybe int
    }
    deriving (Show)

data Content = Content
    { contentSchema   :: SchemaType
    , contentExamples :: Maybe (HashMap Text Value)
    }
    deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (Text.length "Content")} ''Content)

data Header = Header
    { headerExample :: Maybe Value
    , headerSchema  :: Maybe SchemaType
    }
    deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (Text.length "Header")} ''Header)

data Response = Response
    { responseDescription :: Maybe Text
    , responseContent     :: Maybe (HashMap Text Content)
    , responseHeaders     :: Maybe (HashMap Text Header)
    }
    deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (Text.length "Response")} ''Response)

data Request = Request
    { requestContent     :: HashMap Text Content
    , requestDescription :: Maybe Text
    , requestRequired    :: Maybe Bool
    }
    deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (Text.length "Request")} ''Request)

data Parameter = Parameter
    { parameterDescription :: Maybe Text
    , parameterIn          :: Text
    , parameterName        :: Text
    , parameterExample     :: Maybe Value
    , parameterExamples    :: Maybe (HashMap Text Value)
    , parameterSchema      :: SchemaType
    , parameterRequired    :: Maybe Bool
    , parameterDeprecated  :: Maybe Bool
    }
    deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (Text.length "Parameter")} ''Parameter)

data Method = Method
    { methodSummary      :: Text
    , methodServers      :: Maybe [Server]
    , methodDescription  :: Maybe Text
    , methodDeprecated   :: Maybe Bool
    , methodExternalDocs :: Maybe Url
    , methodTags         :: [Text]
    , methodOperationId  :: Text
    , methodParameters   :: Maybe [Parameter]
    , methodResponses    :: HashMap Text Response
    , methodRequestBody  :: Maybe Request
    }
    deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (Text.length "Method")} ''Method)

data Path = Path
    { pathGet    :: Maybe Method
    , pathPost   :: Maybe Method
    , pathDelete :: Maybe Method
    , pathPatch  :: Maybe Method
    , pathPut    :: Maybe Method
    }
    deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (Text.length "Path")} ''Path)

data Spec = Spec
    { specOpenapi      :: Text
    , specInfo         :: Info
    , specTags         :: [Tag]
    , specServers      :: [Server]
    , specExternalDocs :: Url
    , specPaths        :: HashMap Text Path
    }
    deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (Text.length "Spec")} ''Spec)

resolveRef :: Text -> Value -> Value
resolveRef ref root = go (Text.splitOn "/" ref) root
  where
    go :: [Text] -> Value -> Value
    go [] val = resolveRefsWith root val
    go ("#":path) val = go path val
    go (key:path) (Object mems) =
        case KeyMap.lookup (Key.fromText key) mems of
            Nothing -> error $ show key
            Just ob -> go path ob
    go _ val = error $ show (ref, val)

resolveRefsWith :: Value -> Value -> Value
resolveRefsWith root = go
  where
    go (Object mems) =
        case KeyMap.lookup "$ref" mems of
            Just (String ref) -> resolveRef ref root
            _                 -> Object $ KeyMap.map go mems
    go (Array items) = Array $ V.map go items

    go x@String{} = x
    go x@Number{} = x
    go x@Bool{} = x
    go x@Null{} = x

resolveRefs :: Value -> Value
resolveRefs root =
    case resolveRefsWith root root of
        Object mems -> Object (KeyMap.filterWithKey (\k _ -> k `notElem` ["components", ""]) mems)
        resolved -> resolved

parseSpec :: Value -> Either String Spec
parseSpec = parseEither parseJSON

removeNulls :: ToJSON a => a -> Value
removeNulls = go . toJSON
  where
    go (Array  x) = Array . V.map go $ x
    go (Object x) = Object . KeyMap.map go . KeyMap.filterWithKey validPair $ x
    go         x  = x

    isEmpty Null      = True
    isEmpty (Array x) = null x
    isEmpty _         = False

    validPair k v = not (isEmpty v || "x-" `Text.isPrefixOf` Key.toText k)
