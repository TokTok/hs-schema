module Data.Text.Casing
    ( camel
    , pascal
    ) where

import qualified Data.Text   as Text
import qualified Text.Casing as C

wrap :: (String -> String) -> Text.Text -> Text.Text
wrap f = Text.pack . f . Text.unpack

camel :: Text.Text -> Text.Text
camel = wrap C.camel

pascal :: Text.Text -> Text.Text
pascal = wrap C.pascal
