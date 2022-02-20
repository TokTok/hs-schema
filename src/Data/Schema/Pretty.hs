module Data.Schema.Pretty (ppSchema) where

import           Data.Fix                     (foldFix)
import           Data.Schema                  (Schema, SchemaF (..))
import           Prelude                      hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

ppSchema :: Schema -> Doc
ppSchema = foldFix go

go :: SchemaF Doc -> Doc
go (Atom ty)                    = text $ show ty
go (Field name ty)              = text name <+> text "::" <+> ty
go (Con name ty)                = text name <+> char '(' <> ty <> char ')'
go (Prod Nothing fields)        = char '{' <$> indent 2 (vcat fields) <$> char '}'
go (Prod (Just (m, ty)) fields) = text m <> char '.' <> text ty <+> text "= {" <$> indent 2 (vcat fields) <$> char '}'
go (Sum Nothing cons)           = vcat cons
go (Sum (Just (m, ty)) cons)    = text m <> char '.' <> text ty <$> indent 2 (vcat cons)
go Empty                        = text "<empty>"
