module Data.Schema.Pretty (ppSchema) where

import           Data.Fix                     (foldFix)
import           Data.Schema                  (Schema, SchemaF (..))
import           Prelude                      hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

ppSchema :: Schema -> Doc
ppSchema = (<> line) . foldFix go

go :: SchemaF Doc -> Doc
go (Atom ty)                 = text $ show ty
go (Field name ty)           = text name <+> text "::" <+> ty
go (List ty)                 = brackets ty
go (Con name ty)             = text name <+> equals <+> ty
go (Prod fields)             = braces (line <> indent 2 (vcat fields) <> line)
go (Sum Nothing cons)        = vcat cons
go (Sum (Just (m, ty)) cons) =
    text ("type " <> m <> "." <> ty) <+> braces (
        line <> indent 2 (vcat cons)
        <> line)
go (Module name ss)          = text "module" <+> text name <$> vcat ss
go (Schema mods)             = text "schema" <$> vcat mods
go Empty                     = text "<empty>"
