module Data.Schema.Pretty (ppSchema) where

import           Data.Fix      (foldFix)
import           Data.Schema   (Schema, SchemaF (..))
import           Prettyprinter

ppSchema :: Schema -> Doc ()
ppSchema = (<> line) . foldFix go

go :: SchemaF (Doc ()) -> Doc ()
go (Atom ty)                 = pretty $ show ty
go (Field name ty)           = pretty name <+> pretty "::" <+> ty
go (List ty)                 = pretty "repeated" <+> ty
go (Con name ty)             = pretty name <+> equals <+> ty
go (Prod fields)             = braces (line <> indent 2 (vcat fields) <> line)
go (Sum Nothing cons)        = vcat cons
go (Sum (Just (m, ty)) cons) =
    pretty ("type " <> m <> "." <> ty) <+> braces (
        line <> indent 2 (vcat cons)
        <> line)
go (Module name ss)          = vsep [pretty "module" <+> pretty name, vcat ss]
go (Schema mods)             = vsep [pretty "schema 1.0;", line <> vcat (punctuate line mods)]
go Empty                     = pretty "<empty>"
