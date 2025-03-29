-- | A bootstrapping DSL, used for Hydra's inner core models

module Hydra.Dsl.Bootstrap where

import Hydra.Compute
import Hydra.Constants
import Hydra.Core
import Hydra.CoreEncoding
import Hydra.Graph
import Hydra.Lexical
import Hydra.Staging.Annotations
import Hydra.Module
import Hydra.Rewriting
import Hydra.Staging.Rewriting
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Tools.Debug

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


-- | An empty graph (no elements, no schema, but with primitive functions) which is used for bootstrapping Hydra Core
bootstrapGraph :: Graph
bootstrapGraph = emptyGraph {graphPrimitives = M.fromList $ fmap (\p -> (primitiveName p, p)) (L.concat (libraryPrimitives <$> standardLibraries))}

datatype :: Namespace -> String -> Type -> Element
datatype gname lname typ = typeElement elName $ rewriteType replacePlaceholders typ
  where
    elName = qualify gname (Name lname)

    -- Note: placeholders are only expected at the top level, or beneath annotations and/or type lambdas
    replacePlaceholders rec t = case rect of
        TypeRecord (RowType tname fields) -> if tname == placeholderName
          then TypeRecord (RowType elName fields)
          else rect
        TypeUnion (RowType tname fields) -> if tname == placeholderName
          then TypeUnion (RowType elName fields)
          else rect
        TypeWrap (WrappedType tname t) -> if tname == placeholderName
          then TypeWrap (WrappedType elName t)
          else rect
        _ -> rect
      where
        rect = rec t

qualify :: Namespace -> Name -> Name
qualify (Namespace gname) (Name lname) = Name $ gname ++ "." ++ lname

-- TODO: provide a TypeScheme, not just a type, and there is no need to term-encode the type.
typeElement :: Name -> Type -> Element
typeElement name typ = Element {
    elementName = name,
    elementTerm = dataTerm,
    elementType = Just $ TypeScheme [] typ}
  where
    -- These type annotations allow type inference to proceed despite cyclic type definitions, e.g. in Hydra Core
    dataTerm = normalizeTermAnnotations $ TermAnnotated $ AnnotatedTerm (coreEncodeType typ) $ M.fromList [(key_type, schemaTerm)]
    schemaTerm = TermVariable _Type

typeref :: Namespace -> String -> Type
typeref ns = TypeVariable . qualify ns . Name
