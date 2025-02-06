-- | A bootstrapping DSL, used for Hydra's inner core models

module Hydra.Dsl.Bootstrap where

import Hydra.Compute
import Hydra.Constants
import Hydra.Core
import Hydra.CoreEncoding
import Hydra.Graph
import Hydra.Annotations
import Hydra.Module
import Hydra.Tools.Rewriting
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Tools.Debug

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


-- | An empty graph (no elements, no primitives, but an annotation class) which is used for bootstrapping Hydra Core
bootstrapGraph :: Graph
bootstrapGraph = Graph {
  graphElements = M.empty,
  graphEnvironment = M.empty,
  graphTypes = M.empty,
  graphBody = Terms.list [], -- Note: the bootstrap body is arbitrary
  graphPrimitives = M.fromList $ fmap (\p -> (primitiveName p, p)) (L.concat (libraryPrimitives <$> standardLibraries)),
  graphSchema = Nothing}

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

typeref :: Namespace -> String -> Type
typeref ns = TypeVariable . qualify ns . Name

qualify :: Namespace -> Name -> Name
qualify (Namespace gname) (Name lname) = Name $ gname ++ "." ++ lname

typeElement :: Name -> Type -> Element
typeElement name typ = Element {
    elementName = name,
    elementData = dataTerm}
  where
    -- These type annotations allow type inference to proceed despite cyclic type definitions, e.g. in Hydra Core
    dataTerm = normalizeTermAnnotations $ TermAnnotated $ AnnotatedTerm (coreEncodeType typ) $ M.fromList [(key_type, schemaTerm)]
    schemaTerm = TermVariable _Type
