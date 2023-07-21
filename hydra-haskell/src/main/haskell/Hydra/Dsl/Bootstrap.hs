-- | A bootstrapping DSL, used for Hydra's inner core models

module Hydra.Dsl.Bootstrap where

import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Tools.Debug

import qualified Data.Map as M
import qualified Data.Set as S


-- | An empty graph (no elements, no primitives, but an annotation class) which is used for bootstrapping Hydra Core
bootstrapGraph :: Graph Kv
bootstrapGraph = Graph {
  graphElements = M.empty,
  graphEnvironment = M.empty,
  graphBody = Terms.list [], -- Note: the bootstrap body is arbitrary
  graphPrimitives = M.fromList $ fmap (\p -> (primitiveName p, p)) standardPrimitives,
  graphAnnotations = kvAnnotationClass,
  graphSchema = Nothing}

datatype :: Namespace -> String -> Type Kv -> Element Kv
datatype gname lname typ = typeElement elName $ rewriteType replacePlaceholders id typ
  where
    elName = qualify gname (Name lname)

    -- Note: placeholders are only expected at the top level, or beneath annotations and/or type lambdas
    replacePlaceholders rec t = case t' of
        TypeRecord (RowType n e fields) -> if n == placeholderName
          then TypeRecord (RowType elName e fields)
          else t'
        TypeUnion (RowType n e fields) -> if n == placeholderName
          then TypeUnion (RowType elName e fields)
          else t'
        _ -> t'
      where
        t' = rec t

typeref :: Namespace -> String -> Type a
typeref ns = TypeVariable . qualify ns . Name

qualify :: Namespace -> Name -> Name
qualify (Namespace gname) (Name lname) = Name $ gname ++ "." ++ lname

typeElement :: Name -> Type Kv -> Element Kv
typeElement name typ = Element {
    elementName = name,
    elementData = dataTerm}
  where
    -- These type annotations allow type inference to proceed despite cyclic type definitions, e.g. in Hydra Core
    dataTerm = normalizeTermAnnotations $ TermAnnotated $ Annotated (coreEncodeType typ) $ Kv $ M.fromList [(kvType, schemaTerm)]
    schemaTerm = TermVariable _Type
