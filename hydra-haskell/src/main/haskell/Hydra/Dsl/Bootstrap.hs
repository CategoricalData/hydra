-- | A bootstrapping DSL, used for Hydra's inner core models

module Hydra.Dsl.Bootstrap where

import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M
import qualified Data.Set as S


datatype :: Namespace -> String -> Type a -> Element a
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

-- | An empty graph (no elements, no primitives, but an annotation class) which is used for bootstrapping Hydra Core
bootstrapGraph :: Graph Kv
bootstrapGraph = Graph {
  graphElements = M.empty,
  graphEnvironment = M.empty,
  graphBody = Terms.list [], -- Note: the bootstrap body is arbitrary 
  graphPrimitives = M.fromList $ fmap (\p -> (primitiveName p, p)) standardPrimitives,
  graphAnnotations = kvAnnotationClass,
  graphSchema = Nothing}

nsref :: Namespace -> String -> Type a
nsref ns = Types.wrap . qualify ns . Name

qualify :: Namespace -> Name -> Name
qualify (Namespace gname) (Name lname) = Name $ gname ++ "." ++ lname

termElement :: Name -> Type a -> Term a -> Element a
termElement name typ term = Element {
  elementName = name,
  elementSchema = epsilonEncodeType typ,
  elementData = term}

typeElement :: Name -> Type a -> Element a
typeElement name typ = Element {
  elementName = name,
  elementSchema = TermElement _Type,
  elementData = epsilonEncodeType typ}
