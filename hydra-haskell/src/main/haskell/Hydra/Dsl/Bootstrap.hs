-- | A bootstrapping DSL, used for Hydra's inner core models

module Hydra.Dsl.Bootstrap where

import Hydra.Kernel
import Hydra.Meta
import Hydra.CoreEncoding
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M
import qualified Data.Set as S


datatype :: Namespace -> String -> Type m -> Element m
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

bootstrapContext :: Context Meta
bootstrapContext = cx
  where
    cx = Context {
      contextGraph = Graph M.empty Nothing,
      contextFunctions = M.empty,
      contextStrategy = EvaluationStrategy S.empty,
      contextAnnotations = metaAnnotationClass}

nsref :: Namespace -> String -> Type m
nsref ns = Types.wrap . qualify ns . Name

qualify :: Namespace -> Name -> Name
qualify (Namespace gname) (Name lname) = Name $ gname ++ "." ++ lname

termElement :: Name -> Type m -> Term m -> Element m
termElement name typ term = Element {
  elementName = name,
  elementSchema = encodeType typ,
  elementData = term}

typeElement :: Name -> Type m -> Element m
typeElement name typ = Element {
  elementName = name,
  elementSchema = TermElement _Type,
  elementData = encodeType typ}
