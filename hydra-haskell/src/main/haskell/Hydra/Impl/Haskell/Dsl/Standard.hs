module Hydra.Impl.Haskell.Dsl.Standard where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.CoreEncoding
import Hydra.Impl.Haskell.Dsl.Terms as Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Meta
import Hydra.Impl.Haskell.Sources.Libraries

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


key_maxSize = "maxLength"
key_minSize = "minLength"

annotateTerm :: String -> Y.Maybe (Term Meta) -> Term Meta -> Term Meta
annotateTerm = setTermAnnotation standardContext

annotateType :: String -> Y.Maybe (Term Meta) -> Type Meta -> Type Meta
annotateType = setTypeAnnotation standardContext

bounded :: Maybe Int -> Maybe Int -> Type Meta -> Type Meta
bounded min max = annotMin . annotMax
  where
    annotMax t = Y.maybe t (`setMaxLength` t) max
    annotMin t = Y.maybe t (`setMinLength` t) max

boundedList :: Maybe Int -> Maybe Int -> Type Meta -> Type Meta
boundedList min max et = bounded min max $ Types.list et

boundedSet :: Maybe Int -> Maybe Int -> Type Meta -> Type Meta
boundedSet min max et = bounded min max $ Types.set et

boundedString :: Maybe Int -> Maybe Int -> Type Meta
boundedString min max = bounded min max Types.string

dataterm :: GraphName -> String -> Type Meta -> Term Meta -> Element Meta
dataterm gname lname = termElement standardContext (qualify gname (Name lname))

datatype :: Context Meta -> GraphName -> String -> Type Meta -> Element Meta
datatype cx gname lname typ = typeElement cx elName $ replacePlaceholders typ
  where
    elName = qualify gname (Name lname)
    
    replacePlaceholders t = case t of
      TypeAnnotated (Annotated t' ann) -> TypeAnnotated (Annotated (replacePlaceholders t') ann)
      TypeRecord (RowType n fields) -> if n == Types.placeholderName
        then TypeRecord (RowType elName fields)
        else t
      TypeUnion (RowType n fields) -> if n == Types.placeholderName
        then TypeUnion (RowType elName fields)
        else t
      _ -> t

doc :: String -> Type Meta -> Type Meta
doc s = setTypeDescription standardContext (Just s)

dataDoc :: String -> Term Meta -> Term Meta
dataDoc s = setTermDescription standardContext (Just s)

bootstrapContext :: Context Meta
bootstrapContext = cx
  where
    cx = Context {
      contextGraphs = GraphSet {
        graphSetGraphs = M.fromList [(emptyGraphName, emptyGraph)],
        graphSetRoot = emptyGraphName},
      contextElements = M.empty,
      contextFunctions = M.empty,
      contextStrategy = EvaluationStrategy {
        evaluationStrategyOpaqueTermVariants = S.fromList []},
      contextAnnotations = metaAnnotationClass,
      contextTrace = []}

    emptyGraphName = GraphName "empty"

    emptyGraph = Graph emptyGraphName [] (const True) emptyGraphName

graph :: GraphName -> [Context Meta -> Result (Element Meta)] -> Result (Graph Meta)
graph gname cons = do
    elements <- mapM (\f -> f cx) cons
    return $ Graph gname elements terms schemaGraph
  where
    cx = standardContext
    terms = const True
    schemaGraph = GraphName "hydra/core"

nonemptyList :: Type Meta -> Type Meta
nonemptyList = boundedList (Just 1) Nothing

note :: String -> Type Meta -> Type Meta
note s = doc $ "Note: " ++ s

nsref :: GraphName -> String -> Type m
nsref ns = Types.nominal . qualify ns . Name

qualify :: GraphName -> Name -> Name
qualify (GraphName gname) (Name lname) = Name $ gname ++ "." ++ lname

see :: String -> Type Meta -> Type Meta
see s = doc $ "See " ++ s

setMaxLength :: Int -> Type Meta -> Type Meta
setMaxLength m t = setTypeAnnotation standardContext key_maxSize (Just $ Terms.int32 m) t

setMinLength :: Int -> Type Meta -> Type Meta
setMinLength m t = setTypeAnnotation standardContext key_minSize (Just $ Terms.int32 m) t

standardContext :: Context Meta
standardContext = bootstrapContext {
  contextFunctions = M.fromList $ fmap (\p -> (primitiveFunctionName p, p)) (standardPrimitives bootstrapContext)}

twoOrMoreList :: Type Meta -> Type Meta
twoOrMoreList = boundedList (Just 2) Nothing

termElement :: Context Meta -> Name -> Type Meta -> Term Meta -> Element Meta
termElement cx name typ term = Element {
  elementName = name,
  elementSchema = encodeType cx typ,
  elementData = term}

typeElement :: Context Meta -> Name -> Type Meta -> Element Meta
typeElement cx name typ = Element {
  elementName = name,
  elementSchema = TermElement _Type,
  elementData = encodeType cx typ}
