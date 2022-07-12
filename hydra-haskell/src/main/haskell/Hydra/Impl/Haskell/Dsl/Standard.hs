module Hydra.Impl.Haskell.Dsl.Standard where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.CoreEncoding
import Hydra.Impl.Haskell.Dsl.Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Meta
import Hydra.Impl.Haskell.Sources.Libraries
import Hydra.Impl.Haskell.Default
import Hydra.Common

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


datatype :: GraphName -> String -> Type Meta -> Element Meta
datatype gname lname = typeElement standardContext (qualify gname (Name lname))

annotate :: String -> Y.Maybe (Term Meta) -> Type Meta -> Type Meta
annotate = setTypeAnnotation

doc :: String -> Type Meta -> Type Meta
doc s = setTypeDescription (Just s)

dataDoc :: String -> Term Meta -> Term Meta
dataDoc s = setTermDescription (Just s)

nonemptyList :: Type Meta -> Type Meta
nonemptyList t = doc "Note: list cannot be empty" $ Types.list t

project :: Type Meta -> FieldName -> Type Meta -> Term Meta
project dom fname cod = withType standardContext (Types.function dom cod) $ projection fname

qualify :: GraphName -> Name -> Name
qualify (GraphName gname) (Name lname) = Name $ gname ++ "." ++ lname

standardContext :: Context Meta
standardContext = cx
  where
    cx = Context {
      contextGraphs = GraphSet {
        graphSetGraphs = M.fromList [(emptyGraphName, emptyGraph)],
        graphSetRoot = emptyGraphName},
      contextElements = M.empty,
      contextFunctions = M.fromList $ fmap (\p -> (primitiveFunctionName p, p)) standardPrimitives,
      contextStrategy = EvaluationStrategy {
        evaluationStrategyOpaqueTermVariants = S.fromList []},
      contextAnnotations = metaAnnotationClass}

    emptyGraphName = GraphName "empty"
    
    emptyGraph = Graph emptyGraphName [] (const True) emptyGraphName

standardElement :: GraphName -> String -> String -> Type Meta -> Term Meta -> Element Meta
standardElement (GraphName ns) name desc typ term = Element (Name $ ns ++ "." ++ name) (encodeType standardContext typ)
  $ dataDoc desc term

standardFunction :: GraphName -> String -> String -> Type Meta -> Type Meta -> Term Meta -> Element Meta
standardFunction ns name desc dom cod = standardElement ns name desc typ
  where
    typ = Types.function dom cod

standardGraph :: GraphName -> [Element Meta] -> Graph Meta
standardGraph name els = Graph name els termExprs schemaGraph
  where
    termExprs = const True -- TODO
    schemaGraph = GraphName "hydra/core"

typeElement :: Context Meta -> Name -> Type Meta -> Element Meta
typeElement cx name typ = Element {
  elementName = name,
  elementSchema = TermElement _Type,
  elementData = encodeType cx typ}

typed :: Type Meta -> Term Meta -> Term Meta
typed t = setTermType standardContext (Just t)
