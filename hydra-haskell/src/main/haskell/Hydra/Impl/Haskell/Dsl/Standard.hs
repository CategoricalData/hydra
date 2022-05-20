module Hydra.Impl.Haskell.Dsl.Standard where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.CoreEncoding
import Hydra.Impl.Haskell.Dsl.Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Meta

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


datatype :: GraphName -> String -> Type Meta -> Element Meta
datatype gname lname = typeElement standardContext (qualify gname lname)

annotate :: Name -> Y.Maybe (Data Meta) -> Type Meta -> Type Meta
annotate key val (Type term meta) = Type term $ setAnnotation key val meta

doc :: String -> Type Meta -> Type Meta
doc s (Type term meta) = Type term $ setDescription (Just s) meta

dataDoc :: String -> Data Meta -> Data Meta
dataDoc s (Data term meta) = Data term $ setDescription (Just s) meta

project :: Type Meta -> FieldName -> Type Meta -> Data Meta
project dom fname cod = withType standardContext (Types.function dom cod) $ projection fname

qualify :: GraphName -> Name -> Name
qualify (GraphName gname) lname = gname ++ "." ++ lname

standardContext :: Context Meta
standardContext = cx
  where
    cx = Context {
             contextGraphs = GraphSet {
               graphSetGraphs = M.fromList [(emptyGraphName, emptyGraph)],
               graphSetRoot = emptyGraphName},
             contextElements = M.empty,
             contextFunctions = M.empty,
             contextStrategy = EvaluationStrategy {
               evaluationStrategyOpaqueDataVariants = S.fromList []},
             contextDescriptionOf = getDescription,
             contextTypeOf = getType cx,
             contextSetDescriptionOf = \d m -> setDescription d m,
             contextSetTypeOf = \t m -> setType cx t m}
    emptyGraphName = GraphName "empty"
    emptyGraph = Graph emptyGraphName [] (const True) emptyGraphName

standardElement :: GraphName -> String -> String -> Type Meta -> Data Meta -> Element Meta
standardElement (GraphName ns) name desc typ term = Element (ns ++ "." ++ name) (encodeType standardContext typ) $ dataDoc desc term

standardFunction :: GraphName -> String -> String -> Type Meta -> Type Meta -> Data Meta -> Element Meta
standardFunction ns name desc dom cod = standardElement ns name desc typ
  where
    typ = Types.function dom cod

standardGraph :: GraphName -> [Element Meta] -> Graph Meta
standardGraph name els = Graph name els dataTerms schemaGraph
  where
    dataTerms = const True -- TODO
    schemaGraph = GraphName "hydra/core"

standardMatch :: Name -> Type Meta -> [(FieldName, Data Meta)] -> Data Meta
standardMatch = nominalMatch standardContext

standardMatchWithVariants :: Type Meta -> Type Meta -> [(FieldName, FieldName)] -> Data Meta
standardMatchWithVariants = nominalMatchWithVariants standardContext

standardRecord :: Name -> [Field Meta] -> Data Meta
standardRecord = nominalRecord standardContext

standardWithUnitVariant :: Name -> FieldName -> Data Meta
standardWithUnitVariant = nominalWithUnitVariant standardContext

standardWithFunction :: Name -> FieldName -> Element Meta -> Data Meta
standardWithFunction = nominalWithFunction standardContext

standardWithType :: Type Meta -> Data Meta -> Data Meta
standardWithType = withType standardContext

standardWithVariant :: Name -> FieldName -> Data Meta -> Data Meta
standardWithVariant = nominalWithVariant standardContext


typeElement :: Context Meta -> Name -> Type Meta -> Element Meta
typeElement cx name typ = Element {
    elementName = name,
    elementSchema = defaultData $ DataTermElement _Type,
    elementData = encodeType cx typ}
