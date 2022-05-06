module Hydra.Impl.Haskell.Dsl.Standard where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.CoreEncoding
import Hydra.Impl.Haskell.Dsl.Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Extras

import qualified Data.Map as M
import qualified Data.Set as S


datatype gname lname doc typ = typeElement standardContext (qualify gname lname) doc typ

project :: Type Meta -> FieldName -> Type Meta -> Data Meta
project dom fname cod = withType standardContext (Types.function dom cod) $ projection fname

qualify gname lname = gname ++ "." ++ lname

standardContext :: Context Meta
standardContext = Context {
    contextGraphs = GraphSet {
      graphSetGraphs = M.fromList [(emptyGraphName, emptyGraph)],
      graphSetRoot = emptyGraphName},
    contextElements = M.empty,
    contextFunctions = M.empty,
    contextStrategy = EvaluationStrategy {
      evaluationStrategyOpaqueDataVariants = S.fromList []},
    contextDescriptionOf = metaDescription,
    contextTypeOf = metaType,
    contextSetDescriptionOf = \d m -> m {metaDescription = d},
    contextSetTypeOf = \t m -> m {metaType = t}}
  where
    emptyGraphName = "empty"
    emptyGraph = Graph emptyGraphName [] (const True) "empty"

standardElement :: Name -> String -> String -> Type Meta -> Data Meta -> Element Meta
standardElement ns name desc typ term = Element (ns ++ "." ++ name) (encodeType standardContext typ) $ withDoc desc term

standardFunction :: Name -> String -> String -> Type Meta -> Type Meta -> Data Meta -> Element Meta
standardFunction ns name desc dom cod = standardElement ns name desc typ
  where
    typ = Types.function dom cod

standardGraph :: Name -> [Element Meta] -> Graph Meta
standardGraph name els = Graph name els dataTerms schemaGraph
  where
    dataTerms = const True -- TODO
    schemaGraph = "hydra/core"

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

typeElement :: Context Meta -> Name -> String -> Type Meta -> Element Meta
typeElement cx name doc typ = Element {
    elementName = name,
    elementSchema = defaultData $ DataTermElement _Type,
    elementData = setDoc $ encodeType cx typ}
  where
    setDoc (Data d m) = Data d (contextSetDescriptionOf cx (Just doc) m)
