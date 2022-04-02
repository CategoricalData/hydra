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


project :: Type -> FieldName -> Type -> Term Meta
project dom fname cod = withType standardContext (Types.function dom cod) $ projection fname

standardContext :: Context Meta
standardContext = Context {
    contextGraphs = GraphSet {
      graphSetGraphs = M.fromList [(emptyGraphName, emptyGraph)],
      graphSetRoot = emptyGraphName},
    contextElements = M.empty,
    contextFunctions = M.empty,
    contextStrategy = EvaluationStrategy {
      evaluationStrategyOpaqueTermVariants = S.fromList []},
    contextDescriptionOf = metaDescription,
    contextTypeOf = metaType,
    contextSetDescriptionOf = \d m -> m {metaDescription = d},
    contextSetTypeOf = \t m -> m {metaType = t}}
  where
    emptyGraphName = "empty"
    emptyGraph = Graph emptyGraphName [] (const True) "empty"

standardElement :: Name -> String -> String -> Type -> Term Meta -> Element Meta
standardElement ns name desc typ term = Element (ns ++ "." ++ name) (encodeType standardContext typ) $ withDoc desc term

standardFunction :: Name -> String -> String -> Type -> Type -> Term Meta -> Element Meta
standardFunction ns name desc dom cod = standardElement ns name desc typ
  where
    typ = Types.function dom cod

standardGraph :: Name -> [Element Meta] -> Graph Meta
standardGraph name els = Graph name els dataTerms schemaGraph
  where
    dataTerms = const True -- TODO
    schemaGraph = "hydra/core"

standardMatch :: Name -> Type -> [(FieldName, Term Meta)] -> Term Meta
standardMatch = nominalMatch standardContext

standardMatchWithVariants :: Type -> Type -> [(FieldName, FieldName)] -> Term Meta
standardMatchWithVariants = nominalMatchWithVariants standardContext

standardRecord :: Name -> [Field Meta] -> Term Meta
standardRecord = nominalRecord standardContext

standardWithUnitVariant :: Name -> FieldName -> Term Meta
standardWithUnitVariant = nominalWithUnitVariant standardContext

standardWithFunction :: Name -> FieldName -> Element Meta -> Term Meta
standardWithFunction = nominalWithFunction standardContext

standardWithType :: Type -> Term Meta -> Term Meta
standardWithType = withType standardContext

standardWithVariant :: Name -> FieldName -> Term Meta -> Term Meta
standardWithVariant = nominalWithVariant standardContext

typeElement :: Context Meta -> Name -> String -> Type -> Element Meta
typeElement cx name doc typ = Element {
    elementName = name,
    elementSchema = defaultTerm $ ExpressionElement _Type,
    elementData = setDoc $ encodeType cx typ}
  where
    setDoc (Term d m) = Term d (contextSetDescriptionOf cx (Just doc) m)