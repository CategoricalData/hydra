-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.accessors

module Hydra.Decode.Accessors where

import qualified Hydra.Accessors as Accessors
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

accessorEdge :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Accessors.AccessorEdge)
accessorEdge cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "source" accessorNode fieldMap cx) (\field_source -> Eithers.bind (Helpers.requireField "path" accessorPath fieldMap cx) (\field_path -> Eithers.bind (Helpers.requireField "target" accessorNode fieldMap cx) (\field_target -> Right (Accessors.AccessorEdge {
      Accessors.accessorEdgeSource = field_source,
      Accessors.accessorEdgePath = field_path,
      Accessors.accessorEdgeTarget = field_target})))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.accessors.AccessorEdge"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

accessorGraph :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Accessors.AccessorGraph)
accessorGraph cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "nodes" (Helpers.decodeList accessorNode) fieldMap cx) (\field_nodes -> Eithers.bind (Helpers.requireField "edges" (Helpers.decodeList accessorEdge) fieldMap cx) (\field_edges -> Right (Accessors.AccessorGraph {
      Accessors.accessorGraphNodes = field_nodes,
      Accessors.accessorGraphEdges = field_edges}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.accessors.AccessorGraph"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

accessorNode :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Accessors.AccessorNode)
accessorNode cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Eithers.bind (Helpers.requireField "label" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_label -> Eithers.bind (Helpers.requireField "id" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_id -> Right (Accessors.AccessorNode {
      Accessors.accessorNodeName = field_name,
      Accessors.accessorNodeLabel = field_label,
      Accessors.accessorNodeId = field_id})))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.accessors.AccessorNode"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

accessorPath :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Accessors.AccessorPath)
accessorPath cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Accessors.AccessorPath b) (Helpers.decodeList termAccessor cx (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.accessors.AccessorPath"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

termAccessor :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Accessors.TermAccessor)
termAccessor cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "annotatedBody", (\input -> Eithers.map (\t -> Accessors.TermAccessorAnnotatedBody) (Helpers.decodeUnit cx input))),
                (Core.Name "applicationFunction", (\input -> Eithers.map (\t -> Accessors.TermAccessorApplicationFunction) (Helpers.decodeUnit cx input))),
                (Core.Name "applicationArgument", (\input -> Eithers.map (\t -> Accessors.TermAccessorApplicationArgument) (Helpers.decodeUnit cx input))),
                (Core.Name "lambdaBody", (\input -> Eithers.map (\t -> Accessors.TermAccessorLambdaBody) (Helpers.decodeUnit cx input))),
                (Core.Name "unionCasesDefault", (\input -> Eithers.map (\t -> Accessors.TermAccessorUnionCasesDefault) (Helpers.decodeUnit cx input))),
                (Core.Name "unionCasesBranch", (\input -> Eithers.map (\t -> Accessors.TermAccessorUnionCasesBranch t) (Core_.name cx input))),
                (Core.Name "letBody", (\input -> Eithers.map (\t -> Accessors.TermAccessorLetBody) (Helpers.decodeUnit cx input))),
                (Core.Name "letBinding", (\input -> Eithers.map (\t -> Accessors.TermAccessorLetBinding t) (Core_.name cx input))),
                (Core.Name "listElement", (\input -> Eithers.map (\t -> Accessors.TermAccessorListElement t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralInteger v3 -> ((\x -> case x of
                      Core.IntegerValueInt32 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "mapKey", (\input -> Eithers.map (\t -> Accessors.TermAccessorMapKey t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralInteger v3 -> ((\x -> case x of
                      Core.IntegerValueInt32 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "mapValue", (\input -> Eithers.map (\t -> Accessors.TermAccessorMapValue t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralInteger v3 -> ((\x -> case x of
                      Core.IntegerValueInt32 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "maybeTerm", (\input -> Eithers.map (\t -> Accessors.TermAccessorMaybeTerm) (Helpers.decodeUnit cx input))),
                (Core.Name "productTerm", (\input -> Eithers.map (\t -> Accessors.TermAccessorProductTerm t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralInteger v3 -> ((\x -> case x of
                      Core.IntegerValueInt32 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "recordField", (\input -> Eithers.map (\t -> Accessors.TermAccessorRecordField t) (Core_.name cx input))),
                (Core.Name "setElement", (\input -> Eithers.map (\t -> Accessors.TermAccessorSetElement t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralInteger v3 -> ((\x -> case x of
                      Core.IntegerValueInt32 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "sumTerm", (\input -> Eithers.map (\t -> Accessors.TermAccessorSumTerm) (Helpers.decodeUnit cx input))),
                (Core.Name "typeLambdaBody", (\input -> Eithers.map (\t -> Accessors.TermAccessorTypeLambdaBody) (Helpers.decodeUnit cx input))),
                (Core.Name "typeApplicationTerm", (\input -> Eithers.map (\t -> Accessors.TermAccessorTypeApplicationTerm) (Helpers.decodeUnit cx input))),
                (Core.Name "injectionTerm", (\input -> Eithers.map (\t -> Accessors.TermAccessorInjectionTerm) (Helpers.decodeUnit cx input))),
                (Core.Name "wrappedTerm", (\input -> Eithers.map (\t -> Accessors.TermAccessorWrappedTerm) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      (Core.unName fname),
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.accessors.TermAccessor"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
