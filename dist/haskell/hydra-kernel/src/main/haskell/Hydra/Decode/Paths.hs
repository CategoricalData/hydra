-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.paths

module Hydra.Decode.Paths where
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Paths as Paths
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
subtermEdge :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Paths.SubtermEdge
subtermEdge cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "source" subtermNode fieldMap cx) (\field_source -> Eithers.bind (ExtractCore.requireField "path" subtermPath fieldMap cx) (\field_path -> Eithers.bind (ExtractCore.requireField "target" subtermNode fieldMap cx) (\field_target -> Right (Paths.SubtermEdge {
          Paths.subtermEdgeSource = field_source,
          Paths.subtermEdgePath = field_path,
          Paths.subtermEdgeTarget = field_target})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
subtermGraph :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Paths.SubtermGraph
subtermGraph cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "nodes" (ExtractCore.decodeList subtermNode) fieldMap cx) (\field_nodes -> Eithers.bind (ExtractCore.requireField "edges" (ExtractCore.decodeList subtermEdge) fieldMap cx) (\field_edges -> Right (Paths.SubtermGraph {
          Paths.subtermGraphNodes = field_nodes,
          Paths.subtermGraphEdges = field_edges}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
subtermNode :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Paths.SubtermNode
subtermNode cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "label" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_label -> Eithers.bind (ExtractCore.requireField "id" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_id -> Right (Paths.SubtermNode {
          Paths.subtermNodeName = field_name,
          Paths.subtermNodeLabel = field_label,
          Paths.subtermNodeId = field_id})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
subtermPath :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Paths.SubtermPath
subtermPath cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Paths.SubtermPath b) (ExtractCore.decodeList subtermStep cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
subtermStep :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Paths.SubtermStep
subtermStep cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (
                        Core.Name "annotatedBody",
                        (\input -> Eithers.map (\t -> Paths.SubtermStepAnnotatedBody) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "applicationFunction",
                        (\input -> Eithers.map (\t -> Paths.SubtermStepApplicationFunction) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "applicationArgument",
                        (\input -> Eithers.map (\t -> Paths.SubtermStepApplicationArgument) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "lambdaBody", (\input -> Eithers.map (\t -> Paths.SubtermStepLambdaBody) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "unionCasesDefault",
                        (\input -> Eithers.map (\t -> Paths.SubtermStepUnionCasesDefault) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "unionCasesBranch",
                        (\input -> Eithers.map (\t -> Paths.SubtermStepUnionCasesBranch t) (DecodeCore.name cx input))),
                      (Core.Name "letBody", (\input -> Eithers.map (\t -> Paths.SubtermStepLetBody) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "letBinding", (\input -> Eithers.map (\t -> Paths.SubtermStepLetBinding t) (DecodeCore.name cx input))),
                      (
                        Core.Name "listElement",
                        (\input -> Eithers.map (\t -> Paths.SubtermStepListElement t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralInteger v2 -> case v2 of
                              Core.IntegerValueInt32 v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected int32 value")
                            _ -> Left (Errors.DecodingError "expected int32 literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (
                        Core.Name "mapKey",
                        (\input -> Eithers.map (\t -> Paths.SubtermStepMapKey t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralInteger v2 -> case v2 of
                              Core.IntegerValueInt32 v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected int32 value")
                            _ -> Left (Errors.DecodingError "expected int32 literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (
                        Core.Name "mapValue",
                        (\input -> Eithers.map (\t -> Paths.SubtermStepMapValue t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralInteger v2 -> case v2 of
                              Core.IntegerValueInt32 v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected int32 value")
                            _ -> Left (Errors.DecodingError "expected int32 literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "maybeTerm", (\input -> Eithers.map (\t -> Paths.SubtermStepMaybeTerm) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "productTerm",
                        (\input -> Eithers.map (\t -> Paths.SubtermStepProductTerm t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralInteger v2 -> case v2 of
                              Core.IntegerValueInt32 v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected int32 value")
                            _ -> Left (Errors.DecodingError "expected int32 literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "recordField", (\input -> Eithers.map (\t -> Paths.SubtermStepRecordField t) (DecodeCore.name cx input))),
                      (
                        Core.Name "setElement",
                        (\input -> Eithers.map (\t -> Paths.SubtermStepSetElement t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralInteger v2 -> case v2 of
                              Core.IntegerValueInt32 v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected int32 value")
                            _ -> Left (Errors.DecodingError "expected int32 literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "sumTerm", (\input -> Eithers.map (\t -> Paths.SubtermStepSumTerm) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "typeLambdaBody",
                        (\input -> Eithers.map (\t -> Paths.SubtermStepTypeLambdaBody) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "typeApplicationTerm",
                        (\input -> Eithers.map (\t -> Paths.SubtermStepTypeApplicationTerm) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "injectionTerm",
                        (\input -> Eithers.map (\t -> Paths.SubtermStepInjectionTerm) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "wrappedTerm", (\input -> Eithers.map (\t -> Paths.SubtermStepWrappedTerm) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
subtypeEdge :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Paths.SubtypeEdge
subtypeEdge cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "source" subtypeNode fieldMap cx) (\field_source -> Eithers.bind (ExtractCore.requireField "path" subtypePath fieldMap cx) (\field_path -> Eithers.bind (ExtractCore.requireField "target" subtypeNode fieldMap cx) (\field_target -> Right (Paths.SubtypeEdge {
          Paths.subtypeEdgeSource = field_source,
          Paths.subtypeEdgePath = field_path,
          Paths.subtypeEdgeTarget = field_target})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
subtypeGraph :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Paths.SubtypeGraph
subtypeGraph cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "nodes" (ExtractCore.decodeList subtypeNode) fieldMap cx) (\field_nodes -> Eithers.bind (ExtractCore.requireField "edges" (ExtractCore.decodeList subtypeEdge) fieldMap cx) (\field_edges -> Right (Paths.SubtypeGraph {
          Paths.subtypeGraphNodes = field_nodes,
          Paths.subtypeGraphEdges = field_edges}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
subtypeNode :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Paths.SubtypeNode
subtypeNode cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "label" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_label -> Eithers.bind (ExtractCore.requireField "id" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_id -> Right (Paths.SubtypeNode {
          Paths.subtypeNodeName = field_name,
          Paths.subtypeNodeLabel = field_label,
          Paths.subtypeNodeId = field_id})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
subtypePath :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Paths.SubtypePath
subtypePath cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Paths.SubtypePath b) (ExtractCore.decodeList subtypeStep cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
subtypeStep :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Paths.SubtypeStep
subtypeStep cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (
                        Core.Name "annotatedBody",
                        (\input -> Eithers.map (\t -> Paths.SubtypeStepAnnotatedBody) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "applicationFunction",
                        (\input -> Eithers.map (\t -> Paths.SubtypeStepApplicationFunction) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "applicationArgument",
                        (\input -> Eithers.map (\t -> Paths.SubtypeStepApplicationArgument) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "eitherLeft", (\input -> Eithers.map (\t -> Paths.SubtypeStepEitherLeft) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "eitherRight", (\input -> Eithers.map (\t -> Paths.SubtypeStepEitherRight) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "forallBody", (\input -> Eithers.map (\t -> Paths.SubtypeStepForallBody) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "functionDomain",
                        (\input -> Eithers.map (\t -> Paths.SubtypeStepFunctionDomain) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "functionCodomain",
                        (\input -> Eithers.map (\t -> Paths.SubtypeStepFunctionCodomain) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "listElement", (\input -> Eithers.map (\t -> Paths.SubtypeStepListElement) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "mapKeys", (\input -> Eithers.map (\t -> Paths.SubtypeStepMapKeys) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "mapValues", (\input -> Eithers.map (\t -> Paths.SubtypeStepMapValues) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "maybeElement",
                        (\input -> Eithers.map (\t -> Paths.SubtypeStepMaybeElement) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "pairFirst", (\input -> Eithers.map (\t -> Paths.SubtypeStepPairFirst) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "pairSecond", (\input -> Eithers.map (\t -> Paths.SubtypeStepPairSecond) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "recordField", (\input -> Eithers.map (\t -> Paths.SubtypeStepRecordField t) (DecodeCore.name cx input))),
                      (Core.Name "setElement", (\input -> Eithers.map (\t -> Paths.SubtypeStepSetElement) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "unionField", (\input -> Eithers.map (\t -> Paths.SubtypeStepUnionField t) (DecodeCore.name cx input))),
                      (Core.Name "wrappedType", (\input -> Eithers.map (\t -> Paths.SubtypeStepWrappedType) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
