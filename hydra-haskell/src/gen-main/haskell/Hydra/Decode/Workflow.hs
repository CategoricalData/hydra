-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.workflow

module Hydra.Decode.Workflow where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Decode.Module as Module
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Util as Util
import qualified Hydra.Workflow as Workflow
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

hydraSchemaSpec :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Workflow.HydraSchemaSpec)
hydraSchemaSpec cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\modules -> Eithers.either (\err -> Left err) (\typeName -> Right (Workflow.HydraSchemaSpec {
      Workflow.hydraSchemaSpecModules = modules,
      Workflow.hydraSchemaSpecTypeName = typeName})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "typeName",
      " in record"]))) (\fieldTerm -> Core_.name cx fieldTerm) (Maps.lookup (Core.Name "typeName") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "modules",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (Module.module_ cx) v2)
      _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "modules") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.workflow.HydraSchemaSpec"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

schemaSpec :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Workflow.SchemaSpec)
schemaSpec cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "hydra", (\input -> Eithers.map (\t -> Workflow.SchemaSpecHydra t) (hydraSchemaSpec cx input))),
                (Core.Name "file", (\input -> Eithers.map (\t -> Workflow.SchemaSpecFile t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralString v3 -> (Right v3)
                    _ -> (Left (Util.DecodingError "expected string literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "provided", (\input -> Eithers.map (\t -> Workflow.SchemaSpecProvided) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.workflow.SchemaSpec"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

transformWorkflow :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Workflow.TransformWorkflow)
transformWorkflow cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\name -> Eithers.either (\err -> Left err) (\schemaSpec -> Eithers.either (\err -> Left err) (\srcDir -> Eithers.either (\err -> Left err) (\destDir -> Right (Workflow.TransformWorkflow {
      Workflow.transformWorkflowName = name,
      Workflow.transformWorkflowSchemaSpec = schemaSpec,
      Workflow.transformWorkflowSrcDir = srcDir,
      Workflow.transformWorkflowDestDir = destDir})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "destDir",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "destDir") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "srcDir",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "srcDir") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "schemaSpec",
      " in record"]))) (\fieldTerm -> schemaSpec cx fieldTerm) (Maps.lookup (Core.Name "schemaSpec") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "name",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "name") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.workflow.TransformWorkflow"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
