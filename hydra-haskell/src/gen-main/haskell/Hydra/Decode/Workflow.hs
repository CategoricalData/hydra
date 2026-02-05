-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.workflow

module Hydra.Decode.Workflow where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Decode.Module as Module
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Util as Util
import qualified Hydra.Workflow as Workflow
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

hydraSchemaSpec :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Workflow.HydraSchemaSpec)
hydraSchemaSpec cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "modules" (Helpers.decodeList Module.module_) fieldMap cx) (\field_modules -> Eithers.bind (Helpers.requireField "typeName" Core_.name fieldMap cx) (\field_typeName -> Right (Workflow.HydraSchemaSpec {
      Workflow.hydraSchemaSpecModules = field_modules,
      Workflow.hydraSchemaSpecTypeName = field_typeName}))))
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
                (Core.Name "provided", (\input -> Eithers.map (\t -> Workflow.SchemaSpecProvided) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      (Core.unName fname),
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.workflow.SchemaSpec"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

transformWorkflow :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Workflow.TransformWorkflow)
transformWorkflow cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "name" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_name -> Eithers.bind (Helpers.requireField "schemaSpec" schemaSpec fieldMap cx) (\field_schemaSpec -> Eithers.bind (Helpers.requireField "srcDir" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_srcDir -> Eithers.bind (Helpers.requireField "destDir" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_destDir -> Right (Workflow.TransformWorkflow {
      Workflow.transformWorkflowName = field_name,
      Workflow.transformWorkflowSchemaSpec = field_schemaSpec,
      Workflow.transformWorkflowSrcDir = field_srcDir,
      Workflow.transformWorkflowDestDir = field_destDir}))))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.workflow.TransformWorkflow"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
