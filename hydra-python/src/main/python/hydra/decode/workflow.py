# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.workflow."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import cast
import hydra.core
import hydra.decode.core
import hydra.decode.module
import hydra.extract.helpers
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.util
import hydra.workflow

def hydra_schema_spec(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec]:
    def _hoist_hydra_decode_workflow_hydra_schema_spec_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("modules", (lambda v1, v2: hydra.extract.helpers.decode_list(hydra.decode.module.module, v1, v2)), field_map, cx), (lambda field_modules: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("typeName", hydra.decode.core.name, field_map, cx), (lambda field_type_name: cast(Either[hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec], Right(hydra.workflow.HydraSchemaSpec(field_modules, field_type_name)))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec], Left(hydra.util.DecodingError("expected record of type hydra.workflow.HydraSchemaSpec")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_workflow_hydra_schema_spec_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def schema_spec(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.workflow.SchemaSpec]:
    def _hoist_hydra_decode_workflow_schema_spec_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.workflow.SchemaSpec]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                tname = inj.type_name
                field = inj.field
                fname = field.name
                fterm = field.term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.workflow.SchemaSpec]]]:
                    def _hoist_variant_map_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                        match v1:
                            case hydra.core.LiteralString(value=s):
                                return cast(Either[hydra.util.DecodingError, str], Right(s))
                            
                            case _:
                                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
                    def _hoist_variant_map_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_1(v)
                            
                            case _:
                                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
                    return cast(FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.workflow.SchemaSpec]]], hydra.lib.maps.from_list((cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.workflow.SchemaSpec]]], (hydra.core.Name("hydra"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.workflow.SchemaSpec, hydra.workflow.SchemaSpecHydra(t))), hydra_schema_spec(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.workflow.SchemaSpec]]], (hydra.core.Name("file"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.workflow.SchemaSpec, hydra.workflow.SchemaSpecFile(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_variant_map_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.workflow.SchemaSpec]]], (hydra.core.Name("provided"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.workflow.SchemaSpec, hydra.workflow.SchemaSpecProvided())), hydra.extract.helpers.decode_unit(cx, input))))))))
                return hydra.lib.maybes.maybe(cast(Either[hydra.util.DecodingError, hydra.workflow.SchemaSpec], Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union type ", tname.value))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.workflow.SchemaSpec], Left(hydra.util.DecodingError("expected union of type hydra.workflow.SchemaSpec")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.workflow.SchemaSpec], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_workflow_schema_spec_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def transform_workflow(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.workflow.TransformWorkflow]:
    def _hoist_hydra_decode_workflow_transform_workflow_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.workflow.TransformWorkflow]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.LiteralString(value=s):
                            return cast(Either[hydra.util.DecodingError, str], Right(s))
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
                def _hoist_body_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
                def _hoist_body_3(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.LiteralString(value=s):
                            return cast(Either[hydra.util.DecodingError, str], Right(s))
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
                def _hoist_body_4(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_3(v)
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
                def _hoist_body_5(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.LiteralString(value=s):
                            return cast(Either[hydra.util.DecodingError, str], Right(s))
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
                def _hoist_body_6(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_5(v)
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map, cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("schemaSpec", schema_spec, field_map, cx), (lambda field_schema_spec: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("srcDir", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_body_4(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map, cx), (lambda field_src_dir: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("destDir", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_body_6(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map, cx), (lambda field_dest_dir: cast(Either[hydra.util.DecodingError, hydra.workflow.TransformWorkflow], Right(hydra.workflow.TransformWorkflow(field_name, field_schema_spec, field_src_dir, field_dest_dir)))))))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.workflow.TransformWorkflow], Left(hydra.util.DecodingError("expected record of type hydra.workflow.TransformWorkflow")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.workflow.TransformWorkflow], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_workflow_transform_workflow_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
