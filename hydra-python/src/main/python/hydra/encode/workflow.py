# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.workflow."""

from __future__ import annotations
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.core
import hydra.encode.core
import hydra.encode.module
import hydra.lib.lists
import hydra.workflow

def hydra_schema_spec(x: hydra.workflow.HydraSchemaSpec) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.workflow.HydraSchemaSpec"), (hydra.core.Field(hydra.core.Name("modules"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(hydra.encode.module.module, xs))))(x.modules)), hydra.core.Field(hydra.core.Name("typeName"), hydra.encode.core.name(x.type_name))))))

def schema_spec(v1: hydra.workflow.SchemaSpec) -> hydra.core.Type:
    match v1:
        case hydra.workflow.SchemaSpecHydra(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.workflow.SchemaSpec"), hydra.core.Field(hydra.core.Name("hydra"), hydra_schema_spec(v)))))
        
        case hydra.workflow.SchemaSpecFile(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.workflow.SchemaSpec"), hydra.core.Field(hydra.core.Name("file"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x)))))(v2)))))
        
        case hydra.workflow.SchemaSpecProvided(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.workflow.SchemaSpec"), hydra.core.Field(hydra.core.Name("provided"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v3)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def transform_workflow(x: hydra.workflow.TransformWorkflow) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.workflow.TransformWorkflow"), (hydra.core.Field(hydra.core.Name("name"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.name)), hydra.core.Field(hydra.core.Name("schemaSpec"), schema_spec(x.schema_spec)), hydra.core.Field(hydra.core.Name("srcDir"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.src_dir)), hydra.core.Field(hydra.core.Name("destDir"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.dest_dir))))))
