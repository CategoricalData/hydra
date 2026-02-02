# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.workflow."""

from __future__ import annotations
from typing import cast
import hydra.core
import hydra.encode.core
import hydra.encode.module
import hydra.lib.lists
import hydra.workflow

def hydra_schema_spec(x: hydra.workflow.HydraSchemaSpec) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.workflow.HydraSchemaSpec"), (hydra.core.Field(hydra.core.Name("modules"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(hydra.encode.module.module, x.modules)))), hydra.core.Field(hydra.core.Name("typeName"), hydra.encode.core.name(x.type_name))))))

def schema_spec(v1: hydra.workflow.SchemaSpec) -> hydra.core.Type:
    match v1:
        case hydra.workflow.SchemaSpecHydra(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.workflow.SchemaSpec"), hydra.core.Field(hydra.core.Name("hydra"), hydra_schema_spec(y)))))
        
        case hydra.workflow.SchemaSpecFile(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.workflow.SchemaSpec"), hydra.core.Field(hydra.core.Name("file"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(y2))))))))
        
        case hydra.workflow.SchemaSpecProvided():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.workflow.SchemaSpec"), hydra.core.Field(hydra.core.Name("provided"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def transform_workflow(x: hydra.workflow.TransformWorkflow) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.workflow.TransformWorkflow"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.name))))), hydra.core.Field(hydra.core.Name("schemaSpec"), schema_spec(x.schema_spec)), hydra.core.Field(hydra.core.Name("srcDir"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.src_dir))))), hydra.core.Field(hydra.core.Name("destDir"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.dest_dir)))))))))
