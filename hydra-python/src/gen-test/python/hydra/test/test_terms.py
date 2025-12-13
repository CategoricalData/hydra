# Note: this is an automatically generated file. Do not edit.

r"""Term definitions for the test suite."""

from __future__ import annotations
from hydra.dsl.python import Just, Maybe, frozenlist
from typing import cast
import hydra.core
import hydra.test.test_types

def latlon_record(lat: float, lon: float) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.test.test_types.test_type_lat_lon_name, (hydra.core.Field(hydra.core.Name("lat"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat32(lat))))))), hydra.core.Field(hydra.core.Name("lon"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat32(lon)))))))))))

test_data_arthur = cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.test.test_types.test_type_person_name, (hydra.core.Field(hydra.core.Name("firstName"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("Arthur"))))), hydra.core.Field(hydra.core.Name("lastName"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("Dent"))))), hydra.core.Field(hydra.core.Name("age"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42)))))))))))

def test_element_arthur() -> hydra.core.Binding:
    return hydra.core.Binding(hydra.core.Name("firstName"), test_data_arthur, cast(Maybe[hydra.core.TypeScheme], Just(hydra.core.TypeScheme(cast(frozenlist[hydra.core.Name], ()), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.test.test_types.test_type_person_name))))))

def test_element_first_name() -> hydra.core.Binding:
    return hydra.core.Binding(hydra.core.Name("firstName"), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.test.test_types.test_type_person_name, hydra.core.Name("firstName")))))))), cast(Maybe[hydra.core.TypeScheme], Just(hydra.core.TypeScheme(cast(frozenlist[hydra.core.Name], ()), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.test.test_types.test_type_person_name)), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))))))))))
