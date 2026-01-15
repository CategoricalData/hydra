# Note: this is an automatically generated file. Do not edit.

r"""Encoding functions for converting GraphSON syntax to JSON."""

from __future__ import annotations
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import cast
import hydra.core
import hydra.json.model
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.pg.graphson.syntax

def double_value_to_json(v1: hydra.pg.graphson.syntax.DoubleValue) -> hydra.json.model.Value:
    r"""Convert a GraphSON DoubleValue to a JSON Value."""
    
    match v1:
        case hydra.pg.graphson.syntax.DoubleValueFinite(value=d):
            return cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.float64_to_bigfloat(d)))
        
        case hydra.pg.graphson.syntax.DoubleValueInfinity():
            return cast(hydra.json.model.Value, hydra.json.model.ValueString("Infinity"))
        
        case hydra.pg.graphson.syntax.DoubleValueNegativeInfinity():
            return cast(hydra.json.model.Value, hydra.json.model.ValueString("-Infinity"))
        
        case hydra.pg.graphson.syntax.DoubleValueNotANumber():
            return cast(hydra.json.model.Value, hydra.json.model.ValueString("NaN"))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def float_value_to_json(v1: hydra.pg.graphson.syntax.FloatValue) -> hydra.json.model.Value:
    r"""Convert a GraphSON FloatValue to a JSON Value."""
    
    match v1:
        case hydra.pg.graphson.syntax.FloatValueFinite(value=f):
            return cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.float32_to_bigfloat(f)))
        
        case hydra.pg.graphson.syntax.FloatValueInfinity():
            return cast(hydra.json.model.Value, hydra.json.model.ValueString("Infinity"))
        
        case hydra.pg.graphson.syntax.FloatValueNegativeInfinity():
            return cast(hydra.json.model.Value, hydra.json.model.ValueString("-Infinity"))
        
        case hydra.pg.graphson.syntax.FloatValueNotANumber():
            return cast(hydra.json.model.Value, hydra.json.model.ValueString("NaN"))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def to_json_object(pairs: frozenlist[tuple[str, Maybe[hydra.json.model.Value]]]) -> hydra.json.model.Value:
    r"""Create a JSON object from a list of key-value pairs, filtering out Nothing values."""
    
    return cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda p: hydra.lib.maybes.map((lambda v: (hydra.lib.pairs.first(p), v)), hydra.lib.pairs.second(p))), pairs)))))

def typed_value_to_json(type_name: str, value_json: hydra.json.model.Value) -> hydra.json.model.Value:
    r"""Create a typed JSON object with @type and @value fields."""
    
    return to_json_object((("@type", Just(cast(hydra.json.model.Value, hydra.json.model.ValueString(type_name)))), ("@value", Just(value_json))))

def map_to_json(m: hydra.pg.graphson.syntax.Map) -> hydra.json.model.Value:
    r"""Convert a GraphSON Map to a JSON array of alternating keys and values."""
    
    return cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.concat(hydra.lib.lists.map((lambda vp: (value_to_json(vp.first), value_to_json(vp.second))), m.value))))

def value_to_json(v1: hydra.pg.graphson.syntax.Value) -> hydra.json.model.Value:
    r"""Convert a GraphSON Value to a JSON Value."""
    
    match v1:
        case hydra.pg.graphson.syntax.ValueBigDecimal(value=bd):
            return typed_value_to_json("g:BigDecimal", cast(hydra.json.model.Value, hydra.json.model.ValueString(bd.value)))
        
        case hydra.pg.graphson.syntax.ValueBigInteger(value=i):
            return typed_value_to_json("g:BigInteger", cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(i))))
        
        case hydra.pg.graphson.syntax.ValueBinary(value=b):
            return typed_value_to_json("g:Binary", cast(hydra.json.model.Value, hydra.json.model.ValueString(b)))
        
        case hydra.pg.graphson.syntax.ValueBoolean(value=b2):
            return cast(hydra.json.model.Value, hydra.json.model.ValueBoolean(b2))
        
        case hydra.pg.graphson.syntax.ValueByte(value=b3):
            return typed_value_to_json("g:Byte", cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(hydra.lib.literals.uint8_to_bigint(b3)))))
        
        case hydra.pg.graphson.syntax.ValueChar(value=c):
            return typed_value_to_json("g:Char", cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.lib.strings.from_list(hydra.lib.lists.pure(hydra.lib.literals.bigint_to_int32(hydra.lib.literals.uint32_to_bigint(c)))))))
        
        case hydra.pg.graphson.syntax.ValueComposite(value=ctv):
            return typed_value_to_json(ctv.type.value, map_to_json(ctv.fields))
        
        case hydra.pg.graphson.syntax.ValueDateTime(value=dt):
            return typed_value_to_json("g:DateTime", cast(hydra.json.model.Value, hydra.json.model.ValueString(dt.value)))
        
        case hydra.pg.graphson.syntax.ValueDouble(value=dv):
            return typed_value_to_json("g:Double", double_value_to_json(dv))
        
        case hydra.pg.graphson.syntax.ValueDuration(value=dur):
            return typed_value_to_json("g:Duration", cast(hydra.json.model.Value, hydra.json.model.ValueString(dur.value)))
        
        case hydra.pg.graphson.syntax.ValueFloat(value=fv):
            return typed_value_to_json("g:Float", float_value_to_json(fv))
        
        case hydra.pg.graphson.syntax.ValueInteger(value=i2):
            return typed_value_to_json("g:Int32", cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(hydra.lib.literals.int32_to_bigint(i2)))))
        
        case hydra.pg.graphson.syntax.ValueList(value=vals):
            return typed_value_to_json("g:List", cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map(value_to_json, vals))))
        
        case hydra.pg.graphson.syntax.ValueLong(value=l):
            return typed_value_to_json("g:Long", cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(hydra.lib.literals.int64_to_bigint(l)))))
        
        case hydra.pg.graphson.syntax.ValueMap(value=m):
            return typed_value_to_json("g:Map", map_to_json(m))
        
        case hydra.pg.graphson.syntax.ValueNull():
            return cast(hydra.json.model.Value, hydra.json.model.ValueNull())
        
        case hydra.pg.graphson.syntax.ValuePrimitive(value=ptv):
            return typed_value_to_json("g:PrimitivePdt", cast(hydra.json.model.Value, hydra.json.model.ValueString(ptv.value)))
        
        case hydra.pg.graphson.syntax.ValueSet(value=vals2):
            return typed_value_to_json("g:Set", cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map(value_to_json, vals2))))
        
        case hydra.pg.graphson.syntax.ValueShort(value=i3):
            return typed_value_to_json("g:Int16", cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(hydra.lib.literals.int16_to_bigint(i3)))))
        
        case hydra.pg.graphson.syntax.ValueString(value=s):
            return cast(hydra.json.model.Value, hydra.json.model.ValueString(s))
        
        case hydra.pg.graphson.syntax.ValueUuid(value=u):
            return typed_value_to_json("g:UUID", cast(hydra.json.model.Value, hydra.json.model.ValueString(u.value)))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def edge_property_map_to_json(m: FrozenDict[hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.Value]) -> Maybe[hydra.json.model.Value]:
    r"""Convert a map of edge properties to an optional JSON Value."""
    
    return hydra.lib.logic.if_else(hydra.lib.maps.null(m), (lambda : Nothing()), (lambda : Just(cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda p: (hydra.lib.pairs.first(p).value, value_to_json(hydra.lib.pairs.second(p)))), hydra.lib.maps.to_list(m))))))))

def adjacent_edge_to_json(out: bool, ae: hydra.pg.graphson.syntax.AdjacentEdge) -> hydra.json.model.Value:
    r"""Convert a GraphSON AdjacentEdge to a JSON Value. The Bool indicates whether this is an outgoing edge."""
    
    return to_json_object((("id", Just(value_to_json(ae.id))), ("inV", hydra.lib.logic.if_else(out, (lambda : Just(value_to_json(ae.vertex_id))), (lambda : Nothing()))), ("outV", hydra.lib.logic.if_else(out, (lambda : Nothing()), (lambda : Just(value_to_json(ae.vertex_id))))), ("properties", edge_property_map_to_json(ae.properties))))

def edge_map_to_json(out: bool, m: FrozenDict[hydra.pg.graphson.syntax.EdgeLabel, frozenlist[hydra.pg.graphson.syntax.AdjacentEdge]]) -> Maybe[hydra.json.model.Value]:
    r"""Convert a map of edges by label to an optional JSON Value."""
    
    return hydra.lib.logic.if_else(hydra.lib.maps.null(m), (lambda : Nothing()), (lambda : Just(cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda p: (hydra.lib.pairs.first(p).value, cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map((lambda v1: adjacent_edge_to_json(out, v1)), hydra.lib.pairs.second(p)))))), hydra.lib.maps.to_list(m))))))))

def vertex_property_value_to_json(vpv: hydra.pg.graphson.syntax.VertexPropertyValue) -> hydra.json.model.Value:
    r"""Convert a GraphSON VertexPropertyValue to a JSON Value."""
    
    return to_json_object((("id", hydra.lib.maybes.map(value_to_json, vpv.id)), ("value", Just(value_to_json(vpv.value)))))

def vertex_property_map_to_json(m: FrozenDict[hydra.pg.graphson.syntax.PropertyKey, frozenlist[hydra.pg.graphson.syntax.VertexPropertyValue]]) -> Maybe[hydra.json.model.Value]:
    r"""Convert a map of vertex properties to an optional JSON Value."""
    
    return hydra.lib.logic.if_else(hydra.lib.maps.null(m), (lambda : Nothing()), (lambda : Just(cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda p: (hydra.lib.pairs.first(p).value, cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map(vertex_property_value_to_json, hydra.lib.pairs.second(p)))))), hydra.lib.maps.to_list(m))))))))

def vertex_to_json(v: hydra.pg.graphson.syntax.Vertex) -> hydra.json.model.Value:
    r"""Convert a GraphSON Vertex to a JSON Value."""
    
    return to_json_object((("id", Just(value_to_json(v.id))), ("label", hydra.lib.maybes.map((lambda lbl: cast(hydra.json.model.Value, hydra.json.model.ValueString(lbl.value))), v.label)), ("inE", edge_map_to_json(False, v.in_edges)), ("outE", edge_map_to_json(True, v.out_edges)), ("properties", vertex_property_map_to_json(v.properties))))
