package hydra.encode.json.model

import hydra.core.*

import hydra.json.model.*

import hydra.lib.lists

import hydra.lib.maps

def value(v1: hydra.json.model.Value): hydra.core.Term =
  v1 match
  case hydra.json.model.Value.array(v_Value_array_y) => hydra.core.Term.union(hydra.core.Injection("hydra.json.model.Value", hydra.core.Field("array", hydra.core.Term.list(hydra.lib.lists.map[hydra.json.model.Value, hydra.core.Term](hydra.encode.json.model.value)(v_Value_array_y)))))
  case hydra.json.model.Value.boolean(v_Value_boolean_y) => hydra.core.Term.union(hydra.core.Injection("hydra.json.model.Value", hydra.core.Field("boolean", hydra.core.Term.literal(hydra.core.Literal.boolean(v_Value_boolean_y)))))
  case hydra.json.model.Value.`null`() => hydra.core.Term.union(hydra.core.Injection("hydra.json.model.Value", hydra.core.Field("null", hydra.core.Term.unit)))
  case hydra.json.model.Value.number(v_Value_number_y) => hydra.core.Term.union(hydra.core.Injection("hydra.json.model.Value", hydra.core.Field("number", hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(v_Value_number_y))))))
  case hydra.json.model.Value.`object`(v_Value_object_y) => hydra.core.Term.union(hydra.core.Injection("hydra.json.model.Value", hydra.core.Field("object", hydra.core.Term.map(hydra.lib.maps.bimap[scala.Predef.String, hydra.core.Term, hydra.json.model.Value, hydra.core.Term]((x: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(x)))(hydra.encode.json.model.value)(v_Value_object_y)))))
  case hydra.json.model.Value.string(v_Value_string_y) => hydra.core.Term.union(hydra.core.Injection("hydra.json.model.Value", hydra.core.Field("string", hydra.core.Term.literal(hydra.core.Literal.string(v_Value_string_y)))))
