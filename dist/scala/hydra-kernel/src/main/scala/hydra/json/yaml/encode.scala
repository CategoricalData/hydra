package hydra.json.yaml.encode

import hydra.json.model.*

import hydra.yaml.model.*

def jsonToYaml(value: hydra.json.model.Value): hydra.yaml.model.Node =
  value match
  case hydra.json.model.Value.array(v_Value_array_arr) => hydra.yaml.model.Node.sequence(hydra.lib.lists.map[hydra.json.model.Value,
     hydra.yaml.model.Node]((v: hydra.json.model.Value) => hydra.json.yaml.encode.jsonToYaml(v))(v_Value_array_arr))
  case hydra.json.model.Value.boolean(v_Value_boolean_b) => hydra.yaml.model.Node.scalar(hydra.yaml.model.Scalar.bool(v_Value_boolean_b))
  case hydra.json.model.Value.`null` => hydra.yaml.model.Node.scalar(hydra.yaml.model.Scalar.`null`)
  case hydra.json.model.Value.number(v_Value_number_n) => hydra.yaml.model.Node.scalar(hydra.yaml.model.Scalar.float(v_Value_number_n))
  case hydra.json.model.Value.`object`(v_Value_object_obj) => hydra.yaml.model.Node.mapping(hydra.lib.maps.fromList[hydra.yaml.model.Node,
     hydra.yaml.model.Node](hydra.lib.lists.map[Tuple2[scala.Predef.String, hydra.json.model.Value], Tuple2[hydra.yaml.model.Node,
     hydra.yaml.model.Node]]((kv: Tuple2[scala.Predef.String, hydra.json.model.Value]) =>
    Tuple2(hydra.yaml.model.Node.scalar(hydra.yaml.model.Scalar.str(hydra.lib.pairs.first[scala.Predef.String,
       hydra.json.model.Value](kv))), hydra.json.yaml.encode.jsonToYaml(hydra.lib.pairs.second[scala.Predef.String,
       hydra.json.model.Value](kv))))(hydra.lib.maps.toList[scala.Predef.String, hydra.json.model.Value](v_Value_object_obj))))
  case hydra.json.model.Value.string(v_Value_string_s) => hydra.yaml.model.Node.scalar(hydra.yaml.model.Scalar.str(v_Value_string_s))

def toYaml(types: Map[hydra.core.Name, hydra.core.Type])(tname: hydra.core.Name)(typ: hydra.core.Type)(term: hydra.core.Term): Either[scala.Predef.String,
   hydra.yaml.model.Node] =
  hydra.lib.eithers.map[hydra.json.model.Value, hydra.yaml.model.Node, scala.Predef.String]((v: hydra.json.model.Value) => hydra.json.yaml.encode.jsonToYaml(v))(hydra.json.encode.toJson(types)(tname)(typ)(term))
