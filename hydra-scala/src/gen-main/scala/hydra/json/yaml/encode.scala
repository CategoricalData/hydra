package hydra.json.yaml.encode

import hydra.ext.org.yaml.model.*

import hydra.json.model.*

import hydra.lib.eithers

import hydra.lib.lists

import hydra.lib.maps

import hydra.lib.pairs

def jsonToYaml(value: hydra.json.model.Value): hydra.ext.org.yaml.model.Node =
  value match
  case hydra.json.model.Value.array(v_Value_array_arr) => hydra.ext.org.yaml.model.Node.sequence(lists.map[hydra.json.model.Value,
     hydra.ext.org.yaml.model.Node]((v: hydra.json.model.Value) => hydra.json.yaml.encode.jsonToYaml(v))(v_Value_array_arr))
  case hydra.json.model.Value.boolean(v_Value_boolean_b) => hydra.ext.org.yaml.model.Node.scalar(hydra.ext.org.yaml.model.Scalar.bool(v_Value_boolean_b))
  case hydra.json.model.Value.`null` => hydra.ext.org.yaml.model.Node.scalar(hydra.ext.org.yaml.model.Scalar.`null`)
  case hydra.json.model.Value.number(v_Value_number_n) => hydra.ext.org.yaml.model.Node.scalar(hydra.ext.org.yaml.model.Scalar.float(v_Value_number_n))
  case hydra.json.model.Value.`object`(v_Value_object_obj) => hydra.ext.org.yaml.model.Node.mapping(maps.fromList[hydra.ext.org.yaml.model.Node,
     hydra.ext.org.yaml.model.Node](lists.map[Tuple2[scala.Predef.String, hydra.json.model.Value], Tuple2[hydra.ext.org.yaml.model.Node,
     hydra.ext.org.yaml.model.Node]]((kv: Tuple2[scala.Predef.String, hydra.json.model.Value]) =>
    Tuple2(hydra.ext.org.yaml.model.Node.scalar(hydra.ext.org.yaml.model.Scalar.str(pairs.first[scala.Predef.String,
       hydra.json.model.Value](kv))), hydra.json.yaml.encode.jsonToYaml(pairs.second[scala.Predef.String,
       hydra.json.model.Value](kv))))(maps.toList[scala.Predef.String, hydra.json.model.Value](v_Value_object_obj))))
  case hydra.json.model.Value.string(v_Value_string_s) => hydra.ext.org.yaml.model.Node.scalar(hydra.ext.org.yaml.model.Scalar.str(v_Value_string_s))

def toYaml(term: hydra.core.Term): Either[scala.Predef.String, hydra.ext.org.yaml.model.Node] =
  eithers.map[hydra.json.model.Value, hydra.ext.org.yaml.model.Node, scala.Predef.String]((v: hydra.json.model.Value) => hydra.json.yaml.encode.jsonToYaml(v))(hydra.json.encode.toJson(term))
