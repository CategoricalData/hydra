package hydra.json.yaml.decode

import hydra.ext.org.yaml.model.*

import hydra.json.model.*

import hydra.lib.eithers

import hydra.lib.literals

import hydra.lib.maps

import hydra.lib.pairs

def yamlToJson(node: hydra.ext.org.yaml.model.Node): Either[scala.Predef.String, hydra.json.model.Value] =
  node match
  case hydra.ext.org.yaml.model.Node.mapping(v_Node_mapping_m) => {
    def convertEntry(kv: Tuple2[hydra.ext.org.yaml.model.Node, hydra.ext.org.yaml.model.Node]): Either[scala.Predef.String,
       Tuple2[scala.Predef.String, hydra.json.model.Value]] =
      {
      val keyNode: hydra.ext.org.yaml.model.Node = pairs.first[hydra.ext.org.yaml.model.Node, hydra.ext.org.yaml.model.Node](kv)
      val valNode: hydra.ext.org.yaml.model.Node = pairs.second[hydra.ext.org.yaml.model.Node, hydra.ext.org.yaml.model.Node](kv)
      val keyResult: Either[scala.Predef.String, scala.Predef.String] = keyNode match
        case hydra.ext.org.yaml.model.Node.scalar(v_Node_scalar_s) => v_Node_scalar_s match
          case hydra.ext.org.yaml.model.Scalar.str(v_Scalar_str_str) => Right(v_Scalar_str_str)
          case _ => Left("non-string YAML mapping key")
        case _ => Left("non-scalar YAML mapping key")
      eithers.either[scala.Predef.String, scala.Predef.String, Either[scala.Predef.String, Tuple2[scala.Predef.String,
         hydra.json.model.Value]]]((err: scala.Predef.String) => Left(err))((key: scala.Predef.String) =>
        {
        val valResult: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.yaml.decode.yamlToJson(valNode)
        eithers.map[hydra.json.model.Value, Tuple2[scala.Predef.String, hydra.json.model.Value], scala.Predef.String]((v: hydra.json.model.Value) => Tuple2(key,
           v))(valResult)
      })(keyResult)
    }
    {
      val entries: Either[scala.Predef.String, Seq[Tuple2[scala.Predef.String, hydra.json.model.Value]]] = eithers.mapList[Tuple2[hydra.ext.org.yaml.model.Node,
         hydra.ext.org.yaml.model.Node], Tuple2[scala.Predef.String, hydra.json.model.Value], scala.Predef.String](convertEntry)(maps.toList[hydra.ext.org.yaml.model.Node,
         hydra.ext.org.yaml.model.Node](v_Node_mapping_m))
      eithers.map[Seq[Tuple2[scala.Predef.String, hydra.json.model.Value]], hydra.json.model.Value, scala.Predef.String]((es: Seq[Tuple2[scala.Predef.String,
         hydra.json.model.Value]]) =>
        hydra.json.model.Value.`object`(maps.fromList[scala.Predef.String, hydra.json.model.Value](es)))(entries)
    }
  }
  case hydra.ext.org.yaml.model.Node.scalar(v_Node_scalar_s) => v_Node_scalar_s match
    case hydra.ext.org.yaml.model.Scalar.bool(v_Scalar_bool_b) => Right(hydra.json.model.Value.boolean(v_Scalar_bool_b))
    case hydra.ext.org.yaml.model.Scalar.float(v_Scalar_float_f) => Right(hydra.json.model.Value.number(v_Scalar_float_f))
    case hydra.ext.org.yaml.model.Scalar.int(v_Scalar_int_i) => Right(hydra.json.model.Value.number(literals.bigintToBigfloat(v_Scalar_int_i)))
    case hydra.ext.org.yaml.model.Scalar.`null` => Right(hydra.json.model.Value.`null`)
    case hydra.ext.org.yaml.model.Scalar.str(v_Scalar_str_str) => Right(hydra.json.model.Value.string(v_Scalar_str_str))
  case hydra.ext.org.yaml.model.Node.sequence(v_Node_sequence_nodes) => {
    val results: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = eithers.mapList[hydra.ext.org.yaml.model.Node,
       hydra.json.model.Value, scala.Predef.String]((n: hydra.ext.org.yaml.model.Node) => hydra.json.yaml.decode.yamlToJson(n))(v_Node_sequence_nodes)
    eithers.map[Seq[hydra.json.model.Value], hydra.json.model.Value, scala.Predef.String]((vs: Seq[hydra.json.model.Value]) => hydra.json.model.Value.array(vs))(results)
  }

def fromYaml(types: Map[hydra.core.Name, hydra.core.Type])(tname: hydra.core.Name)(typ: hydra.core.Type)(node: hydra.ext.org.yaml.model.Node): Either[scala.Predef.String,
   hydra.core.Term] =
  {
  val jsonResult: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.yaml.decode.yamlToJson(node)
  eithers.either[scala.Predef.String, hydra.json.model.Value, Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((json: hydra.json.model.Value) => hydra.json.decode.fromJson(types)(tname)(typ)(json))(jsonResult)
}
