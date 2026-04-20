package hydra.json.yaml.decode

import hydra.json.model.*

import hydra.yaml.model.*

def fromYaml(types: Map[hydra.core.Name, hydra.core.Type])(tname: hydra.core.Name)(typ: hydra.core.Type)(node: hydra.yaml.model.Node): Either[scala.Predef.String,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term] =
  {
  lazy val jsonResult: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.yaml.decode.yamlToJson(node)
  hydra.lib.eithers.either[scala.Predef.String, hydra.json.model.Value, Either[scala.Predef.String,
     hydra.core.Term]]((err: scala.Predef.String) => Left(err))((json: hydra.json.model.Value) => hydra.json.decode.fromJson(types)(tname)(typ)(json))(jsonResult)
}

def yamlToJson(node: hydra.yaml.model.Node): Either[scala.Predef.String, hydra.json.model.Value] =
  node match
  case hydra.yaml.model.Node.mapping(v_Node_mapping_m) => {
    def convertEntry(kv: Tuple2[hydra.yaml.model.Node, hydra.yaml.model.Node]): Either[scala.Predef.String,
       Tuple2[scala.Predef.String, hydra.json.model.Value]] =
      {
      lazy val keyNode: hydra.yaml.model.Node = hydra.lib.pairs.first[hydra.yaml.model.Node, hydra.yaml.model.Node](kv)
      lazy val valNode: hydra.yaml.model.Node = hydra.lib.pairs.second[hydra.yaml.model.Node, hydra.yaml.model.Node](kv)
      lazy val keyResult: Either[scala.Predef.String, scala.Predef.String] = keyNode match
        case hydra.yaml.model.Node.scalar(v_Node_scalar_s) => v_Node_scalar_s match
          case hydra.yaml.model.Scalar.str(v_Scalar_str_str) => Right(v_Scalar_str_str)
          case _ => Left("non-string YAML mapping key")
        case _ => Left("non-scalar YAML mapping key")
      hydra.lib.eithers.either[scala.Predef.String, scala.Predef.String, Either[scala.Predef.String,
         Tuple2[scala.Predef.String, hydra.json.model.Value]]]((err: scala.Predef.String) => Left(err))((key: scala.Predef.String) =>
        {
        lazy val valResult: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.yaml.decode.yamlToJson(valNode)
        hydra.lib.eithers.map[hydra.json.model.Value, Tuple2[scala.Predef.String,
           hydra.json.model.Value], scala.Predef.String]((v: hydra.json.model.Value) => Tuple2(key,
           v))(valResult)
      })(keyResult)
    }
    {
      lazy val entries: Either[scala.Predef.String, Seq[Tuple2[scala.Predef.String,
         hydra.json.model.Value]]] = hydra.lib.eithers.mapList[Tuple2[hydra.yaml.model.Node,
         hydra.yaml.model.Node], Tuple2[scala.Predef.String, hydra.json.model.Value],
         scala.Predef.String](convertEntry)(hydra.lib.maps.toList[hydra.yaml.model.Node,
         hydra.yaml.model.Node](v_Node_mapping_m))
      hydra.lib.eithers.map[Seq[Tuple2[scala.Predef.String, hydra.json.model.Value]],
         hydra.json.model.Value, scala.Predef.String]((es: Seq[Tuple2[scala.Predef.String,
         hydra.json.model.Value]]) =>
        hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String,
           hydra.json.model.Value](es)))(entries)
    }
  }
  case hydra.yaml.model.Node.scalar(v_Node_scalar_s) => v_Node_scalar_s match
    case hydra.yaml.model.Scalar.bool(v_Scalar_bool_b) => Right(hydra.json.model.Value.boolean(v_Scalar_bool_b))
    case hydra.yaml.model.Scalar.decimal(v_Scalar_decimal_d) => Right(hydra.json.model.Value.number(v_Scalar_decimal_d))
    case hydra.yaml.model.Scalar.float(v_Scalar_float_f) => Right(hydra.json.model.Value.number(hydra.lib.literals.float64ToDecimal(hydra.lib.literals.bigfloatToFloat64(v_Scalar_float_f))))
    case hydra.yaml.model.Scalar.int(v_Scalar_int_i) => Right(hydra.json.model.Value.number(hydra.lib.literals.bigintToDecimal(v_Scalar_int_i)))
    case hydra.yaml.model.Scalar.`null` => Right(hydra.json.model.Value.`null`)
    case hydra.yaml.model.Scalar.str(v_Scalar_str_str) => Right(hydra.json.model.Value.string(v_Scalar_str_str))
  case hydra.yaml.model.Node.sequence(v_Node_sequence_nodes) => {
    lazy val results: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = hydra.lib.eithers.mapList[hydra.yaml.model.Node,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.json.model.Value, scala.Predef.String]((n: hydra.yaml.model.Node) => hydra.json.yaml.decode.yamlToJson(n))(v_Node_sequence_nodes)
    hydra.lib.eithers.map[Seq[hydra.json.model.Value], hydra.json.model.Value, scala.Predef.String]((vs: Seq[hydra.json.model.Value]) => hydra.json.model.Value.array(vs))(results)
  }
