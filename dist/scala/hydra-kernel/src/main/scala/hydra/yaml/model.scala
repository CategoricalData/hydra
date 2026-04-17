package hydra.yaml.model

import hydra.core.*

enum Node :
   case mapping(value: Map[hydra.yaml.model.Node, hydra.yaml.model.Node]) extends Node
   case scalar(value: hydra.yaml.model.Scalar) extends Node
   case sequence(value: Seq[hydra.yaml.model.Node]) extends Node

enum Scalar :
   case bool(value: Boolean) extends Scalar
   case decimal(value: BigDecimal) extends Scalar
   case float(value: BigDecimal) extends Scalar
   case int(value: BigInt) extends Scalar
   case `null` extends Scalar
   case str(value: scala.Predef.String) extends Scalar
