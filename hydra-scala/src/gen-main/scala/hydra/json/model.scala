package hydra.json.model

import hydra.core.*

enum Value :
   case array(value: Seq[hydra.json.model.Value]) extends Value
   case boolean(value: Boolean) extends Value
   case `null` extends Value
   case number(value: BigDecimal) extends Value
   case `object`(value: Map[scala.Predef.String, hydra.json.model.Value]) extends Value
   case string(value: scala.Predef.String) extends Value
