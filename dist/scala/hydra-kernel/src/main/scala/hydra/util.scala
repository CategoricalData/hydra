package hydra.util

import hydra.core.*

enum CaseConvention :
   case camel extends CaseConvention
   case pascal extends CaseConvention
   case lowerSnake extends CaseConvention
   case upperSnake extends CaseConvention

enum Comparison :
   case lessThan extends Comparison
   case equalTo extends Comparison
   case greaterThan extends Comparison

enum Precision :
   case arbitrary extends Precision
   case bits(value: Int) extends Precision
