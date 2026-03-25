package hydra.util

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.context

import hydra.errors

case class Adapter[T1, T2, V1, V2](isLossy: Boolean, source: T1, target: T2, coder: hydra.util.Coder[V1, V2])

case class Bicoder[T1, T2, V1, V2](encode: (T1 => hydra.util.Adapter[T1, T2, V1, V2]), decode: (T2 => hydra.util.Adapter[T2, T1, V2, V1]))

enum CaseConvention :
   case camel extends CaseConvention
   case pascal extends CaseConvention
   case lowerSnake extends CaseConvention
   case upperSnake extends CaseConvention

case class Coder[V1, V2](encode: (hydra.context.Context => V1 => Either[hydra.context.InContext[hydra.errors.Error], V2]), decode: (hydra.context.Context => V2 => Either[hydra.context.InContext[hydra.errors.Error], V1]))

enum Comparison :
   case lessThan extends Comparison
   case equalTo extends Comparison
   case greaterThan extends Comparison

enum Precision :
   case arbitrary extends Precision
   case bits(value: Int) extends Precision
