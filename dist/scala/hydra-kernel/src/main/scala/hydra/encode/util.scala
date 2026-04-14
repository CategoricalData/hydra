package hydra.encode.util

import hydra.core.*

import hydra.util.*

def caseConvention(v1: hydra.util.CaseConvention): hydra.core.Term =
  v1 match
  case hydra.util.CaseConvention.camel => hydra.core.Term.inject(hydra.core.Injection("hydra.util.CaseConvention",
     hydra.core.Field("camel", hydra.core.Term.unit)))
  case hydra.util.CaseConvention.pascal => hydra.core.Term.inject(hydra.core.Injection("hydra.util.CaseConvention",
     hydra.core.Field("pascal", hydra.core.Term.unit)))
  case hydra.util.CaseConvention.lowerSnake => hydra.core.Term.inject(hydra.core.Injection("hydra.util.CaseConvention",
     hydra.core.Field("lowerSnake", hydra.core.Term.unit)))
  case hydra.util.CaseConvention.upperSnake => hydra.core.Term.inject(hydra.core.Injection("hydra.util.CaseConvention",
     hydra.core.Field("upperSnake", hydra.core.Term.unit)))

def comparison(v1: hydra.util.Comparison): hydra.core.Term =
  v1 match
  case hydra.util.Comparison.lessThan => hydra.core.Term.inject(hydra.core.Injection("hydra.util.Comparison",
     hydra.core.Field("lessThan", hydra.core.Term.unit)))
  case hydra.util.Comparison.equalTo => hydra.core.Term.inject(hydra.core.Injection("hydra.util.Comparison", hydra.core.Field("equalTo", hydra.core.Term.unit)))
  case hydra.util.Comparison.greaterThan => hydra.core.Term.inject(hydra.core.Injection("hydra.util.Comparison",
     hydra.core.Field("greaterThan", hydra.core.Term.unit)))

def precision(v1: hydra.util.Precision): hydra.core.Term =
  v1 match
  case hydra.util.Precision.arbitrary => hydra.core.Term.inject(hydra.core.Injection("hydra.util.Precision",
     hydra.core.Field("arbitrary", hydra.core.Term.unit)))
  case hydra.util.Precision.bits(v_Precision_bits_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.util.Precision",
     hydra.core.Field("bits", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(v_Precision_bits_y))))))
