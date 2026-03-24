package hydra.show.util

import hydra.util.*

def caseConvention(c: hydra.util.CaseConvention): scala.Predef.String =
  c match
  case hydra.util.CaseConvention.lowerSnake() => "lower_snake_case"
  case hydra.util.CaseConvention.upperSnake() => "UPPER_SNAKE_CASE"
  case hydra.util.CaseConvention.camel() => "camelCase"
  case hydra.util.CaseConvention.pascal() => "PascalCase"
