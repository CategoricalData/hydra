package hydra.encode.parsing

import hydra.core.*

import hydra.parsing.*

def parseError(x: hydra.parsing.ParseError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.parsing.ParseError", Seq(hydra.core.Field("message",
     hydra.core.Term.literal(hydra.core.Literal.string(x.message))), hydra.core.Field("remainder", hydra.core.Term.literal(hydra.core.Literal.string(x.remainder))))))

def parseResult[T0](a: (T0 => hydra.core.Term))(v1: hydra.parsing.ParseResult[T0]): hydra.core.Term =
  v1 match
  case hydra.parsing.ParseResult.success(v_ParseResult_success_y) => hydra.core.Term.union(hydra.core.Injection("hydra.parsing.ParseResult",
     hydra.core.Field("success", hydra.encode.parsing.parseSuccess(a)(v_ParseResult_success_y))))
  case hydra.parsing.ParseResult.failure(v_ParseResult_failure_y) => hydra.core.Term.union(hydra.core.Injection("hydra.parsing.ParseResult",
     hydra.core.Field("failure", hydra.encode.parsing.parseError(v_ParseResult_failure_y))))

def parseSuccess[T0](a: (T0 => hydra.core.Term))(x: hydra.parsing.ParseSuccess[T0]): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.parsing.ParseSuccess", Seq(hydra.core.Field("value",
     a(x.value)), hydra.core.Field("remainder", hydra.core.Term.literal(hydra.core.Literal.string(x.remainder))))))
