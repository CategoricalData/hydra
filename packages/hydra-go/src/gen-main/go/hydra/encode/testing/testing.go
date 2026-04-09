// Note: this is an automatically generated file. Do not edit.

package encodetesting

import (
  "hydra.dev/hydra/ast"
  "hydra.dev/hydra/coders"
  "hydra.dev/hydra/core"
  encodeast "hydra.dev/hydra/encode/ast"
  encodecoders "hydra.dev/hydra/encode/coders"
  encodecore "hydra.dev/hydra/encode/core"
  encodejsonmodel "hydra.dev/hydra/encode/json/model"
  encodeparsing "hydra.dev/hydra/encode/parsing"
  encodetyping "hydra.dev/hydra/encode/typing"
  encodeutil "hydra.dev/hydra/encode/util"
  jsonmodel "hydra.dev/hydra/json/model"
  libeithers "hydra.dev/hydra/lib/eithers"
  liblists "hydra.dev/hydra/lib/lists"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libpairs "hydra.dev/hydra/lib/pairs"
  libsets "hydra.dev/hydra/lib/sets"
  "hydra.dev/hydra/parsing"
  "hydra.dev/hydra/testing"
  "hydra.dev/hydra/util"
)

func AlphaConversionTestCase (x testing.AlphaConversionTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.AlphaConversionTestCase"), Fields: []any{core.Field{Name: core.Name("term"), Term: encodecore.Term(func (v any) any {
    return v.(testing.AlphaConversionTestCase).Term
  }(x).(core.Term))}, core.Field{Name: core.Name("oldVariable"), Term: encodecore.Name(func (v any) any {
    return v.(testing.AlphaConversionTestCase).OldVariable
  }(x).(core.Name))}, core.Field{Name: core.Name("newVariable"), Term: encodecore.Name(func (v any) any {
    return v.(testing.AlphaConversionTestCase).NewVariable
  }(x).(core.Name))}, core.Field{Name: core.Name("result"), Term: encodecore.Term(func (v any) any {
    return v.(testing.AlphaConversionTestCase).Result
  }(x).(core.Term))}}}}
}

func EvaluationStyle (v1 testing.EvaluationStyle) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case testing.EvaluationStyleEager:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.EvaluationStyle"), Field: core.Field{Name: core.Name("eager"), Term: core.TermUnit{}}}}
      }(v)
      case testing.EvaluationStyleLazy:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.EvaluationStyle"), Field: core.Field{Name: core.Name("lazy"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func CaseConversionTestCase (x testing.CaseConversionTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.CaseConversionTestCase"), Fields: []any{core.Field{Name: core.Name("fromConvention"), Term: encodeutil.CaseConvention(func (v any) any {
    return v.(testing.CaseConversionTestCase).FromConvention
  }(x).(util.CaseConvention))}, core.Field{Name: core.Name("toConvention"), Term: encodeutil.CaseConvention(func (v any) any {
    return v.(testing.CaseConversionTestCase).ToConvention
  }(x).(util.CaseConvention))}, core.Field{Name: core.Name("fromString"), Term: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v.(testing.CaseConversionTestCase).FromString
  }(x).(string)}}}, core.Field{Name: core.Name("toString"), Term: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v.(testing.CaseConversionTestCase).ToString
  }(x).(string)}}}}}}
}

func DelegatedEvaluationTestCase (x testing.DelegatedEvaluationTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.DelegatedEvaluationTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.DelegatedEvaluationTestCase).Input
  }(x).(core.Term))}, core.Field{Name: core.Name("output"), Term: encodecore.Term(func (v any) any {
    return v.(testing.DelegatedEvaluationTestCase).Output
  }(x).(core.Term))}}}}
}

func EtaExpansionTestCase (x testing.EtaExpansionTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.EtaExpansionTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.EtaExpansionTestCase).Input
  }(x).(core.Term))}, core.Field{Name: core.Name("output"), Term: encodecore.Term(func (v any) any {
    return v.(testing.EtaExpansionTestCase).Output
  }(x).(core.Term))}}}}
}

func DeannotateTermTestCase (x testing.DeannotateTermTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.DeannotateTermTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.DeannotateTermTestCase).Input
  }(x).(core.Term))}, core.Field{Name: core.Name("output"), Term: encodecore.Term(func (v any) any {
    return v.(testing.DeannotateTermTestCase).Output
  }(x).(core.Term))}}}}
}

func DeannotateTypeTestCase (x testing.DeannotateTypeTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.DeannotateTypeTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Type_(func (v any) any {
    return v.(testing.DeannotateTypeTestCase).Input
  }(x).(core.Type))}, core.Field{Name: core.Name("output"), Term: encodecore.Type_(func (v any) any {
    return v.(testing.DeannotateTypeTestCase).Output
  }(x).(core.Type))}}}}
}

func FlattenLetTermsTestCase (x testing.FlattenLetTermsTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.FlattenLetTermsTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.FlattenLetTermsTestCase).Input
  }(x).(core.Term))}, core.Field{Name: core.Name("output"), Term: encodecore.Term(func (v any) any {
    return v.(testing.FlattenLetTermsTestCase).Output
  }(x).(core.Term))}}}}
}

func FoldOperation (v1 testing.FoldOperation) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case testing.FoldOperationSumInt32Literals:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.FoldOperation"), Field: core.Field{Name: core.Name("sumInt32Literals"), Term: core.TermUnit{}}}}
      }(v)
      case testing.FoldOperationCollectListLengths:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.FoldOperation"), Field: core.Field{Name: core.Name("collectListLengths"), Term: core.TermUnit{}}}}
      }(v)
      case testing.FoldOperationCollectLabels:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.FoldOperation"), Field: core.Field{Name: core.Name("collectLabels"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func FoldOverTermTestCase (x testing.FoldOverTermTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.FoldOverTermTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.FoldOverTermTestCase).Input
  }(x).(core.Term))}, core.Field{Name: core.Name("traversalOrder"), Term: encodecoders.TraversalOrder(func (v any) any {
    return v.(testing.FoldOverTermTestCase).TraversalOrder
  }(x).(coders.TraversalOrder))}, core.Field{Name: core.Name("operation"), Term: FoldOperation(func (v any) any {
    return v.(testing.FoldOverTermTestCase).Operation
  }(x).(testing.FoldOperation))}, core.Field{Name: core.Name("output"), Term: encodecore.Term(func (v any) any {
    return v.(testing.FoldOverTermTestCase).Output
  }(x).(core.Term))}}}}
}

func FreeVariablesTestCase (x testing.FreeVariablesTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.FreeVariablesTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.FreeVariablesTestCase).Input
  }(x).(core.Term))}, core.Field{Name: core.Name("output"), Term: core.TermSet{Value: libsets.Map(encodecore.Name).(func(any) any)(func (v any) any {
    return v.(testing.FreeVariablesTestCase).Output
  }(x)).([]any)}}}}}
}

func HoistPredicate (v1 testing.HoistPredicate) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case testing.HoistPredicateCaseStatements:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.HoistPredicate"), Field: core.Field{Name: core.Name("caseStatements"), Term: core.TermUnit{}}}}
      }(v)
      case testing.HoistPredicateApplications:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.HoistPredicate"), Field: core.Field{Name: core.Name("applications"), Term: core.TermUnit{}}}}
      }(v)
      case testing.HoistPredicateLists:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.HoistPredicate"), Field: core.Field{Name: core.Name("lists"), Term: core.TermUnit{}}}}
      }(v)
      case testing.HoistPredicateNothing:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.HoistPredicate"), Field: core.Field{Name: core.Name("nothing"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func HoistLetBindingsTestCase (x testing.HoistLetBindingsTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.HoistLetBindingsTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Let(func (v any) any {
    return v.(testing.HoistLetBindingsTestCase).Input
  }(x).(core.Let))}, core.Field{Name: core.Name("output"), Term: encodecore.Let(func (v any) any {
    return v.(testing.HoistLetBindingsTestCase).Output
  }(x).(core.Let))}}}}
}

func HoistPolymorphicLetBindingsTestCase (x testing.HoistPolymorphicLetBindingsTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.HoistPolymorphicLetBindingsTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Let(func (v any) any {
    return v.(testing.HoistPolymorphicLetBindingsTestCase).Input
  }(x).(core.Let))}, core.Field{Name: core.Name("output"), Term: encodecore.Let(func (v any) any {
    return v.(testing.HoistPolymorphicLetBindingsTestCase).Output
  }(x).(core.Let))}}}}
}

func HoistSubtermsTestCase (x testing.HoistSubtermsTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.HoistSubtermsTestCase"), Fields: []any{core.Field{Name: core.Name("predicate"), Term: HoistPredicate(func (v any) any {
    return v.(testing.HoistSubtermsTestCase).Predicate
  }(x).(testing.HoistPredicate))}, core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.HoistSubtermsTestCase).Input
  }(x).(core.Term))}, core.Field{Name: core.Name("output"), Term: encodecore.Term(func (v any) any {
    return v.(testing.HoistSubtermsTestCase).Output
  }(x).(core.Term))}}}}
}

func HoistCaseStatementsTestCase (x testing.HoistCaseStatementsTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.HoistCaseStatementsTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.HoistCaseStatementsTestCase).Input
  }(x).(core.Term))}, core.Field{Name: core.Name("output"), Term: encodecore.Term(func (v any) any {
    return v.(testing.HoistCaseStatementsTestCase).Output
  }(x).(core.Term))}}}}
}

func TermRewriter (v1 testing.TermRewriter) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case testing.TermRewriterReplaceFooWithBar:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TermRewriter"), Field: core.Field{Name: core.Name("replaceFooWithBar"), Term: core.TermUnit{}}}}
      }(v)
      case testing.TermRewriterReplaceInt32WithInt64:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TermRewriter"), Field: core.Field{Name: core.Name("replaceInt32WithInt64"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func RewriteTermTestCase (x testing.RewriteTermTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.RewriteTermTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.RewriteTermTestCase).Input
  }(x).(core.Term))}, core.Field{Name: core.Name("rewriter"), Term: TermRewriter(func (v any) any {
    return v.(testing.RewriteTermTestCase).Rewriter
  }(x).(testing.TermRewriter))}, core.Field{Name: core.Name("output"), Term: encodecore.Term(func (v any) any {
    return v.(testing.RewriteTermTestCase).Output
  }(x).(core.Term))}}}}
}

func TypeRewriter (v1 testing.TypeRewriter) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case testing.TypeRewriterReplaceStringWithInt32:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TypeRewriter"), Field: core.Field{Name: core.Name("replaceStringWithInt32"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func RewriteTypeTestCase (x testing.RewriteTypeTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.RewriteTypeTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Type_(func (v any) any {
    return v.(testing.RewriteTypeTestCase).Input
  }(x).(core.Type))}, core.Field{Name: core.Name("rewriter"), Term: TypeRewriter(func (v any) any {
    return v.(testing.RewriteTypeTestCase).Rewriter
  }(x).(testing.TypeRewriter))}, core.Field{Name: core.Name("output"), Term: encodecore.Type_(func (v any) any {
    return v.(testing.RewriteTypeTestCase).Output
  }(x).(core.Type))}}}}
}

func EvaluationTestCase (x testing.EvaluationTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.EvaluationTestCase"), Fields: []any{core.Field{Name: core.Name("evaluationStyle"), Term: EvaluationStyle(func (v any) any {
    return v.(testing.EvaluationTestCase).EvaluationStyle
  }(x).(testing.EvaluationStyle))}, core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.EvaluationTestCase).Input
  }(x).(core.Term))}, core.Field{Name: core.Name("output"), Term: encodecore.Term(func (v any) any {
    return v.(testing.EvaluationTestCase).Output
  }(x).(core.Term))}}}}
}

func InferenceFailureTestCase (x testing.InferenceFailureTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.InferenceFailureTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.InferenceFailureTestCase).Input
  }(x).(core.Term))}}}}
}

func InferenceTestCase (x testing.InferenceTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.InferenceTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.InferenceTestCase).Input
  }(x).(core.Term))}, core.Field{Name: core.Name("output"), Term: encodecore.TypeScheme(func (v any) any {
    return v.(testing.InferenceTestCase).Output
  }(x).(core.TypeScheme))}}}}
}

func JsonDecodeTestCase (x testing.JsonDecodeTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.JsonDecodeTestCase"), Fields: []any{core.Field{Name: core.Name("type"), Term: encodecore.Type_(func (v any) any {
    return v.(testing.JsonDecodeTestCase).Type_
  }(x).(core.Type))}, core.Field{Name: core.Name("json"), Term: encodejsonmodel.Value(func (v any) any {
    return v.(testing.JsonDecodeTestCase).Json
  }(x).(jsonmodel.Value))}, core.Field{Name: core.Name("expected"), Term: core.TermEither{Value: libeithers.Bimap(func (x2 string) any {
    return core.TermLiteral{Value: core.LiteralString_{Value: x2}}
  }).(func(any) any)(encodecore.Term).(func(any) any)(func (v any) any {
    return v.(testing.JsonDecodeTestCase).Expected
  }(x))}}}}}
}

func JsonEncodeTestCase (x testing.JsonEncodeTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.JsonEncodeTestCase"), Fields: []any{core.Field{Name: core.Name("term"), Term: encodecore.Term(func (v any) any {
    return v.(testing.JsonEncodeTestCase).Term
  }(x).(core.Term))}, core.Field{Name: core.Name("expected"), Term: core.TermEither{Value: libeithers.Bimap(func (x2 string) any {
    return core.TermLiteral{Value: core.LiteralString_{Value: x2}}
  }).(func(any) any)(encodejsonmodel.Value).(func(any) any)(func (v any) any {
    return v.(testing.JsonEncodeTestCase).Expected
  }(x))}}}}}
}

func JsonParserTestCase (v1 testing.ParserTestCase[jsonmodel.Value]) core.Term {
  return ParserTestCase[jsonmodel.Value](encodejsonmodel.Value, v1)
}

func JsonRoundtripTestCase (x testing.JsonRoundtripTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.JsonRoundtripTestCase"), Fields: []any{core.Field{Name: core.Name("type"), Term: encodecore.Type_(func (v any) any {
    return v.(testing.JsonRoundtripTestCase).Type_
  }(x).(core.Type))}, core.Field{Name: core.Name("term"), Term: encodecore.Term(func (v any) any {
    return v.(testing.JsonRoundtripTestCase).Term
  }(x).(core.Term))}}}}
}

func LiftLambdaAboveLetTestCase (x testing.LiftLambdaAboveLetTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.LiftLambdaAboveLetTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.LiftLambdaAboveLetTestCase).Input
  }(x).(core.Term))}, core.Field{Name: core.Name("output"), Term: encodecore.Term(func (v any) any {
    return v.(testing.LiftLambdaAboveLetTestCase).Output
  }(x).(core.Term))}}}}
}

func JsonWriterTestCase (v1 testing.WriterTestCase[jsonmodel.Value]) core.Term {
  return WriterTestCase[jsonmodel.Value](encodejsonmodel.Value, v1)
}

func ParserTestCase[T0 any] (a func(T0) core.Term, x testing.ParserTestCase[T0]) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.ParserTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v.(testing.ParserTestCase[T0]).Input
  }(x).(string)}}}, core.Field{Name: core.Name("output"), Term: encodeparsing.ParseResult[T0](a, func (v any) any {
    return v.(testing.ParserTestCase[T0]).Output
  }(x).(parsing.ParseResult[T0]))}}}}
}

func Tag (x testing.Tag) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.testing.Tag"), Body: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v
  }(x).(string)}}}}
}

func TestCase (v1 testing.TestCase) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case testing.TestCaseAlphaConversion:
      return func (y testing.AlphaConversionTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("alphaConversion"), Term: AlphaConversionTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseCaseConversion:
      return func (y testing.CaseConversionTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("caseConversion"), Term: CaseConversionTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseDeannotateTerm:
      return func (y testing.DeannotateTermTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("deannotateTerm"), Term: DeannotateTermTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseDeannotateType:
      return func (y testing.DeannotateTypeTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("deannotateType"), Term: DeannotateTypeTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseDelegatedEvaluation:
      return func (y testing.DelegatedEvaluationTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("delegatedEvaluation"), Term: DelegatedEvaluationTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseEtaExpansion:
      return func (y testing.EtaExpansionTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("etaExpansion"), Term: EtaExpansionTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseFlattenLetTerms:
      return func (y testing.FlattenLetTermsTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("flattenLetTerms"), Term: FlattenLetTermsTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseFreeVariables:
      return func (y testing.FreeVariablesTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("freeVariables"), Term: FreeVariablesTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseEvaluation:
      return func (y testing.EvaluationTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("evaluation"), Term: EvaluationTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseInference:
      return func (y testing.InferenceTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("inference"), Term: InferenceTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseInferenceFailure:
      return func (y testing.InferenceFailureTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("inferenceFailure"), Term: InferenceFailureTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseJsonDecode:
      return func (y testing.JsonDecodeTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("jsonDecode"), Term: JsonDecodeTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseJsonEncode:
      return func (y testing.JsonEncodeTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("jsonEncode"), Term: JsonEncodeTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseJsonParser:
      return func (y testing.ParserTestCase[jsonmodel.Value]) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("jsonParser"), Term: JsonParserTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseJsonRoundtrip:
      return func (y testing.JsonRoundtripTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("jsonRoundtrip"), Term: JsonRoundtripTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseJsonWriter:
      return func (y testing.WriterTestCase[jsonmodel.Value]) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("jsonWriter"), Term: JsonWriterTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseLiftLambdaAboveLet:
      return func (y testing.LiftLambdaAboveLetTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("liftLambdaAboveLet"), Term: LiftLambdaAboveLetTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseSerialization:
      return func (y testing.SerializationTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("serialization"), Term: SerializationTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseSimplifyTerm:
      return func (y testing.SimplifyTermTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("simplifyTerm"), Term: SimplifyTermTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseTopologicalSort:
      return func (y testing.TopologicalSortTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("topologicalSort"), Term: TopologicalSortTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseTopologicalSortBindings:
      return func (y testing.TopologicalSortBindingsTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("topologicalSortBindings"), Term: TopologicalSortBindingsTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseTopologicalSortSCC:
      return func (y testing.TopologicalSortSCCTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("topologicalSortSCC"), Term: TopologicalSortSCCTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseTypeChecking:
      return func (y testing.TypeCheckingTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("typeChecking"), Term: TypeCheckingTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseTypeCheckingFailure:
      return func (y testing.TypeCheckingFailureTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("typeCheckingFailure"), Term: TypeCheckingFailureTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseTypeReduction:
      return func (y testing.TypeReductionTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("typeReduction"), Term: TypeReductionTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseNormalizeTypeVariables:
      return func (y testing.NormalizeTypeVariablesTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("normalizeTypeVariables"), Term: NormalizeTypeVariablesTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseFoldOverTerm:
      return func (y testing.FoldOverTermTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("foldOverTerm"), Term: FoldOverTermTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseRewriteTerm:
      return func (y testing.RewriteTermTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("rewriteTerm"), Term: RewriteTermTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseRewriteType:
      return func (y testing.RewriteTypeTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("rewriteType"), Term: RewriteTypeTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseHoistSubterms:
      return func (y testing.HoistSubtermsTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("hoistSubterms"), Term: HoistSubtermsTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseHoistCaseStatements:
      return func (y testing.HoistCaseStatementsTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("hoistCaseStatements"), Term: HoistCaseStatementsTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseHoistLetBindings:
      return func (y testing.HoistLetBindingsTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("hoistLetBindings"), Term: HoistLetBindingsTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseHoistPolymorphicLetBindings:
      return func (y testing.HoistPolymorphicLetBindingsTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("hoistPolymorphicLetBindings"), Term: HoistPolymorphicLetBindingsTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseSubstInType:
      return func (y testing.SubstInTypeTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("substInType"), Term: SubstInTypeTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseVariableOccursInType:
      return func (y testing.VariableOccursInTypeTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("variableOccursInType"), Term: VariableOccursInTypeTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseUnifyTypes:
      return func (y testing.UnifyTypesTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("unifyTypes"), Term: UnifyTypesTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseJoinTypes:
      return func (y testing.JoinTypesTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("joinTypes"), Term: JoinTypesTestCase(y)}}}
      }(v.Value)
      case testing.TestCaseUnshadowVariables:
      return func (y testing.UnshadowVariablesTestCase) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.testing.TestCase"), Field: core.Field{Name: core.Name("unshadowVariables"), Term: UnshadowVariablesTestCase(y)}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func TestCaseWithMetadata (x testing.TestCaseWithMetadata) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.TestCaseWithMetadata"), Fields: []any{core.Field{Name: core.Name("name"), Term: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v.(testing.TestCaseWithMetadata).Name
  }(x).(string)}}}, core.Field{Name: core.Name("case"), Term: TestCase(func (v any) any {
    return v.(testing.TestCaseWithMetadata).Case_
  }(x).(testing.TestCase))}, core.Field{Name: core.Name("description"), Term: core.TermMaybe{Value: libmaybes.Map(func (x2 string) any {
    return core.TermLiteral{Value: core.LiteralString_{Value: x2}}
  }).(func(any) any)(func (v any) any {
    return v.(testing.TestCaseWithMetadata).Description
  }(x))}}, core.Field{Name: core.Name("tags"), Term: core.TermList{Value: liblists.Map(Tag).(func(any) any)(func (v any) any {
    return v.(testing.TestCaseWithMetadata).Tags
  }(x)).([]any)}}}}}
}

func TestGroup (x testing.TestGroup) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.TestGroup"), Fields: []any{core.Field{Name: core.Name("name"), Term: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v.(testing.TestGroup).Name
  }(x).(string)}}}, core.Field{Name: core.Name("description"), Term: core.TermMaybe{Value: libmaybes.Map(func (x2 string) any {
    return core.TermLiteral{Value: core.LiteralString_{Value: x2}}
  }).(func(any) any)(func (v any) any {
    return v.(testing.TestGroup).Description
  }(x))}}, core.Field{Name: core.Name("subgroups"), Term: core.TermList{Value: liblists.Map(TestGroup).(func(any) any)(func (v any) any {
    return v.(testing.TestGroup).Subgroups
  }(x)).([]any)}}, core.Field{Name: core.Name("cases"), Term: core.TermList{Value: liblists.Map(TestCaseWithMetadata).(func(any) any)(func (v any) any {
    return v.(testing.TestGroup).Cases
  }(x)).([]any)}}}}}
}

func TypeCheckingTestCase (x testing.TypeCheckingTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.TypeCheckingTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.TypeCheckingTestCase).Input
  }(x).(core.Term))}, core.Field{Name: core.Name("outputTerm"), Term: encodecore.Term(func (v any) any {
    return v.(testing.TypeCheckingTestCase).OutputTerm
  }(x).(core.Term))}, core.Field{Name: core.Name("outputType"), Term: encodecore.Type_(func (v any) any {
    return v.(testing.TypeCheckingTestCase).OutputType
  }(x).(core.Type))}}}}
}

func TypeCheckingFailureTestCase (x testing.TypeCheckingFailureTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.TypeCheckingFailureTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.TypeCheckingFailureTestCase).Input
  }(x).(core.Term))}}}}
}

func TopologicalSortBindingsTestCase (x testing.TopologicalSortBindingsTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.TopologicalSortBindingsTestCase"), Fields: []any{core.Field{Name: core.Name("bindings"), Term: core.TermList{Value: liblists.Map(func (p any) any {
    return core.TermPair{Value: libpairs.Bimap(encodecore.Name).(func(any) any)(encodecore.Term).(func(any) any)(p)}
  }).(func(any) any)(func (v any) any {
    return v.(testing.TopologicalSortBindingsTestCase).Bindings
  }(x)).([]any)}}, core.Field{Name: core.Name("expected"), Term: core.TermList{Value: liblists.Map(func (xs2 []any) any {
    return core.TermList{Value: liblists.Map(func (p any) any {
      return core.TermPair{Value: libpairs.Bimap(encodecore.Name).(func(any) any)(encodecore.Term).(func(any) any)(p)}
    }).(func(any) any)(xs2).([]any)}
  }).(func(any) any)(func (v any) any {
    return v.(testing.TopologicalSortBindingsTestCase).Expected
  }(x)).([]any)}}}}}
}

func TopologicalSortTestCase (x testing.TopologicalSortTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.TopologicalSortTestCase"), Fields: []any{core.Field{Name: core.Name("adjacencyList"), Term: core.TermList{Value: liblists.Map(func (p any) any {
    return core.TermPair{Value: libpairs.Bimap(func (x2 int32) any {
      return core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: x2}}}
    }).(func(any) any)(func (xs2 []any) any {
      return core.TermList{Value: liblists.Map(func (x2 int32) any {
        return core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: x2}}}
      }).(func(any) any)(xs2).([]any)}
    }).(func(any) any)(p)}
  }).(func(any) any)(func (v any) any {
    return v.(testing.TopologicalSortTestCase).AdjacencyList
  }(x)).([]any)}}, core.Field{Name: core.Name("expected"), Term: core.TermEither{Value: libeithers.Bimap(func (xs []any) any {
    return core.TermList{Value: liblists.Map(func (xs2 []any) any {
      return core.TermList{Value: liblists.Map(func (x2 int32) any {
        return core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: x2}}}
      }).(func(any) any)(xs2).([]any)}
    }).(func(any) any)(xs).([]any)}
  }).(func(any) any)(func (xs []any) any {
    return core.TermList{Value: liblists.Map(func (x2 int32) any {
      return core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: x2}}}
    }).(func(any) any)(xs).([]any)}
  }).(func(any) any)(func (v any) any {
    return v.(testing.TopologicalSortTestCase).Expected
  }(x))}}}}}
}

func TopologicalSortSCCTestCase (x testing.TopologicalSortSCCTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.TopologicalSortSCCTestCase"), Fields: []any{core.Field{Name: core.Name("adjacencyList"), Term: core.TermList{Value: liblists.Map(func (p any) any {
    return core.TermPair{Value: libpairs.Bimap(func (x2 int32) any {
      return core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: x2}}}
    }).(func(any) any)(func (xs2 []any) any {
      return core.TermList{Value: liblists.Map(func (x2 int32) any {
        return core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: x2}}}
      }).(func(any) any)(xs2).([]any)}
    }).(func(any) any)(p)}
  }).(func(any) any)(func (v any) any {
    return v.(testing.TopologicalSortSCCTestCase).AdjacencyList
  }(x)).([]any)}}, core.Field{Name: core.Name("expected"), Term: core.TermList{Value: liblists.Map(func (xs2 []any) any {
    return core.TermList{Value: liblists.Map(func (x2 int32) any {
      return core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: x2}}}
    }).(func(any) any)(xs2).([]any)}
  }).(func(any) any)(func (v any) any {
    return v.(testing.TopologicalSortSCCTestCase).Expected
  }(x)).([]any)}}}}}
}

func SerializationTestCase (x testing.SerializationTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.SerializationTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodeast.Expr(func (v any) any {
    return v.(testing.SerializationTestCase).Input
  }(x).(ast.Expr))}, core.Field{Name: core.Name("output"), Term: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v.(testing.SerializationTestCase).Output
  }(x).(string)}}}}}}
}

func SimplifyTermTestCase (x testing.SimplifyTermTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.SimplifyTermTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.SimplifyTermTestCase).Input
  }(x).(core.Term))}, core.Field{Name: core.Name("output"), Term: encodecore.Term(func (v any) any {
    return v.(testing.SimplifyTermTestCase).Output
  }(x).(core.Term))}}}}
}

func NormalizeTypeVariablesTestCase (x testing.NormalizeTypeVariablesTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.NormalizeTypeVariablesTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.NormalizeTypeVariablesTestCase).Input
  }(x).(core.Term))}, core.Field{Name: core.Name("output"), Term: encodecore.Term(func (v any) any {
    return v.(testing.NormalizeTypeVariablesTestCase).Output
  }(x).(core.Term))}}}}
}

func TypeReductionTestCase (x testing.TypeReductionTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.TypeReductionTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Type_(func (v any) any {
    return v.(testing.TypeReductionTestCase).Input
  }(x).(core.Type))}, core.Field{Name: core.Name("output"), Term: encodecore.Type_(func (v any) any {
    return v.(testing.TypeReductionTestCase).Output
  }(x).(core.Type))}}}}
}

func WriterTestCase[T0 any] (a func(T0) core.Term, x testing.WriterTestCase[T0]) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.WriterTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: a(func (v any) any {
    return v.(testing.WriterTestCase[T0]).Input
  }(x).(T0))}, core.Field{Name: core.Name("output"), Term: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v.(testing.WriterTestCase[T0]).Output
  }(x).(string)}}}}}}
}

func SubstInTypeTestCase (x testing.SubstInTypeTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.SubstInTypeTestCase"), Fields: []any{core.Field{Name: core.Name("substitution"), Term: core.TermList{Value: liblists.Map(func (p any) any {
    return core.TermPair{Value: libpairs.Bimap(encodecore.Name).(func(any) any)(encodecore.Type_).(func(any) any)(p)}
  }).(func(any) any)(func (v any) any {
    return v.(testing.SubstInTypeTestCase).Substitution
  }(x)).([]any)}}, core.Field{Name: core.Name("input"), Term: encodecore.Type_(func (v any) any {
    return v.(testing.SubstInTypeTestCase).Input
  }(x).(core.Type))}, core.Field{Name: core.Name("output"), Term: encodecore.Type_(func (v any) any {
    return v.(testing.SubstInTypeTestCase).Output
  }(x).(core.Type))}}}}
}

func VariableOccursInTypeTestCase (x testing.VariableOccursInTypeTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.VariableOccursInTypeTestCase"), Fields: []any{core.Field{Name: core.Name("variable"), Term: encodecore.Name(func (v any) any {
    return v.(testing.VariableOccursInTypeTestCase).Variable
  }(x).(core.Name))}, core.Field{Name: core.Name("type"), Term: encodecore.Type_(func (v any) any {
    return v.(testing.VariableOccursInTypeTestCase).Type_
  }(x).(core.Type))}, core.Field{Name: core.Name("expected"), Term: core.TermLiteral{Value: core.LiteralBoolean{Value: func (v any) any {
    return v.(testing.VariableOccursInTypeTestCase).Expected
  }(x).(bool)}}}}}}
}

func UnshadowVariablesTestCase (x testing.UnshadowVariablesTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.UnshadowVariablesTestCase"), Fields: []any{core.Field{Name: core.Name("input"), Term: encodecore.Term(func (v any) any {
    return v.(testing.UnshadowVariablesTestCase).Input
  }(x).(core.Term))}, core.Field{Name: core.Name("output"), Term: encodecore.Term(func (v any) any {
    return v.(testing.UnshadowVariablesTestCase).Output
  }(x).(core.Term))}}}}
}

func UnifyTypesTestCase (x testing.UnifyTypesTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.UnifyTypesTestCase"), Fields: []any{core.Field{Name: core.Name("schemaTypes"), Term: core.TermList{Value: liblists.Map(encodecore.Name).(func(any) any)(func (v any) any {
    return v.(testing.UnifyTypesTestCase).SchemaTypes
  }(x)).([]any)}}, core.Field{Name: core.Name("left"), Term: encodecore.Type_(func (v any) any {
    return v.(testing.UnifyTypesTestCase).Left
  }(x).(core.Type))}, core.Field{Name: core.Name("right"), Term: encodecore.Type_(func (v any) any {
    return v.(testing.UnifyTypesTestCase).Right
  }(x).(core.Type))}, core.Field{Name: core.Name("expected"), Term: core.TermEither{Value: libeithers.Bimap(func (x2 string) any {
    return core.TermLiteral{Value: core.LiteralString_{Value: x2}}
  }).(func(any) any)(encodetyping.TypeSubst).(func(any) any)(func (v any) any {
    return v.(testing.UnifyTypesTestCase).Expected
  }(x))}}}}}
}

func JoinTypesTestCase (x testing.JoinTypesTestCase) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.testing.JoinTypesTestCase"), Fields: []any{core.Field{Name: core.Name("left"), Term: encodecore.Type_(func (v any) any {
    return v.(testing.JoinTypesTestCase).Left
  }(x).(core.Type))}, core.Field{Name: core.Name("right"), Term: encodecore.Type_(func (v any) any {
    return v.(testing.JoinTypesTestCase).Right
  }(x).(core.Type))}, core.Field{Name: core.Name("expected"), Term: core.TermEither{Value: libeithers.Bimap(func (_ struct{}) any {
    return core.TermUnit{}
  }).(func(any) any)(func (xs []any) any {
    return core.TermList{Value: liblists.Map(encodetyping.TypeConstraint).(func(any) any)(xs).([]any)}
  }).(func(any) any)(func (v any) any {
    return v.(testing.JoinTypesTestCase).Expected
  }(x))}}}}}
}
