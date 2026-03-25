// Note: this is an automatically generated file. Do not edit.

package hydra.encode;

/**
 * Term encoders for hydra.testing
 */
public interface Testing {
  static hydra.core.Term alphaConversionTestCase(hydra.testing.AlphaConversionTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("term"), hydra.encode.Core.term((x).term)),
      new hydra.core.Field(new hydra.core.Name("oldVariable"), hydra.encode.Core.name((x).oldVariable)),
      new hydra.core.Field(new hydra.core.Name("newVariable"), hydra.encode.Core.name((x).newVariable)),
      new hydra.core.Field(new hydra.core.Name("result"), hydra.encode.Core.term((x).result)))));
  }

  static hydra.core.Term caseConversionTestCase(hydra.testing.CaseConversionTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("fromConvention"), hydra.encode.Util.caseConvention((x).fromConvention)),
      new hydra.core.Field(new hydra.core.Name("toConvention"), hydra.encode.Util.caseConvention((x).toConvention)),
      new hydra.core.Field(new hydra.core.Name("fromString"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).fromString))),
      new hydra.core.Field(new hydra.core.Name("toString"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).toString))))));
  }

  static hydra.core.Term deannotateTermTestCase(hydra.testing.DeannotateTermTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.DeannotateTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.term((x).output)))));
  }

  static hydra.core.Term deannotateTypeTestCase(hydra.testing.DeannotateTypeTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.DeannotateTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.type((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.type((x).output)))));
  }

  static hydra.core.Term delegatedEvaluationTestCase(hydra.testing.DelegatedEvaluationTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.DelegatedEvaluationTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.term((x).output)))));
  }

  static hydra.core.Term etaExpansionTestCase(hydra.testing.EtaExpansionTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.EtaExpansionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.term((x).output)))));
  }

  static hydra.core.Term evaluationStyle(hydra.testing.EvaluationStyle v1) {
    return (v1).accept(new hydra.testing.EvaluationStyle.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.testing.EvaluationStyle.Eager y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.EvaluationStyle"), new hydra.core.Field(new hydra.core.Name("eager"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.EvaluationStyle.Lazy y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.EvaluationStyle"), new hydra.core.Field(new hydra.core.Name("lazy"), new hydra.core.Term.Unit())));
      }
    });
  }

  static hydra.core.Term evaluationTestCase(hydra.testing.EvaluationTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.EvaluationTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("evaluationStyle"), hydra.encode.Testing.evaluationStyle((x).evaluationStyle)),
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.term((x).output)))));
  }

  static hydra.core.Term flattenLetTermsTestCase(hydra.testing.FlattenLetTermsTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.FlattenLetTermsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.term((x).output)))));
  }

  static hydra.core.Term foldOperation(hydra.testing.FoldOperation v1) {
    return (v1).accept(new hydra.testing.FoldOperation.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.testing.FoldOperation.SumInt32Literals y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.FoldOperation"), new hydra.core.Field(new hydra.core.Name("sumInt32Literals"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.FoldOperation.CollectListLengths y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.FoldOperation"), new hydra.core.Field(new hydra.core.Name("collectListLengths"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.FoldOperation.CollectLabels y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.FoldOperation"), new hydra.core.Field(new hydra.core.Name("collectLabels"), new hydra.core.Term.Unit())));
      }
    });
  }

  static hydra.core.Term foldOverTermTestCase(hydra.testing.FoldOverTermTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("traversalOrder"), hydra.encode.Coders.traversalOrder((x).traversalOrder)),
      new hydra.core.Field(new hydra.core.Name("operation"), hydra.encode.Testing.foldOperation((x).operation)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.term((x).output)))));
  }

  static hydra.core.Term freeVariablesTestCase(hydra.testing.FreeVariablesTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.FreeVariablesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Set(hydra.lib.sets.Map.apply(
        hydra.encode.Core::name,
        (x).output))))));
  }

  static hydra.core.Term hoistCaseStatementsTestCase(hydra.testing.HoistCaseStatementsTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistCaseStatementsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.term((x).output)))));
  }

  static hydra.core.Term hoistLetBindingsTestCase(hydra.testing.HoistLetBindingsTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistLetBindingsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.let((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.let((x).output)))));
  }

  static hydra.core.Term hoistPolymorphicLetBindingsTestCase(hydra.testing.HoistPolymorphicLetBindingsTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistPolymorphicLetBindingsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.let((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.let((x).output)))));
  }

  static hydra.core.Term hoistPredicate(hydra.testing.HoistPredicate v1) {
    return (v1).accept(new hydra.testing.HoistPredicate.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.testing.HoistPredicate.CaseStatements y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.HoistPredicate"), new hydra.core.Field(new hydra.core.Name("caseStatements"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.HoistPredicate.Applications y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.HoistPredicate"), new hydra.core.Field(new hydra.core.Name("applications"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.HoistPredicate.Lists y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.HoistPredicate"), new hydra.core.Field(new hydra.core.Name("lists"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.HoistPredicate.Nothing y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.HoistPredicate"), new hydra.core.Field(new hydra.core.Name("nothing"), new hydra.core.Term.Unit())));
      }
    });
  }

  static hydra.core.Term hoistSubtermsTestCase(hydra.testing.HoistSubtermsTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistSubtermsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("predicate"), hydra.encode.Testing.hoistPredicate((x).predicate)),
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.term((x).output)))));
  }

  static hydra.core.Term inferenceFailureTestCase(hydra.testing.InferenceFailureTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.InferenceFailureTestCase"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)))));
  }

  static hydra.core.Term inferenceTestCase(hydra.testing.InferenceTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.InferenceTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.typeScheme((x).output)))));
  }

  static hydra.core.Term joinTypesTestCase(hydra.testing.JoinTypesTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JoinTypesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("left"), hydra.encode.Core.type((x).left)),
      new hydra.core.Field(new hydra.core.Name("right"), hydra.encode.Core.type((x).right)),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Either(hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<java.lang.Void, hydra.core.Term>) (ignored -> new hydra.core.Term.Unit()),
        (java.util.function.Function<hydra.util.ConsList<hydra.typing.TypeConstraint>, hydra.core.Term>) (xs -> new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          hydra.encode.Typing::typeConstraint,
          xs))),
        (x).expected))))));
  }

  static hydra.core.Term jsonDecodeTestCase(hydra.testing.JsonDecodeTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JsonDecodeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.Core.type((x).type)),
      new hydra.core.Field(new hydra.core.Name("json"), hydra.encode.json.Model.value((x).json)),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Either(hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<String, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(x2))),
        hydra.encode.Core::term,
        (x).expected))))));
  }

  static hydra.core.Term jsonEncodeTestCase(hydra.testing.JsonEncodeTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JsonEncodeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("term"), hydra.encode.Core.term((x).term)),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Either(hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<String, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(x2))),
        hydra.encode.json.Model::value,
        (x).expected))))));
  }

  static hydra.core.Term jsonParserTestCase(hydra.testing.ParserTestCase<hydra.json.model.Value> v1) {
    return hydra.encode.Testing.parserTestCase(
      hydra.encode.json.Model::value,
      v1);
  }

  static hydra.core.Term jsonRoundtripTestCase(hydra.testing.JsonRoundtripTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JsonRoundtripTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.Core.type((x).type)),
      new hydra.core.Field(new hydra.core.Name("term"), hydra.encode.Core.term((x).term)))));
  }

  static hydra.core.Term jsonWriterTestCase(hydra.testing.WriterTestCase<hydra.json.model.Value> v1) {
    return hydra.encode.Testing.writerTestCase(
      hydra.encode.json.Model::value,
      v1);
  }

  static hydra.core.Term liftLambdaAboveLetTestCase(hydra.testing.LiftLambdaAboveLetTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.LiftLambdaAboveLetTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.term((x).output)))));
  }

  static hydra.core.Term normalizeTypeVariablesTestCase(hydra.testing.NormalizeTypeVariablesTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.NormalizeTypeVariablesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.term((x).output)))));
  }

  static <T0> hydra.core.Term parserTestCase(java.util.function.Function<T0, hydra.core.Term> a, hydra.testing.ParserTestCase<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.ParserTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((java.util.function.Function<hydra.testing.ParserTestCase<T0>, String>) (projected -> projected.input)).apply(x)))),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Parsing.<T0>parseResult(
        a,
        ((java.util.function.Function<hydra.testing.ParserTestCase<T0>, hydra.parsing.ParseResult<T0>>) (projected -> projected.output)).apply(x))))));
  }

  static hydra.core.Term rewriteTermTestCase(hydra.testing.RewriteTermTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.RewriteTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("rewriter"), hydra.encode.Testing.termRewriter((x).rewriter)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.term((x).output)))));
  }

  static hydra.core.Term rewriteTypeTestCase(hydra.testing.RewriteTypeTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.RewriteTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.type((x).input)),
      new hydra.core.Field(new hydra.core.Name("rewriter"), hydra.encode.Testing.typeRewriter((x).rewriter)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.type((x).output)))));
  }

  static hydra.core.Term serializationTestCase(hydra.testing.SerializationTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.SerializationTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Ast.expr((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).output))))));
  }

  static hydra.core.Term simplifyTermTestCase(hydra.testing.SimplifyTermTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.SimplifyTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.term((x).output)))));
  }

  static hydra.core.Term substInTypeTestCase(hydra.testing.SubstInTypeTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.SubstInTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("substitution"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, hydra.core.Term>) (p -> new hydra.core.Term.Pair(hydra.lib.pairs.Bimap.apply(
          hydra.encode.Core::name,
          hydra.encode.Core::type,
          p))),
        (x).substitution))),
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.type((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.type((x).output)))));
  }

  static hydra.core.Term tag(hydra.testing.Tag x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.testing.Tag"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).value))));
  }

  static hydra.core.Term termRewriter(hydra.testing.TermRewriter v1) {
    return (v1).accept(new hydra.testing.TermRewriter.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.testing.TermRewriter.ReplaceFooWithBar y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TermRewriter"), new hydra.core.Field(new hydra.core.Name("replaceFooWithBar"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TermRewriter.ReplaceInt32WithInt64 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TermRewriter"), new hydra.core.Field(new hydra.core.Name("replaceInt32WithInt64"), new hydra.core.Term.Unit())));
      }
    });
  }

  static hydra.core.Term testCase(hydra.testing.TestCase v1) {
    return (v1).accept(new hydra.testing.TestCase.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.AlphaConversion y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("alphaConversion"), hydra.encode.Testing.alphaConversionTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.CaseConversion y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("caseConversion"), hydra.encode.Testing.caseConversionTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.DeannotateTerm y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("deannotateTerm"), hydra.encode.Testing.deannotateTermTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.DeannotateType y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("deannotateType"), hydra.encode.Testing.deannotateTypeTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.DelegatedEvaluation y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("delegatedEvaluation"), hydra.encode.Testing.delegatedEvaluationTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.EtaExpansion y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("etaExpansion"), hydra.encode.Testing.etaExpansionTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.FlattenLetTerms y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("flattenLetTerms"), hydra.encode.Testing.flattenLetTermsTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.FreeVariables y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("freeVariables"), hydra.encode.Testing.freeVariablesTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.Evaluation y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("evaluation"), hydra.encode.Testing.evaluationTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.Inference y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("inference"), hydra.encode.Testing.inferenceTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.InferenceFailure y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("inferenceFailure"), hydra.encode.Testing.inferenceFailureTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.JsonDecode y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("jsonDecode"), hydra.encode.Testing.jsonDecodeTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.JsonEncode y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("jsonEncode"), hydra.encode.Testing.jsonEncodeTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.JsonParser y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("jsonParser"), hydra.encode.Testing.jsonParserTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.JsonRoundtrip y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("jsonRoundtrip"), hydra.encode.Testing.jsonRoundtripTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.JsonWriter y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("jsonWriter"), hydra.encode.Testing.jsonWriterTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.LiftLambdaAboveLet y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("liftLambdaAboveLet"), hydra.encode.Testing.liftLambdaAboveLetTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.Serialization y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("serialization"), hydra.encode.Testing.serializationTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.SimplifyTerm y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("simplifyTerm"), hydra.encode.Testing.simplifyTermTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.TopologicalSort y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("topologicalSort"), hydra.encode.Testing.topologicalSortTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.TopologicalSortBindings y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("topologicalSortBindings"), hydra.encode.Testing.topologicalSortBindingsTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.TopologicalSortSCC y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("topologicalSortSCC"), hydra.encode.Testing.topologicalSortSCCTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.TypeChecking y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("typeChecking"), hydra.encode.Testing.typeCheckingTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.TypeCheckingFailure y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("typeCheckingFailure"), hydra.encode.Testing.typeCheckingFailureTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.TypeReduction y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("typeReduction"), hydra.encode.Testing.typeReductionTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.NormalizeTypeVariables y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("normalizeTypeVariables"), hydra.encode.Testing.normalizeTypeVariablesTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.FoldOverTerm y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("foldOverTerm"), hydra.encode.Testing.foldOverTermTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.RewriteTerm y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("rewriteTerm"), hydra.encode.Testing.rewriteTermTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.RewriteType y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("rewriteType"), hydra.encode.Testing.rewriteTypeTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.HoistSubterms y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("hoistSubterms"), hydra.encode.Testing.hoistSubtermsTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.HoistCaseStatements y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("hoistCaseStatements"), hydra.encode.Testing.hoistCaseStatementsTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.HoistLetBindings y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("hoistLetBindings"), hydra.encode.Testing.hoistLetBindingsTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.HoistPolymorphicLetBindings y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("hoistPolymorphicLetBindings"), hydra.encode.Testing.hoistPolymorphicLetBindingsTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.SubstInType y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("substInType"), hydra.encode.Testing.substInTypeTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.VariableOccursInType y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("variableOccursInType"), hydra.encode.Testing.variableOccursInTypeTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.UnifyTypes y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("unifyTypes"), hydra.encode.Testing.unifyTypesTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.JoinTypes y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("joinTypes"), hydra.encode.Testing.joinTypesTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.UnshadowVariables y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("unshadowVariables"), hydra.encode.Testing.unshadowVariablesTestCase((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.ValidateCoreTerm y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("validateCoreTerm"), hydra.encode.Testing.validateCoreTermTestCase((y).value))));
      }
    });
  }

  static hydra.core.Term testCaseWithMetadata(hydra.testing.TestCaseWithMetadata x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).name))),
      new hydra.core.Field(new hydra.core.Name("case"), hydra.encode.Testing.testCase((x).case_)),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(x2))),
        (x).description))),
      new hydra.core.Field(new hydra.core.Name("tags"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.Testing::tag,
        (x).tags))))));
  }

  static hydra.core.Term testGroup(hydra.testing.TestGroup x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGroup"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).name))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(x2))),
        (x).description))),
      new hydra.core.Field(new hydra.core.Name("subgroups"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.Testing::testGroup,
        (x).subgroups))),
      new hydra.core.Field(new hydra.core.Name("cases"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.Testing::testCaseWithMetadata,
        (x).cases))))));
  }

  static hydra.core.Term topologicalSortBindingsTestCase(hydra.testing.TopologicalSortBindingsTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TopologicalSortBindingsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("bindings"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Term>, hydra.core.Term>) (p -> new hydra.core.Term.Pair(hydra.lib.pairs.Bimap.apply(
          hydra.encode.Core::name,
          hydra.encode.Core::term,
          p))),
        (x).bindings))),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>, hydra.core.Term>) (xs2 -> new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Term>, hydra.core.Term>) (p -> new hydra.core.Term.Pair(hydra.lib.pairs.Bimap.apply(
            hydra.encode.Core::name,
            hydra.encode.Core::term,
            p))),
          xs2))),
        (x).expected))))));
  }

  static hydra.core.Term topologicalSortSCCTestCase(hydra.testing.TopologicalSortSCCTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TopologicalSortSCCTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("adjacencyList"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>, hydra.core.Term>) (p -> new hydra.core.Term.Pair(hydra.lib.pairs.Bimap.apply(
          (java.util.function.Function<Integer, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(x2)))),
          (java.util.function.Function<hydra.util.ConsList<Integer>, hydra.core.Term>) (xs2 -> new hydra.core.Term.List(hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(x2)))),
            xs2))),
          p))),
        (x).adjacencyList))),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.ConsList<Integer>, hydra.core.Term>) (xs2 -> new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<Integer, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(x2)))),
          xs2))),
        (x).expected))))));
  }

  static hydra.core.Term topologicalSortTestCase(hydra.testing.TopologicalSortTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TopologicalSortTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("adjacencyList"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>, hydra.core.Term>) (p -> new hydra.core.Term.Pair(hydra.lib.pairs.Bimap.apply(
          (java.util.function.Function<Integer, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(x2)))),
          (java.util.function.Function<hydra.util.ConsList<Integer>, hydra.core.Term>) (xs2 -> new hydra.core.Term.List(hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(x2)))),
            xs2))),
          p))),
        (x).adjacencyList))),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Either(hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.util.ConsList<hydra.util.ConsList<Integer>>, hydra.core.Term>) (xs -> new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.ConsList<Integer>, hydra.core.Term>) (xs2 -> new hydra.core.Term.List(hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(x2)))),
            xs2))),
          xs))),
        (java.util.function.Function<hydra.util.ConsList<Integer>, hydra.core.Term>) (xs -> new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<Integer, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(x2)))),
          xs))),
        (x).expected))))));
  }

  static hydra.core.Term typeCheckingFailureTestCase(hydra.testing.TypeCheckingFailureTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TypeCheckingFailureTestCase"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)))));
  }

  static hydra.core.Term typeCheckingTestCase(hydra.testing.TypeCheckingTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TypeCheckingTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("outputTerm"), hydra.encode.Core.term((x).outputTerm)),
      new hydra.core.Field(new hydra.core.Name("outputType"), hydra.encode.Core.type((x).outputType)))));
  }

  static hydra.core.Term typeReductionTestCase(hydra.testing.TypeReductionTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TypeReductionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.type((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.type((x).output)))));
  }

  static hydra.core.Term typeRewriter(hydra.testing.TypeRewriter v1) {
    return (v1).accept(new hydra.testing.TypeRewriter.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.testing.TypeRewriter.ReplaceStringWithInt32 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TypeRewriter"), new hydra.core.Field(new hydra.core.Name("replaceStringWithInt32"), new hydra.core.Term.Unit())));
      }
    });
  }

  static hydra.core.Term unifyTypesTestCase(hydra.testing.UnifyTypesTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.Core::name,
        (x).schemaTypes))),
      new hydra.core.Field(new hydra.core.Name("left"), hydra.encode.Core.type((x).left)),
      new hydra.core.Field(new hydra.core.Name("right"), hydra.encode.Core.type((x).right)),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Either(hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<String, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(x2))),
        hydra.encode.Typing::typeSubst,
        (x).expected))))));
  }

  static hydra.core.Term unshadowVariablesTestCase(hydra.testing.UnshadowVariablesTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UnshadowVariablesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), hydra.encode.Core.term((x).output)))));
  }

  static hydra.core.Term validateCoreTermTestCase(hydra.testing.ValidateCoreTermTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.ValidateCoreTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("typed"), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_((x).typed))),
      new hydra.core.Field(new hydra.core.Name("input"), hydra.encode.Core.term((x).input)),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        hydra.encode.error.Core::invalidTermError,
        (x).output))))));
  }

  static hydra.core.Term variableOccursInTypeTestCase(hydra.testing.VariableOccursInTypeTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("variable"), hydra.encode.Core.name((x).variable)),
      new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.Core.type((x).type)),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_((x).expected))))));
  }

  static <T0> hydra.core.Term writerTestCase(java.util.function.Function<T0, hydra.core.Term> a, hydra.testing.WriterTestCase<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.WriterTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (a).apply(((java.util.function.Function<hydra.testing.WriterTestCase<T0>, T0>) (projected -> projected.input)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((java.util.function.Function<hydra.testing.WriterTestCase<T0>, String>) (projected -> projected.output)).apply(x)))))));
  }
}
