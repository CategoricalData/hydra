// Note: this is an automatically generated file. Do not edit.

package hydra.test.checking;

/**
 * Nominal type checking test cases: records, unions, field access, injection, projection
 */
public interface NominalTypes {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("Nominal types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.NominalTypes.recordsTests(),
      hydra.test.checking.NominalTypes.unionsTests(),
      hydra.test.checking.NominalTypes.wrappedTermsTests(),
      hydra.test.checking.NominalTypes.eliminationsTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup chainedUnwrappingTests() {
    return new hydra.testing.TestGroup("Chained unwrapping", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("unwrap then process", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("wrapped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.strings.cat2")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(hydra.test.TestTypes.testTypeStringAliasName()), new hydra.core.Term.Variable(new hydra.core.Name("wrapped")))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_(" suffix")))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeStringAliasName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("unwrap polymorphic then map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("wrappedList"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.map")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(hydra.test.TestTypes.testTypePolymorphicWrapperName()), new hydra.core.Term.Variable(new hydra.core.Name("wrappedList")))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePolymorphicWrapperName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup eliminationsTests() {
    return new hydra.testing.TestGroup("Eliminations", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.NominalTypes.recordEliminationsTests(),
      hydra.test.checking.NominalTypes.unionEliminationsTests(),
      hydra.test.checking.NominalTypes.wrapEliminationsTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup higherOrderRecordProjectionsTests() {
    return new hydra.testing.TestGroup("Higher-order record projections", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("map projection over list of records", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.map")), new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("firstName"))))), new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypePersonName(), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice"))),
              new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Smith"))),
              new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30))))))),
            new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypePersonName(), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Bob"))),
              new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Jones"))),
              new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(25))))))))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("map polymorphic projection", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.map")), new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeLatLonPolyName(), new hydra.core.Name("lat"))))), new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypeLatLonPolyName(), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("lat"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(40)))),
              new hydra.core.Field(new hydra.core.Name("lon"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-74))))))),
            new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypeLatLonPolyName(), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("lat"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(34)))),
              new hydra.core.Field(new hydra.core.Name("lon"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-118))))))))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("filter using projection", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.filter")), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("person"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.equality.gt")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("age"))), new hydra.core.Term.Variable(new hydra.core.Name("person")))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30))))))))), new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypePersonName(), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice"))),
              new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Smith"))),
              new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(35))))))),
            new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypePersonName(), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Bob"))),
              new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Jones"))),
              new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(25))))))))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonName()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup higherOrderUnionEliminationsTests() {
    return new hydra.testing.TestGroup("Higher-order union eliminations", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("map match over list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.map")), new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeComparisonName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lessThan"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("less"))))),
            new hydra.core.Field(new hydra.core.Name("equalTo"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("equal"))))),
            new hydra.core.Field(new hydra.core.Name("greaterThan"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("greater")))))))))), new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeComparisonName(), new hydra.core.Field(new hydra.core.Name("lessThan"), new hydra.core.Term.Unit()))),
            new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeComparisonName(), new hydra.core.Field(new hydra.core.Name("equalTo"), new hydra.core.Term.Unit()))))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("compose match with other functions", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("comp"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.strings.length")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeComparisonName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lessThan"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("less"))))),
            new hydra.core.Field(new hydra.core.Name("equalTo"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("equal"))))),
            new hydra.core.Field(new hydra.core.Name("greaterThan"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("greater")))))))), new hydra.core.Term.Variable(new hydra.core.Name("comp")))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeComparisonName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("match in lambda body", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("unionValue"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeNumberName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("int"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Variable(new hydra.core.Name("i")))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))))),
            new hydra.core.Field(new hydra.core.Name("float"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))))), new hydra.core.Term.Variable(new hydra.core.Name("unionValue")))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeNumberName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup monomorphicRecordsTests() {
    return new hydra.testing.TestGroup("Monomorphic records", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("latlon record", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("LatLon"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lat"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (19.54290008544922))))),
            new hydra.core.Field(new hydra.core.Name("lon"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (-155.6658935546875)))))))))), hydra.show.Core.type(new hydra.core.Type.Variable(new hydra.core.Name("LatLon"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("latlon with variable", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("LatLon"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lat"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (19.54290008544922))))),
            new hydra.core.Field(new hydra.core.Name("lon"), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32())), new hydra.core.Type.Variable(new hydra.core.Name("LatLon"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("person record", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Person"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice"))),
            new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Smith"))),
            new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30))))))))), hydra.show.Core.type(new hydra.core.Type.Variable(new hydra.core.Name("Person"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("empty record", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Unit"), (java.util.List<hydra.core.Field>) (java.util.Collections.<hydra.core.Field>emptyList()))))), hydra.show.Core.type(new hydra.core.Type.Variable(new hydra.core.Name("Unit"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("person with variables", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("name"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("age"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Person"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Variable(new hydra.core.Name("name"))),
            new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Doe"))),
            new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Variable(new hydra.core.Name("age")))))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Variable(new hydra.core.Name("Person"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup monomorphicUnwrappingTests() {
    return new hydra.testing.TestGroup("Monomorphic unwrapping", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("unwrap string alias", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
      hydra.Inference.inferTypeOf(
        hydra.test.TestGraph.testContext(),
        hydra.test.TestGraph.testGraph(),
        new hydra.core.Term.Unwrap(hydra.test.TestTypes.testTypeStringAliasName()))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeStringAliasName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup monomorphicWrappedTermsTests() {
    return new hydra.testing.TestGroup("Monomorphic wrapped terms", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("string alias", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypeStringAliasName(), new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))))), hydra.show.Core.type(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeStringAliasName())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("wrapped integer", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypeStringAliasName(), new hydra.core.Term.Literal(new hydra.core.Literal.String_("wrapped")))))), hydra.show.Core.type(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeStringAliasName())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("wrapped in tuple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypeStringAliasName(), new hydra.core.Term.Literal(new hydra.core.Literal.String_("first")))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("second")))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeStringAliasName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup multiParameterPolymorphicCaseStatementsTests() {
    return new hydra.testing.TestGroup("Multi-parameter polymorphic case statements", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("case Either converting both to string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeEitherName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.literals.showInt32")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))),
            new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.literals.showFloat32")), new hydra.core.Term.Variable(new hydra.core.Name("y")))))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeEitherName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32())))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("case Either applied to injection", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeEitherName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("n"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Variable(new hydra.core.Name("n")))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))))),
            new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("s"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.strings.length")), new hydra.core.Term.Variable(new hydra.core.Name("s")))))))))), new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeEitherName(), new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("case Either with Triple and nested projections", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("triple"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeEitherName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("coords"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeLatLonPolyName(), new hydra.core.Name("lat"))), new hydra.core.Term.Variable(new hydra.core.Name("coords"))))))),
            new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("t"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeTripleName(), new hydra.core.Name("first"))), new hydra.core.Term.Variable(new hydra.core.Name("t")))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeTripleName(), new hydra.core.Name("second"))), new hydra.core.Term.Variable(new hydra.core.Name("triple")))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t2"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t3"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t4"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeTripleName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeEitherName()), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeLatLonPolyName()), new hydra.core.Type.Variable(new hydra.core.Name("t1")))))), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeTripleName()), new hydra.core.Type.Variable(new hydra.core.Name("t1")))), new hydra.core.Type.Variable(new hydra.core.Name("t2")))), new hydra.core.Type.Variable(new hydra.core.Name("t3")))))))), new hydra.core.Type.Variable(new hydra.core.Name("t4")))), new hydra.core.Type.Variable(new hydra.core.Name("t1"))))))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("case Either with polymorphic let bindings", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
            new hydra.core.Binding(new hydra.core.Name("makeLeft"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeEitherName(), new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
            new hydra.core.Binding(new hydra.core.Name("makeRight"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeEitherName(), new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Variable(new hydra.core.Name("y"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("flag"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeEitherName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("n"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("makeRight")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Variable(new hydra.core.Name("n")))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10)))))))))),
            new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("s"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("makeLeft")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.strings.length")), new hydra.core.Term.Variable(new hydra.core.Name("s")))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("flag")))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeEitherName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeEitherName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup multiParameterPolymorphicInjectionsTests() {
    return new hydra.testing.TestGroup("Multi-parameter polymorphic injections", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("either left with int", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeEitherName(), new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeEitherName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("either right with string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeEitherName(), new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello"))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeEitherName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("either containing LatLonPoly in list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeEitherName(), new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypeLatLonPolyName(), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lat"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(40)))),
            new hydra.core.Field(new hydra.core.Name("lon"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-74)))))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeEitherName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.List(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeLatLonPolyName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("either in triple in map with shared type variables", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x0"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x1"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x2"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("key")),
            new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypeTripleName(), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("first"), new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeEitherName(), new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Variable(new hydra.core.Name("x0")))))),
              new hydra.core.Field(new hydra.core.Name("second"), new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeEitherName(), new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Variable(new hydra.core.Name("x0")))))),
              new hydra.core.Field(new hydra.core.Name("third"), new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeEitherName(), new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Variable(new hydra.core.Name("x1"))))))))))))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t2"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t3"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t4"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t5"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t1")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t2")), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeTripleName()), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeEitherName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Variable(new hydra.core.Name("t3")))))), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeEitherName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Variable(new hydra.core.Name("t4")))))), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeEitherName()), new hydra.core.Type.Variable(new hydra.core.Name("t5")))), new hydra.core.Type.Variable(new hydra.core.Name("t1"))))))))))))))))))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup multiParameterPolymorphicProjectionsTests() {
    return new hydra.testing.TestGroup("Multi-parameter polymorphic projections", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("project first from Triple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeTripleName(), new hydra.core.Name("first"))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t2"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeTripleName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Variable(new hydra.core.Name("t1")))), new hydra.core.Type.Variable(new hydra.core.Name("t2")))), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("project second from Triple applied", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeTripleName(), new hydra.core.Name("second"))), new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypeTripleName(), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("first"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),
            new hydra.core.Field(new hydra.core.Name("second"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("middle"))),
            new hydra.core.Field(new hydra.core.Name("third"), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("project from Triple and use second field, which is another polymorphic record", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("triple"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("key"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypePersonOrSomethingName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("person"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("p"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))),
            new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("m"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.lookup")), new hydra.core.Term.Variable(new hydra.core.Name("key")))), new hydra.core.Term.Variable(new hydra.core.Name("m")))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeTripleName(), new hydra.core.Name("second"))), new hydra.core.Term.Variable(new hydra.core.Name("triple")))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t2"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t3"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeTripleName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonOrSomethingName()), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t1")), new hydra.core.Type.Variable(new hydra.core.Name("t2")))))))), new hydra.core.Type.Variable(new hydra.core.Name("t3")))), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t1")), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("t2")))))))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup multiParameterPolymorphicRecordsTests() {
    return new hydra.testing.TestGroup("Multi-parameter polymorphic records", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("triple with three monomorphic types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Triple"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("first"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),
            new hydra.core.Field(new hydra.core.Name("second"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("middle"))),
            new hydra.core.Field(new hydra.core.Name("third"), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))))), hydra.show.Core.type(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("Triple")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("triple with PersonOrSomething containing map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("k"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Triple"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("first"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("prefix"))),
            new hydra.core.Field(new hydra.core.Name("second"), new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypePersonOrSomethingName(), new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
              new hydra.core.Term.Variable(new hydra.core.Name("k")),
              new hydra.core.Term.Variable(new hydra.core.Name("v")))))))))),
            new hydra.core.Field(new hydra.core.Name("third"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(999))))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t1")), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("Triple")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("PersonOrSomething")), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1")))))))), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup multiParameterPolymorphicUnwrappersTests() {
    return new hydra.testing.TestGroup("Multi-parameter polymorphic unwrappers", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("unwrap symmetric triple to tuple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("st"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeTripleName(), new hydra.core.Name("first"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(hydra.test.TestTypes.testTypeSymmetricTripleName()), new hydra.core.Term.Variable(new hydra.core.Name("st")))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeTripleName(), new hydra.core.Name("third"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(hydra.test.TestTypes.testTypeSymmetricTripleName()), new hydra.core.Term.Variable(new hydra.core.Name("st")))))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeSymmetricTripleName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Variable(new hydra.core.Name("t1")))), new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("unwrap and collect edges in set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("getEdge"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("st"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeTripleName(), new hydra.core.Name("second"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(hydra.test.TestTypes.testTypeSymmetricTripleName()), new hydra.core.Term.Variable(new hydra.core.Name("st")))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("triples"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.map")), new hydra.core.Term.Variable(new hydra.core.Name("getEdge")))), new hydra.core.Term.Variable(new hydra.core.Name("triples")))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Set(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeSymmetricTripleName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Variable(new hydra.core.Name("t1"))))), new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t1")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("unwrap with maybe to handle optional symmetric triple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("mst"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maybes.maybe")), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("st"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeTripleName(), new hydra.core.Name("second"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(hydra.test.TestTypes.testTypeSymmetricTripleName()), new hydra.core.Term.Variable(new hydra.core.Name("st")))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("mst")))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Maybe(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeSymmetricTripleName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Variable(new hydra.core.Name("t1"))))), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("t1")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup multiParameterPolymorphicWrappersTests() {
    return new hydra.testing.TestGroup("Multi-parameter polymorphic wrappers", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("symmetric triple wrapping simple types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypeSymmetricTripleName(), new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypeTripleName(), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("first"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),
            new hydra.core.Field(new hydra.core.Name("second"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("edge"))),
            new hydra.core.Field(new hydra.core.Name("third"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))))))), hydra.show.Core.type(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeSymmetricTripleName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("symmetric triple from lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("v1"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("e"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("v2"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypeSymmetricTripleName(), new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypeTripleName(), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("first"), new hydra.core.Term.Variable(new hydra.core.Name("v1"))),
            new hydra.core.Field(new hydra.core.Name("second"), new hydra.core.Term.Variable(new hydra.core.Name("e"))),
            new hydra.core.Field(new hydra.core.Name("third"), new hydra.core.Term.Variable(new hydra.core.Name("v2")))))))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t1")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeSymmetricTripleName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Variable(new hydra.core.Name("t1"))))))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("symmetric triple with nested polymorphic types and foldl", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("sumList"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("lst"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.foldl")), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("acc"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Variable(new hydra.core.Name("acc")))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))), new hydra.core.Term.Variable(new hydra.core.Name("lst")))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("nums1"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("nums2"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypeSymmetricTripleName(), new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypeTripleName(), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("first"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("sumList")), new hydra.core.Term.Variable(new hydra.core.Name("nums1"))))),
            new hydra.core.Field(new hydra.core.Name("second"), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Variable(new hydra.core.Name("nums1")),
              new hydra.core.Term.Variable(new hydra.core.Name("nums2"))))),
            new hydra.core.Field(new hydra.core.Name("third"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("sumList")), new hydra.core.Term.Variable(new hydra.core.Name("nums2")))))))))))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeSymmetricTripleName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.List(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup multipleUnwrapOperationsTests() {
    return new hydra.testing.TestGroup("Multiple unwrap operations", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("unwrap different types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
      hydra.Inference.inferTypeOf(
        hydra.test.TestGraph.testContext(),
        hydra.test.TestGraph.testGraph(),
        new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("stringWrapped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("listWrapped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(hydra.test.TestTypes.testTypeStringAliasName()), new hydra.core.Term.Variable(new hydra.core.Name("stringWrapped")))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(hydra.test.TestTypes.testTypePolymorphicWrapperName()), new hydra.core.Term.Variable(new hydra.core.Name("listWrapped")))))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeStringAliasName()), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePolymorphicWrapperName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0")))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup multipleWrappingLevelsTests() {
    return new hydra.testing.TestGroup("Multiple wrapping levels", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("wrapped in optional", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypeStringAliasName(), new hydra.core.Term.Literal(new hydra.core.Literal.String_("wrapped")))))))), hydra.show.Core.type(new hydra.core.Type.Maybe(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeStringAliasName()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("list of wrapped polymorphic", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypePolymorphicWrapperName(), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))),
            new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypePolymorphicWrapperName(), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePolymorphicWrapperName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup nestedUnionEliminationsTests() {
    return new hydra.testing.TestGroup("Nested union eliminations", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("nested match statements", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypePersonOrSomethingName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("person"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("p"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("firstName"))), new hydra.core.Term.Variable(new hydra.core.Name("p"))))))),
            new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeNumberName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("int"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.literals.showInt32")), new hydra.core.Term.Variable(new hydra.core.Name("i"))))))),
              new hydra.core.Field(new hydra.core.Name("float"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.literals.showFloat32")), new hydra.core.Term.Variable(new hydra.core.Name("f")))))))))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonOrSomethingName()), new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeNumberName()))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("match in tuple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeComparisonName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lessThan"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))),
            new hydra.core.Field(new hydra.core.Name("equalTo"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))),
            new hydra.core.Field(new hydra.core.Name("greaterThan"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-1))))))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("context")))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeComparisonName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup nestedWrappedTermsTests() {
    return new hydra.testing.TestGroup("Nested wrapped terms", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("wrapped tuple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypePolymorphicWrapperName(), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")))))))))))), hydra.show.Core.type(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePolymorphicWrapperName()), new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("wrapped optional", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypePolymorphicWrapperName(), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))))))), hydra.show.Core.type(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePolymorphicWrapperName()), new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("wrapped map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypePolymorphicWrapperName(), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("key")),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))))))))), hydra.show.Core.type(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePolymorphicWrapperName()), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup polymorphicRecordProjectionsAppliedTests() {
    return new hydra.testing.TestGroup("Polymorphic record projections applied", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("project lat from LatLonPoly with int32", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeLatLonPolyName(), new hydra.core.Name("lat"))), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("LatLonPoly"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lat"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(40)))),
            new hydra.core.Field(new hydra.core.Name("lon"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-74))))))))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("project lon from LatLonPoly with float64", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeLatLonPolyName(), new hydra.core.Name("lon"))), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("LatLonPoly"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lat"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(40.7128)))),
            new hydra.core.Field(new hydra.core.Name("lon"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-74.006))))))))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float64()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("project head from BuddyListA with string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeBuddyListAName(), new hydra.core.Name("head"))), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("BuddyListA"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("head"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice"))),
            new hydra.core.Field(new hydra.core.Name("tail"), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))))))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup polymorphicRecordProjectionsTests() {
    return new hydra.testing.TestGroup("Polymorphic record projections", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("project lat from polymorphic LatLonPoly", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeLatLonPolyName(), new hydra.core.Name("lat"))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("LatLonPoly")), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("project lon from polymorphic LatLonPoly", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeLatLonPolyName(), new hydra.core.Name("lon"))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("LatLonPoly")), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("project head from BuddyListA", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeBuddyListAName(), new hydra.core.Name("head"))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("BuddyListA")), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("project tail from BuddyListA", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeBuddyListAName(), new hydra.core.Name("tail"))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("BuddyListA")), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Maybe(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("BuddyListB")), new hydra.core.Type.Variable(new hydra.core.Name("t0")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup polymorphicRecordsTests() {
    return new hydra.testing.TestGroup("Polymorphic records", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("latlon poly float", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("LatLonPoly"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lat"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (19.54290008544922))))),
            new hydra.core.Field(new hydra.core.Name("lon"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (-155.6658935546875)))))))))), hydra.show.Core.type(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("LatLonPoly")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("latlon poly int64", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("LatLonPoly"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lat"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((long) (195429))))),
            new hydra.core.Field(new hydra.core.Name("lon"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((long) (-1556659)))))))))), hydra.show.Core.type(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("LatLonPoly")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int64()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("latlon poly variable", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("LatLonPoly"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lat"), new hydra.core.Term.Variable(new hydra.core.Name("x"))),
            new hydra.core.Field(new hydra.core.Name("lon"), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("LatLonPoly")), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("buddylist string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("BuddyListA"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("head"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("first"))),
            new hydra.core.Field(new hydra.core.Name("tail"), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))))))), hydra.show.Core.type(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("BuddyListA")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("buddylist variable", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("BuddyListA"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("head"), new hydra.core.Term.Variable(new hydra.core.Name("x"))),
            new hydra.core.Field(new hydra.core.Name("tail"), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("BuddyListA")), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup polymorphicRecursiveUnionInjectionsTests() {
    return new hydra.testing.TestGroup("Polymorphic recursive union injections", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("inject boolean into UnionPolymorphicRecursive", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName(), new hydra.core.Field(new hydra.core.Name("bool"), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName()), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("inject string value into UnionPolymorphicRecursive", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName(), new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("test"))))))), hydra.show.Core.type(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("inject int value into UnionPolymorphicRecursive", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName(), new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(123)))))))), hydra.show.Core.type(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup polymorphicUnionEliminationsTests() {
    return new hydra.testing.TestGroup("Polymorphic union eliminations", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.NominalTypes.simplePolymorphicUnionTests(),
      hydra.test.checking.NominalTypes.usingUnionPolymorphicRecursiveTests(),
      hydra.test.checking.NominalTypes.usingKernelTypesTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup polymorphicUnionInjectionsTests() {
    return new hydra.testing.TestGroup("Polymorphic union injections", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("inject person into PersonOrSomething", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypePersonOrSomethingName(), new hydra.core.Field(new hydra.core.Name("person"), new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypePersonName(), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice"))),
            new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Smith"))),
            new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30)))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonOrSomethingName()), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("inject string into PersonOrSomething other variant", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypePersonOrSomethingName(), new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("something else"))))))), hydra.show.Core.type(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonOrSomethingName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("inject int into PersonOrSomething other variant", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypePersonOrSomethingName(), new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))), hydra.show.Core.type(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonOrSomethingName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup polymorphicUnionsFromLambdaTests() {
    return new hydra.testing.TestGroup("Polymorphic unions from lambda", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("lambda creating PersonOrSomething other variant", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypePersonOrSomethingName(), new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonOrSomethingName()), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("lambda creating UnionPolymorphicRecursive value variant", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName(), new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName()), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup polymorphicUnwrappingTests() {
    return new hydra.testing.TestGroup("Polymorphic unwrapping", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("unwrap polymorphic wrapper", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
      hydra.Inference.inferTypeOf(
        hydra.test.TestGraph.testContext(),
        hydra.test.TestGraph.testGraph(),
        new hydra.core.Term.Unwrap(hydra.test.TestTypes.testTypePolymorphicWrapperName()))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePolymorphicWrapperName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0")))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup polymorphicWrappedTermsTests() {
    return new hydra.testing.TestGroup("Polymorphic wrapped terms", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("polymorphic wrapper with int", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypePolymorphicWrapperName(), new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))))), hydra.show.Core.type(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePolymorphicWrapperName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("polymorphic wrapper with string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypePolymorphicWrapperName(), new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")))))))), hydra.show.Core.type(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePolymorphicWrapperName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("polymorphic wrapper from lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypePolymorphicWrapperName(), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePolymorphicWrapperName()), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup projectionsWithVariablesTests() {
    return new hydra.testing.TestGroup("Projections with variables", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("project from lambda parameter", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("person"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("firstName"))), new hydra.core.Term.Variable(new hydra.core.Name("person")))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("Person")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("project from polymorphic lambda parameter", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("coords"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeLatLonPolyName(), new hydra.core.Name("lat"))), new hydra.core.Term.Variable(new hydra.core.Name("coords")))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("LatLonPoly")), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("multiple projections from same record", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("person"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("firstName"))), new hydra.core.Term.Variable(new hydra.core.Name("person")))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("lastName"))), new hydra.core.Term.Variable(new hydra.core.Name("person")))))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("Person")), new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup recordEliminationsTests() {
    return new hydra.testing.TestGroup("Record eliminations", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.NominalTypes.simpleRecordProjectionsTests(),
      hydra.test.checking.NominalTypes.recordProjectionsAppliedToRecordsTests(),
      hydra.test.checking.NominalTypes.polymorphicRecordProjectionsTests(),
      hydra.test.checking.NominalTypes.polymorphicRecordProjectionsAppliedTests(),
      hydra.test.checking.NominalTypes.recordProjectionsWithVariablesTests(),
      hydra.test.checking.NominalTypes.recordProjectionsInComplexContextsTests(),
      hydra.test.checking.NominalTypes.multiParameterPolymorphicProjectionsTests(),
      hydra.test.checking.NominalTypes.higherOrderRecordProjectionsTests(),
      hydra.test.checking.NominalTypes.recursiveRecordProjectionsTests(),
      hydra.test.checking.NominalTypes.recordProjectionsWithMutualRecursionTests(),
      hydra.test.checking.NominalTypes.projectionsWithVariablesTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup recordProjectionsAppliedToRecordsTests() {
    return new hydra.testing.TestGroup("Record projections applied to records", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("project firstName applied to person record", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("firstName"))), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Person"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice"))),
            new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Smith"))),
            new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30))))))))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("project age applied to person record", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("age"))), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Person"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Bob"))),
            new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Jones"))),
            new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(25))))))))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("project lat applied to LatLon record", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeLatLonName(), new hydra.core.Name("lat"))), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("LatLon"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lat"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (40.712799072265625))))),
            new hydra.core.Field(new hydra.core.Name("lon"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (-74.00599670410156)))))))))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup recordProjectionsInComplexContextsTests() {
    return new hydra.testing.TestGroup("Record projections in complex contexts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("projection in let binding", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
            new hydra.core.Binding(new hydra.core.Name("person"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Person"), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Charlie"))),
              new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Brown"))),
              new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(35))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
            new hydra.core.Binding(new hydra.core.Name("getName"), new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("firstName"))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("getName")), new hydra.core.Term.Variable(new hydra.core.Name("person")))))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("projection in tuple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("firstName"))), new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("age"))))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("Person")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("Person")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("projection in list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("firstName"))),
            new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("lastName"))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("Person")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup recordProjectionsWithMutualRecursionTests() {
    return new hydra.testing.TestGroup("Record projections with mutual recursion", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("project head from BuddyListA", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeBuddyListAName(), new hydra.core.Name("head"))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeBuddyListAName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("project tail from BuddyListB", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeBuddyListBName(), new hydra.core.Name("tail"))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeBuddyListBName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Maybe(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeBuddyListAName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("chained projections across mutual recursion", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("listA"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maybes.maybe")), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("listB"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maybes.maybe")), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))), new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeBuddyListAName(), new hydra.core.Name("tail"))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeBuddyListBName(), new hydra.core.Name("tail"))), new hydra.core.Term.Variable(new hydra.core.Name("listB")))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeBuddyListAName(), new hydra.core.Name("tail"))), new hydra.core.Term.Variable(new hydra.core.Name("listA")))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeBuddyListAName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Maybe(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeBuddyListBName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup recordProjectionsWithVariablesTests() {
    return new hydra.testing.TestGroup("Record projections with variables", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("project from lambda parameter", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("person"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("firstName"))), new hydra.core.Term.Variable(new hydra.core.Name("person")))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("Person")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("project from polymorphic lambda parameter", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("coords"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeLatLonPolyName(), new hydra.core.Name("lat"))), new hydra.core.Term.Variable(new hydra.core.Name("coords")))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("LatLonPoly")), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("multiple projections from same record", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("person"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("firstName"))), new hydra.core.Term.Variable(new hydra.core.Name("person")))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("lastName"))), new hydra.core.Term.Variable(new hydra.core.Name("person")))))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("Person")), new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup recordsInComplexContextsTests() {
    return new hydra.testing.TestGroup("Records in complex contexts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("records in tuple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Person"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Bob"))),
            new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Jones"))),
            new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(25))))))), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("LatLon"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lat"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (1.0))))),
            new hydra.core.Field(new hydra.core.Name("lon"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (2.0)))))))))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Variable(new hydra.core.Name("Person")), new hydra.core.Type.Variable(new hydra.core.Name("LatLon"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("poly records in tuple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("LatLonPoly"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lat"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),
            new hydra.core.Field(new hydra.core.Name("lon"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("BuddyListA"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("head"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("test"))),
            new hydra.core.Field(new hydra.core.Name("tail"), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))))))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("LatLonPoly")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("BuddyListA")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("recursive record", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("IntList"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("head"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))),
            new hydra.core.Field(new hydra.core.Name("tail"), new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("IntList"), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("head"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(43)))),
              new hydra.core.Field(new hydra.core.Name("tail"), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))))))))))))), hydra.show.Core.type(new hydra.core.Type.Variable(new hydra.core.Name("IntList"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup recordsTests() {
    return new hydra.testing.TestGroup("Records", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.NominalTypes.monomorphicRecordsTests(),
      hydra.test.checking.NominalTypes.polymorphicRecordsTests(),
      hydra.test.checking.NominalTypes.recordsInComplexContextsTests(),
      hydra.test.checking.NominalTypes.multiParameterPolymorphicRecordsTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup recursiveRecordProjectionsTests() {
    return new hydra.testing.TestGroup("Recursive record projections", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("nested projection from recursive record", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
      hydra.Inference.inferTypeOf(
        hydra.test.TestGraph.testContext(),
        hydra.test.TestGraph.testGraph(),
        new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("intList"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maybes.maybe")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))), new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeIntListName(), new hydra.core.Name("head"))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeIntListName(), new hydra.core.Name("tail"))), new hydra.core.Term.Variable(new hydra.core.Name("intList")))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeIntListName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup recursiveUnionEliminationsTests() {
    return new hydra.testing.TestGroup("Recursive union eliminations", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("match HydraType recursively", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
      hydra.Inference.inferTypeOf(
        hydra.test.TestGraph.testContext(),
        hydra.test.TestGraph.testGraph(),
        new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeHydraTypeName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
          new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("lit"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeHydraLiteralTypeName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("boolean"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("b"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.literals.showBoolean")), new hydra.core.Term.Variable(new hydra.core.Name("b"))))))),
            new hydra.core.Field(new hydra.core.Name("string"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("s"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("s")))))))), new hydra.core.Term.Variable(new hydra.core.Name("lit"))))))),
          new hydra.core.Field(new hydra.core.Name("list"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("nested"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("list")))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeHydraTypeName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup simplePolymorphicUnionTests() {
    return new hydra.testing.TestGroup("Simple polymorphic unions", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("match PersonOrSomething with string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypePersonOrSomethingName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("person"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("p"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("firstName"))), new hydra.core.Term.Variable(new hydra.core.Name("p"))))))),
            new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonOrSomethingName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("match PersonOrSomething instantiated with string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypePersonOrSomethingName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("person"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("p"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("firstName"))), new hydra.core.Term.Variable(new hydra.core.Name("p"))))))),
            new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))), new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypePersonOrSomethingName(), new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("test"))))))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup simpleRecordProjectionsTests() {
    return new hydra.testing.TestGroup("Simple record projections", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("project firstName from Person", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("firstName"))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("Person")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("project lastName from Person", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("lastName"))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("Person")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("project age from Person", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("age"))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("Person")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("project lat from LatLon", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeLatLonName(), new hydra.core.Name("lat"))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("LatLon")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("project lon from LatLon", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypeLatLonName(), new hydra.core.Name("lon"))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("LatLon")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup simpleUnionInjectionsTests() {
    return new hydra.testing.TestGroup("Simple union injections", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("inject into Comparison lessThan variant", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeComparisonName(), new hydra.core.Field(new hydra.core.Name("lessThan"), new hydra.core.Term.Unit()))))), hydra.show.Core.type(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeComparisonName())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("inject into Comparison equalTo variant", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeComparisonName(), new hydra.core.Field(new hydra.core.Name("equalTo"), new hydra.core.Term.Unit()))))), hydra.show.Core.type(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeComparisonName())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("inject into Comparison greaterThan variant", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeComparisonName(), new hydra.core.Field(new hydra.core.Name("greaterThan"), new hydra.core.Term.Unit()))))), hydra.show.Core.type(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeComparisonName())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup simpleUnitVariantEliminationsTests() {
    return new hydra.testing.TestGroup("Simple unit inject eliminations", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("match Comparison with all cases", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeComparisonName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lessThan"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("less"))))),
            new hydra.core.Field(new hydra.core.Name("equalTo"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("equal"))))),
            new hydra.core.Field(new hydra.core.Name("greaterThan"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("greater")))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeComparisonName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("match Comparison returning int32", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeComparisonName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lessThan"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-1)))))),
            new hydra.core.Field(new hydra.core.Name("equalTo"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))),
            new hydra.core.Field(new hydra.core.Name("greaterThan"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeComparisonName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("match applied to Comparison variant", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeComparisonName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lessThan"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("less"))))),
            new hydra.core.Field(new hydra.core.Name("equalTo"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("equal"))))),
            new hydra.core.Field(new hydra.core.Name("greaterThan"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("greater")))))))), new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeComparisonName(), new hydra.core.Field(new hydra.core.Name("equalTo"), new hydra.core.Term.Unit()))))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup unionEliminationsInComplexContextsTests() {
    return new hydra.testing.TestGroup("Union eliminations in complex contexts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("match in let binding", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("matcher"), new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeComparisonName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lessThan"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("less"))))),
            new hydra.core.Field(new hydra.core.Name("equalTo"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("equal"))))),
            new hydra.core.Field(new hydra.core.Name("greaterThan"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("greater")))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("matcher")))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeComparisonName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("match in record", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypePersonName(), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypePersonOrSomethingName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("person"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("p"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("firstName"))), new hydra.core.Term.Variable(new hydra.core.Name("p"))))))),
              new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))), new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypePersonOrSomethingName(), new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("John")))))))),
            new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Doe"))),
            new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30))))))))), hydra.show.Core.type(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonName())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("match with polymorphic result in list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypePersonOrSomethingName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("person"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("p"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("age"))), new hydra.core.Term.Variable(new hydra.core.Name("p"))))))),
              new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))), new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypePersonOrSomethingName(), new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(25)))))))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup unionEliminationsTests() {
    return new hydra.testing.TestGroup("Union eliminations", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.NominalTypes.simpleUnitVariantEliminationsTests(),
      hydra.test.checking.NominalTypes.unionEliminationsWithDataTests(),
      hydra.test.checking.NominalTypes.polymorphicUnionEliminationsTests(),
      hydra.test.checking.NominalTypes.unionEliminationsWithDefaultsTests(),
      hydra.test.checking.NominalTypes.nestedUnionEliminationsTests(),
      hydra.test.checking.NominalTypes.unionEliminationsInComplexContextsTests(),
      hydra.test.checking.NominalTypes.multiParameterPolymorphicCaseStatementsTests(),
      hydra.test.checking.NominalTypes.higherOrderUnionEliminationsTests(),
      hydra.test.checking.NominalTypes.recursiveUnionEliminationsTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup unionEliminationsWithDataTests() {
    return new hydra.testing.TestGroup("Union eliminations with data", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("match Number extracting int values", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeNumberName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("int"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("i"))))),
            new hydra.core.Field(new hydra.core.Name("float"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeNumberName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("match Number converting to string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeNumberName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("int"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.literals.showInt32")), new hydra.core.Term.Variable(new hydra.core.Name("i"))))))),
            new hydra.core.Field(new hydra.core.Name("float"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.literals.showFloat32")), new hydra.core.Term.Variable(new hydra.core.Name("f")))))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeNumberName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("match Number applied to int variant", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeNumberName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("int"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Variable(new hydra.core.Name("i")))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10)))))))),
            new hydra.core.Field(new hydra.core.Name("float"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))))), new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeNumberName(), new hydra.core.Field(new hydra.core.Name("int"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("match Timestamp with mixed data types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeTimestampName(), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("unixTimeMillis"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("millis"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.literals.showUint64")), new hydra.core.Term.Variable(new hydra.core.Name("millis"))))))),
            new hydra.core.Field(new hydra.core.Name("date"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("dateStr"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("dateStr")))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeTimestampName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup unionEliminationsWithDefaultsTests() {
    return new hydra.testing.TestGroup("Union eliminations with defaults", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("match Comparison with default case", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeComparisonName(), hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("unknown"))), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("lessThan"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("less"))))),
            new hydra.core.Field(new hydra.core.Name("equalTo"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("equal")))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeComparisonName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("match Number with default case", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeNumberName(), hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-1)))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("int"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("i")))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeNumberName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("match UnionMonomorphic with default", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeUnionMonomorphicName(), hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("fallback"))), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("bool"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("b"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.literals.showBoolean")), new hydra.core.Term.Variable(new hydra.core.Name("b"))))))),
            new hydra.core.Field(new hydra.core.Name("string"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("s"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("s")))))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeUnionMonomorphicName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup unionInjectionsWithDataTests() {
    return new hydra.testing.TestGroup("Union injections with data", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("inject into Number int variant", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeNumberName(), new hydra.core.Field(new hydra.core.Name("int"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))), hydra.show.Core.type(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeNumberName())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("inject into Number float variant", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeNumberName(), new hydra.core.Field(new hydra.core.Name("float"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (3.140000104904175))))))))), hydra.show.Core.type(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeNumberName())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("inject into Timestamp unixTimeMillis variant", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeTimestampName(), new hydra.core.Field(new hydra.core.Name("unixTimeMillis"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint64(new java.math.BigInteger("1609459200000"))))))))), hydra.show.Core.type(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeTimestampName())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("inject into Timestamp date variant", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeTimestampName(), new hydra.core.Field(new hydra.core.Name("date"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("2021-01-01"))))))), hydra.show.Core.type(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeTimestampName())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup unionsInComplexContextsTests() {
    return new hydra.testing.TestGroup("Unions in complex contexts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("union in tuple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeNumberName(), new hydra.core.Field(new hydra.core.Name("int"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("context")))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeNumberName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("union in list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeNumberName(), new hydra.core.Field(new hydra.core.Name("int"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))),
            new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeNumberName(), new hydra.core.Field(new hydra.core.Name("float"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (2.5))))))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeNumberName()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("polymorphic union in let binding", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("value"), new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypePersonOrSomethingName(), new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("test"))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("value")))))), hydra.show.Core.type(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonOrSomethingName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup unionsTests() {
    return new hydra.testing.TestGroup("Unions", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.NominalTypes.simpleUnionInjectionsTests(),
      hydra.test.checking.NominalTypes.unionInjectionsWithDataTests(),
      hydra.test.checking.NominalTypes.polymorphicUnionInjectionsTests(),
      hydra.test.checking.NominalTypes.polymorphicRecursiveUnionInjectionsTests(),
      hydra.test.checking.NominalTypes.polymorphicUnionsFromLambdaTests(),
      hydra.test.checking.NominalTypes.unionsInComplexContextsTests(),
      hydra.test.checking.NominalTypes.multiParameterPolymorphicInjectionsTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup unwrapEliminationsInApplicationsTests() {
    return new hydra.testing.TestGroup("Unwrap eliminations in applications", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("unwrap applied to wrapped term", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(hydra.test.TestTypes.testTypeStringAliasName()), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypeStringAliasName(), new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("unwrap polymorphic applied", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(hydra.test.TestTypes.testTypePolymorphicWrapperName()), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypePolymorphicWrapperName(), new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup unwrapInComplexContextsTests() {
    return new hydra.testing.TestGroup("Unwrap in complex contexts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("unwrap in let binding", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
            new hydra.core.Binding(new hydra.core.Name("unwrapper"), new hydra.core.Term.Unwrap(hydra.test.TestTypes.testTypeStringAliasName()), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
            new hydra.core.Binding(new hydra.core.Name("wrapped"), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypeStringAliasName(), new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("unwrapper")), new hydra.core.Term.Variable(new hydra.core.Name("wrapped")))))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("unwrap in tuple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Unwrap(hydra.test.TestTypes.testTypeStringAliasName()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("context")))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeStringAliasName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("unwrap in lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("wrapped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(hydra.test.TestTypes.testTypeStringAliasName()), new hydra.core.Term.Variable(new hydra.core.Name("wrapped")))))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeStringAliasName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup usingKernelTypesTests() {
    return new hydra.testing.TestGroup("Using kernel types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("case statement on CoderDirection applied to argument", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
      hydra.Inference.inferTypeOf(
        hydra.test.TestGraph.testContext(),
        hydra.test.TestGraph.testGraph(),
        new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("dir"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("coder"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(new hydra.core.Name("hydra.coders.CoderDirection"), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
          new hydra.core.Field(new hydra.core.Name("encode"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("v12"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Coder"), new hydra.core.Name("encode"))), new hydra.core.Term.Variable(new hydra.core.Name("coder")))), new hydra.core.Term.Variable(new hydra.core.Name("v12"))))))))),
          new hydra.core.Field(new hydra.core.Name("decode"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("v12"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Coder"), new hydra.core.Name("decode"))), new hydra.core.Term.Variable(new hydra.core.Name("coder")))), new hydra.core.Term.Variable(new hydra.core.Name("v12")))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("dir")))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.coders.CoderDirection")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.coders.Coder")), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.context.Context")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.context.InContext")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.errors.Error")))), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(new hydra.testing.Tag("disabled")))));
  }

  static hydra.testing.TestGroup usingUnionPolymorphicRecursiveTests() {
    return new hydra.testing.TestGroup("using UnionPolymorphicRecursive", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("non-applied UnionPolymorphicRecursive", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("test"), new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName(), hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("other"))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.literals.showInt32")), new hydra.core.Term.Variable(new hydra.core.Name("i")))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("test")))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("applied UnionPolymorphicRecursive with int32", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("test"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName(), hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("other"))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.literals.showInt32")), new hydra.core.Term.Variable(new hydra.core.Name("i")))))))))), new hydra.core.Term.Inject(new hydra.core.Injection(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName(), new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("test")))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("applied UnionPolymorphicRecursive with int32 in lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("test"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName(), hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("other"))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.literals.showInt32")), new hydra.core.Term.Variable(new hydra.core.Name("i")))))))))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("test")))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("applied generic UnionPolymorphicRecursive in lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("test"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Cases(new hydra.core.CaseStatement(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName(), hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("other"))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("ignored"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("test")))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup wrapEliminationsTests() {
    return new hydra.testing.TestGroup("Wrap eliminations", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.NominalTypes.monomorphicUnwrappingTests(),
      hydra.test.checking.NominalTypes.polymorphicUnwrappingTests(),
      hydra.test.checking.NominalTypes.unwrapEliminationsInApplicationsTests(),
      hydra.test.checking.NominalTypes.unwrapInComplexContextsTests(),
      hydra.test.checking.NominalTypes.multiParameterPolymorphicUnwrappersTests(),
      hydra.test.checking.NominalTypes.chainedUnwrappingTests(),
      hydra.test.checking.NominalTypes.multipleUnwrapOperationsTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup wrappedTermsInComplexContextsTests() {
    return new hydra.testing.TestGroup("Wrapped terms in complex contexts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("wrapped in record", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypePersonName(), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("John"))),
            new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Doe"))),
            new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30))))))))), hydra.show.Core.type(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonName())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("wrapped in let binding", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("alias"), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypeStringAliasName(), new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("alias")))))), hydra.show.Core.type(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeStringAliasName())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("wrapped in list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypeStringAliasName(), new hydra.core.Term.Literal(new hydra.core.Literal.String_("first")))),
            new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(hydra.test.TestTypes.testTypeStringAliasName(), new hydra.core.Term.Literal(new hydra.core.Literal.String_("second")))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeStringAliasName()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup wrappedTermsTests() {
    return new hydra.testing.TestGroup("Wrapped terms", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.NominalTypes.monomorphicWrappedTermsTests(),
      hydra.test.checking.NominalTypes.polymorphicWrappedTermsTests(),
      hydra.test.checking.NominalTypes.wrappedTermsInComplexContextsTests(),
      hydra.test.checking.NominalTypes.nestedWrappedTermsTests(),
      hydra.test.checking.NominalTypes.multipleWrappingLevelsTests(),
      hydra.test.checking.NominalTypes.multiParameterPolymorphicWrappersTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}
