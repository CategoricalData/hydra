// Note: this is an automatically generated file. Do not edit.

package hydra.test.validate;

/**
 * Test cases for core term and type validation
 */
public interface Core {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("validate.core", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.validate.Core.duplicateBindingsTests(),
      hydra.test.validate.Core.duplicateFieldsTests(),
      hydra.test.validate.Core.emptyLetBindingsTests(),
      hydra.test.validate.Core.identityApplicationTests(),
      hydra.test.validate.Core.variableShadowingTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup duplicateBindingsTests() {
    return new hydra.testing.TestGroup("duplicate bindings", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("no bindings (literal)", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("single binding", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("distinct bindings", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
            new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
            new hydra.core.Binding(new hydra.core.Name("y"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("duplicate bindings at top level", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
            new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
            new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.DuplicateBinding(new hydra.error.core.DuplicateBindingError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList())), new hydra.core.Name("x"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("duplicate bindings in lambda body", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
            new hydra.core.Binding(new hydra.core.Name("a"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
            new hydra.core.Binding(new hydra.core.Name("a"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("a"))))))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.DuplicateBinding(new hydra.error.core.DuplicateBindingError(new hydra.paths.SubtermPath(java.util.Arrays.asList(new hydra.paths.SubtermStep.LambdaBody())), new hydra.core.Name("a"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("duplicate bindings in let body", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
            new hydra.core.Binding(new hydra.core.Name("y"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
            new hydra.core.Binding(new hydra.core.Name("y"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("y")))))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.DuplicateBinding(new hydra.error.core.DuplicateBindingError(new hydra.paths.SubtermPath(java.util.Arrays.asList(new hydra.paths.SubtermStep.LetBody())), new hydra.core.Name("y"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("duplicate bindings in let binding value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
            new hydra.core.Binding(new hydra.core.Name("a"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
            new hydra.core.Binding(new hydra.core.Name("a"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("a")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.DuplicateBinding(new hydra.error.core.DuplicateBindingError(new hydra.paths.SubtermPath(java.util.Arrays.asList(new hydra.paths.SubtermStep.LetBinding(new hydra.core.Name("x")))), new hydra.core.Name("a"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("same name in different scopes is valid", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup duplicateFieldsTests() {
    return new hydra.testing.TestGroup("duplicate fields", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("no fields (literal)", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("distinct record fields", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Point"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),
            new hydra.core.Field(new hydra.core.Name("y"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("duplicate record fields at top level", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Point"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),
            new hydra.core.Field(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.DuplicateField(new hydra.error.core.DuplicateFieldError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList())), new hydra.core.Name("x"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("duplicate fields in record inside lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Point"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),
            new hydra.core.Field(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))))))))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.DuplicateField(new hydra.error.core.DuplicateFieldError(new hydra.paths.SubtermPath(java.util.Arrays.asList(new hydra.paths.SubtermStep.LambdaBody())), new hydra.core.Name("x"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("duplicate fields in record inside let body", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("r"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Point"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),
            new hydra.core.Field(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.DuplicateField(new hydra.error.core.DuplicateFieldError(new hydra.paths.SubtermPath(java.util.Arrays.asList(new hydra.paths.SubtermStep.LetBody())), new hydra.core.Name("x"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup emptyLetBindingsTests() {
    return new hydra.testing.TestGroup("empty let bindings", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("let with bindings is valid", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("empty let bindings", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let((java.util.List<hydra.core.Binding>) (java.util.Collections.<hydra.core.Binding>emptyList()), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.EmptyLetBindings(new hydra.error.core.EmptyLetBindingsError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup identityApplicationTests() {
    return new hydra.testing.TestGroup("identity application", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("non-identity lambda application is valid", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("identity lambda application", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.UnnecessaryIdentityApplication(new hydra.error.core.UnnecessaryIdentityApplicationError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup variableShadowingTests() {
    return new hydra.testing.TestGroup("variable shadowing", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("lambda with fresh variable is valid", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("lambda shadows outer lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("let binding shadows lambda parameter", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        hydra.validate.Core.term(
          false,
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))), hydra.lib.maybes.Maybe.applyLazy(
        () -> "valid",
        (java.util.function.Function<hydra.error.core.InvalidTermError, String>) (e -> hydra.show.error.Core.invalidTermError(e)),
        (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }
}
