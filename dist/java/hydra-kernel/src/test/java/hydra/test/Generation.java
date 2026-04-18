// Note: this is an automatically generated file. Do not edit.

package hydra.test;

/**
 * Test cases for code generation operations such as inferModules and inferModulesGiven
 */
public interface Generation {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("generation", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(new hydra.testing.TestGroup("inferModulesGiven", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("incremental inference of subset matches full inference", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<java.util.List<hydra.packaging.Module>, String>) (ms -> hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.packaging.Module, String>) (m -> hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.packaging.Definition, String>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
              @Override
              public String visit(hydra.packaging.Definition.Type td) {
                return "";
              }

              @Override
              public String visit(hydra.packaging.Definition.Term td) {
                return hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  (td).value.name.value,
                  " :: ",
                  hydra.lib.maybes.Maybe.applyLazy(
                    () -> "<no scheme>",
                    (java.util.function.Function<hydra.core.TypeScheme, String>) (ts -> hydra.show.Core.typeScheme(ts)),
                    (td).value.type),
                  " = ",
                  hydra.show.Core.term((td).value.term),
                  "\n"));
              }
            })),
            (m).definitions))),
          ms))),
        hydra.Codegen.inferModulesGiven(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          java.util.Arrays.asList(
            new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.a"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.a.idA"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))), hydra.util.Maybe.just(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("a")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Variable(new hydra.core.Name("a")))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
            new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.b"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.b.useId"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.testInput.a.idA")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())))), java.util.Arrays.asList(new hydra.packaging.Namespace("hydra.testInput.a")), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))),
          java.util.Arrays.asList(new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.b"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.b.useId"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.testInput.a.idA")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())))), java.util.Arrays.asList(new hydra.packaging.Namespace("hydra.testInput.a")), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))))), hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<java.util.List<hydra.packaging.Module>, String>) (ms -> hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.packaging.Module, String>) (m -> hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.packaging.Definition, String>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
              @Override
              public String visit(hydra.packaging.Definition.Type td) {
                return "";
              }

              @Override
              public String visit(hydra.packaging.Definition.Term td) {
                return hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  (td).value.name.value,
                  " :: ",
                  hydra.lib.maybes.Maybe.applyLazy(
                    () -> "<no scheme>",
                    (java.util.function.Function<hydra.core.TypeScheme, String>) (ts -> hydra.show.Core.typeScheme(ts)),
                    (td).value.type),
                  " = ",
                  hydra.show.Core.term((td).value.term),
                  "\n"));
              }
            })),
            (m).definitions))),
          ms))),
        hydra.Codegen.inferModules(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          java.util.Arrays.asList(
            new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.a"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.a.idA"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))), hydra.util.Maybe.just(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("a")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Variable(new hydra.core.Name("a")))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
            new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.b"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.b.useId"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.testInput.a.idA")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())))), java.util.Arrays.asList(new hydra.packaging.Namespace("hydra.testInput.a")), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))),
          java.util.Arrays.asList(new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.b"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.b.useId"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.testInput.a.idA")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())))), java.util.Arrays.asList(new hydra.packaging.Namespace("hydra.testInput.a")), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("incremental inference of full universe matches full inference", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<java.util.List<hydra.packaging.Module>, String>) (ms -> hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.packaging.Module, String>) (m -> hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.packaging.Definition, String>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
              @Override
              public String visit(hydra.packaging.Definition.Type td) {
                return "";
              }

              @Override
              public String visit(hydra.packaging.Definition.Term td) {
                return hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  (td).value.name.value,
                  " :: ",
                  hydra.lib.maybes.Maybe.applyLazy(
                    () -> "<no scheme>",
                    (java.util.function.Function<hydra.core.TypeScheme, String>) (ts -> hydra.show.Core.typeScheme(ts)),
                    (td).value.type),
                  " = ",
                  hydra.show.Core.term((td).value.term),
                  "\n"));
              }
            })),
            (m).definitions))),
          ms))),
        hydra.Codegen.inferModulesGiven(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          java.util.Arrays.asList(
            new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.a"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.a.idA"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))), hydra.util.Maybe.just(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("a")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Variable(new hydra.core.Name("a")))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
            new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.b"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.b.useId"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.testInput.a.idA")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())))), java.util.Arrays.asList(new hydra.packaging.Namespace("hydra.testInput.a")), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))),
          java.util.Arrays.asList(
            new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.a"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.a.idA"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))), hydra.util.Maybe.just(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("a")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Variable(new hydra.core.Name("a")))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
            new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.b"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.b.useId"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.testInput.a.idA")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())))), java.util.Arrays.asList(new hydra.packaging.Namespace("hydra.testInput.a")), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))))), hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<java.util.List<hydra.packaging.Module>, String>) (ms -> hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.packaging.Module, String>) (m -> hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.packaging.Definition, String>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
              @Override
              public String visit(hydra.packaging.Definition.Type td) {
                return "";
              }

              @Override
              public String visit(hydra.packaging.Definition.Term td) {
                return hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  (td).value.name.value,
                  " :: ",
                  hydra.lib.maybes.Maybe.applyLazy(
                    () -> "<no scheme>",
                    (java.util.function.Function<hydra.core.TypeScheme, String>) (ts -> hydra.show.Core.typeScheme(ts)),
                    (td).value.type),
                  " = ",
                  hydra.show.Core.term((td).value.term),
                  "\n"));
              }
            })),
            (m).definitions))),
          ms))),
        hydra.Codegen.inferModules(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          java.util.Arrays.asList(
            new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.a"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.a.idA"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))), hydra.util.Maybe.just(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("a")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Variable(new hydra.core.Name("a")))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
            new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.b"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.b.useId"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.testInput.a.idA")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())))), java.util.Arrays.asList(new hydra.packaging.Namespace("hydra.testInput.a")), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))),
          java.util.Arrays.asList(
            new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.a"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.a.idA"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))), hydra.util.Maybe.just(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("a")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Variable(new hydra.core.Name("a")))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
            new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.b"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.b.useId"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.testInput.a.idA")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())))), java.util.Arrays.asList(new hydra.packaging.Namespace("hydra.testInput.a")), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("incremental inference uses cached scheme verbatim on vacuous-quantifier universe", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<java.util.List<hydra.packaging.Module>, String>) (ms -> hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.packaging.Module, String>) (m -> hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.packaging.Definition, String>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
              @Override
              public String visit(hydra.packaging.Definition.Type td) {
                return "";
              }

              @Override
              public String visit(hydra.packaging.Definition.Term td) {
                return hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  (td).value.name.value,
                  " :: ",
                  hydra.lib.maybes.Maybe.applyLazy(
                    () -> "<no scheme>",
                    (java.util.function.Function<hydra.core.TypeScheme, String>) (ts -> hydra.show.Core.typeScheme(ts)),
                    (td).value.type),
                  " = ",
                  hydra.show.Core.term((td).value.term),
                  "\n"));
              }
            })),
            (m).definitions))),
          ms))),
        hydra.Codegen.inferModulesGiven(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          java.util.Arrays.asList(
            new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.v"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.v.funky"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("z"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("z")))))))), hydra.util.Maybe.just(new hydra.core.TypeScheme(java.util.Arrays.asList(
              new hydra.core.Name("t0"),
              new hydra.core.Name("t1"),
              new hydra.core.Name("t2")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t1")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t2")), new hydra.core.Type.Variable(new hydra.core.Name("t2")))))))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
            new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.w"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.w.useFunky"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.testInput.v.funky")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(7))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(100))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())))), java.util.Arrays.asList(new hydra.packaging.Namespace("hydra.testInput.v")), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))),
          java.util.Arrays.asList(new hydra.packaging.Module(new hydra.packaging.Namespace("hydra.testInput.w"), java.util.Arrays.asList(new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name("hydra.testInput.w.useFunky"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.testInput.v.funky")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(7))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(100))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())))), java.util.Arrays.asList(new hydra.packaging.Namespace("hydra.testInput.v")), (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))))), "hydra.testInput.w.useFunky :: (int32) = (hydra.testInput.v.funky\u27E8string\u27E9\u27E8int32\u27E9\u27E8int32\u27E9 @ \"foo\" @ 7:int32 @ 100:int32)\n")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}
