// Note: this is an automatically generated file. Do not edit.

package hydra.test.serialization;

/**
 * Test cases for AST serialization
 */
public interface Serialization {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("serialization", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.List.of(
      new hydra.testing.TestGroup("associativity", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.List.<hydra.testing.TestGroup>of()), java.util.List.of(new hydra.testing.TestCaseWithMetadata("right-associative operator", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.serialization.Serialization.ifx(
        hydra.ext.haskell.operators.Operators.arrowOp(),
        hydra.serialization.Serialization.ifx(
          hydra.ext.haskell.operators.Operators.arrowOp(),
          hydra.serialization.Serialization.cst("a"),
          hydra.serialization.Serialization.cst("b")),
        hydra.serialization.Serialization.ifx(
          hydra.ext.haskell.operators.Operators.arrowOp(),
          hydra.serialization.Serialization.cst("c"),
          hydra.serialization.Serialization.cst("d"))), "(a -> b) -> c -> d")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())))),
      new hydra.testing.TestGroup("case statements", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.List.<hydra.testing.TestGroup>of()), java.util.List.of(
        new hydra.testing.TestCaseWithMetadata("simple case statement", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.serialization.Serialization.ifx(
          new hydra.ast.Op(new hydra.ast.Symbol("of"), new hydra.ast.Padding(new hydra.ast.Ws.Space(), new hydra.ast.Ws.BreakAndIndent("  ")), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None()),
          hydra.serialization.Serialization.spaceSep(java.util.List.of(
            hydra.serialization.Serialization.cst("case"),
            hydra.serialization.Serialization.ifx(
              hydra.ext.haskell.operators.Operators.gtOp(),
              hydra.serialization.Serialization.cst("x"),
              hydra.serialization.Serialization.num(42)))),
          hydra.serialization.Serialization.newlineSep(java.util.List.of(
            hydra.serialization.Serialization.ifx(
              hydra.ext.haskell.operators.Operators.caseOp(),
              hydra.serialization.Serialization.cst("False"),
              hydra.serialization.Serialization.cst("Big")),
            hydra.serialization.Serialization.ifx(
              hydra.ext.haskell.operators.Operators.caseOp(),
              hydra.serialization.Serialization.cst("True"),
              hydra.serialization.Serialization.cst("Small"))))), "case x > 42 of\n  False -> Big\n  True -> Small")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("nested case statement", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.serialization.Serialization.ifx(
          new hydra.ast.Op(new hydra.ast.Symbol("of"), new hydra.ast.Padding(new hydra.ast.Ws.Space(), new hydra.ast.Ws.BreakAndIndent("  ")), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None()),
          hydra.serialization.Serialization.spaceSep(java.util.List.of(
            hydra.serialization.Serialization.cst("case"),
            hydra.serialization.Serialization.ifx(
              hydra.ext.haskell.operators.Operators.gtOp(),
              hydra.serialization.Serialization.cst("x"),
              hydra.serialization.Serialization.num(42)))),
          hydra.serialization.Serialization.newlineSep(java.util.List.of(
            hydra.serialization.Serialization.ifx(
              hydra.ext.haskell.operators.Operators.caseOp(),
              hydra.serialization.Serialization.cst("True"),
              hydra.serialization.Serialization.ifx(
                new hydra.ast.Op(new hydra.ast.Symbol("of"), new hydra.ast.Padding(new hydra.ast.Ws.Space(), new hydra.ast.Ws.BreakAndIndent("  ")), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None()),
                hydra.serialization.Serialization.spaceSep(java.util.List.of(
                  hydra.serialization.Serialization.cst("case"),
                  hydra.serialization.Serialization.ifx(
                    hydra.ext.haskell.operators.Operators.gtOp(),
                    hydra.serialization.Serialization.cst("x"),
                    hydra.serialization.Serialization.num(100)))),
                hydra.serialization.Serialization.newlineSep(java.util.List.of(
                  hydra.serialization.Serialization.ifx(
                    hydra.ext.haskell.operators.Operators.caseOp(),
                    hydra.serialization.Serialization.cst("True"),
                    hydra.serialization.Serialization.cst("ReallyBig")),
                  hydra.serialization.Serialization.ifx(
                    hydra.ext.haskell.operators.Operators.caseOp(),
                    hydra.serialization.Serialization.cst("False"),
                    hydra.serialization.Serialization.cst("Big")))))),
            hydra.serialization.Serialization.ifx(
              hydra.ext.haskell.operators.Operators.caseOp(),
              hydra.serialization.Serialization.cst("False"),
              hydra.serialization.Serialization.cst("Small"))))), "case x > 42 of\n  True -> case x > 100 of\n    True -> ReallyBig\n    False -> Big\n  False -> Small")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())))),
      new hydra.testing.TestGroup("lambdas", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.List.<hydra.testing.TestGroup>of()), java.util.List.of(new hydra.testing.TestCaseWithMetadata("simple lambda", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.serialization.Serialization.ifx(
        hydra.ext.haskell.operators.Operators.lambdaOp(),
        hydra.serialization.Serialization.cst("\\x y"),
        hydra.serialization.Serialization.ifx(
          hydra.ext.haskell.operators.Operators.plusOp(),
          hydra.serialization.Serialization.cst("x"),
          hydra.serialization.Serialization.cst("y"))), "\\x y -> x + y")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())))),
      new hydra.testing.TestGroup("lists", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.List.<hydra.testing.TestGroup>of()), java.util.List.of(
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.serialization.Serialization.bracketList(
          hydra.serialization.Serialization.inlineStyle(),
          (java.util.List<hydra.ast.Expr>) (java.util.List.<hydra.ast.Expr>of())), "[]")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("simple non-empty list", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.serialization.Serialization.bracketList(
          hydra.serialization.Serialization.inlineStyle(),
          java.util.List.of(
            hydra.serialization.Serialization.num(1),
            hydra.serialization.Serialization.num(2),
            hydra.serialization.Serialization.num(3))), "[1, 2, 3]")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("nested list", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.serialization.Serialization.bracketList(
          hydra.serialization.Serialization.inlineStyle(),
          java.util.List.of(
            hydra.serialization.Serialization.bracketList(
              hydra.serialization.Serialization.inlineStyle(),
              java.util.List.of(
                hydra.serialization.Serialization.num(1),
                hydra.serialization.Serialization.num(3))),
            hydra.serialization.Serialization.num(2))), "[[1, 3], 2]")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("list with parenthesized expression inside", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.serialization.Serialization.bracketList(
          hydra.serialization.Serialization.inlineStyle(),
          java.util.List.of(
            hydra.serialization.Serialization.bracketList(
              hydra.serialization.Serialization.inlineStyle(),
              java.util.List.of(
                hydra.serialization.Serialization.num(1),
                hydra.serialization.Serialization.ifx(
                  hydra.ext.haskell.operators.Operators.multOp(),
                  hydra.serialization.Serialization.ifx(
                    hydra.ext.haskell.operators.Operators.plusOp(),
                    hydra.serialization.Serialization.num(2),
                    hydra.serialization.Serialization.num(3)),
                  hydra.serialization.Serialization.ifx(
                    hydra.ext.haskell.operators.Operators.plusOp(),
                    hydra.serialization.Serialization.num(1),
                    hydra.serialization.Serialization.num(10))))),
            hydra.serialization.Serialization.num(2))), "[[1, (2 + 3) * (1 + 10)], 2]")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())))),
      new hydra.testing.TestGroup("precedence", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.List.<hydra.testing.TestGroup>of()), java.util.List.of(
        new hydra.testing.TestCaseWithMetadata("operators with different precedence - no parens needed", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.serialization.Serialization.ifx(
          hydra.ext.haskell.operators.Operators.plusOp(),
          hydra.serialization.Serialization.ifx(
            hydra.ext.haskell.operators.Operators.multOp(),
            hydra.serialization.Serialization.num(2),
            hydra.serialization.Serialization.num(3)),
          hydra.serialization.Serialization.ifx(
            hydra.ext.haskell.operators.Operators.multOp(),
            hydra.serialization.Serialization.num(1),
            hydra.serialization.Serialization.num(10))), "2 * 3 + 1 * 10")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("operators with different precedence - parens needed", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.serialization.Serialization.ifx(
          hydra.ext.haskell.operators.Operators.multOp(),
          hydra.serialization.Serialization.ifx(
            hydra.ext.haskell.operators.Operators.plusOp(),
            hydra.serialization.Serialization.num(2),
            hydra.serialization.Serialization.num(3)),
          hydra.serialization.Serialization.ifx(
            hydra.ext.haskell.operators.Operators.plusOp(),
            hydra.serialization.Serialization.num(1),
            hydra.serialization.Serialization.num(10))), "(2 + 3) * (1 + 10)")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("associative operator left nesting", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.serialization.Serialization.ifx(
          hydra.ext.haskell.operators.Operators.multOp(),
          hydra.serialization.Serialization.cst("x"),
          hydra.serialization.Serialization.ifx(
            hydra.ext.haskell.operators.Operators.multOp(),
            hydra.serialization.Serialization.cst("y"),
            hydra.serialization.Serialization.cst("z"))), "x * y * z")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("associative operator right nesting", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.serialization.Serialization.ifx(
          hydra.ext.haskell.operators.Operators.multOp(),
          hydra.serialization.Serialization.ifx(
            hydra.ext.haskell.operators.Operators.multOp(),
            hydra.serialization.Serialization.cst("x"),
            hydra.serialization.Serialization.cst("y")),
          hydra.serialization.Serialization.cst("z")), "x * y * z")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.List.<hydra.testing.TestCaseWithMetadata>of()));
  }
}
