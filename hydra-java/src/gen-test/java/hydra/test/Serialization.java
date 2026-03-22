// Note: this is an automatically generated file. Do not edit.

package hydra.test;

/**
 * Test cases for AST serialization
 */
public interface Serialization {
  static hydra.ast.Op arrowOp() {
    return hydra.Serialization.op(
      "->",
      hydra.lib.math.Negate.apply(1),
      new hydra.ast.Associativity.Right());
  }

  static hydra.ast.Op gtOp() {
    return hydra.Serialization.op(
      ">",
      4,
      new hydra.ast.Associativity.None());
  }

  static hydra.ast.Op plusOp() {
    return hydra.Serialization.op(
      "+",
      6,
      new hydra.ast.Associativity.Both());
  }

  static hydra.ast.Op multOp() {
    return hydra.Serialization.op(
      "*",
      7,
      new hydra.ast.Associativity.Both());
  }

  static hydra.ast.Op lambdaOp() {
    return hydra.Serialization.op(
      "->",
      hydra.lib.math.Negate.apply(1),
      new hydra.ast.Associativity.Right());
  }

  static hydra.ast.Op caseOp() {
    return hydra.Serialization.op(
      "->",
      0,
      new hydra.ast.Associativity.None());
  }

  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("serialization", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), hydra.util.ConsList.of(
      new hydra.testing.TestGroup("associativity", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(new hydra.testing.TestCaseWithMetadata("right-associative operator", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.Serialization.ifx(
        hydra.test.Serialization.arrowOp(),
        hydra.Serialization.ifx(
          hydra.test.Serialization.arrowOp(),
          hydra.Serialization.cst("a"),
          hydra.Serialization.cst("b")),
        hydra.Serialization.ifx(
          hydra.test.Serialization.arrowOp(),
          hydra.Serialization.cst("c"),
          hydra.Serialization.cst("d"))), "(a -> b) -> c -> d")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("case statements", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("simple case statement", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.Serialization.ifx(
          new hydra.ast.Op(new hydra.ast.Symbol("of"), new hydra.ast.Padding(new hydra.ast.Ws.Space(), new hydra.ast.Ws.BreakAndIndent("  ")), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None()),
          hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.Serialization.cst("case"),
            hydra.Serialization.ifx(
              hydra.test.Serialization.gtOp(),
              hydra.Serialization.cst("x"),
              hydra.Serialization.num(42)))),
          hydra.Serialization.newlineSep(hydra.util.ConsList.of(
            hydra.Serialization.ifx(
              hydra.test.Serialization.caseOp(),
              hydra.Serialization.cst("False"),
              hydra.Serialization.cst("Big")),
            hydra.Serialization.ifx(
              hydra.test.Serialization.caseOp(),
              hydra.Serialization.cst("True"),
              hydra.Serialization.cst("Small"))))), "case x > 42 of\n  False -> Big\n  True -> Small")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("nested case statement", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.Serialization.ifx(
          new hydra.ast.Op(new hydra.ast.Symbol("of"), new hydra.ast.Padding(new hydra.ast.Ws.Space(), new hydra.ast.Ws.BreakAndIndent("  ")), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None()),
          hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.Serialization.cst("case"),
            hydra.Serialization.ifx(
              hydra.test.Serialization.gtOp(),
              hydra.Serialization.cst("x"),
              hydra.Serialization.num(42)))),
          hydra.Serialization.newlineSep(hydra.util.ConsList.of(
            hydra.Serialization.ifx(
              hydra.test.Serialization.caseOp(),
              hydra.Serialization.cst("True"),
              hydra.Serialization.ifx(
                new hydra.ast.Op(new hydra.ast.Symbol("of"), new hydra.ast.Padding(new hydra.ast.Ws.Space(), new hydra.ast.Ws.BreakAndIndent("  ")), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None()),
                hydra.Serialization.spaceSep(hydra.util.ConsList.of(
                  hydra.Serialization.cst("case"),
                  hydra.Serialization.ifx(
                    hydra.test.Serialization.gtOp(),
                    hydra.Serialization.cst("x"),
                    hydra.Serialization.num(100)))),
                hydra.Serialization.newlineSep(hydra.util.ConsList.of(
                  hydra.Serialization.ifx(
                    hydra.test.Serialization.caseOp(),
                    hydra.Serialization.cst("True"),
                    hydra.Serialization.cst("ReallyBig")),
                  hydra.Serialization.ifx(
                    hydra.test.Serialization.caseOp(),
                    hydra.Serialization.cst("False"),
                    hydra.Serialization.cst("Big")))))),
            hydra.Serialization.ifx(
              hydra.test.Serialization.caseOp(),
              hydra.Serialization.cst("False"),
              hydra.Serialization.cst("Small"))))), "case x > 42 of\n  True -> case x > 100 of\n    True -> ReallyBig\n    False -> Big\n  False -> Small")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("lambdas", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(new hydra.testing.TestCaseWithMetadata("simple lambda", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.Serialization.ifx(
        hydra.test.Serialization.lambdaOp(),
        hydra.Serialization.cst("\\x y"),
        hydra.Serialization.ifx(
          hydra.test.Serialization.plusOp(),
          hydra.Serialization.cst("x"),
          hydra.Serialization.cst("y"))), "\\x y -> x + y")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("lists", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.Serialization.bracketList(
          hydra.Serialization.inlineStyle(),
          (hydra.util.ConsList<hydra.ast.Expr>) (hydra.util.ConsList.<hydra.ast.Expr>empty())), "[]")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("simple non-empty list", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.Serialization.bracketList(
          hydra.Serialization.inlineStyle(),
          hydra.util.ConsList.of(
            hydra.Serialization.num(1),
            hydra.Serialization.num(2),
            hydra.Serialization.num(3))), "[1, 2, 3]")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("nested list", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.Serialization.bracketList(
          hydra.Serialization.inlineStyle(),
          hydra.util.ConsList.of(
            hydra.Serialization.bracketList(
              hydra.Serialization.inlineStyle(),
              hydra.util.ConsList.of(
                hydra.Serialization.num(1),
                hydra.Serialization.num(3))),
            hydra.Serialization.num(2))), "[[1, 3], 2]")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("list with parenthesized expression inside", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.Serialization.bracketList(
          hydra.Serialization.inlineStyle(),
          hydra.util.ConsList.of(
            hydra.Serialization.bracketList(
              hydra.Serialization.inlineStyle(),
              hydra.util.ConsList.of(
                hydra.Serialization.num(1),
                hydra.Serialization.ifx(
                  hydra.test.Serialization.multOp(),
                  hydra.Serialization.ifx(
                    hydra.test.Serialization.plusOp(),
                    hydra.Serialization.num(2),
                    hydra.Serialization.num(3)),
                  hydra.Serialization.ifx(
                    hydra.test.Serialization.plusOp(),
                    hydra.Serialization.num(1),
                    hydra.Serialization.num(10))))),
            hydra.Serialization.num(2))), "[[1, (2 + 3) * (1 + 10)], 2]")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("precedence", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("operators with different precedence - no parens needed", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.Serialization.ifx(
          hydra.test.Serialization.plusOp(),
          hydra.Serialization.ifx(
            hydra.test.Serialization.multOp(),
            hydra.Serialization.num(2),
            hydra.Serialization.num(3)),
          hydra.Serialization.ifx(
            hydra.test.Serialization.multOp(),
            hydra.Serialization.num(1),
            hydra.Serialization.num(10))), "2 * 3 + 1 * 10")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("operators with different precedence - parens needed", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.Serialization.ifx(
          hydra.test.Serialization.multOp(),
          hydra.Serialization.ifx(
            hydra.test.Serialization.plusOp(),
            hydra.Serialization.num(2),
            hydra.Serialization.num(3)),
          hydra.Serialization.ifx(
            hydra.test.Serialization.plusOp(),
            hydra.Serialization.num(1),
            hydra.Serialization.num(10))), "(2 + 3) * (1 + 10)")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("associative operator left nesting", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.Serialization.ifx(
          hydra.test.Serialization.multOp(),
          hydra.Serialization.cst("x"),
          hydra.Serialization.ifx(
            hydra.test.Serialization.multOp(),
            hydra.Serialization.cst("y"),
            hydra.Serialization.cst("z"))), "x * y * z")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("associative operator right nesting", new hydra.testing.TestCase.Serialization(new hydra.testing.SerializationTestCase(hydra.Serialization.ifx(
          hydra.test.Serialization.multOp(),
          hydra.Serialization.ifx(
            hydra.test.Serialization.multOp(),
            hydra.Serialization.cst("x"),
            hydra.Serialization.cst("y")),
          hydra.Serialization.cst("z")), "x * y * z")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty()))))), (hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>) (hydra.util.ConsList.<hydra.testing.TestCaseWithMetadata>empty()));
  }
}
