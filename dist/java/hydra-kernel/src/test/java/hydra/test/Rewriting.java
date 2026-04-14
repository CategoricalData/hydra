// Note: this is an automatically generated file. Do not edit.

package hydra.test;

/**
 * Test cases for core rewrite/fold combinators
 */
public interface Rewriting {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("rewriting", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("foldOverTerm", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("collect labels from single node - pre-order", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Literal, hydra.core.Term>) (lit -> new hydra.core.Term.Literal(lit)),
          hydra.Rewriting.foldOverTerm(
            new hydra.coders.TraversalOrder.Pre(),
            (java.util.function.Function<java.util.List<hydra.core.Literal>, java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Literal>>>) (acc -> (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Literal>>) (term -> hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              acc,
              (term).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public java.util.List<hydra.core.Literal> otherwise(hydra.core.Term instance) {
                  return (java.util.List<hydra.core.Literal>) (java.util.Collections.<hydra.core.Literal>emptyList());
                }

                @Override
                public java.util.List<hydra.core.Literal> visit(hydra.core.Term.Pair p) {
                  return hydra.lib.pairs.First.apply((p).value).accept(new hydra.core.Term.PartialVisitor<>() {
                    @Override
                    public java.util.List<hydra.core.Literal> otherwise(hydra.core.Term instance) {
                      return (java.util.List<hydra.core.Literal>) (java.util.Collections.<hydra.core.Literal>emptyList());
                    }

                    @Override
                    public java.util.List<hydra.core.Literal> visit(hydra.core.Term.Literal lit) {
                      return java.util.Arrays.asList((lit).value);
                    }
                  });
                }
              }))))),
            (java.util.List<hydra.core.Literal>) (java.util.Collections.<hydra.core.Literal>emptyList()),
            new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")), new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())))))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.String_("a"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("collect labels from tree - pre-order", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Literal, hydra.core.Term>) (lit -> new hydra.core.Term.Literal(lit)),
          hydra.Rewriting.foldOverTerm(
            new hydra.coders.TraversalOrder.Pre(),
            (java.util.function.Function<java.util.List<hydra.core.Literal>, java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Literal>>>) (acc -> (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Literal>>) (term -> hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              acc,
              (term).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public java.util.List<hydra.core.Literal> otherwise(hydra.core.Term instance) {
                  return (java.util.List<hydra.core.Literal>) (java.util.Collections.<hydra.core.Literal>emptyList());
                }

                @Override
                public java.util.List<hydra.core.Literal> visit(hydra.core.Term.Pair p) {
                  return hydra.lib.pairs.First.apply((p).value).accept(new hydra.core.Term.PartialVisitor<>() {
                    @Override
                    public java.util.List<hydra.core.Literal> otherwise(hydra.core.Term instance) {
                      return (java.util.List<hydra.core.Literal>) (java.util.Collections.<hydra.core.Literal>emptyList());
                    }

                    @Override
                    public java.util.List<hydra.core.Literal> visit(hydra.core.Term.Literal lit) {
                      return java.util.Arrays.asList((lit).value);
                    }
                  });
                }
              }))))),
            (java.util.List<hydra.core.Literal>) (java.util.Collections.<hydra.core.Literal>emptyList()),
            new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")), new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())))))),
              new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("c")), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("d")), new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())))))))))))))))))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("c")),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("d"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("collect labels from single node - post-order", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Literal, hydra.core.Term>) (lit -> new hydra.core.Term.Literal(lit)),
          hydra.Rewriting.foldOverTerm(
            new hydra.coders.TraversalOrder.Post(),
            (java.util.function.Function<java.util.List<hydra.core.Literal>, java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Literal>>>) (acc -> (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Literal>>) (term -> hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              acc,
              (term).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public java.util.List<hydra.core.Literal> otherwise(hydra.core.Term instance) {
                  return (java.util.List<hydra.core.Literal>) (java.util.Collections.<hydra.core.Literal>emptyList());
                }

                @Override
                public java.util.List<hydra.core.Literal> visit(hydra.core.Term.Pair p) {
                  return hydra.lib.pairs.First.apply((p).value).accept(new hydra.core.Term.PartialVisitor<>() {
                    @Override
                    public java.util.List<hydra.core.Literal> otherwise(hydra.core.Term instance) {
                      return (java.util.List<hydra.core.Literal>) (java.util.Collections.<hydra.core.Literal>emptyList());
                    }

                    @Override
                    public java.util.List<hydra.core.Literal> visit(hydra.core.Term.Literal lit) {
                      return java.util.Arrays.asList((lit).value);
                    }
                  });
                }
              }))))),
            (java.util.List<hydra.core.Literal>) (java.util.Collections.<hydra.core.Literal>emptyList()),
            new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")), new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())))))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.String_("a"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("collect labels from tree - post-order", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Literal, hydra.core.Term>) (lit -> new hydra.core.Term.Literal(lit)),
          hydra.Rewriting.foldOverTerm(
            new hydra.coders.TraversalOrder.Post(),
            (java.util.function.Function<java.util.List<hydra.core.Literal>, java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Literal>>>) (acc -> (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Literal>>) (term -> hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              acc,
              (term).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public java.util.List<hydra.core.Literal> otherwise(hydra.core.Term instance) {
                  return (java.util.List<hydra.core.Literal>) (java.util.Collections.<hydra.core.Literal>emptyList());
                }

                @Override
                public java.util.List<hydra.core.Literal> visit(hydra.core.Term.Pair p) {
                  return hydra.lib.pairs.First.apply((p).value).accept(new hydra.core.Term.PartialVisitor<>() {
                    @Override
                    public java.util.List<hydra.core.Literal> otherwise(hydra.core.Term instance) {
                      return (java.util.List<hydra.core.Literal>) (java.util.Collections.<hydra.core.Literal>emptyList());
                    }

                    @Override
                    public java.util.List<hydra.core.Literal> visit(hydra.core.Term.Literal lit) {
                      return java.util.Arrays.asList((lit).value);
                    }
                  });
                }
              }))))),
            (java.util.List<hydra.core.Literal>) (java.util.Collections.<hydra.core.Literal>emptyList()),
            new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")), new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())))))),
              new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("c")), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("d")), new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())))))))))))))))))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("d")),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("c")),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("a"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sum int32 literals", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(hydra.Rewriting.foldOverTerm(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, Integer>>) (acc -> (java.util.function.Function<hydra.core.Term, Integer>) (term -> hydra.lib.math.Add.apply(
            acc,
            (term).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public Integer otherwise(hydra.core.Term instance) {
                return 0;
              }

              @Override
              public Integer visit(hydra.core.Term.Literal lit) {
                return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                  @Override
                  public Integer otherwise(hydra.core.Literal instance) {
                    return 0;
                  }

                  @Override
                  public Integer visit(hydra.core.Literal.Integer_ intVal) {
                    return (intVal).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                      @Override
                      public Integer otherwise(hydra.core.IntegerValue instance) {
                        return 0;
                      }

                      @Override
                      public Integer visit(hydra.core.IntegerValue.Int32 n) {
                        return (n).value;
                      }
                    });
                  }
                });
              }
            })))),
          0,
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10)))))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(52)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("collect list lengths - pre-order", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<Integer, hydra.core.Term>) (n -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(n)))),
          hydra.Rewriting.foldOverTerm(
            new hydra.coders.TraversalOrder.Pre(),
            (java.util.function.Function<java.util.List<Integer>, java.util.function.Function<hydra.core.Term, java.util.List<Integer>>>) (acc -> (java.util.function.Function<hydra.core.Term, java.util.List<Integer>>) (term -> hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              acc,
              (term).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public java.util.List<Integer> otherwise(hydra.core.Term instance) {
                  return (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList());
                }

                @Override
                public java.util.List<Integer> visit(hydra.core.Term.List elems) {
                  return java.util.Arrays.asList(hydra.lib.lists.Length.apply((elems).value));
                }
              }))))),
            (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()),
            new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.List(java.util.Arrays.asList(
                new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")),
                new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")))),
              new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.String_("quux")))))))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("collect list lengths - post-order", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<Integer, hydra.core.Term>) (n -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(n)))),
          hydra.Rewriting.foldOverTerm(
            new hydra.coders.TraversalOrder.Post(),
            (java.util.function.Function<java.util.List<Integer>, java.util.function.Function<hydra.core.Term, java.util.List<Integer>>>) (acc -> (java.util.function.Function<hydra.core.Term, java.util.List<Integer>>) (term -> hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              acc,
              (term).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public java.util.List<Integer> otherwise(hydra.core.Term instance) {
                  return (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList());
                }

                @Override
                public java.util.List<Integer> visit(hydra.core.Term.List elems) {
                  return java.util.Arrays.asList(hydra.lib.lists.Length.apply((elems).value));
                }
              }))))),
            (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()),
            new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.List(java.util.Arrays.asList(
                new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")),
                new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")))),
              new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.String_("quux")))))))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("rewriteType", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("String type in left side of either is replaced", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.type(hydra.Rewriting.rewriteType(
          (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              typ,
              new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            () -> new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            () -> (recurse).apply(typ)))),
          new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), hydra.show.Core.type(new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("String type in right side of either is replaced", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.type(hydra.Rewriting.rewriteType(
          (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              typ,
              new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            () -> new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            () -> (recurse).apply(typ)))),
          new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))), hydra.show.Core.type(new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("String types in both sides of either are replaced", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.type(hydra.Rewriting.rewriteType(
          (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              typ,
              new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            () -> new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            () -> (recurse).apply(typ)))),
          new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))), hydra.show.Core.type(new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("String type in nested either (left of left) is replaced", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.type(hydra.Rewriting.rewriteType(
          (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              typ,
              new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            () -> new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            () -> (recurse).apply(typ)))),
          new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int64())))))), hydra.show.Core.type(new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int64()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("String type in nested either (right of right) is replaced", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.type(hydra.Rewriting.rewriteType(
          (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              typ,
              new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            () -> new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            () -> (recurse).apply(typ)))),
          new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int64())), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))))), hydra.show.Core.type(new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int64())), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("String types in complex nested either are all replaced", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.type(hydra.Rewriting.rewriteType(
          (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              typ,
              new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            () -> new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            () -> (recurse).apply(typ)))),
          new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int64())))))))), hydra.show.Core.type(new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int64()))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("String in list type is replaced", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.type(hydra.Rewriting.rewriteType(
          (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              typ,
              new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            () -> new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            () -> (recurse).apply(typ)))),
          new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("String in function domain is replaced", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.type(hydra.Rewriting.rewriteType(
          (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              typ,
              new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            () -> new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            () -> (recurse).apply(typ)))),
          new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int64())))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int64()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("String in function codomain is replaced", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.type(hydra.Rewriting.rewriteType(
          (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              typ,
              new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            () -> new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            () -> (recurse).apply(typ)))),
          new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int64())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int64())), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("String in optional type is replaced", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.type(hydra.Rewriting.rewriteType(
          (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              typ,
              new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            () -> new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            () -> (recurse).apply(typ)))),
          new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))), hydra.show.Core.type(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("rewriteTerm", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("string literal foo replaced with bar", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in variable not changed", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Variable(new hydra.core.Name("x")))), hydra.show.Core.term(new hydra.core.Term.Variable(new hydra.core.Name("x"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz")))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("multiple strings in list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz")))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in optional (just)", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in function application", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("print")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))), hydra.show.Core.term(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("print")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in lambda body", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))), hydra.show.Core.term(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in nested applications", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))), hydra.show.Core.term(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in record field", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Person"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))), hydra.show.Core.term(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Person"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("strings in multiple record fields", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Data"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("a"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            new hydra.core.Field(new hydra.core.Name("b"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz"))),
            new hydra.core.Field(new hydra.core.Name("c"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))), hydra.show.Core.term(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Data"), java.util.Arrays.asList(
          new hydra.core.Field(new hydra.core.Name("a"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))),
          new hydra.core.Field(new hydra.core.Name("b"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz"))),
          new hydra.core.Field(new hydra.core.Name("c"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in pair", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))))), hydra.show.Core.term(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in let binding value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), hydra.show.Core.term(new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in let body", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))), hydra.show.Core.term(new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in first case branch", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Cases(new hydra.core.CaseStatement(new hydra.core.Name("Result"), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("success"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            new hydra.core.Field(new hydra.core.Name("error"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz")))))))), hydra.show.Core.term(new hydra.core.Term.Cases(new hydra.core.CaseStatement(new hydra.core.Name("Result"), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
          new hydra.core.Field(new hydra.core.Name("success"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))),
          new hydra.core.Field(new hydra.core.Name("error"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in second case branch", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Cases(new hydra.core.CaseStatement(new hydra.core.Name("Result"), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("success"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz"))),
            new hydra.core.Field(new hydra.core.Name("error"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))), hydra.show.Core.term(new hydra.core.Term.Cases(new hydra.core.CaseStatement(new hydra.core.Name("Result"), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
          new hydra.core.Field(new hydra.core.Name("success"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz"))),
          new hydra.core.Field(new hydra.core.Name("error"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in default branch", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Cases(new hydra.core.CaseStatement(new hydra.core.Name("Result"), hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("success"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz"))),
            new hydra.core.Field(new hydra.core.Name("error"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz")))))))), hydra.show.Core.term(new hydra.core.Term.Cases(new hydra.core.CaseStatement(new hydra.core.Name("Result"), hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))), java.util.Arrays.asList(
          new hydra.core.Field(new hydra.core.Name("success"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz"))),
          new hydra.core.Field(new hydra.core.Name("error"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string deeply nested in record in list in application", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("process")), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Item"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))))))), hydra.show.Core.term(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("process")), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Item"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in union inject value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("Result"), new hydra.core.Field(new hydra.core.Name("success"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))))))), hydra.show.Core.term(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("Result"), new hydra.core.Field(new hydra.core.Name("success"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in wrapped term", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("Email"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))), hydra.show.Core.term(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("Email"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in annotated term body", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))))), hydra.show.Core.term(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in first of multiple let bindings", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
            new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
            new hydra.core.Binding(new hydra.core.Name("y"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), hydra.show.Core.term(new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
          new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
          new hydra.core.Binding(new hydra.core.Name("y"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in second of multiple let bindings", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
            new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
            new hydra.core.Binding(new hydra.core.Name("y"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("y")))))), hydra.show.Core.term(new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
          new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
          new hydra.core.Binding(new hydra.core.Name("y"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("y"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in all let bindings and body", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
            new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
            new hydra.core.Binding(new hydra.core.Name("y"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))), hydra.show.Core.term(new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
          new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
          new hydra.core.Binding(new hydra.core.Name("y"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz")),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))))))), hydra.show.Core.term(new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz")))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in type lambda body", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(new hydra.core.Name("a"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))), hydra.show.Core.term(new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(new hydra.core.Name("a"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in type application body", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))), hydra.show.Core.term(new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in nested type lambdas", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(new hydra.core.Name("a"), new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(new hydra.core.Name("b"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))), hydra.show.Core.term(new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(new hydra.core.Name("a"), new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(new hydra.core.Name("b"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in case branch within let binding", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("handler"), new hydra.core.Term.Cases(new hydra.core.CaseStatement(new hydra.core.Name("Result"), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("ok"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            new hydra.core.Field(new hydra.core.Name("err"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz")))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("handler")))))), hydra.show.Core.term(new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("handler"), new hydra.core.Term.Cases(new hydra.core.CaseStatement(new hydra.core.Name("Result"), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
          new hydra.core.Field(new hydra.core.Name("ok"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))),
          new hydra.core.Field(new hydra.core.Name("err"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("baz")))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("handler"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string in annotated wrapped record field", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Rewriting.rewriteTerm(
          (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              term,
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
            () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")),
            () -> (recurse).apply(term)))),
          new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("User"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("UserData"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))))), hydra.show.Core.term(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("User"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("UserData"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")))))))), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("rewriteAndFoldTermWithPath", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("path tracking through application - sum literals", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(hydra.Rewriting.foldOverTerm(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, Integer>>) (acc -> (java.util.function.Function<hydra.core.Term, Integer>) (term -> hydra.lib.math.Add.apply(
            acc,
            (term).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public Integer otherwise(hydra.core.Term instance) {
                return 0;
              }

              @Override
              public Integer visit(hydra.core.Term.Literal lit) {
                return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                  @Override
                  public Integer otherwise(hydra.core.Literal instance) {
                    return 0;
                  }

                  @Override
                  public Integer visit(hydra.core.Literal.Integer_ intVal) {
                    return (intVal).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                      @Override
                      public Integer otherwise(hydra.core.IntegerValue instance) {
                        return 0;
                      }

                      @Override
                      public Integer visit(hydra.core.IntegerValue.Int32 n) {
                        return (n).value;
                      }
                    });
                  }
                });
              }
            })))),
          0,
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("path tracking through nested applications", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(hydra.Rewriting.foldOverTerm(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, Integer>>) (acc -> (java.util.function.Function<hydra.core.Term, Integer>) (term -> hydra.lib.math.Add.apply(
            acc,
            (term).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public Integer otherwise(hydra.core.Term instance) {
                return 0;
              }

              @Override
              public Integer visit(hydra.core.Term.Literal lit) {
                return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                  @Override
                  public Integer otherwise(hydra.core.Literal instance) {
                    return 0;
                  }

                  @Override
                  public Integer visit(hydra.core.Literal.Integer_ intVal) {
                    return (intVal).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                      @Override
                      public Integer otherwise(hydra.core.IntegerValue instance) {
                        return 0;
                      }

                      @Override
                      public Integer visit(hydra.core.IntegerValue.Int32 n) {
                        return (n).value;
                      }
                    });
                  }
                });
              }
            })))),
          0,
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Variable(new hydra.core.Name("x")),
            new hydra.core.Term.Variable(new hydra.core.Name("y")))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("path tracking through let bindings", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(hydra.Rewriting.foldOverTerm(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, Integer>>) (acc -> (java.util.function.Function<hydra.core.Term, Integer>) (term -> hydra.lib.math.Add.apply(
            acc,
            (term).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public Integer otherwise(hydra.core.Term instance) {
                return 0;
              }

              @Override
              public Integer visit(hydra.core.Term.Literal lit) {
                return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                  @Override
                  public Integer otherwise(hydra.core.Literal instance) {
                    return 0;
                  }

                  @Override
                  public Integer visit(hydra.core.Literal.Integer_ intVal) {
                    return (intVal).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                      @Override
                      public Integer otherwise(hydra.core.IntegerValue instance) {
                        return 0;
                      }

                      @Override
                      public Integer visit(hydra.core.IntegerValue.Int32 n) {
                        return (n).value;
                      }
                    });
                  }
                });
              }
            })))),
          0,
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Variable(new hydra.core.Name("x")),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(32)))))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("path tracking through record fields", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(hydra.Rewriting.foldOverTerm(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, Integer>>) (acc -> (java.util.function.Function<hydra.core.Term, Integer>) (term -> hydra.lib.math.Add.apply(
            acc,
            (term).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public Integer otherwise(hydra.core.Term instance) {
                return 0;
              }

              @Override
              public Integer visit(hydra.core.Term.Literal lit) {
                return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                  @Override
                  public Integer otherwise(hydra.core.Literal instance) {
                    return 0;
                  }

                  @Override
                  public Integer visit(hydra.core.Literal.Integer_ intVal) {
                    return (intVal).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                      @Override
                      public Integer otherwise(hydra.core.IntegerValue instance) {
                        return 0;
                      }

                      @Override
                      public Integer visit(hydra.core.IntegerValue.Int32 n) {
                        return (n).value;
                      }
                    });
                  }
                });
              }
            })))),
          0,
          new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Point"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10)))),
            new hydra.core.Field(new hydra.core.Name("y"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(20)))))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("path tracking through case branches", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(hydra.Rewriting.foldOverTerm(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, Integer>>) (acc -> (java.util.function.Function<hydra.core.Term, Integer>) (term -> hydra.lib.math.Add.apply(
            acc,
            (term).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public Integer otherwise(hydra.core.Term instance) {
                return 0;
              }

              @Override
              public Integer visit(hydra.core.Term.Literal lit) {
                return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                  @Override
                  public Integer otherwise(hydra.core.Literal instance) {
                    return 0;
                  }

                  @Override
                  public Integer visit(hydra.core.Literal.Integer_ intVal) {
                    return (intVal).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                      @Override
                      public Integer otherwise(hydra.core.IntegerValue instance) {
                        return 0;
                      }

                      @Override
                      public Integer visit(hydra.core.IntegerValue.Int32 n) {
                        return (n).value;
                      }
                    });
                  }
                });
              }
            })))),
          0,
          new hydra.core.Term.Cases(new hydra.core.CaseStatement(new hydra.core.Name("Result"), (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("ok"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),
            new hydra.core.Field(new hydra.core.Name("err"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("path tracking through pair", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(hydra.Rewriting.foldOverTerm(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, Integer>>) (acc -> (java.util.function.Function<hydra.core.Term, Integer>) (term -> hydra.lib.math.Add.apply(
            acc,
            (term).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public Integer otherwise(hydra.core.Term instance) {
                return 0;
              }

              @Override
              public Integer visit(hydra.core.Term.Literal lit) {
                return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                  @Override
                  public Integer otherwise(hydra.core.Literal instance) {
                    return 0;
                  }

                  @Override
                  public Integer visit(hydra.core.Literal.Integer_ intVal) {
                    return (intVal).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                      @Override
                      public Integer otherwise(hydra.core.IntegerValue instance) {
                        return 0;
                      }

                      @Override
                      public Integer visit(hydra.core.IntegerValue.Int32 n) {
                        return (n).value;
                      }
                    });
                  }
                });
              }
            })))),
          0,
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(7)))))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("path tracking through optional", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(hydra.Rewriting.foldOverTerm(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, Integer>>) (acc -> (java.util.function.Function<hydra.core.Term, Integer>) (term -> hydra.lib.math.Add.apply(
            acc,
            (term).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public Integer otherwise(hydra.core.Term instance) {
                return 0;
              }

              @Override
              public Integer visit(hydra.core.Term.Literal lit) {
                return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                  @Override
                  public Integer otherwise(hydra.core.Literal instance) {
                    return 0;
                  }

                  @Override
                  public Integer visit(hydra.core.Literal.Integer_ intVal) {
                    return (intVal).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                      @Override
                      public Integer otherwise(hydra.core.IntegerValue instance) {
                        return 0;
                      }

                      @Override
                      public Integer visit(hydra.core.IntegerValue.Int32 n) {
                        return (n).value;
                      }
                    });
                  }
                });
              }
            })))),
          0,
          new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("path tracking through wrapped term", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(hydra.Rewriting.foldOverTerm(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, Integer>>) (acc -> (java.util.function.Function<hydra.core.Term, Integer>) (term -> hydra.lib.math.Add.apply(
            acc,
            (term).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public Integer otherwise(hydra.core.Term instance) {
                return 0;
              }

              @Override
              public Integer visit(hydra.core.Term.Literal lit) {
                return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                  @Override
                  public Integer otherwise(hydra.core.Literal instance) {
                    return 0;
                  }

                  @Override
                  public Integer visit(hydra.core.Literal.Integer_ intVal) {
                    return (intVal).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                      @Override
                      public Integer otherwise(hydra.core.IntegerValue instance) {
                        return 0;
                      }

                      @Override
                      public Integer visit(hydra.core.IntegerValue.Int32 n) {
                        return (n).value;
                      }
                    });
                  }
                });
              }
            })))),
          0,
          new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("Age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(25)))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(25)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("path tracking through type lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(hydra.Rewriting.foldOverTerm(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, Integer>>) (acc -> (java.util.function.Function<hydra.core.Term, Integer>) (term -> hydra.lib.math.Add.apply(
            acc,
            (term).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public Integer otherwise(hydra.core.Term instance) {
                return 0;
              }

              @Override
              public Integer visit(hydra.core.Term.Literal lit) {
                return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                  @Override
                  public Integer otherwise(hydra.core.Literal instance) {
                    return 0;
                  }

                  @Override
                  public Integer visit(hydra.core.Literal.Integer_ intVal) {
                    return (intVal).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                      @Override
                      public Integer otherwise(hydra.core.IntegerValue instance) {
                        return 0;
                      }

                      @Override
                      public Integer visit(hydra.core.IntegerValue.Int32 n) {
                        return (n).value;
                      }
                    });
                  }
                });
              }
            })))),
          0,
          new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(new hydra.core.Name("a"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(100)))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(100)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("path tracking through type application", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(hydra.Rewriting.foldOverTerm(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, Integer>>) (acc -> (java.util.function.Function<hydra.core.Term, Integer>) (term -> hydra.lib.math.Add.apply(
            acc,
            (term).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public Integer otherwise(hydra.core.Term instance) {
                return 0;
              }

              @Override
              public Integer visit(hydra.core.Term.Literal lit) {
                return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                  @Override
                  public Integer otherwise(hydra.core.Literal instance) {
                    return 0;
                  }

                  @Override
                  public Integer visit(hydra.core.Literal.Integer_ intVal) {
                    return (intVal).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                      @Override
                      public Integer otherwise(hydra.core.IntegerValue instance) {
                        return 0;
                      }

                      @Override
                      public Integer visit(hydra.core.IntegerValue.Int32 n) {
                        return (n).value;
                      }
                    });
                  }
                });
              }
            })))),
          0,
          new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(50))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(50)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("path tracking through set elements", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(hydra.Rewriting.foldOverTerm(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, Integer>>) (acc -> (java.util.function.Function<hydra.core.Term, Integer>) (term -> hydra.lib.math.Add.apply(
            acc,
            (term).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public Integer otherwise(hydra.core.Term instance) {
                return 0;
              }

              @Override
              public Integer visit(hydra.core.Term.Literal lit) {
                return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                  @Override
                  public Integer otherwise(hydra.core.Literal instance) {
                    return 0;
                  }

                  @Override
                  public Integer visit(hydra.core.Literal.Integer_ intVal) {
                    return (intVal).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                      @Override
                      public Integer otherwise(hydra.core.IntegerValue instance) {
                        return 0;
                      }

                      @Override
                      public Integer visit(hydra.core.IntegerValue.Int32 n) {
                        return (n).value;
                      }
                    });
                  }
                });
              }
            })))),
          0,
          new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(6)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("deep nesting - application in lambda in let", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(hydra.Rewriting.foldOverTerm(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, Integer>>) (acc -> (java.util.function.Function<hydra.core.Term, Integer>) (term -> hydra.lib.math.Add.apply(
            acc,
            (term).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public Integer otherwise(hydra.core.Term instance) {
                return 0;
              }

              @Override
              public Integer visit(hydra.core.Term.Literal lit) {
                return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                  @Override
                  public Integer otherwise(hydra.core.Literal instance) {
                    return 0;
                  }

                  @Override
                  public Integer visit(hydra.core.Literal.Integer_ intVal) {
                    return (intVal).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                      @Override
                      public Integer otherwise(hydra.core.IntegerValue instance) {
                        return 0;
                      }

                      @Override
                      public Integer visit(hydra.core.IntegerValue.Int32 n) {
                        return (n).value;
                      }
                    });
                  }
                });
              }
            })))),
          0,
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("f"), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10)))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(15)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("collect list lengths in nested structure", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<Integer, hydra.core.Term>) (n -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(n)))),
          hydra.Rewriting.foldOverTerm(
            new hydra.coders.TraversalOrder.Pre(),
            (java.util.function.Function<java.util.List<Integer>, java.util.function.Function<hydra.core.Term, java.util.List<Integer>>>) (acc -> (java.util.function.Function<hydra.core.Term, java.util.List<Integer>>) (term -> hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              acc,
              (term).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public java.util.List<Integer> otherwise(hydra.core.Term instance) {
                  return (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList());
                }

                @Override
                public java.util.List<Integer> visit(hydra.core.Term.List elems) {
                  return java.util.Arrays.asList(hydra.lib.lists.Length.apply((elems).value));
                }
              }))))),
            (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()),
            new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.List(java.util.Arrays.asList(
                new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
                new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))),
              new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("collect list lengths in let body", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<Integer, hydra.core.Term>) (n -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(n)))),
          hydra.Rewriting.foldOverTerm(
            new hydra.coders.TraversalOrder.Pre(),
            (java.util.function.Function<java.util.List<Integer>, java.util.function.Function<hydra.core.Term, java.util.List<Integer>>>) (acc -> (java.util.function.Function<hydra.core.Term, java.util.List<Integer>>) (term -> hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              acc,
              (term).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public java.util.List<Integer> otherwise(hydra.core.Term instance) {
                  return (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList());
                }

                @Override
                public java.util.List<Integer> visit(hydra.core.Term.List elems) {
                  return java.util.Arrays.asList(hydra.lib.lists.Length.apply((elems).value));
                }
              }))))),
            (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("xs"), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}
