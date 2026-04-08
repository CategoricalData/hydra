// Note: this is an automatically generated file. Do not edit.

package hydra.extract;

/**
 * Extraction and validation for hydra.core types
 */
public interface Core {
  static hydra.util.Either<hydra.errors.Error_, java.math.BigDecimal> bigfloat(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, java.math.BigDecimal>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.Core.floatLiteral(l),
        (java.util.function.Function<hydra.core.FloatValue, hydra.util.Either<hydra.errors.Error_, java.math.BigDecimal>>) (f -> hydra.extract.Core.bigfloatValue(f)))));
  }

  static hydra.util.Either<hydra.errors.Error_, java.math.BigDecimal> bigfloatValue(hydra.core.FloatValue v) {
    return (v).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, java.math.BigDecimal> otherwise(hydra.core.FloatValue instance) {
        return hydra.util.Either.<hydra.errors.Error_, java.math.BigDecimal>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("bigfloat", hydra.show.Core.float_(v)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.math.BigDecimal> visit(hydra.core.FloatValue.Bigfloat f) {
        return hydra.util.Either.<hydra.errors.Error_, java.math.BigDecimal>right((f).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, java.math.BigInteger> bigint(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, java.math.BigInteger>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.Core.integerLiteral(l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.errors.Error_, java.math.BigInteger>>) (i -> hydra.extract.Core.bigintValue(i)))));
  }

  static hydra.util.Either<hydra.errors.Error_, java.math.BigInteger> bigintValue(hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, java.math.BigInteger> otherwise(hydra.core.IntegerValue instance) {
        return hydra.util.Either.<hydra.errors.Error_, java.math.BigInteger>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("bigint", hydra.show.Core.integer(v)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.math.BigInteger> visit(hydra.core.IntegerValue.Bigint i) {
        return hydra.util.Either.<hydra.errors.Error_, java.math.BigInteger>right((i).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, byte[]> binary(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, byte[]>>) (l -> hydra.extract.Core.binaryLiteral(l)));
  }

  static hydra.util.Either<hydra.errors.Error_, byte[]> binaryLiteral(hydra.core.Literal v) {
    return (v).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, byte[]> otherwise(hydra.core.Literal instance) {
        return hydra.util.Either.<hydra.errors.Error_, byte[]>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("binary", hydra.show.Core.literal(v)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, byte[]> visit(hydra.core.Literal.Binary b) {
        return hydra.util.Either.<hydra.errors.Error_, byte[]>right((b).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, Boolean> boolean_(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, Boolean>>) (l -> hydra.extract.Core.booleanLiteral(l)));
  }

  static hydra.util.Either<hydra.errors.Error_, Boolean> booleanLiteral(hydra.core.Literal v) {
    return (v).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, Boolean> otherwise(hydra.core.Literal instance) {
        return hydra.util.Either.<hydra.errors.Error_, Boolean>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("boolean", hydra.show.Core.literal(v)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, Boolean> visit(hydra.core.Literal.Boolean_ b) {
        return hydra.util.Either.<hydra.errors.Error_, Boolean>right((b).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.Field> caseField(hydra.core.Name name, String n, hydra.graph.Graph graph, hydra.core.Term term) {
    hydra.core.Name fieldName = new hydra.core.Name(n);
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.cases(
        name,
        graph,
        term),
      (java.util.function.Function<hydra.core.CaseStatement, hydra.util.Either<hydra.errors.Error_, hydra.core.Field>>) (cs -> {
        hydra.util.Lazy<java.util.List<hydra.core.Field>> matching = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
            (f).name.value,
            (fieldName).value)),
          (cs).cases));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(matching.get()),
          () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Field>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("matching case", "no matching case")))),
          () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Field>right(hydra.lib.lists.Head.apply(matching.get())));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.CaseStatement> cases(hydra.core.Name name, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Lexical.stripAndDereferenceTerm(
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.CaseStatement>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.CaseStatement> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.core.CaseStatement>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("case statement", hydra.show.Core.term(term)))));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.CaseStatement> visit(hydra.core.Term.Function function) {
          return (function).value.accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.util.Either<hydra.errors.Error_, hydra.core.CaseStatement> otherwise(hydra.core.Function instance) {
              return hydra.util.Either.<hydra.errors.Error_, hydra.core.CaseStatement>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("case statement", hydra.show.Core.term(term)))));
            }

            @Override
            public hydra.util.Either<hydra.errors.Error_, hydra.core.CaseStatement> visit(hydra.core.Function.Elimination elimination) {
              return (elimination).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.errors.Error_, hydra.core.CaseStatement> otherwise(hydra.core.Elimination instance) {
                  return hydra.util.Either.<hydra.errors.Error_, hydra.core.CaseStatement>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("case statement", hydra.show.Core.term(term)))));
                }

                @Override
                public hydra.util.Either<hydra.errors.Error_, hydra.core.CaseStatement> visit(hydra.core.Elimination.Union cs) {
                  return hydra.lib.logic.IfElse.lazy(
                    hydra.lib.equality.Equal.apply(
                      (cs).value.typeName.value,
                      (name).value),
                    () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.CaseStatement>right((cs).value),
                    () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.CaseStatement>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError(hydra.lib.strings.Cat2.apply(
                      "case statement for type ",
                      (name).value), hydra.show.Core.term(term))))));
                }
              });
            }
          });
        }
      })));
  }

  static <T0, T1> hydra.util.Either<hydra.errors.DecodingError, hydra.util.Either<T0, T1>> decodeEither(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> leftDecoder, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T1>>> rightDecoder, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.stripWithDecodingError(
        g,
        term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Either<T0, T1>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.util.Either<T0, T1>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.util.Either<T0, T1>>left(new hydra.errors.DecodingError("expected either value"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.util.Either<T0, T1>> visit(hydra.core.Term.Either e) {
          return hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Either<T0, T1>>>) (lv -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<T0, hydra.util.Either<T0, T1>>) (x -> hydra.util.Either.<T0, T1>left(x)),
              (leftDecoder).apply(g).apply(lv))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Either<T0, T1>>>) (rv -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<T1, hydra.util.Either<T0, T1>>) (x -> hydra.util.Either.<T0, T1>right(x)),
              (rightDecoder).apply(g).apply(rv))),
            (e).value);
        }
      })));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, java.util.List<T0>> decodeList(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> elemDecoder, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.stripWithDecodingError(
        g,
        term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, java.util.List<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, java.util.List<T0>>left(new hydra.errors.DecodingError("expected list"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, java.util.List<T0>> visit(hydra.core.Term.List els) {
          return hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>) (v1 -> (elemDecoder).apply(g).apply(v1)),
            (els).value);
        }
      })));
  }

  static <T0, T1> hydra.util.Either<hydra.errors.DecodingError, java.util.Map<T0, T1>> decodeMap(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> keyDecoder, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T1>>> valDecoder, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.stripWithDecodingError(
        g,
        term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.Map<T0, T1>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, java.util.Map<T0, T1>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, java.util.Map<T0, T1>>left(new hydra.errors.DecodingError("expected map"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, java.util.Map<T0, T1>> visit(hydra.core.Term.Map m) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.List<hydra.util.Pair<T0, T1>>, java.util.Map<T0, T1>>) ((java.util.function.Function<java.util.List<hydra.util.Pair<T0, T1>>, java.util.Map<T0, T1>>) (hydra.lib.maps.FromList::apply)),
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Pair<T0, T1>>>) (kv -> hydra.lib.eithers.Bind.apply(
                (keyDecoder).apply(g).apply(hydra.lib.pairs.First.apply(kv)),
                (java.util.function.Function<T0, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Pair<T0, T1>>>) (k -> hydra.lib.eithers.Map.apply(
                  (java.util.function.Function<T1, hydra.util.Pair<T0, T1>>) (v -> (hydra.util.Pair<T0, T1>) ((hydra.util.Pair<T0, T1>) (new hydra.util.Pair<T0, T1>(k, v)))),
                  (valDecoder).apply(g).apply(hydra.lib.pairs.Second.apply(kv)))))),
              hydra.lib.maps.ToList.apply((m).value)));
        }
      })));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<T0>> decodeMaybe(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> elemDecoder, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.stripWithDecodingError(
        g,
        term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.util.Maybe<T0>>left(new hydra.errors.DecodingError("expected optional value"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<T0>> visit(hydra.core.Term.Maybe opt) {
          return hydra.lib.eithers.MapMaybe.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>) (v1 -> (elemDecoder).apply(g).apply(v1)),
            (opt).value);
        }
      })));
  }

  static <T0, T1> hydra.util.Either<hydra.errors.DecodingError, hydra.util.Pair<T0, T1>> decodePair(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> firstDecoder, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T1>>> secondDecoder, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.stripWithDecodingError(
        g,
        term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Pair<T0, T1>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.util.Pair<T0, T1>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.util.Pair<T0, T1>>left(new hydra.errors.DecodingError("expected pair"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.util.Pair<T0, T1>> visit(hydra.core.Term.Pair p) {
          return hydra.lib.eithers.Bind.apply(
            (firstDecoder).apply(g).apply(hydra.lib.pairs.First.apply((p).value)),
            (java.util.function.Function<T0, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Pair<T0, T1>>>) (f -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<T1, hydra.util.Pair<T0, T1>>) (s -> (hydra.util.Pair<T0, T1>) ((hydra.util.Pair<T0, T1>) (new hydra.util.Pair<T0, T1>(f, s)))),
              (secondDecoder).apply(g).apply(hydra.lib.pairs.Second.apply((p).value)))));
        }
      })));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, java.util.Set<T0>> decodeSet(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> elemDecoder, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.stripWithDecodingError(
        g,
        term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.Set<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, java.util.Set<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, java.util.Set<T0>>left(new hydra.errors.DecodingError("expected set"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, java.util.Set<T0>> visit(hydra.core.Term.Set s) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.List<T0>, java.util.Set<T0>>) (hydra.lib.sets.FromList::apply),
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>) (v1 -> (elemDecoder).apply(g).apply(v1)),
              hydra.lib.sets.ToList.apply((s).value)));
        }
      })));
  }

  static hydra.util.Either<hydra.errors.DecodingError, java.lang.Void> decodeUnit(hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.stripWithDecodingError(
        g,
        term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.lang.Void>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, java.lang.Void> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, java.lang.Void>left(new hydra.errors.DecodingError("expected a unit value"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, java.lang.Void> visit(hydra.core.Term.Unit ignored) {
          return hydra.util.Either.<hydra.errors.DecodingError, java.lang.Void>right(null);
        }
      })));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, T0> decodeWrapped(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> bodyDecoder, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.stripWithDecodingError(
        g,
        term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, T0> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, T0>left(new hydra.errors.DecodingError("expected wrapped value"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, T0> visit(hydra.core.Term.Wrap wt) {
          return (bodyDecoder).apply(g).apply((wt).value.body);
        }
      })));
  }

  static <T0, T1> hydra.util.Either<hydra.errors.Error_, hydra.util.Either<T0, T1>> eitherTerm(java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, T0>> leftFun, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, T1>> rightFun, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Lexical.stripAndDereferenceTerm(
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Either<T0, T1>>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.util.Either<T0, T1>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.util.Either<T0, T1>>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("either value", hydra.show.Core.term(term)))));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.util.Either<T0, T1>> visit(hydra.core.Term.Either et) {
          return hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Either<T0, T1>>>) (l -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<T0, hydra.util.Either<T0, T1>>) (x -> hydra.util.Either.<T0, T1>left(x)),
              (leftFun).apply(l))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Either<T0, T1>>>) (r -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<T1, hydra.util.Either<T0, T1>>) (x -> hydra.util.Either.<T0, T1>right(x)),
              (rightFun).apply(r))),
            (et).value);
        }
      })));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.EitherType> eitherType(hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.Strip.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.EitherType> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.EitherType>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("either type", hydra.show.Core.type(typ)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.EitherType> visit(hydra.core.Type.Either et) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.EitherType>right((et).value);
      }
    });
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, T0> field(hydra.core.Name fname, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, T0>> mapping, hydra.graph.Graph graph, java.util.List<hydra.core.Field> fields) {
    hydra.util.Lazy<java.util.List<hydra.core.Field>> matchingFields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
        (f).name.value,
        (fname).value)),
      fields));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(matchingFields.get()),
      () -> hydra.util.Either.<hydra.errors.Error_, T0>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError(hydra.lib.strings.Cat2.apply(
        "field ",
        (fname).value), "no matching field")))),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(matchingFields.get()),
          1),
        () -> hydra.lib.eithers.Bind.apply(
          hydra.Lexical.stripAndDereferenceTerm(
            graph,
            hydra.lib.lists.Head.apply(matchingFields.get()).term),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, T0>>) (stripped -> (mapping).apply(stripped))),
        () -> hydra.util.Either.<hydra.errors.Error_, T0>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("single field", hydra.lib.strings.Cat2.apply(
          "multiple fields named ",
          (fname).value)))))));
  }

  static hydra.util.Either<hydra.errors.Error_, Float> float32(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, Float>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.Core.floatLiteral(l),
        (java.util.function.Function<hydra.core.FloatValue, hydra.util.Either<hydra.errors.Error_, Float>>) (f -> hydra.extract.Core.float32Value(f)))));
  }

  static hydra.util.Either<hydra.errors.Error_, Float> float32Value(hydra.core.FloatValue v) {
    return (v).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, Float> otherwise(hydra.core.FloatValue instance) {
        return hydra.util.Either.<hydra.errors.Error_, Float>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("float32", hydra.show.Core.float_(v)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, Float> visit(hydra.core.FloatValue.Float32 f) {
        return hydra.util.Either.<hydra.errors.Error_, Float>right((f).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, Double> float64(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, Double>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.Core.floatLiteral(l),
        (java.util.function.Function<hydra.core.FloatValue, hydra.util.Either<hydra.errors.Error_, Double>>) (f -> hydra.extract.Core.float64Value(f)))));
  }

  static hydra.util.Either<hydra.errors.Error_, Double> float64Value(hydra.core.FloatValue v) {
    return (v).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, Double> otherwise(hydra.core.FloatValue instance) {
        return hydra.util.Either.<hydra.errors.Error_, Double>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("float64", hydra.show.Core.float_(v)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, Double> visit(hydra.core.FloatValue.Float64 f) {
        return hydra.util.Either.<hydra.errors.Error_, Double>right((f).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.FloatValue> floatLiteral(hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.FloatValue> otherwise(hydra.core.Literal instance) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.FloatValue>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("floating-point value", hydra.show.Core.literal(lit)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.FloatValue> visit(hydra.core.Literal.Float_ v) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.FloatValue>right((v).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.FloatValue> floatValue(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, hydra.core.FloatValue>>) (l -> hydra.extract.Core.floatLiteral(l)));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.FunctionType> functionType(hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.Strip.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.FunctionType> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.FunctionType>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("function type", hydra.show.Core.type(typ)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.FunctionType> visit(hydra.core.Type.Function ft) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.FunctionType>right((ft).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.Field> injection(hydra.core.Name expected, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Lexical.stripAndDereferenceTerm(
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Field>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Field> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.core.Field>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("injection", hydra.show.Core.term(term)))));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Field> visit(hydra.core.Term.Union injection) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              (injection).value.typeName.value,
              (expected).value),
            () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Field>right((injection).value.field),
            () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Field>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError(hydra.lib.strings.Cat2.apply(
              "injection of type ",
              (expected).value), (injection).value.typeName.value)))));
        }
      })));
  }

  static hydra.util.Either<hydra.errors.Error_, Short> int16(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, Short>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.Core.integerLiteral(l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.errors.Error_, Short>>) (i -> hydra.extract.Core.int16Value(i)))));
  }

  static hydra.util.Either<hydra.errors.Error_, Short> int16Value(hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, Short> otherwise(hydra.core.IntegerValue instance) {
        return hydra.util.Either.<hydra.errors.Error_, Short>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("int16", hydra.show.Core.integer(v)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, Short> visit(hydra.core.IntegerValue.Int16 i) {
        return hydra.util.Either.<hydra.errors.Error_, Short>right((i).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, Integer> int32(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, Integer>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.Core.integerLiteral(l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.errors.Error_, Integer>>) (i -> hydra.extract.Core.int32Value(i)))));
  }

  static hydra.util.Either<hydra.errors.Error_, Integer> int32Value(hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, Integer> otherwise(hydra.core.IntegerValue instance) {
        return hydra.util.Either.<hydra.errors.Error_, Integer>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("int32", hydra.show.Core.integer(v)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, Integer> visit(hydra.core.IntegerValue.Int32 i) {
        return hydra.util.Either.<hydra.errors.Error_, Integer>right((i).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, Long> int64(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, Long>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.Core.integerLiteral(l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.errors.Error_, Long>>) (i -> hydra.extract.Core.int64Value(i)))));
  }

  static hydra.util.Either<hydra.errors.Error_, Long> int64Value(hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, Long> otherwise(hydra.core.IntegerValue instance) {
        return hydra.util.Either.<hydra.errors.Error_, Long>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("int64", hydra.show.Core.integer(v)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, Long> visit(hydra.core.IntegerValue.Int64 i) {
        return hydra.util.Either.<hydra.errors.Error_, Long>right((i).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, Byte> int8(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, Byte>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.Core.integerLiteral(l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.errors.Error_, Byte>>) (i -> hydra.extract.Core.int8Value(i)))));
  }

  static hydra.util.Either<hydra.errors.Error_, Byte> int8Value(hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, Byte> otherwise(hydra.core.IntegerValue instance) {
        return hydra.util.Either.<hydra.errors.Error_, Byte>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("int8", hydra.show.Core.integer(v)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, Byte> visit(hydra.core.IntegerValue.Int8 i) {
        return hydra.util.Either.<hydra.errors.Error_, Byte>right((i).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.IntegerValue> integerLiteral(hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.IntegerValue> otherwise(hydra.core.Literal instance) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.IntegerValue>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("integer value", hydra.show.Core.literal(lit)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.IntegerValue> visit(hydra.core.Literal.Integer_ v) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.IntegerValue>right((v).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.IntegerValue> integerValue(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, hydra.core.IntegerValue>>) (l -> hydra.extract.Core.integerLiteral(l)));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.Lambda> lambda(hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Lexical.stripAndDereferenceTerm(
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Lambda>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Lambda> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.core.Lambda>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("lambda", hydra.show.Core.term(term)))));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Lambda> visit(hydra.core.Term.Function function) {
          return (function).value.accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.util.Either<hydra.errors.Error_, hydra.core.Lambda> otherwise(hydra.core.Function instance) {
              return hydra.util.Either.<hydra.errors.Error_, hydra.core.Lambda>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("lambda", hydra.show.Core.term(term)))));
            }

            @Override
            public hydra.util.Either<hydra.errors.Error_, hydra.core.Lambda> visit(hydra.core.Function.Lambda l) {
              return hydra.util.Either.<hydra.errors.Error_, hydra.core.Lambda>right((l).value);
            }
          });
        }
      })));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.Term> lambdaBody(hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.lib.eithers.Map.apply(
      projected -> projected.body,
      hydra.extract.Core.lambda(
        graph,
        term));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.Let> let(hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Lexical.stripAndDereferenceTerm(
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Let>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Let> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.core.Let>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("let term", hydra.show.Core.term(term)))));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Let> visit(hydra.core.Term.Let lt) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.core.Let>right((lt).value);
        }
      })));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.Term> letBinding(String n, hydra.graph.Graph graph, hydra.core.Term term) {
    hydra.core.Name name = new hydra.core.Name(n);
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.let(
        graph,
        term),
      (java.util.function.Function<hydra.core.Let, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (letExpr -> {
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> matchingBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.equality.Equal.apply(
            (b).name.value,
            (name).value)),
          (letExpr).bindings));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(matchingBindings.get()),
          () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.NoSuchBinding(new hydra.errors.NoSuchBindingError(name)))),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              hydra.lib.lists.Length.apply(matchingBindings.get()),
              1),
            () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(hydra.lib.lists.Head.apply(matchingBindings.get()).term),
            () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.MultipleBindings(new hydra.errors.MultipleBindingsError(name))))));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Term>> list(hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Lexical.stripAndDereferenceTerm(
        graph,
        term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Term>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Term>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.Term>>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("list", hydra.show.Core.term(stripped)))));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Term>> visit(hydra.core.Term.List l) {
          return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.Term>>right((l).value);
        }
      })));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.Term> listHead(hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        graph,
        term),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (l -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(l),
        () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("non-empty list", "empty list")))),
        () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(hydra.lib.lists.Head.apply(l)))));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, java.util.List<T0>> listOf(java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, T0>> f, hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        graph,
        term),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, java.util.List<T0>>>) (els -> hydra.lib.eithers.MapList.apply(
        f,
        els)));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.Type> listType(hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.Strip.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.Type>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("list type", hydra.show.Core.type(typ)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.Type> visit(hydra.core.Type.List t) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.Type>right((t).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.Literal> literal(hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Lexical.stripAndDereferenceTerm(
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Literal>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Literal> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.core.Literal>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("literal", hydra.show.Core.term(term)))));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Literal> visit(hydra.core.Term.Literal lit) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.core.Literal>right((lit).value);
        }
      })));
  }

  static <T0, T1> hydra.util.Either<hydra.errors.Error_, java.util.Map<T0, T1>> map(java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, T0>> fk, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, T1>> fv, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Lexical.stripAndDereferenceTerm(
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, java.util.Map<T0, T1>>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, java.util.Map<T0, T1>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.Error_, java.util.Map<T0, T1>>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("map", hydra.show.Core.term(term)))));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, java.util.Map<T0, T1>> visit(hydra.core.Term.Map m) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.List<hydra.util.Pair<T0, T1>>, java.util.Map<T0, T1>>) ((java.util.function.Function<java.util.List<hydra.util.Pair<T0, T1>>, java.util.Map<T0, T1>>) (hydra.lib.maps.FromList::apply)),
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<T0, T1>>>) (v1 -> hydra.extract.Core.<T0, T1>map_pair(
                fk,
                fv,
                v1)),
              hydra.lib.maps.ToList.apply((m).value)));
        }
      })));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.MapType> mapType(hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.Strip.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.MapType> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.MapType>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("map type", hydra.show.Core.type(typ)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.MapType> visit(hydra.core.Type.Map mt) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.MapType>right((mt).value);
      }
    });
  }

  static <T0, T1> hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<T0, T1>> map_pair(java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, T0>> fk, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, T1>> fv, hydra.util.Pair<hydra.core.Term, hydra.core.Term> kvPair) {
    hydra.util.Lazy<hydra.core.Term> kterm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kvPair));
    hydra.util.Lazy<hydra.core.Term> vterm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(kvPair));
    return hydra.lib.eithers.Bind.apply(
      (fk).apply(kterm.get()),
      (java.util.function.Function<T0, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<T0, T1>>>) (kval -> hydra.lib.eithers.Bind.apply(
        (fv).apply(vterm.get()),
        (java.util.function.Function<T1, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<T0, T1>>>) (vval -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<T0, T1>>right((hydra.util.Pair<T0, T1>) ((hydra.util.Pair<T0, T1>) (new hydra.util.Pair<T0, T1>(kval, vval))))))));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<T0>> maybeTerm(java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, T0>> f, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Lexical.stripAndDereferenceTerm(
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<T0>>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<T0>>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("maybe value", hydra.show.Core.term(term)))));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<T0>> visit(hydra.core.Term.Maybe mt) {
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<T0>>right((hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing())),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<T0>>>) (t -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<T0, hydra.util.Maybe<T0>>) (hydra.lib.maybes.Pure::apply),
              (f).apply(t))),
            (mt).value);
        }
      })));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.Type> maybeType(hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.Strip.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.Type>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("maybe type", hydra.show.Core.type(typ)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.Type> visit(hydra.core.Type.Maybe t) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.Type>right((t).value);
      }
    });
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, java.lang.Void> nArgs(hydra.core.Name name, Integer n, java.util.List<T0> args) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply(args),
        n),
      () -> hydra.util.Either.<hydra.errors.Error_, java.lang.Void>right(null),
      () -> hydra.util.Either.<hydra.errors.Error_, java.lang.Void>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        hydra.lib.literals.ShowInt32.apply(n),
        " arguments to primitive ",
        hydra.lib.literals.ShowString.apply((name).value))), hydra.lib.literals.ShowInt32.apply(hydra.lib.lists.Length.apply(args)))))));
  }

  static <T0, T1> hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<T0, T1>> pair(java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, T0>> kf, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, T1>> vf, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Lexical.stripAndDereferenceTerm(
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<T0, T1>>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<T0, T1>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<T0, T1>>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("pair", hydra.show.Core.term(term)))));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<T0, T1>> visit(hydra.core.Term.Pair p) {
          return hydra.lib.eithers.Bind.apply(
            (kf).apply(hydra.lib.pairs.First.apply((p).value)),
            (java.util.function.Function<T0, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<T0, T1>>>) (kVal -> hydra.lib.eithers.Bind.apply(
              (vf).apply(hydra.lib.pairs.Second.apply((p).value)),
              (java.util.function.Function<T1, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<T0, T1>>>) (vVal -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<T0, T1>>right((hydra.util.Pair<T0, T1>) ((hydra.util.Pair<T0, T1>) (new hydra.util.Pair<T0, T1>(kVal, vVal))))))));
        }
      })));
  }

  static hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Field>> record(hydra.core.Name expected, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.termRecord(
        graph,
        term0),
      (java.util.function.Function<hydra.core.Record, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Field>>>) (record -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          (record).typeName,
          expected),
        () -> hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.Field>>right((record).fields),
        () -> hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.Field>>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError(hydra.lib.strings.Cat2.apply(
          "record of type ",
          (expected).value), (record).typeName.value)))))));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.FieldType>> recordType(T0 ename, hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.Strip.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.FieldType>> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.FieldType>>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("record type", hydra.show.Core.type(typ)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.FieldType>> visit(hydra.core.Type.Record fields) {
        return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.FieldType>>right((fields).value);
      }
    });
  }

  static <T0, T1, T2> hydra.util.Either<hydra.errors.DecodingError, T2> requireField(String fieldName, java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Either<hydra.errors.DecodingError, T2>>> decoder, java.util.Map<hydra.core.Name, T1> fieldMap, T0 g) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.errors.DecodingError, T2>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        "missing field ",
        fieldName,
        " in record")))),
      (java.util.function.Function<T1, hydra.util.Either<hydra.errors.DecodingError, T2>>) (fieldTerm -> (decoder).apply(g).apply(fieldTerm)),
      hydra.lib.maps.Lookup.apply(
        new hydra.core.Name(fieldName),
        fieldMap));
  }

  static hydra.util.Either<hydra.errors.Error_, java.util.Set<hydra.core.Term>> set(hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Lexical.stripAndDereferenceTerm(
        graph,
        term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, java.util.Set<hydra.core.Term>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, java.util.Set<hydra.core.Term>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.Error_, java.util.Set<hydra.core.Term>>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("set", hydra.show.Core.term(stripped)))));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, java.util.Set<hydra.core.Term>> visit(hydra.core.Term.Set s) {
          return hydra.util.Either.<hydra.errors.Error_, java.util.Set<hydra.core.Term>>right((s).value);
        }
      })));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, java.util.Set<T0>> setOf(java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, T0>> f, hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.set(
        graph,
        term),
      (java.util.function.Function<java.util.Set<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, java.util.Set<T0>>>) (els -> hydra.lib.eithers.MapSet.apply(
        f,
        els)));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.Type> setType(hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.Strip.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.Type>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("set type", hydra.show.Core.type(typ)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.Type> visit(hydra.core.Type.Set t) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.Type>right((t).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, String> string(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, String>>) (l -> hydra.extract.Core.stringLiteral(l)));
  }

  static hydra.util.Either<hydra.errors.Error_, String> stringLiteral(hydra.core.Literal v) {
    return (v).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, String> otherwise(hydra.core.Literal instance) {
        return hydra.util.Either.<hydra.errors.Error_, String>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("string", hydra.show.Core.literal(v)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, String> visit(hydra.core.Literal.String_ s) {
        return hydra.util.Either.<hydra.errors.Error_, String>right((s).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term> stripWithDecodingError(hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bimap.apply(
      (java.util.function.Function<hydra.errors.Error_, hydra.errors.DecodingError>) (_e -> new hydra.errors.DecodingError(hydra.show.Errors.error(_e))),
      (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (x -> x),
      hydra.Lexical.stripAndDereferenceTermEither(
        g,
        term));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.Record> termRecord(hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Lexical.stripAndDereferenceTerm(
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Record>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Record> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.core.Record>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("record", hydra.show.Core.term(term)))));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Record> visit(hydra.core.Term.Record record) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.core.Record>right((record).value);
        }
      })));
  }

  static java.util.Map<hydra.core.Name, hydra.core.Term> toFieldMap(hydra.core.Record record) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Field, hydra.util.Pair<hydra.core.Name, hydra.core.Term>>) (f -> (hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>((f).name, (f).term)))),
      (record).fields));
  }

  static hydra.util.Either<hydra.errors.Error_, Character> uint16(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, Character>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.Core.integerLiteral(l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.errors.Error_, Character>>) (i -> hydra.extract.Core.uint16Value(i)))));
  }

  static hydra.util.Either<hydra.errors.Error_, Character> uint16Value(hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, Character> otherwise(hydra.core.IntegerValue instance) {
        return hydra.util.Either.<hydra.errors.Error_, Character>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("uint16", hydra.show.Core.integer(v)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, Character> visit(hydra.core.IntegerValue.Uint16 i) {
        return hydra.util.Either.<hydra.errors.Error_, Character>right((i).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, Long> uint32(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, Long>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.Core.integerLiteral(l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.errors.Error_, Long>>) (i -> hydra.extract.Core.uint32Value(i)))));
  }

  static hydra.util.Either<hydra.errors.Error_, Long> uint32Value(hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, Long> otherwise(hydra.core.IntegerValue instance) {
        return hydra.util.Either.<hydra.errors.Error_, Long>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("uint32", hydra.show.Core.integer(v)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, Long> visit(hydra.core.IntegerValue.Uint32 i) {
        return hydra.util.Either.<hydra.errors.Error_, Long>right((i).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, java.math.BigInteger> uint64(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, java.math.BigInteger>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.Core.integerLiteral(l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.errors.Error_, java.math.BigInteger>>) (i -> hydra.extract.Core.uint64Value(i)))));
  }

  static hydra.util.Either<hydra.errors.Error_, java.math.BigInteger> uint64Value(hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, java.math.BigInteger> otherwise(hydra.core.IntegerValue instance) {
        return hydra.util.Either.<hydra.errors.Error_, java.math.BigInteger>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("uint64", hydra.show.Core.integer(v)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.math.BigInteger> visit(hydra.core.IntegerValue.Uint64 i) {
        return hydra.util.Either.<hydra.errors.Error_, java.math.BigInteger>right((i).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, Short> uint8(hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.literal(
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.errors.Error_, Short>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.Core.integerLiteral(l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.errors.Error_, Short>>) (i -> hydra.extract.Core.uint8Value(i)))));
  }

  static hydra.util.Either<hydra.errors.Error_, Short> uint8Value(hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, Short> otherwise(hydra.core.IntegerValue instance) {
        return hydra.util.Either.<hydra.errors.Error_, Short>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("uint8", hydra.show.Core.integer(v)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, Short> visit(hydra.core.IntegerValue.Uint8 i) {
        return hydra.util.Either.<hydra.errors.Error_, Short>right((i).value);
      }
    });
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.FieldType>> unionType(T0 ename, hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.Strip.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.FieldType>> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.FieldType>>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("union type", hydra.show.Core.type(typ)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.FieldType>> visit(hydra.core.Type.Union fields) {
        return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.FieldType>>right((fields).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, java.lang.Void> unit(hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, java.lang.Void> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.errors.Error_, java.lang.Void>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("unit", hydra.show.Core.term(term)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.lang.Void> visit(hydra.core.Term.Unit ignored) {
        return hydra.util.Either.<hydra.errors.Error_, java.lang.Void>right(null);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.Name> unitVariant(hydra.core.Name tname, hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.injection(
        tname,
        graph,
        term),
      (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.errors.Error_, hydra.core.Name>>) (field -> hydra.lib.eithers.Bind.apply(
        hydra.extract.Core.unit((field).term),
        (java.util.function.Function<java.lang.Void, hydra.util.Either<hydra.errors.Error_, hydra.core.Name>>) (ignored -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Name>right((field).name)))));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.Term> wrap(hydra.core.Name expected, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Lexical.stripAndDereferenceTerm(
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "wrap(",
              (expected).value),
            ")"), hydra.show.Core.term(term)))));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              (wrappedTerm).value.typeName.value,
              (expected).value),
            () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right((wrappedTerm).value.body),
            () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError(hydra.lib.strings.Cat2.apply(
              "wrapper of type ",
              (expected).value), (wrappedTerm).value.typeName.value)))));
        }
      })));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.core.Type> wrappedType(T0 ename, hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.Strip.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.Type>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("wrapped type", hydra.show.Core.type(typ)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.Type> visit(hydra.core.Type.Wrap innerType) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.Type>right((innerType).value);
      }
    });
  }
}
