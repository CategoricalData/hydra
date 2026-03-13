// Note: this is an automatically generated file. Do not edit.

package hydra.extract.core;

/**
 * Extraction and validation for hydra.core types
 */
public interface Core {
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigDecimal> bigfloat(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigDecimal>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.core.Core.floatLiteral(
          cx,
          l),
        (java.util.function.Function<hydra.core.FloatValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigDecimal>>) (f -> hydra.extract.core.Core.bigfloatValue(
          cx,
          f)))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigDecimal> bigfloatValue(hydra.context.Context cx, hydra.core.FloatValue v) {
    return (v).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigDecimal> otherwise(hydra.core.FloatValue instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigDecimal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigDecimal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.math.BigDecimal>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "bigfloat"),
            " but found "),
          hydra.show.core.Core.float_(v)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigDecimal> visit(hydra.core.FloatValue.Bigfloat f) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigDecimal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigDecimal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.math.BigDecimal>right((f).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger> bigint(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.core.Core.integerLiteral(
          cx,
          l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger>>) (i -> hydra.extract.core.Core.bigintValue(
          cx,
          i)))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger> bigintValue(hydra.context.Context cx, hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger> otherwise(hydra.core.IntegerValue instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "bigint"),
            " but found "),
          hydra.show.core.Core.integer(v)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger> visit(hydra.core.IntegerValue.Bigint i) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger>right((i).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, byte[]> binary(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, byte[]>>) (l -> hydra.extract.core.Core.binaryLiteral(
        cx,
        l)));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, byte[]> binaryLiteral(hydra.context.Context cx, hydra.core.Literal v) {
    return (v).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, byte[]> otherwise(hydra.core.Literal instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, byte[]>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, byte[]>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, byte[]>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "binary"),
            " but found "),
          hydra.show.core.Core.literal(v)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, byte[]> visit(hydra.core.Literal.Binary b) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, byte[]>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, byte[]>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, byte[]>right((b).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Boolean> boolean_(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Boolean>>) (l -> hydra.extract.core.Core.booleanLiteral(
        cx,
        l)));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Boolean> booleanLiteral(hydra.context.Context cx, hydra.core.Literal v) {
    return (v).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Boolean> otherwise(hydra.core.Literal instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Boolean>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Boolean>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Boolean>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "boolean"),
            " but found "),
          hydra.show.core.Core.literal(v)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Boolean> visit(hydra.core.Literal.Boolean_ b) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Boolean>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Boolean>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Boolean>right((b).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field> caseField(hydra.context.Context cx, hydra.core.Name name, String n, hydra.graph.Graph graph, hydra.core.Term term) {
    hydra.core.Name fieldName = new hydra.core.Name(n);
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.cases(
        cx,
        name,
        graph,
        term),
      (java.util.function.Function<hydra.core.CaseStatement, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>>) (cs -> {
        hydra.util.Lazy<java.util.List<hydra.core.Field>> matching = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
            ((f).name).value,
            (fieldName).value)),
          (cs).cases));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(matching.get()),
          () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError("not enough cases")), cx))))),
          () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>right(hydra.lib.lists.Head.apply(matching.get())))));
      }));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement> cases(hydra.context.Context cx, hydra.core.Name name, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm(
        cx,
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                "case statement"),
              " but found "),
            hydra.show.core.Core.term(term)))), cx)))));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement> visit(hydra.core.Term.Function function) {
          return ((function).value).accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement> otherwise(hydra.core.Function instance) {
              return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
                hydra.lib.strings.Cat2.apply(
                  hydra.lib.strings.Cat2.apply(
                    "expected ",
                    "case statement"),
                  " but found "),
                hydra.show.core.Core.term(term)))), cx)))));
            }
            
            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement> visit(hydra.core.Function.Elimination elimination) {
              return ((elimination).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement> otherwise(hydra.core.Elimination instance) {
                  return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
                    hydra.lib.strings.Cat2.apply(
                      hydra.lib.strings.Cat2.apply(
                        "expected ",
                        "case statement"),
                      " but found "),
                    hydra.show.core.Core.term(term)))), cx)))));
                }
                
                @Override
                public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement> visit(hydra.core.Elimination.Union cs) {
                  return hydra.lib.logic.IfElse.lazy(
                    hydra.lib.equality.Equal.apply(
                      (((cs).value).typeName).value,
                      (name).value),
                    () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement>right((cs).value))),
                    () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.CaseStatement>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
                      hydra.lib.strings.Cat2.apply(
                        hydra.lib.strings.Cat2.apply(
                          "expected ",
                          hydra.lib.strings.Cat2.apply(
                            "case statement for type ",
                            (name).value)),
                        " but found "),
                      hydra.show.core.Core.term(term)))), cx))))));
                }
              });
            }
          });
        }
      })));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0> field(hydra.context.Context cx, hydra.core.Name fname, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>> mapping, hydra.graph.Graph graph, java.util.List<hydra.core.Field> fields) {
    hydra.util.Lazy<java.util.List<hydra.core.Field>> matchingFields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
        ((f).name).value,
        (fname).value)),
      fields));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(matchingFields.get()),
      () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T0>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            "expected ",
            hydra.lib.strings.Cat2.apply(
              "field ",
              (fname).value)),
          " but found "),
        "no matching field"))), cx))))),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(matchingFields.get()),
          1),
        () -> hydra.lib.eithers.Bind.apply(
          hydra.lexical.Lexical.stripAndDereferenceTerm(
            cx,
            graph,
            (hydra.lib.lists.Head.apply(matchingFields.get())).term),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>) (stripped -> (mapping).apply(stripped))),
        () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T0>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "single field"),
            " but found "),
          hydra.lib.strings.Cat2.apply(
            "multiple fields named ",
            (fname).value)))), cx)))))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Float> float32(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Float>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.core.Core.floatLiteral(
          cx,
          l),
        (java.util.function.Function<hydra.core.FloatValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Float>>) (f -> hydra.extract.core.Core.float32Value(
          cx,
          f)))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Float> float32Value(hydra.context.Context cx, hydra.core.FloatValue v) {
    return (v).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Float> otherwise(hydra.core.FloatValue instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Float>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Float>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Float>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "float32"),
            " but found "),
          hydra.show.core.Core.float_(v)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Float> visit(hydra.core.FloatValue.Float32 f) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Float>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Float>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Float>right((f).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Double> float64(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Double>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.core.Core.floatLiteral(
          cx,
          l),
        (java.util.function.Function<hydra.core.FloatValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Double>>) (f -> hydra.extract.core.Core.float64Value(
          cx,
          f)))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Double> float64Value(hydra.context.Context cx, hydra.core.FloatValue v) {
    return (v).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Double> otherwise(hydra.core.FloatValue instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Double>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Double>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Double>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "float64"),
            " but found "),
          hydra.show.core.Core.float_(v)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Double> visit(hydra.core.FloatValue.Float64 f) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Double>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Double>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Double>right((f).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue> floatLiteral(hydra.context.Context cx, hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue> otherwise(hydra.core.Literal instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "floating-point value"),
            " but found "),
          hydra.show.core.Core.literal(lit)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue> visit(hydra.core.Literal.Float_ v) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>right((v).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue> floatValue(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>>) (l -> hydra.extract.core.Core.floatLiteral(
        cx,
        l)));
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Either<T0, T1>> eitherTerm(hydra.context.Context cx, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>> leftFun, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T1>> rightFun, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm(
        cx,
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Either<T0, T1>>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Either<T0, T1>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Either<T0, T1>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Either<T0, T1>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Either<T0, T1>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                "either value"),
              " but found "),
            hydra.show.core.Core.term(term)))), cx)))));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Either<T0, T1>> visit(hydra.core.Term.Either et) {
          return hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Either<T0, T1>>>) (l -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<T0, hydra.util.Either<T0, T1>>) (x -> (hydra.util.Either<T0, T1>) ((hydra.util.Either<T0, T1>) (hydra.util.Either.<T0, T1>left(x)))),
              (leftFun).apply(l))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Either<T0, T1>>>) (r -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<T1, hydra.util.Either<T0, T1>>) (x -> (hydra.util.Either<T0, T1>) ((hydra.util.Either<T0, T1>) (hydra.util.Either.<T0, T1>right(x)))),
              (rightFun).apply(r))),
            (et).value);
        }
      })));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.EitherType> eitherType(hydra.context.Context cx, hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.EitherType> otherwise(hydra.core.Type instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.EitherType>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.EitherType>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.EitherType>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "either type"),
            " but found "),
          hydra.show.core.Core.type(typ)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.EitherType> visit(hydra.core.Type.Either et) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.EitherType>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.EitherType>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.EitherType>right((et).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FunctionType> functionType(hydra.context.Context cx, hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FunctionType> otherwise(hydra.core.Type instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FunctionType>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FunctionType>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.FunctionType>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "function type"),
            " but found "),
          hydra.show.core.Core.type(typ)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FunctionType> visit(hydra.core.Type.Function ft) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FunctionType>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FunctionType>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.FunctionType>right((ft).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field> injection(hydra.context.Context cx, hydra.core.Name expected, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm(
        cx,
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                "injection"),
              " but found "),
            hydra.show.core.Core.term(term)))), cx)))));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field> visit(hydra.core.Term.Union injection) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              (((injection).value).typeName).value,
              (expected).value),
            () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>right(((injection).value).field))),
            () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                hydra.lib.strings.Cat2.apply(
                  "expected ",
                  hydra.lib.strings.Cat2.apply(
                    "injection of type ",
                    (expected).value)),
                " but found "),
              (((injection).value).typeName).value))), cx))))));
        }
      })));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short> int16(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.core.Core.integerLiteral(
          cx,
          l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short>>) (i -> hydra.extract.core.Core.int16Value(
          cx,
          i)))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short> int16Value(hydra.context.Context cx, hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short> otherwise(hydra.core.IntegerValue instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Short>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "int16"),
            " but found "),
          hydra.show.core.Core.integer(v)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short> visit(hydra.core.IntegerValue.Int16 i) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Short>right((i).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Integer> int32(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Integer>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.core.Core.integerLiteral(
          cx,
          l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Integer>>) (i -> hydra.extract.core.Core.int32Value(
          cx,
          i)))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Integer> int32Value(hydra.context.Context cx, hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Integer> otherwise(hydra.core.IntegerValue instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Integer>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Integer>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Integer>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "int32"),
            " but found "),
          hydra.show.core.Core.integer(v)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Integer> visit(hydra.core.IntegerValue.Int32 i) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Integer>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Integer>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Integer>right((i).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long> int64(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.core.Core.integerLiteral(
          cx,
          l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long>>) (i -> hydra.extract.core.Core.int64Value(
          cx,
          i)))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long> int64Value(hydra.context.Context cx, hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long> otherwise(hydra.core.IntegerValue instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Long>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "int64"),
            " but found "),
          hydra.show.core.Core.integer(v)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long> visit(hydra.core.IntegerValue.Int64 i) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Long>right((i).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Byte> int8(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Byte>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.core.Core.integerLiteral(
          cx,
          l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Byte>>) (i -> hydra.extract.core.Core.int8Value(
          cx,
          i)))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Byte> int8Value(hydra.context.Context cx, hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Byte> otherwise(hydra.core.IntegerValue instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Byte>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Byte>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Byte>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "int8"),
            " but found "),
          hydra.show.core.Core.integer(v)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Byte> visit(hydra.core.IntegerValue.Int8 i) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Byte>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Byte>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Byte>right((i).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue> integerLiteral(hydra.context.Context cx, hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue> otherwise(hydra.core.Literal instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "integer value"),
            " but found "),
          hydra.show.core.Core.literal(lit)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue> visit(hydra.core.Literal.Integer_ v) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>right((v).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue> integerValue(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>>) (l -> hydra.extract.core.Core.integerLiteral(
        cx,
        l)));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> lambdaBody(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.lib.eithers.Map.apply(
      projected -> projected.body,
      hydra.extract.core.Core.lambda(
        cx,
        graph,
        term));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Lambda> lambda(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm(
        cx,
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Lambda>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Lambda> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Lambda>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Lambda>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Lambda>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                "lambda"),
              " but found "),
            hydra.show.core.Core.term(term)))), cx)))));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Lambda> visit(hydra.core.Term.Function function) {
          return ((function).value).accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Lambda> otherwise(hydra.core.Function instance) {
              return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Lambda>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Lambda>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Lambda>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
                hydra.lib.strings.Cat2.apply(
                  hydra.lib.strings.Cat2.apply(
                    "expected ",
                    "lambda"),
                  " but found "),
                hydra.show.core.Core.term(term)))), cx)))));
            }
            
            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Lambda> visit(hydra.core.Function.Lambda l) {
              return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Lambda>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Lambda>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Lambda>right((l).value)));
            }
          });
        }
      })));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> letBinding(hydra.context.Context cx, String n, hydra.graph.Graph graph, hydra.core.Term term) {
    hydra.core.Name name = new hydra.core.Name(n);
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.let(
        cx,
        graph,
        term),
      (java.util.function.Function<hydra.core.Let, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (letExpr -> {
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> matchingBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.equality.Equal.apply(
            ((b).name).value,
            (name).value)),
          (letExpr).bindings));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(matchingBindings.get()),
          () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            "no such binding: ",
            n))), cx))))),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              hydra.lib.lists.Length.apply(matchingBindings.get()),
              1),
            () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right((hydra.lib.lists.Head.apply(matchingBindings.get())).term))),
            () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
              "multiple bindings named ",
              n))), cx)))))));
      }));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Let> let(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm(
        cx,
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Let>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Let> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Let>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Let>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Let>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                "let term"),
              " but found "),
            hydra.show.core.Core.term(term)))), cx)))));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Let> visit(hydra.core.Term.Let lt) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Let>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Let>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Let>right((lt).value)));
        }
      })));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>> list(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm(
        cx,
        graph,
        term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                "list"),
              " but found "),
            hydra.show.core.Core.term(stripped)))), cx)))));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>> visit(hydra.core.Term.List l) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>right((l).value)));
        }
      })));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> listHead(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.list(
        cx,
        graph,
        term),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (l -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(l),
        () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError("empty list")), cx))))),
        () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(hydra.lib.lists.Head.apply(l)))))));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T0>> listOf(hydra.context.Context cx, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>> f, hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.list(
        cx,
        graph,
        term),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T0>>>) (els -> hydra.lib.eithers.MapList.apply(
        f,
        els)));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> listType(hydra.context.Context cx, hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "list type"),
            " but found "),
          hydra.show.core.Core.type(typ)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> visit(hydra.core.Type.List t) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>right((t).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> literal(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm(
        cx,
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                "literal"),
              " but found "),
            hydra.show.core.Core.term(term)))), cx)))));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> visit(hydra.core.Term.Literal lit) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>right((lit).value)));
        }
      })));
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Map<T0, T1>> map(hydra.context.Context cx, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>> fk, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T1>> fv, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm(
        cx,
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Map<T0, T1>>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Map<T0, T1>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Map<T0, T1>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Map<T0, T1>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.Map<T0, T1>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                "map"),
              " but found "),
            hydra.show.core.Core.term(term)))), cx)))));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Map<T0, T1>> visit(hydra.core.Term.Map m) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.List<hydra.util.Pair<T0, T1>>, java.util.Map<T0, T1>>) ((java.util.function.Function<java.util.List<hydra.util.Pair<T0, T1>>, java.util.Map<T0, T1>>) (hydra.lib.maps.FromList::apply)),
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>>>) (v1 -> hydra.extract.core.Core.<T0, T1>map_pair(
                fk,
                fv,
                v1)),
              hydra.lib.maps.ToList.apply((m).value)));
        }
      })));
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>> map_pair(java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>> fk, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T1>> fv, hydra.util.Pair<hydra.core.Term, hydra.core.Term> kvPair) {
    hydra.util.Lazy<hydra.core.Term> kterm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kvPair));
    hydra.util.Lazy<hydra.core.Term> vterm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(kvPair));
    return hydra.lib.eithers.Bind.apply(
      (fk).apply(kterm.get()),
      (java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>>>) (kval -> hydra.lib.eithers.Bind.apply(
        (fv).apply(vterm.get()),
        (java.util.function.Function<T1, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>>>) (vval -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>>right((hydra.util.Pair<T0, T1>) ((hydra.util.Pair<T0, T1>) (new hydra.util.Pair<T0, T1>(kval, vval))))))))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.MapType> mapType(hydra.context.Context cx, hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.MapType> otherwise(hydra.core.Type instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.MapType>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.MapType>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.MapType>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "map type"),
            " but found "),
          hydra.show.core.Core.type(typ)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.MapType> visit(hydra.core.Type.Map mt) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.MapType>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.MapType>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.MapType>right((mt).value)));
      }
    });
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void> nArgs(hydra.context.Context cx, hydra.core.Name name, Integer n, java.util.List<T0> args) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply(args),
        n),
      () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>right(null))),
      () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            "expected ",
            hydra.lib.strings.Cat.apply(java.util.List.of(
              hydra.lib.literals.ShowInt32.apply(n),
              " arguments to primitive ",
              hydra.lib.literals.ShowString.apply((name).value)))),
          " but found "),
        hydra.lib.literals.ShowInt32.apply(hydra.lib.lists.Length.apply(args))))), cx))))));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T0>> maybeTerm(hydra.context.Context cx, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>> f, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm(
        cx,
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T0>>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T0>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T0>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T0>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T0>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                "maybe value"),
              " but found "),
            hydra.show.core.Core.term(term)))), cx)))));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T0>> visit(hydra.core.Term.Maybe mt) {
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T0>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T0>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T0>>right((hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing())))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T0>>>) (t -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<T0, hydra.util.Maybe<T0>>) (hydra.lib.maybes.Pure::apply),
              (f).apply(t))),
            (mt).value);
        }
      })));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> maybeType(hydra.context.Context cx, hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "maybe type"),
            " but found "),
          hydra.show.core.Core.type(typ)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> visit(hydra.core.Type.Maybe t) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>right((t).value)));
      }
    });
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>> pair(hydra.context.Context cx, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>> kf, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T1>> vf, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm(
        cx,
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                "pair"),
              " but found "),
            hydra.show.core.Core.term(term)))), cx)))));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>> visit(hydra.core.Term.Pair p) {
          return hydra.lib.eithers.Bind.apply(
            (kf).apply(hydra.lib.pairs.First.apply((p).value)),
            (java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>>>) (kVal -> hydra.lib.eithers.Bind.apply(
              (vf).apply(hydra.lib.pairs.Second.apply((p).value)),
              (java.util.function.Function<T1, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>>>) (vVal -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<T0, T1>>right((hydra.util.Pair<T0, T1>) ((hydra.util.Pair<T0, T1>) (new hydra.util.Pair<T0, T1>(kVal, vVal))))))))));
        }
      })));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Field>> record(hydra.context.Context cx, hydra.core.Name expected, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.termRecord(
        cx,
        graph,
        term0),
      (java.util.function.Function<hydra.core.Record, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Field>>>) (record -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          (record).typeName,
          expected),
        () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Field>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Field>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Field>>right((record).fields))),
        () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Field>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Field>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Field>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              hydra.lib.strings.Cat2.apply(
                "record of type ",
                (expected).value)),
            " but found "),
          ((record).typeName).value))), cx))))))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>> recordType(hydra.context.Context cx, hydra.core.Name ename, hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>> otherwise(hydra.core.Type instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "record type"),
            " but found "),
          hydra.show.core.Core.type(typ)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>> visit(hydra.core.Type.Record rowType) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (((rowType).value).typeName).value,
            (ename).value),
          () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>right(((rowType).value).fields))),
          () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                hydra.lib.strings.Cat2.apply(
                  "record of type ",
                  (ename).value)),
              " but found "),
            hydra.lib.strings.Cat2.apply(
              "record of type ",
              (((rowType).value).typeName).value)))), cx))))));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Set<hydra.core.Term>> set(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm(
        cx,
        graph,
        term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Set<hydra.core.Term>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Set<hydra.core.Term>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Set<hydra.core.Term>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Set<hydra.core.Term>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.Set<hydra.core.Term>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                "set"),
              " but found "),
            hydra.show.core.Core.term(stripped)))), cx)))));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Set<hydra.core.Term>> visit(hydra.core.Term.Set s) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Set<hydra.core.Term>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Set<hydra.core.Term>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.Set<hydra.core.Term>>right((s).value)));
        }
      })));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Set<T0>> setOf(hydra.context.Context cx, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>> f, hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.set(
        cx,
        graph,
        term),
      (java.util.function.Function<java.util.Set<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Set<T0>>>) (els -> hydra.lib.eithers.MapSet.apply(
        f,
        els)));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> setType(hydra.context.Context cx, hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "set type"),
            " but found "),
          hydra.show.core.Core.type(typ)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> visit(hydra.core.Type.Set t) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>right((t).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, String> string(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, String>>) (l -> hydra.extract.core.Core.stringLiteral(
        cx,
        l)));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, String> stringLiteral(hydra.context.Context cx, hydra.core.Literal v) {
    return (v).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, String> otherwise(hydra.core.Literal instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, String>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, String>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, String>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "string"),
            " but found "),
          hydra.show.core.Core.literal(v)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, String> visit(hydra.core.Literal.String_ s) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, String>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, String>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, String>right((s).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Record> termRecord(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm(
        cx,
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Record>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Record> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Record>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Record>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Record>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                "record"),
              " but found "),
            hydra.show.core.Core.term(term)))), cx)))));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Record> visit(hydra.core.Term.Record record) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Record>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Record>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Record>right((record).value)));
        }
      })));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Character> uint16(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Character>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.core.Core.integerLiteral(
          cx,
          l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Character>>) (i -> hydra.extract.core.Core.uint16Value(
          cx,
          i)))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Character> uint16Value(hydra.context.Context cx, hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Character> otherwise(hydra.core.IntegerValue instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Character>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Character>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Character>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "uint16"),
            " but found "),
          hydra.show.core.Core.integer(v)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Character> visit(hydra.core.IntegerValue.Uint16 i) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Character>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Character>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Character>right((i).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long> uint32(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.core.Core.integerLiteral(
          cx,
          l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long>>) (i -> hydra.extract.core.Core.uint32Value(
          cx,
          i)))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long> uint32Value(hydra.context.Context cx, hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long> otherwise(hydra.core.IntegerValue instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Long>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "uint32"),
            " but found "),
          hydra.show.core.Core.integer(v)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long> visit(hydra.core.IntegerValue.Uint32 i) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Long>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Long>right((i).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger> uint64(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.core.Core.integerLiteral(
          cx,
          l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger>>) (i -> hydra.extract.core.Core.uint64Value(
          cx,
          i)))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger> uint64Value(hydra.context.Context cx, hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger> otherwise(hydra.core.IntegerValue instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "uint64"),
            " but found "),
          hydra.show.core.Core.integer(v)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger> visit(hydra.core.IntegerValue.Uint64 i) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.math.BigInteger>right((i).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short> uint8(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term t) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.literal(
        cx,
        graph,
        t),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.extract.core.Core.integerLiteral(
          cx,
          l),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short>>) (i -> hydra.extract.core.Core.uint8Value(
          cx,
          i)))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short> uint8Value(hydra.context.Context cx, hydra.core.IntegerValue v) {
    return (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short> otherwise(hydra.core.IntegerValue instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Short>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "uint8"),
            " but found "),
          hydra.show.core.Core.integer(v)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short> visit(hydra.core.IntegerValue.Uint8 i) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Short>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, Short>right((i).value)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>> unionType(hydra.context.Context cx, hydra.core.Name ename, hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>> otherwise(hydra.core.Type instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "union type"),
            " but found "),
          hydra.show.core.Core.type(typ)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>> visit(hydra.core.Type.Union rowType) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            ((rowType).value).typeName,
            ename),
          () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>right(((rowType).value).fields))),
          () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.FieldType>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                hydra.lib.strings.Cat2.apply(
                  "union of type ",
                  (ename).value)),
              " but found "),
            hydra.lib.strings.Cat2.apply(
              "union of type ",
              (((rowType).value).typeName).value)))), cx))))));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void> unit(hydra.context.Context cx, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void> otherwise(hydra.core.Term instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "unit"),
            " but found "),
          hydra.show.core.Core.term(term)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void> visit(hydra.core.Term.Unit ignored) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>right(null)));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Name> unitVariant(hydra.context.Context cx, hydra.core.Name tname, hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.injection(
        cx,
        tname,
        graph,
        term),
      (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Name>>) (field -> hydra.lib.eithers.Bind.apply(
        hydra.extract.core.Core.unit(
          cx,
          (field).term),
        (java.util.function.Function<java.lang.Void, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Name>>) (ignored -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Name>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Name>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Name>right((field).name)))))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> wrap(hydra.context.Context cx, hydra.core.Name expected, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm(
        cx,
        graph,
        term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                hydra.lib.strings.Cat2.apply(
                  hydra.lib.strings.Cat2.apply(
                    "wrap(",
                    (expected).value),
                  ")")),
              " but found "),
            hydra.show.core.Core.term(term)))), cx)))));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              (((wrappedTerm).value).typeName).value,
              (expected).value),
            () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(((wrappedTerm).value).body))),
            () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                hydra.lib.strings.Cat2.apply(
                  "expected ",
                  hydra.lib.strings.Cat2.apply(
                    "wrapper of type ",
                    (expected).value)),
                " but found "),
              (((wrappedTerm).value).typeName).value))), cx))))));
        }
      })));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> wrappedType(hydra.context.Context cx, hydra.core.Name ename, hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "wrapped type"),
            " but found "),
          hydra.show.core.Core.type(typ)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> visit(hydra.core.Type.Wrap wrappedType) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (((wrappedType).value).typeName).value,
            (ename).value),
          () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>right(((wrappedType).value).body))),
          () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                hydra.lib.strings.Cat2.apply(
                  "wrapped type ",
                  (ename).value)),
              " but found "),
            hydra.lib.strings.Cat2.apply(
              "wrapped type ",
              (((wrappedType).value).typeName).value)))), cx))))));
      }
    });
  }
}
