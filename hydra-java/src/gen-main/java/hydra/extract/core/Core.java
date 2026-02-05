// Note: this is an automatically generated file. Do not edit.

package hydra.extract.core;

/**
 * Extraction and validation for hydra.core types
 */
public interface Core {
  static hydra.compute.Flow<hydra.graph.Graph, java.math.BigDecimal> bigfloat(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, java.math.BigDecimal>>) (l -> hydra.lib.flows.Bind.apply(
        hydra.extract.core.Core.floatLiteral((l)),
        (java.util.function.Function<hydra.core.FloatValue, hydra.compute.Flow<hydra.graph.Graph, java.math.BigDecimal>>) (f -> hydra.extract.core.Core.bigfloatValue((f))))));
  }
  
  static <T0> hydra.compute.Flow<T0, java.math.BigDecimal> bigfloatValue(hydra.core.FloatValue v) {
    return ((v)).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, java.math.BigDecimal> otherwise(hydra.core.FloatValue instance) {
        return hydra.monads.Monads.<T0, java.math.BigDecimal>unexpected(
          "bigfloat",
          hydra.show.core.Core.float_((v)));
      }
      
      @Override
      public hydra.compute.Flow<T0, java.math.BigDecimal> visit(hydra.core.FloatValue.Bigfloat f) {
        return hydra.lib.flows.Pure.apply(((f)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.math.BigInteger> bigint(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, java.math.BigInteger>>) (l -> hydra.lib.flows.Bind.apply(
        hydra.extract.core.Core.integerLiteral((l)),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<hydra.graph.Graph, java.math.BigInteger>>) (i -> hydra.extract.core.Core.bigintValue((i))))));
  }
  
  static <T0> hydra.compute.Flow<T0, java.math.BigInteger> bigintValue(hydra.core.IntegerValue v) {
    return ((v)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, java.math.BigInteger> otherwise(hydra.core.IntegerValue instance) {
        return hydra.monads.Monads.<T0, java.math.BigInteger>unexpected(
          "bigint",
          hydra.show.core.Core.integer((v)));
      }
      
      @Override
      public hydra.compute.Flow<T0, java.math.BigInteger> visit(hydra.core.IntegerValue.Bigint i) {
        return hydra.lib.flows.Pure.apply(((i)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, byte[]> binary(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, byte[]>>) (l -> hydra.extract.core.Core.binaryLiteral((l))));
  }
  
  static <T0> hydra.compute.Flow<T0, byte[]> binaryLiteral(hydra.core.Literal v) {
    return ((v)).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, byte[]> otherwise(hydra.core.Literal instance) {
        return hydra.monads.Monads.<T0, byte[]>unexpected(
          "binary",
          hydra.show.core.Core.literal((v)));
      }
      
      @Override
      public hydra.compute.Flow<T0, byte[]> visit(hydra.core.Literal.Binary b) {
        return hydra.lib.flows.Pure.apply(((b)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, Boolean> boolean_(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, Boolean>>) (l -> hydra.extract.core.Core.booleanLiteral((l))));
  }
  
  static <T0> hydra.compute.Flow<T0, Boolean> booleanLiteral(hydra.core.Literal v) {
    return ((v)).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, Boolean> otherwise(hydra.core.Literal instance) {
        return hydra.monads.Monads.<T0, Boolean>unexpected(
          "boolean",
          hydra.show.core.Core.literal((v)));
      }
      
      @Override
      public hydra.compute.Flow<T0, Boolean> visit(hydra.core.Literal.Boolean_ b) {
        return hydra.lib.flows.Pure.apply(((b)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Field> caseField(hydra.core.Name name, String n, hydra.core.Term term) {
    hydra.core.Name fieldName = new hydra.core.Name((n));
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.cases(
        (name),
        (term)),
      (java.util.function.Function<hydra.core.CaseStatement, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Field>>) (cs -> {
        hydra.util.Lazy<java.util.List<hydra.core.Field>> matching = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
            (((f)).name).value,
            ((fieldName)).value)),
          ((cs)).cases));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(matching.get()),
          () -> hydra.lib.flows.Fail.apply("not enough cases"),
          () -> hydra.lib.flows.Pure.apply(hydra.lib.lists.Head.apply(matching.get())));
      }));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.CaseStatement> cases(hydra.core.Name name, hydra.core.Term term0) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm((term0)),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.CaseStatement>>) (term -> ((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.CaseStatement>>) (v1 -> hydra.extract.core.Core.cases_extract(
        (hydra.show.core.Core::term),
        (name),
        (v1)))).apply((term))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.CaseStatement> cases_extract(java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, hydra.core.Name name, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.CaseStatement> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "case statement",
          ((hydra_show_core_term2)).apply((term)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.CaseStatement> visit(hydra.core.Term.Function function) {
        return (((function)).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, hydra.core.CaseStatement> otherwise(hydra.core.Function instance) {
            return hydra.monads.Monads.unexpected(
              "case statement",
              ((hydra_show_core_term2)).apply((term)));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.core.CaseStatement> visit(hydra.core.Function.Elimination elimination) {
            return (((elimination)).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public hydra.compute.Flow<T0, hydra.core.CaseStatement> otherwise(hydra.core.Elimination instance) {
                return hydra.monads.Monads.unexpected(
                  "case statement",
                  ((hydra_show_core_term2)).apply((term)));
              }
              
              @Override
              public hydra.compute.Flow<T0, hydra.core.CaseStatement> visit(hydra.core.Elimination.Union cs) {
                return hydra.lib.logic.IfElse.lazy(
                  hydra.lib.equality.Equal.apply(
                    ((((cs)).value).typeName).value,
                    ((name)).value),
                  () -> hydra.lib.flows.Pure.apply(((cs)).value),
                  () -> hydra.monads.Monads.unexpected(
                    hydra.lib.strings.Cat2.apply(
                      "case statement for type ",
                      ((name)).value),
                    ((hydra_show_core_term2)).apply((term))));
              }
            });
          }
        });
      }
    });
  }
  
  static <T0> hydra.compute.Flow<hydra.graph.Graph, T0> field(hydra.core.Name fname, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, T0>> mapping, java.util.List<hydra.core.Field> fields) {
    hydra.util.Lazy<java.util.List<hydra.core.Field>> matchingFields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
        (((f)).name).value,
        ((fname)).value)),
      (fields)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(matchingFields.get()),
      () -> hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "field ",
          ((fname)).value),
        " not found")),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(matchingFields.get()),
          1),
        () -> hydra.lib.flows.Bind.apply(
          hydra.lexical.Lexical.stripAndDereferenceTerm((hydra.lib.lists.Head.apply(matchingFields.get())).term),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, T0>>) (stripped -> ((mapping)).apply((stripped)))),
        () -> hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
          "multiple fields named ",
          ((fname)).value))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, Float> float32(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, Float>>) (l -> hydra.lib.flows.Bind.apply(
        hydra.extract.core.Core.floatLiteral((l)),
        (java.util.function.Function<hydra.core.FloatValue, hydra.compute.Flow<hydra.graph.Graph, Float>>) (f -> hydra.extract.core.Core.float32Value((f))))));
  }
  
  static <T0> hydra.compute.Flow<T0, Float> float32Value(hydra.core.FloatValue v) {
    return ((v)).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, Float> otherwise(hydra.core.FloatValue instance) {
        return hydra.monads.Monads.<T0, Float>unexpected(
          "float32",
          hydra.show.core.Core.float_((v)));
      }
      
      @Override
      public hydra.compute.Flow<T0, Float> visit(hydra.core.FloatValue.Float32 f) {
        return hydra.lib.flows.Pure.apply(((f)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, Double> float64(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, Double>>) (l -> hydra.lib.flows.Bind.apply(
        hydra.extract.core.Core.floatLiteral((l)),
        (java.util.function.Function<hydra.core.FloatValue, hydra.compute.Flow<hydra.graph.Graph, Double>>) (f -> hydra.extract.core.Core.float64Value((f))))));
  }
  
  static <T0> hydra.compute.Flow<T0, Double> float64Value(hydra.core.FloatValue v) {
    return ((v)).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, Double> otherwise(hydra.core.FloatValue instance) {
        return hydra.monads.Monads.<T0, Double>unexpected(
          "float64",
          hydra.show.core.Core.float_((v)));
      }
      
      @Override
      public hydra.compute.Flow<T0, Double> visit(hydra.core.FloatValue.Float64 f) {
        return hydra.lib.flows.Pure.apply(((f)).value);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.FloatValue> floatLiteral(hydra.core.Literal lit) {
    return ((lit)).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.FloatValue> otherwise(hydra.core.Literal instance) {
        return hydra.monads.Monads.unexpected(
          "floating-point value",
          hydra.show.core.Core.literal((lit)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.FloatValue> visit(hydra.core.Literal.Float_ v) {
        return hydra.lib.flows.Pure.apply(((v)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.FloatValue> floatValue(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, hydra.core.FloatValue>>) (l -> hydra.extract.core.Core.floatLiteral((l))));
  }
  
  static <T0, T1> hydra.compute.Flow<hydra.graph.Graph, hydra.util.Either<T0, T1>> eitherTerm(java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, T0>> leftFun, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, T1>> rightFun, hydra.core.Term term0) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm((term0)),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Either<T0, T1>>>) (term -> hydra.extract.core.Core.eitherTerm_extract(
        (hydra.show.core.Core::term),
        (leftFun),
        (rightFun),
        (term))));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T0, hydra.util.Either<T1, T2>> eitherTerm_extract(java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, T1>> leftFun, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, T2>> rightFun, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.util.Either<T1, T2>> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.<T0, hydra.util.Either<T1, T2>>unexpected(
          "either value",
          ((hydra_show_core_term2)).apply((term)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.util.Either<T1, T2>> visit(hydra.core.Term.Either et) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Either<T1, T2>>>) (l -> hydra.lib.flows.Map.apply(
            (java.util.function.Function<T1, hydra.util.Either<T1, T2>>) (x -> (hydra.util.Either<T1, T2>) ((hydra.util.Either<T1, T2>) (hydra.util.Either.<T1, T2>left((x))))),
            ((leftFun)).apply((l)))),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Either<T1, T2>>>) (r -> hydra.lib.flows.Map.apply(
            (java.util.function.Function<T2, hydra.util.Either<T1, T2>>) (x -> (hydra.util.Either<T1, T2>) ((hydra.util.Either<T1, T2>) (hydra.util.Either.<T1, T2>right((x))))),
            ((rightFun)).apply((r)))),
          ((et)).value);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.EitherType> eitherType(hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType((typ));
    return ((stripped)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.EitherType> otherwise(hydra.core.Type instance) {
        return hydra.monads.Monads.unexpected(
          "either type",
          hydra.show.core.Core.type((typ)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.EitherType> visit(hydra.core.Type.Either et) {
        return hydra.lib.flows.Pure.apply(((et)).value);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.FunctionType> functionType(hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType((typ));
    return ((stripped)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.FunctionType> otherwise(hydra.core.Type instance) {
        return hydra.monads.Monads.unexpected(
          "function type",
          hydra.show.core.Core.type((typ)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.FunctionType> visit(hydra.core.Type.Function ft) {
        return hydra.lib.flows.Pure.apply(((ft)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Field> injection(hydra.core.Name expected, hydra.core.Term term0) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm((term0)),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Field>>) (term -> ((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Field>>) (v1 -> hydra.extract.core.Core.injection_extract(
        (expected),
        (hydra.show.core.Core::term),
        (v1)))).apply((term))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Field> injection_extract(hydra.core.Name expected, java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Field> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "injection",
          ((hydra_show_core_term2)).apply((term)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Field> visit(hydra.core.Term.Union injection) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            ((((injection)).value).typeName).value,
            ((expected)).value),
          () -> hydra.lib.flows.Pure.apply((((injection)).value).field),
          () -> hydra.monads.Monads.unexpected(
            hydra.lib.strings.Cat2.apply(
              "injection of type ",
              ((expected)).value),
            ((((injection)).value).typeName).value));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, Short> int16(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, Short>>) (l -> hydra.lib.flows.Bind.apply(
        hydra.extract.core.Core.integerLiteral((l)),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<hydra.graph.Graph, Short>>) (i -> hydra.extract.core.Core.int16Value((i))))));
  }
  
  static <T0> hydra.compute.Flow<T0, Short> int16Value(hydra.core.IntegerValue v) {
    return ((v)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, Short> otherwise(hydra.core.IntegerValue instance) {
        return hydra.monads.Monads.<T0, Short>unexpected(
          "int16",
          hydra.show.core.Core.integer((v)));
      }
      
      @Override
      public hydra.compute.Flow<T0, Short> visit(hydra.core.IntegerValue.Int16 i) {
        return hydra.lib.flows.Pure.apply(((i)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, Integer> int32(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, Integer>>) (l -> hydra.lib.flows.Bind.apply(
        hydra.extract.core.Core.integerLiteral((l)),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<hydra.graph.Graph, Integer>>) (i -> hydra.extract.core.Core.int32Value((i))))));
  }
  
  static <T0> hydra.compute.Flow<T0, Integer> int32Value(hydra.core.IntegerValue v) {
    return ((v)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, Integer> otherwise(hydra.core.IntegerValue instance) {
        return hydra.monads.Monads.<T0, Integer>unexpected(
          "int32",
          hydra.show.core.Core.integer((v)));
      }
      
      @Override
      public hydra.compute.Flow<T0, Integer> visit(hydra.core.IntegerValue.Int32 i) {
        return hydra.lib.flows.Pure.apply(((i)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, Long> int64(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, Long>>) (l -> hydra.lib.flows.Bind.apply(
        hydra.extract.core.Core.integerLiteral((l)),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<hydra.graph.Graph, Long>>) (i -> hydra.extract.core.Core.int64Value((i))))));
  }
  
  static <T0> hydra.compute.Flow<T0, Long> int64Value(hydra.core.IntegerValue v) {
    return ((v)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, Long> otherwise(hydra.core.IntegerValue instance) {
        return hydra.monads.Monads.<T0, Long>unexpected(
          "int64",
          hydra.show.core.Core.integer((v)));
      }
      
      @Override
      public hydra.compute.Flow<T0, Long> visit(hydra.core.IntegerValue.Int64 i) {
        return hydra.lib.flows.Pure.apply(((i)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, Byte> int8(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, Byte>>) (l -> hydra.lib.flows.Bind.apply(
        hydra.extract.core.Core.integerLiteral((l)),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<hydra.graph.Graph, Byte>>) (i -> hydra.extract.core.Core.int8Value((i))))));
  }
  
  static <T0> hydra.compute.Flow<T0, Byte> int8Value(hydra.core.IntegerValue v) {
    return ((v)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, Byte> otherwise(hydra.core.IntegerValue instance) {
        return hydra.monads.Monads.<T0, Byte>unexpected(
          "int8",
          hydra.show.core.Core.integer((v)));
      }
      
      @Override
      public hydra.compute.Flow<T0, Byte> visit(hydra.core.IntegerValue.Int8 i) {
        return hydra.lib.flows.Pure.apply(((i)).value);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.IntegerValue> integerLiteral(hydra.core.Literal lit) {
    return ((lit)).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.IntegerValue> otherwise(hydra.core.Literal instance) {
        return hydra.monads.Monads.unexpected(
          "integer value",
          hydra.show.core.Core.literal((lit)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.IntegerValue> visit(hydra.core.Literal.Integer_ v) {
        return hydra.lib.flows.Pure.apply(((v)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.IntegerValue> integerValue(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, hydra.core.IntegerValue>>) (l -> hydra.extract.core.Core.integerLiteral((l))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> lambdaBody(hydra.core.Term term) {
    return hydra.lib.flows.Map.apply(
      projected -> projected.body,
      hydra.extract.core.Core.lambda((term)));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Lambda> lambda(hydra.core.Term term0) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm((term0)),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Lambda>>) (term -> ((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Lambda>>) (v1 -> hydra.extract.core.Core.lambda_extract(
        (hydra.show.core.Core::term),
        (v1)))).apply((term))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Lambda> lambda_extract(java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Lambda> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "lambda",
          ((hydra_show_core_term2)).apply((term)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Lambda> visit(hydra.core.Term.Function function) {
        return (((function)).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, hydra.core.Lambda> otherwise(hydra.core.Function instance) {
            return hydra.monads.Monads.unexpected(
              "lambda",
              ((hydra_show_core_term2)).apply((term)));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.core.Lambda> visit(hydra.core.Function.Lambda l) {
            return hydra.lib.flows.Pure.apply(((l)).value);
          }
        });
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> letBinding(String n, hydra.core.Term term) {
    hydra.core.Name name = new hydra.core.Name((n));
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.let((term)),
      (java.util.function.Function<hydra.core.Let, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (letExpr -> {
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> matchingBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.equality.Equal.apply(
            (((b)).name).value,
            ((name)).value)),
          ((letExpr)).bindings));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(matchingBindings.get()),
          () -> hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
            "no such binding: ",
            (n))),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              hydra.lib.lists.Length.apply(matchingBindings.get()),
              1),
            () -> hydra.lib.flows.Pure.apply((hydra.lib.lists.Head.apply(matchingBindings.get())).term),
            () -> hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
              "multiple bindings named ",
              (n)))));
      }));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Let> let(hydra.core.Term term0) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm((term0)),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Let>>) (term -> ((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Let>>) (v1 -> hydra.extract.core.Core.let_extract(
        (hydra.show.core.Core::term),
        (v1)))).apply((term))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Let> let_extract(java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Let> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "let term",
          ((hydra_show_core_term2)).apply((term)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Let> visit(hydra.core.Term.Let lt) {
        return hydra.lib.flows.Pure.apply(((lt)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>> list(hydra.core.Term term) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm((term)),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>>>) (stripped -> ((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>>>) (v1 -> hydra.extract.core.Core.list_extract(
        (hydra.show.core.Core::term),
        (v1)))).apply((stripped))));
  }
  
  static <T0> hydra.compute.Flow<T0, java.util.List<hydra.core.Term>> list_extract(java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, hydra.core.Term stripped) {
    return ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.core.Term>> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "list",
          ((hydra_show_core_term2)).apply((stripped)));
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.core.Term>> visit(hydra.core.Term.List l) {
        return hydra.lib.flows.Pure.apply(((l)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> listHead(hydra.core.Term term) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.list((term)),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (l -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply((l)),
        () -> hydra.lib.flows.Fail.apply("empty list"),
        () -> hydra.lib.flows.Pure.apply(hydra.lib.lists.Head.apply((l))))));
  }
  
  static <T0> hydra.compute.Flow<hydra.graph.Graph, java.util.List<T0>> listOf(java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, T0>> f, hydra.core.Term term) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.list((term)),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<T0>>>) (els -> hydra.lib.flows.MapList.apply(
        (f),
        (els))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> listType(hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType((typ));
    return ((stripped)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.monads.Monads.unexpected(
          "list type",
          hydra.show.core.Core.type((typ)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.List t) {
        return hydra.lib.flows.Pure.apply(((t)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Literal> literal(hydra.core.Term term0) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm((term0)),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Literal>>) (term -> ((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Literal>>) (v1 -> hydra.extract.core.Core.literal_extract(
        (hydra.show.core.Core::term),
        (v1)))).apply((term))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Literal> literal_extract(java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "literal",
          ((hydra_show_core_term2)).apply((term)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> visit(hydra.core.Term.Literal lit) {
        return hydra.lib.flows.Pure.apply(((lit)).value);
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<hydra.graph.Graph, java.util.Map<T0, T1>> map(java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, T0>> fk, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, T1>> fv, hydra.core.Term term0) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm((term0)),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<T0, T1>>>) (term -> hydra.extract.core.Core.map_extract(
        (hydra.show.core.Core::term),
        (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<T0, T1>>>) (v1 -> hydra.extract.core.Core.map_pair(
          (fk),
          (fv),
          (v1))),
        (term))));
  }
  
  static <T0, T1, T2, T3, T4> hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T2, T4>> map_pair(java.util.function.Function<T0, hydra.compute.Flow<T1, T2>> fk, java.util.function.Function<T3, hydra.compute.Flow<T1, T4>> fv, hydra.util.Tuple.Tuple2<T0, T3> kvPair) {
    hydra.util.Lazy<T0> kterm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((kvPair)));
    hydra.util.Lazy<T3> vterm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((kvPair)));
    return hydra.lib.flows.Bind.apply(
      ((fk)).apply(kterm.get()),
      (java.util.function.Function<T2, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T2, T4>>>) (kval -> hydra.lib.flows.Bind.apply(
        ((fv)).apply(vterm.get()),
        (java.util.function.Function<T4, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T2, T4>>>) (vval -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T2, T4>) ((hydra.util.Tuple.Tuple2<T2, T4>) (new hydra.util.Tuple.Tuple2<T2, T4>((kval), (vval)))))))));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T0, java.util.Map<T1, T2>> map_extract(java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<T1, T2>>> pair, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, java.util.Map<T1, T2>> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.<T0, java.util.Map<T1, T2>>unexpected(
          "map",
          ((hydra_show_core_term2)).apply((term)));
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.Map<T1, T2>> visit(hydra.core.Term.Map m) {
        return hydra.lib.flows.Map.apply(
          (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<T1, T2>>, java.util.Map<T1, T2>>) ((java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<T1, T2>>, java.util.Map<T1, T2>>) ((hydra.lib.maps.FromList::apply))),
          hydra.lib.flows.MapList.apply(
            (pair),
            hydra.lib.maps.ToList.apply(((m)).value)));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.MapType> mapType(hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType((typ));
    return ((stripped)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.MapType> otherwise(hydra.core.Type instance) {
        return hydra.monads.Monads.unexpected(
          "map type",
          hydra.show.core.Core.type((typ)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.MapType> visit(hydra.core.Type.Map mt) {
        return hydra.lib.flows.Pure.apply(((mt)).value);
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T1, java.lang.Void> nArgs(hydra.core.Name name, Integer n, java.util.List<T0> args) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply((args)),
        (n)),
      () -> hydra.lib.flows.Pure.apply(null),
      () -> hydra.monads.Monads.<T1, java.lang.Void>unexpected(
        hydra.lib.strings.Cat.apply(java.util.List.of(
          hydra.lib.literals.ShowInt32.apply((n)),
          " arguments to primitive ",
          hydra.lib.literals.ShowString.apply(((name)).value))),
        hydra.lib.literals.ShowInt32.apply(hydra.lib.lists.Length.apply((args)))));
  }
  
  static <T0> hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<T0>> maybeTerm(java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, T0>> f, hydra.core.Term term0) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm((term0)),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<T0>>>) (term -> hydra.extract.core.Core.maybeTerm_extract(
        (f),
        (hydra.show.core.Core::term),
        (term))));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, hydra.util.Maybe<T1>> maybeTerm_extract(java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, T1>> f, java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.util.Maybe<T1>> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.<T0, hydra.util.Maybe<T1>>unexpected(
          "maybe value",
          ((hydra_show_core_term2)).apply((term)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.util.Maybe<T1>> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Maybe.apply(
          hydra.lib.flows.Pure.apply((hydra.util.Maybe<T1>) (hydra.util.Maybe.<T1>nothing())),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Maybe<T1>>>) (t -> hydra.lib.flows.Map.apply(
            (java.util.function.Function<T1, hydra.util.Maybe<T1>>) ((hydra.lib.maybes.Pure::apply)),
            ((f)).apply((t)))),
          ((mt)).value);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> maybeType(hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType((typ));
    return ((stripped)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.monads.Monads.unexpected(
          "maybe type",
          hydra.show.core.Core.type((typ)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Maybe t) {
        return hydra.lib.flows.Pure.apply(((t)).value);
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<T0, T1>> pair(java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, T0>> kf, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, T1>> vf, hydra.core.Term term0) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm((term0)),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<T0, T1>>>) (term -> hydra.extract.core.Core.pair_extract(
        (hydra.show.core.Core::term),
        (kf),
        (vf),
        (term))));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<T1, T2>> pair_extract(java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, T1>> kf, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, T2>> vf, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<T1, T2>> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.<T0, hydra.util.Tuple.Tuple2<T1, T2>>unexpected(
          "pair",
          ((hydra_show_core_term2)).apply((term)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<T1, T2>> visit(hydra.core.Term.Pair p) {
        return hydra.lib.flows.Bind.apply(
          ((kf)).apply(hydra.lib.pairs.First.apply(((p)).value)),
          (java.util.function.Function<T1, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<T1, T2>>>) (kVal -> hydra.lib.flows.Bind.apply(
            ((vf)).apply(hydra.lib.pairs.Second.apply(((p)).value)),
            (java.util.function.Function<T2, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<T1, T2>>>) (vVal -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T1, T2>) ((hydra.util.Tuple.Tuple2<T1, T2>) (new hydra.util.Tuple.Tuple2<T1, T2>((kVal), (vVal)))))))));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Field>> record(hydra.core.Name expected, hydra.core.Term term0) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.termRecord((term0)),
      (java.util.function.Function<hydra.core.Record, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Field>>>) (record -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          ((record)).typeName,
          (expected)),
        () -> hydra.lib.flows.Pure.apply(((record)).fields),
        () -> hydra.monads.Monads.unexpected(
          hydra.lib.strings.Cat2.apply(
            "record of type ",
            ((expected)).value),
          (((record)).typeName).value))));
  }
  
  static <T0> hydra.compute.Flow<T0, java.util.List<hydra.core.FieldType>> recordType(hydra.core.Name ename, hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType((typ));
    return ((stripped)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.core.FieldType>> otherwise(hydra.core.Type instance) {
        return hydra.monads.Monads.unexpected(
          "record type",
          hydra.show.core.Core.type((typ)));
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.core.FieldType>> visit(hydra.core.Type.Record rowType) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            ((((rowType)).value).typeName).value,
            ((ename)).value),
          () -> hydra.lib.flows.Pure.apply((((rowType)).value).fields),
          () -> hydra.monads.Monads.unexpected(
            hydra.lib.strings.Cat2.apply(
              "record of type ",
              ((ename)).value),
            hydra.lib.strings.Cat2.apply(
              "record of type ",
              ((((rowType)).value).typeName).value)));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.Set<hydra.core.Term>> set(hydra.core.Term term) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm((term)),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, java.util.Set<hydra.core.Term>>>) (stripped -> ((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, java.util.Set<hydra.core.Term>>>) (v1 -> hydra.extract.core.Core.set_extract(
        (hydra.show.core.Core::term),
        (v1)))).apply((stripped))));
  }
  
  static <T0> hydra.compute.Flow<T0, java.util.Set<hydra.core.Term>> set_extract(java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, hydra.core.Term stripped) {
    return ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, java.util.Set<hydra.core.Term>> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "set",
          ((hydra_show_core_term2)).apply((stripped)));
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.Set<hydra.core.Term>> visit(hydra.core.Term.Set s) {
        return hydra.lib.flows.Pure.apply(((s)).value);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<hydra.graph.Graph, java.util.Set<T0>> setOf(java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, T0>> f, hydra.core.Term term) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.set((term)),
      (java.util.function.Function<java.util.Set<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, java.util.Set<T0>>>) (els -> hydra.lib.flows.MapSet.apply(
        (f),
        (els))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> setType(hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType((typ));
    return ((stripped)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.monads.Monads.unexpected(
          "set type",
          hydra.show.core.Core.type((typ)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Set t) {
        return hydra.lib.flows.Pure.apply(((t)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, String> string(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, String>>) (l -> hydra.extract.core.Core.stringLiteral((l))));
  }
  
  static <T0> hydra.compute.Flow<T0, String> stringLiteral(hydra.core.Literal v) {
    return ((v)).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, String> otherwise(hydra.core.Literal instance) {
        return hydra.monads.Monads.<T0, String>unexpected(
          "string",
          hydra.show.core.Core.literal((v)));
      }
      
      @Override
      public hydra.compute.Flow<T0, String> visit(hydra.core.Literal.String_ s) {
        return hydra.lib.flows.Pure.apply(((s)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Record> termRecord(hydra.core.Term term0) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm((term0)),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Record>>) (term -> ((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Record>>) (v1 -> hydra.extract.core.Core.termRecord_extract(
        (hydra.show.core.Core::term),
        (v1)))).apply((term))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Record> termRecord_extract(java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Record> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "record",
          ((hydra_show_core_term2)).apply((term)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Record> visit(hydra.core.Term.Record record) {
        return hydra.lib.flows.Pure.apply(((record)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, Character> uint16(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, Character>>) (l -> hydra.lib.flows.Bind.apply(
        hydra.extract.core.Core.integerLiteral((l)),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<hydra.graph.Graph, Character>>) (i -> hydra.extract.core.Core.uint16Value((i))))));
  }
  
  static <T0> hydra.compute.Flow<T0, Character> uint16Value(hydra.core.IntegerValue v) {
    return ((v)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, Character> otherwise(hydra.core.IntegerValue instance) {
        return hydra.monads.Monads.<T0, Character>unexpected(
          "uint16",
          hydra.show.core.Core.integer((v)));
      }
      
      @Override
      public hydra.compute.Flow<T0, Character> visit(hydra.core.IntegerValue.Uint16 i) {
        return hydra.lib.flows.Pure.apply(((i)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, Long> uint32(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, Long>>) (l -> hydra.lib.flows.Bind.apply(
        hydra.extract.core.Core.integerLiteral((l)),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<hydra.graph.Graph, Long>>) (i -> hydra.extract.core.Core.uint32Value((i))))));
  }
  
  static <T0> hydra.compute.Flow<T0, Long> uint32Value(hydra.core.IntegerValue v) {
    return ((v)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, Long> otherwise(hydra.core.IntegerValue instance) {
        return hydra.monads.Monads.<T0, Long>unexpected(
          "uint32",
          hydra.show.core.Core.integer((v)));
      }
      
      @Override
      public hydra.compute.Flow<T0, Long> visit(hydra.core.IntegerValue.Uint32 i) {
        return hydra.lib.flows.Pure.apply(((i)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.math.BigInteger> uint64(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, java.math.BigInteger>>) (l -> hydra.lib.flows.Bind.apply(
        hydra.extract.core.Core.integerLiteral((l)),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<hydra.graph.Graph, java.math.BigInteger>>) (i -> hydra.extract.core.Core.uint64Value((i))))));
  }
  
  static <T0> hydra.compute.Flow<T0, java.math.BigInteger> uint64Value(hydra.core.IntegerValue v) {
    return ((v)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, java.math.BigInteger> otherwise(hydra.core.IntegerValue instance) {
        return hydra.monads.Monads.<T0, java.math.BigInteger>unexpected(
          "uint64",
          hydra.show.core.Core.integer((v)));
      }
      
      @Override
      public hydra.compute.Flow<T0, java.math.BigInteger> visit(hydra.core.IntegerValue.Uint64 i) {
        return hydra.lib.flows.Pure.apply(((i)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, Short> uint8(hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.literal((t)),
      (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<hydra.graph.Graph, Short>>) (l -> hydra.lib.flows.Bind.apply(
        hydra.extract.core.Core.integerLiteral((l)),
        (java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<hydra.graph.Graph, Short>>) (i -> hydra.extract.core.Core.uint8Value((i))))));
  }
  
  static <T0> hydra.compute.Flow<T0, Short> uint8Value(hydra.core.IntegerValue v) {
    return ((v)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, Short> otherwise(hydra.core.IntegerValue instance) {
        return hydra.monads.Monads.<T0, Short>unexpected(
          "uint8",
          hydra.show.core.Core.integer((v)));
      }
      
      @Override
      public hydra.compute.Flow<T0, Short> visit(hydra.core.IntegerValue.Uint8 i) {
        return hydra.lib.flows.Pure.apply(((i)).value);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, java.util.List<hydra.core.FieldType>> unionType(hydra.core.Name ename, hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType((typ));
    return ((stripped)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.core.FieldType>> otherwise(hydra.core.Type instance) {
        return hydra.monads.Monads.unexpected(
          "union type",
          hydra.show.core.Core.type((typ)));
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.core.FieldType>> visit(hydra.core.Type.Union rowType) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (((rowType)).value).typeName,
            (ename)),
          () -> hydra.lib.flows.Pure.apply((((rowType)).value).fields),
          () -> hydra.monads.Monads.unexpected(
            hydra.lib.strings.Cat2.apply(
              "union of type ",
              ((ename)).value),
            hydra.lib.strings.Cat2.apply(
              "union of type ",
              ((((rowType)).value).typeName).value)));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, java.lang.Void> unit(hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, java.lang.Void> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.<T0, java.lang.Void>unexpected(
          "unit",
          hydra.show.core.Core.term((term)));
      }
      
      @Override
      public hydra.compute.Flow<T0, java.lang.Void> visit(hydra.core.Term.Unit ignored) {
        return hydra.lib.flows.Pure.apply(null);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Name> unitVariant(hydra.core.Name tname, hydra.core.Term term) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.injection(
        (tname),
        (term)),
      (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Name>>) (field -> hydra.lib.flows.Bind.apply(
        hydra.extract.core.Core.unit(((field)).term),
        (java.util.function.Function<java.lang.Void, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Name>>) (ignored -> hydra.lib.flows.Pure.apply(((field)).name)))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> wrap(hydra.core.Name expected, hydra.core.Term term0) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.stripAndDereferenceTerm((term0)),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (term -> ((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (v1 -> hydra.extract.core.Core.wrap_extract(
        (expected),
        (hydra.show.core.Core::term),
        (v1)))).apply((term))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> wrap_extract(hydra.core.Name expected, java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "wrap(",
              ((expected)).value),
            ")"),
          ((hydra_show_core_term2)).apply((term)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Wrap wrappedTerm) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            ((((wrappedTerm)).value).typeName).value,
            ((expected)).value),
          () -> hydra.lib.flows.Pure.apply((((wrappedTerm)).value).body),
          () -> hydra.monads.Monads.unexpected(
            hydra.lib.strings.Cat2.apply(
              "wrapper of type ",
              ((expected)).value),
            ((((wrappedTerm)).value).typeName).value));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> wrappedType(hydra.core.Name ename, hydra.core.Type typ) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType((typ));
    return ((stripped)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.monads.Monads.unexpected(
          "wrapped type",
          hydra.show.core.Core.type((typ)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Wrap wrappedType) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            ((((wrappedType)).value).typeName).value,
            ((ename)).value),
          () -> hydra.lib.flows.Pure.apply((((wrappedType)).value).body),
          () -> hydra.monads.Monads.unexpected(
            hydra.lib.strings.Cat2.apply(
              "wrapped type ",
              ((ename)).value),
            hydra.lib.strings.Cat2.apply(
              "wrapped type ",
              ((((wrappedType)).value).typeName).value)));
      }
    });
  }
}
