// Note: this is an automatically generated file. Do not edit.

package hydra.scala;

/**
 * Type preparation functions for Scala code generation
 */
public interface Prepare {
  static hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>> prepareLiteralType(hydra.core.LiteralType at) {
    return (at).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>> otherwise(hydra.core.LiteralType instance) {
        return hydra.scala.Prepare.same(at);
      }

      @Override
      public hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>> visit(hydra.core.LiteralType.Binary ignored) {
        return (hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>>) ((hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>>) (new hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>>(new hydra.core.LiteralType.String_(), (hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>((java.util.function.Function<hydra.core.Literal, hydra.core.Literal>) (v -> (v).accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public hydra.core.Literal otherwise(hydra.core.Literal instance) {
            return v;
          }

          @Override
          public hydra.core.Literal visit(hydra.core.Literal.Binary b) {
            return new hydra.core.Literal.String_(hydra.lib.literals.BinaryToString.apply((b).value));
          }
        })), hydra.lib.sets.FromList.apply(hydra.util.ConsList.of("replace binary strings with character strings"))))))));
      }

      @Override
      public hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>> visit(hydra.core.LiteralType.Float_ ft) {
        hydra.util.Pair<hydra.core.FloatType, hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, hydra.util.PersistentSet<String>>> result = hydra.scala.Prepare.prepareFloatType((ft).value);
        hydra.util.Lazy<hydra.util.PersistentSet<String>> msgs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(result)));
        java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue> rep = hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(result));
        hydra.util.Lazy<hydra.core.FloatType> rtyp = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
        return (hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>>) ((hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>>) (new hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>>(new hydra.core.LiteralType.Float_(rtyp.get()), (hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>((java.util.function.Function<hydra.core.Literal, hydra.core.Literal>) (v -> (v).accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public hydra.core.Literal otherwise(hydra.core.Literal instance) {
            return v;
          }

          @Override
          public hydra.core.Literal visit(hydra.core.Literal.Float_ fv) {
            return new hydra.core.Literal.Float_((rep).apply((fv).value));
          }
        })), msgs.get()))))));
      }

      @Override
      public hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>> visit(hydra.core.LiteralType.Integer_ it) {
        hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>> result = hydra.scala.Prepare.prepareIntegerType((it).value);
        hydra.util.Lazy<hydra.util.PersistentSet<String>> msgs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(result)));
        java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue> rep = hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(result));
        hydra.util.Lazy<hydra.core.IntegerType> rtyp = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
        return (hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>>) ((hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>>) (new hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>>(new hydra.core.LiteralType.Integer_(rtyp.get()), (hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>((java.util.function.Function<hydra.core.Literal, hydra.core.Literal>) (v -> (v).accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public hydra.core.Literal otherwise(hydra.core.Literal instance) {
            return v;
          }

          @Override
          public hydra.core.Literal visit(hydra.core.Literal.Integer_ iv) {
            return new hydra.core.Literal.Integer_((rep).apply((iv).value));
          }
        })), msgs.get()))))));
      }
    });
  }

  static hydra.util.Pair<hydra.core.FloatType, hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, hydra.util.PersistentSet<String>>> prepareFloatType(hydra.core.FloatType ft) {
    return (ft).accept(new hydra.core.FloatType.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.core.FloatType, hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, hydra.util.PersistentSet<String>>> otherwise(hydra.core.FloatType instance) {
        return hydra.scala.Prepare.same(ft);
      }

      @Override
      public hydra.util.Pair<hydra.core.FloatType, hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, hydra.util.PersistentSet<String>>> visit(hydra.core.FloatType.Bigfloat ignored) {
        return (hydra.util.Pair<hydra.core.FloatType, hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, hydra.util.PersistentSet<String>>>) ((hydra.util.Pair<hydra.core.FloatType, hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, hydra.util.PersistentSet<String>>>) (new hydra.util.Pair<hydra.core.FloatType, hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, hydra.util.PersistentSet<String>>>(new hydra.core.FloatType.Float64(), (hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, hydra.util.PersistentSet<String>>((java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>) (v -> (v).accept(new hydra.core.FloatValue.PartialVisitor<>() {
          @Override
          public hydra.core.FloatValue otherwise(hydra.core.FloatValue instance) {
            return v;
          }

          @Override
          public hydra.core.FloatValue visit(hydra.core.FloatValue.Bigfloat d) {
            return new hydra.core.FloatValue.Float64(hydra.lib.literals.BigfloatToFloat64.apply((d).value));
          }
        })), hydra.lib.sets.FromList.apply(hydra.util.ConsList.of("replace arbitrary-precision floating-point numbers with 64-bit floating-point numbers (doubles)"))))))));
      }
    });
  }

  static hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>> prepareIntegerType(hydra.core.IntegerType it) {
    return (it).accept(new hydra.core.IntegerType.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>> otherwise(hydra.core.IntegerType instance) {
        return hydra.scala.Prepare.same(it);
      }

      @Override
      public hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>> visit(hydra.core.IntegerType.Bigint ignored) {
        return (hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>>) ((hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>>) (new hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>>(new hydra.core.IntegerType.Int64(), (hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>((java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>) (v -> (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public hydra.core.IntegerValue otherwise(hydra.core.IntegerValue instance) {
            return v;
          }

          @Override
          public hydra.core.IntegerValue visit(hydra.core.IntegerValue.Bigint i) {
            return new hydra.core.IntegerValue.Int64(hydra.lib.literals.BigintToInt64.apply((i).value));
          }
        })), hydra.lib.sets.FromList.apply(hydra.util.ConsList.of("replace arbitrary-precision integers with 64-bit integers"))))))));
      }

      @Override
      public hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>> visit(hydra.core.IntegerType.Uint8 ignored) {
        return (hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>>) ((hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>>) (new hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>>(new hydra.core.IntegerType.Int8(), (hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>((java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>) (v -> (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public hydra.core.IntegerValue otherwise(hydra.core.IntegerValue instance) {
            return v;
          }

          @Override
          public hydra.core.IntegerValue visit(hydra.core.IntegerValue.Uint8 i) {
            return new hydra.core.IntegerValue.Int8(hydra.lib.literals.BigintToInt8.apply(hydra.lib.literals.Uint8ToBigint.apply((i).value)));
          }
        })), hydra.lib.sets.FromList.apply(hydra.util.ConsList.of("replace unsigned 8-bit integers with signed 8-bit integers"))))))));
      }

      @Override
      public hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>> visit(hydra.core.IntegerType.Uint32 ignored) {
        return (hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>>) ((hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>>) (new hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>>(new hydra.core.IntegerType.Int32(), (hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>((java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>) (v -> (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public hydra.core.IntegerValue otherwise(hydra.core.IntegerValue instance) {
            return v;
          }

          @Override
          public hydra.core.IntegerValue visit(hydra.core.IntegerValue.Uint32 i) {
            return new hydra.core.IntegerValue.Int32(hydra.lib.literals.BigintToInt32.apply(hydra.lib.literals.Uint32ToBigint.apply((i).value)));
          }
        })), hydra.lib.sets.FromList.apply(hydra.util.ConsList.of("replace unsigned 32-bit integers with signed 32-bit integers"))))))));
      }

      @Override
      public hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>> visit(hydra.core.IntegerType.Uint64 ignored) {
        return (hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>>) ((hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>>) (new hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>>(new hydra.core.IntegerType.Int64(), (hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.PersistentSet<String>>((java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>) (v -> (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public hydra.core.IntegerValue otherwise(hydra.core.IntegerValue instance) {
            return v;
          }

          @Override
          public hydra.core.IntegerValue visit(hydra.core.IntegerValue.Uint64 i) {
            return new hydra.core.IntegerValue.Int64(hydra.lib.literals.BigintToInt64.apply(hydra.lib.literals.Uint64ToBigint.apply((i).value)));
          }
        })), hydra.lib.sets.FromList.apply(hydra.util.ConsList.of("replace unsigned 64-bit integers with signed 64-bit integers"))))))));
      }
    });
  }

  static <T0> hydra.util.Pair<hydra.core.Type, hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, hydra.util.PersistentSet<String>>> prepareType(T0 cx, hydra.core.Type typ) {
    return hydra.Strip.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.core.Type, hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, hydra.util.PersistentSet<String>>> otherwise(hydra.core.Type instance) {
        return hydra.scala.Prepare.same(typ);
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, hydra.util.PersistentSet<String>>> visit(hydra.core.Type.Literal at) {
        hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, hydra.util.PersistentSet<String>>> result = hydra.scala.Prepare.prepareLiteralType((at).value);
        hydra.util.Lazy<hydra.util.PersistentSet<String>> msgs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(result)));
        java.util.function.Function<hydra.core.Literal, hydra.core.Literal> rep = hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(result));
        hydra.util.Lazy<hydra.core.LiteralType> rtyp = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
        return (hydra.util.Pair<hydra.core.Type, hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, hydra.util.PersistentSet<String>>>) ((hydra.util.Pair<hydra.core.Type, hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, hydra.util.PersistentSet<String>>>) (new hydra.util.Pair<hydra.core.Type, hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, hydra.util.PersistentSet<String>>>(new hydra.core.Type.Literal(rtyp.get()), (hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, hydra.util.PersistentSet<String>>((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v -> (v).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Term instance) {
            return v;
          }

          @Override
          public hydra.core.Term visit(hydra.core.Term.Literal av) {
            return new hydra.core.Term.Literal((rep).apply((av).value));
          }
        })), msgs.get()))))));
      }
    });
  }

  static <T0, T1, T2> hydra.util.Pair<T0, hydra.util.Pair<java.util.function.Function<T1, T1>, hydra.util.PersistentSet<T2>>> same(T0 x) {
    return (hydra.util.Pair<T0, hydra.util.Pair<java.util.function.Function<T1, T1>, hydra.util.PersistentSet<T2>>>) ((hydra.util.Pair<T0, hydra.util.Pair<java.util.function.Function<T1, T1>, hydra.util.PersistentSet<T2>>>) (new hydra.util.Pair<T0, hydra.util.Pair<java.util.function.Function<T1, T1>, hydra.util.PersistentSet<T2>>>(x, (hydra.util.Pair<java.util.function.Function<T1, T1>, hydra.util.PersistentSet<T2>>) ((hydra.util.Pair<java.util.function.Function<T1, T1>, hydra.util.PersistentSet<T2>>) (new hydra.util.Pair<java.util.function.Function<T1, T1>, hydra.util.PersistentSet<T2>>((java.util.function.Function<T1, T1>) (y -> y), (hydra.util.PersistentSet<T2>) (hydra.lib.sets.Empty.<T2>apply())))))));
  }
}
