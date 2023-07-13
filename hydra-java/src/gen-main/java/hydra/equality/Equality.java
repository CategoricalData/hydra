package hydra.equality;

/**
 * Functions for determining equality of types and terms
 */
public interface Equality {
  static java.util.function.Function<hydra.core.FloatValue, Boolean> floatEqual(hydra.core.FloatValue v1) {
    return ((v1)).accept(new hydra.core.FloatValue.Visitor<>() {
      @Override
      public java.util.function.Function<hydra.core.FloatValue, Boolean> visit(hydra.core.FloatValue.Bigfloat instance) {
        return (java.util.function.Function<hydra.core.FloatValue, Boolean>) (v1 -> ((v1)).accept(new hydra.core.FloatValue.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.FloatValue instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.FloatValue.Bigfloat instance) {
            return hydra.lib.literals.EqualBigfloat.apply(
              (instance.value),
              (instance.value));
          }
        }));
      }
      
      @Override
      public java.util.function.Function<hydra.core.FloatValue, Boolean> visit(hydra.core.FloatValue.Float32 instance) {
        return (java.util.function.Function<hydra.core.FloatValue, Boolean>) (v1 -> ((v1)).accept(new hydra.core.FloatValue.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.FloatValue instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.FloatValue.Float32 instance) {
            return hydra.lib.literals.EqualFloat32.apply(
              (instance.value),
              (instance.value));
          }
        }));
      }
      
      @Override
      public java.util.function.Function<hydra.core.FloatValue, Boolean> visit(hydra.core.FloatValue.Float64 instance) {
        return (java.util.function.Function<hydra.core.FloatValue, Boolean>) (v1 -> ((v1)).accept(new hydra.core.FloatValue.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.FloatValue instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.FloatValue.Float64 instance) {
            return hydra.lib.literals.EqualFloat64.apply(
              (instance.value),
              (instance.value));
          }
        }));
      }
    });
  }
  
  static java.util.function.Function<hydra.core.IntegerValue, Boolean> integerEqual(hydra.core.IntegerValue v1) {
    return ((v1)).accept(new hydra.core.IntegerValue.Visitor<>() {
      @Override
      public java.util.function.Function<hydra.core.IntegerValue, Boolean> visit(hydra.core.IntegerValue.Bigint instance) {
        return (java.util.function.Function<hydra.core.IntegerValue, Boolean>) (v1 -> ((v1)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.IntegerValue instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.IntegerValue.Bigint instance) {
            return hydra.lib.literals.EqualBigint.apply(
              (instance.value),
              (instance.value));
          }
        }));
      }
      
      @Override
      public java.util.function.Function<hydra.core.IntegerValue, Boolean> visit(hydra.core.IntegerValue.Int8 instance) {
        return (java.util.function.Function<hydra.core.IntegerValue, Boolean>) (v1 -> ((v1)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.IntegerValue instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.IntegerValue.Int8 instance) {
            return hydra.lib.literals.EqualInt8.apply(
              (instance.value),
              (instance.value));
          }
        }));
      }
      
      @Override
      public java.util.function.Function<hydra.core.IntegerValue, Boolean> visit(hydra.core.IntegerValue.Int16 instance) {
        return (java.util.function.Function<hydra.core.IntegerValue, Boolean>) (v1 -> ((v1)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.IntegerValue instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.IntegerValue.Int16 instance) {
            return hydra.lib.literals.EqualInt16.apply(
              (instance.value),
              (instance.value));
          }
        }));
      }
      
      @Override
      public java.util.function.Function<hydra.core.IntegerValue, Boolean> visit(hydra.core.IntegerValue.Int32 instance) {
        return (java.util.function.Function<hydra.core.IntegerValue, Boolean>) (v1 -> ((v1)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.IntegerValue instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.IntegerValue.Int32 instance) {
            return hydra.lib.literals.EqualInt32.apply(
              (instance.value),
              (instance.value));
          }
        }));
      }
      
      @Override
      public java.util.function.Function<hydra.core.IntegerValue, Boolean> visit(hydra.core.IntegerValue.Int64 instance) {
        return (java.util.function.Function<hydra.core.IntegerValue, Boolean>) (v1 -> ((v1)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.IntegerValue instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.IntegerValue.Int64 instance) {
            return hydra.lib.literals.EqualInt64.apply(
              (instance.value),
              (instance.value));
          }
        }));
      }
      
      @Override
      public java.util.function.Function<hydra.core.IntegerValue, Boolean> visit(hydra.core.IntegerValue.Uint8 instance) {
        return (java.util.function.Function<hydra.core.IntegerValue, Boolean>) (v1 -> ((v1)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.IntegerValue instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.IntegerValue.Uint8 instance) {
            return hydra.lib.literals.EqualUint8.apply(
              (instance.value),
              (instance.value));
          }
        }));
      }
      
      @Override
      public java.util.function.Function<hydra.core.IntegerValue, Boolean> visit(hydra.core.IntegerValue.Uint16 instance) {
        return (java.util.function.Function<hydra.core.IntegerValue, Boolean>) (v1 -> ((v1)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.IntegerValue instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.IntegerValue.Uint16 instance) {
            return hydra.lib.literals.EqualUint16.apply(
              (instance.value),
              (instance.value));
          }
        }));
      }
      
      @Override
      public java.util.function.Function<hydra.core.IntegerValue, Boolean> visit(hydra.core.IntegerValue.Uint32 instance) {
        return (java.util.function.Function<hydra.core.IntegerValue, Boolean>) (v1 -> ((v1)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.IntegerValue instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.IntegerValue.Uint32 instance) {
            return hydra.lib.literals.EqualUint32.apply(
              (instance.value),
              (instance.value));
          }
        }));
      }
      
      @Override
      public java.util.function.Function<hydra.core.IntegerValue, Boolean> visit(hydra.core.IntegerValue.Uint64 instance) {
        return (java.util.function.Function<hydra.core.IntegerValue, Boolean>) (v1 -> ((v1)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.IntegerValue instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.IntegerValue.Uint64 instance) {
            return hydra.lib.literals.EqualUint64.apply(
              (instance.value),
              (instance.value));
          }
        }));
      }
    });
  }
  
  static java.util.function.Function<hydra.core.Literal, Boolean> literalEqual(hydra.core.Literal v1) {
    return ((v1)).accept(new hydra.core.Literal.Visitor<>() {
      @Override
      public java.util.function.Function<hydra.core.Literal, Boolean> visit(hydra.core.Literal.Binary instance) {
        return (java.util.function.Function<hydra.core.Literal, Boolean>) (v1 -> ((v1)).accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Literal instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.Literal.Binary instance) {
            return hydra.lib.literals.EqualBinary.apply(
              (instance.value),
              (instance.value));
          }
        }));
      }
      
      @Override
      public java.util.function.Function<hydra.core.Literal, Boolean> visit(hydra.core.Literal.Boolean_ instance) {
        return (java.util.function.Function<hydra.core.Literal, Boolean>) (v1 -> ((v1)).accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Literal instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.Literal.Boolean_ instance) {
            return hydra.lib.literals.EqualBoolean.apply(
              (instance.value),
              (instance.value));
          }
        }));
      }
      
      @Override
      public java.util.function.Function<hydra.core.Literal, Boolean> visit(hydra.core.Literal.Float_ instance) {
        return (java.util.function.Function<hydra.core.Literal, Boolean>) (v1 -> ((v1)).accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Literal instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.Literal.Float_ instance) {
            return (hydra.equality.Equality.floatEqual((instance.value))).apply((instance.value));
          }
        }));
      }
      
      @Override
      public java.util.function.Function<hydra.core.Literal, Boolean> visit(hydra.core.Literal.Integer_ instance) {
        return (java.util.function.Function<hydra.core.Literal, Boolean>) (v1 -> ((v1)).accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Literal instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.Literal.Integer_ instance) {
            return (hydra.equality.Equality.integerEqual((instance.value))).apply((instance.value));
          }
        }));
      }
      
      @Override
      public java.util.function.Function<hydra.core.Literal, Boolean> visit(hydra.core.Literal.String_ instance) {
        return (java.util.function.Function<hydra.core.Literal, Boolean>) (v1 -> ((v1)).accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Literal instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.Literal.String_ instance) {
            return hydra.lib.literals.EqualString.apply(
              (instance.value),
              (instance.value));
          }
        }));
      }
    });
  }
  
  static <A> java.util.function.Function<hydra.core.Term<A>, Boolean> termEqual(hydra.core.Term<A> v1) {
    return ((v1)).accept(new hydra.core.Term.Visitor<>() {
      @Override
      public java.util.function.Function<hydra.core.Term<A>, Boolean> visit(hydra.core.Term.Annotated<A> instance) {
        return (java.util.function.Function<hydra.core.Term<A>, Boolean>) (ignored -> false);
      }
      
      @Override
      public java.util.function.Function<hydra.core.Term<A>, Boolean> visit(hydra.core.Term.Application<A> instance) {
        return (java.util.function.Function<hydra.core.Term<A>, Boolean>) (ignored -> false);
      }
      
      @Override
      public java.util.function.Function<hydra.core.Term<A>, Boolean> visit(hydra.core.Term.Function<A> instance) {
        return (java.util.function.Function<hydra.core.Term<A>, Boolean>) (ignored -> false);
      }
      
      @Override
      public java.util.function.Function<hydra.core.Term<A>, Boolean> visit(hydra.core.Term.Let<A> instance) {
        return (java.util.function.Function<hydra.core.Term<A>, Boolean>) (ignored -> false);
      }
      
      @Override
      public java.util.function.Function<hydra.core.Term<A>, Boolean> visit(hydra.core.Term.List<A> instance) {
        return (java.util.function.Function<hydra.core.Term<A>, Boolean>) (ignored -> false);
      }
      
      @Override
      public java.util.function.Function<hydra.core.Term<A>, Boolean> visit(hydra.core.Term.Literal<A> instance) {
        return (java.util.function.Function<hydra.core.Term<A>, Boolean>) (v1 -> ((v1)).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Term<A> instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.Term.Literal<A> instance) {
            return (hydra.equality.Equality.literalEqual((instance.value))).apply((instance.value));
          }
        }));
      }
      
      @Override
      public java.util.function.Function<hydra.core.Term<A>, Boolean> visit(hydra.core.Term.Map<A> instance) {
        return (java.util.function.Function<hydra.core.Term<A>, Boolean>) (ignored -> false);
      }
      
      @Override
      public java.util.function.Function<hydra.core.Term<A>, Boolean> visit(hydra.core.Term.Optional<A> instance) {
        return (java.util.function.Function<hydra.core.Term<A>, Boolean>) (ignored -> false);
      }
      
      @Override
      public java.util.function.Function<hydra.core.Term<A>, Boolean> visit(hydra.core.Term.Product<A> instance) {
        return (java.util.function.Function<hydra.core.Term<A>, Boolean>) (ignored -> false);
      }
      
      @Override
      public java.util.function.Function<hydra.core.Term<A>, Boolean> visit(hydra.core.Term.Record<A> instance) {
        return (java.util.function.Function<hydra.core.Term<A>, Boolean>) (ignored -> false);
      }
      
      @Override
      public java.util.function.Function<hydra.core.Term<A>, Boolean> visit(hydra.core.Term.Set<A> instance) {
        return (java.util.function.Function<hydra.core.Term<A>, Boolean>) (ignored -> false);
      }
      
      @Override
      public java.util.function.Function<hydra.core.Term<A>, Boolean> visit(hydra.core.Term.Stream<A> instance) {
        return (java.util.function.Function<hydra.core.Term<A>, Boolean>) (ignored -> false);
      }
      
      @Override
      public java.util.function.Function<hydra.core.Term<A>, Boolean> visit(hydra.core.Term.Sum<A> instance) {
        return (java.util.function.Function<hydra.core.Term<A>, Boolean>) (ignored -> false);
      }
      
      @Override
      public java.util.function.Function<hydra.core.Term<A>, Boolean> visit(hydra.core.Term.Union<A> instance) {
        return (java.util.function.Function<hydra.core.Term<A>, Boolean>) (ignored -> false);
      }
      
      @Override
      public java.util.function.Function<hydra.core.Term<A>, Boolean> visit(hydra.core.Term.Variable<A> instance) {
        return (java.util.function.Function<hydra.core.Term<A>, Boolean>) (ignored -> false);
      }
      
      @Override
      public java.util.function.Function<hydra.core.Term<A>, Boolean> visit(hydra.core.Term.Wrap<A> instance) {
        return (java.util.function.Function<hydra.core.Term<A>, Boolean>) (ignored -> false);
      }
    });
  }
}