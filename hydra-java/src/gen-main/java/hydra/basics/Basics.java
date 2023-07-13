package hydra.basics;

/**
 * Basic functions for working with types and terms. These functions are not allowed to include references to primitive functions, as the definitions of some primitive functions in turn depend on them.
 */
public interface Basics {
  static <A> hydra.mantle.EliminationVariant eliminationVariant(hydra.core.Elimination<A> v1) {
    return ((v1)).accept(new hydra.core.Elimination.Visitor<>() {
      @Override
      public hydra.mantle.EliminationVariant visit(hydra.core.Elimination.List<A> instance) {
        return new hydra.mantle.EliminationVariant.List();
      }
      
      @Override
      public hydra.mantle.EliminationVariant visit(hydra.core.Elimination.Optional<A> instance) {
        return new hydra.mantle.EliminationVariant.Optional();
      }
      
      @Override
      public hydra.mantle.EliminationVariant visit(hydra.core.Elimination.Record<A> instance) {
        return new hydra.mantle.EliminationVariant.Record();
      }
      
      @Override
      public hydra.mantle.EliminationVariant visit(hydra.core.Elimination.Union<A> instance) {
        return new hydra.mantle.EliminationVariant.Union();
      }
      
      @Override
      public hydra.mantle.EliminationVariant visit(hydra.core.Elimination.Wrap<A> instance) {
        return new hydra.mantle.EliminationVariant.Wrap();
      }
    });
  }
  
  java.util.List<hydra.mantle.EliminationVariant> eliminationVariants = java.util.Arrays.asList(
    new hydra.mantle.EliminationVariant.List(),
    new hydra.mantle.EliminationVariant.Wrap(),
    new hydra.mantle.EliminationVariant.Optional(),
    new hydra.mantle.EliminationVariant.Record(),
    new hydra.mantle.EliminationVariant.Union());
  
  static hydra.mantle.Precision floatTypePrecision(hydra.core.FloatType v1) {
    return ((v1)).accept(new hydra.core.FloatType.Visitor<>() {
      @Override
      public hydra.mantle.Precision visit(hydra.core.FloatType.Bigfloat instance) {
        return new hydra.mantle.Precision.Arbitrary();
      }
      
      @Override
      public hydra.mantle.Precision visit(hydra.core.FloatType.Float32 instance) {
        return new hydra.mantle.Precision.Bits(32);
      }
      
      @Override
      public hydra.mantle.Precision visit(hydra.core.FloatType.Float64 instance) {
        return new hydra.mantle.Precision.Bits(64);
      }
    });
  }
  
  java.util.List<hydra.core.FloatType> floatTypes = java.util.Arrays.asList(
    new hydra.core.FloatType.Bigfloat(),
    new hydra.core.FloatType.Float32(),
    new hydra.core.FloatType.Float64());
  
  static hydra.core.FloatType floatValueType(hydra.core.FloatValue v1) {
    return ((v1)).accept(new hydra.core.FloatValue.Visitor<>() {
      @Override
      public hydra.core.FloatType visit(hydra.core.FloatValue.Bigfloat instance) {
        return new hydra.core.FloatType.Bigfloat();
      }
      
      @Override
      public hydra.core.FloatType visit(hydra.core.FloatValue.Float32 instance) {
        return new hydra.core.FloatType.Float32();
      }
      
      @Override
      public hydra.core.FloatType visit(hydra.core.FloatValue.Float64 instance) {
        return new hydra.core.FloatType.Float64();
      }
    });
  }
  
  static <A> hydra.mantle.FunctionVariant functionVariant(hydra.core.Function<A> v1) {
    return ((v1)).accept(new hydra.core.Function.Visitor<>() {
      @Override
      public hydra.mantle.FunctionVariant visit(hydra.core.Function.Elimination<A> instance) {
        return new hydra.mantle.FunctionVariant.Elimination();
      }
      
      @Override
      public hydra.mantle.FunctionVariant visit(hydra.core.Function.Lambda<A> instance) {
        return new hydra.mantle.FunctionVariant.Lambda();
      }
      
      @Override
      public hydra.mantle.FunctionVariant visit(hydra.core.Function.Primitive<A> instance) {
        return new hydra.mantle.FunctionVariant.Primitive();
      }
    });
  }
  
  java.util.List<hydra.mantle.FunctionVariant> functionVariants = java.util.Arrays.asList(
    new hydra.mantle.FunctionVariant.Elimination(),
    new hydra.mantle.FunctionVariant.Lambda(),
    new hydra.mantle.FunctionVariant.Primitive());
  
  static Boolean integerTypeIsSigned(hydra.core.IntegerType v1) {
    return ((v1)).accept(new hydra.core.IntegerType.Visitor<>() {
      @Override
      public Boolean visit(hydra.core.IntegerType.Bigint instance) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.IntegerType.Int8 instance) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.IntegerType.Int16 instance) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.IntegerType.Int32 instance) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.IntegerType.Int64 instance) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.IntegerType.Uint8 instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.IntegerType.Uint16 instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.IntegerType.Uint32 instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.IntegerType.Uint64 instance) {
        return false;
      }
    });
  }
  
  static hydra.mantle.Precision integerTypePrecision(hydra.core.IntegerType v1) {
    return ((v1)).accept(new hydra.core.IntegerType.Visitor<>() {
      @Override
      public hydra.mantle.Precision visit(hydra.core.IntegerType.Bigint instance) {
        return new hydra.mantle.Precision.Arbitrary();
      }
      
      @Override
      public hydra.mantle.Precision visit(hydra.core.IntegerType.Int8 instance) {
        return new hydra.mantle.Precision.Bits(8);
      }
      
      @Override
      public hydra.mantle.Precision visit(hydra.core.IntegerType.Int16 instance) {
        return new hydra.mantle.Precision.Bits(16);
      }
      
      @Override
      public hydra.mantle.Precision visit(hydra.core.IntegerType.Int32 instance) {
        return new hydra.mantle.Precision.Bits(32);
      }
      
      @Override
      public hydra.mantle.Precision visit(hydra.core.IntegerType.Int64 instance) {
        return new hydra.mantle.Precision.Bits(64);
      }
      
      @Override
      public hydra.mantle.Precision visit(hydra.core.IntegerType.Uint8 instance) {
        return new hydra.mantle.Precision.Bits(8);
      }
      
      @Override
      public hydra.mantle.Precision visit(hydra.core.IntegerType.Uint16 instance) {
        return new hydra.mantle.Precision.Bits(16);
      }
      
      @Override
      public hydra.mantle.Precision visit(hydra.core.IntegerType.Uint32 instance) {
        return new hydra.mantle.Precision.Bits(32);
      }
      
      @Override
      public hydra.mantle.Precision visit(hydra.core.IntegerType.Uint64 instance) {
        return new hydra.mantle.Precision.Bits(64);
      }
    });
  }
  
  java.util.List<hydra.core.IntegerType> integerTypes = java.util.Arrays.asList(
    new hydra.core.IntegerType.Bigint(),
    new hydra.core.IntegerType.Int8(),
    new hydra.core.IntegerType.Int16(),
    new hydra.core.IntegerType.Int32(),
    new hydra.core.IntegerType.Int64(),
    new hydra.core.IntegerType.Uint8(),
    new hydra.core.IntegerType.Uint16(),
    new hydra.core.IntegerType.Uint32(),
    new hydra.core.IntegerType.Uint64());
  
  static hydra.core.IntegerType integerValueType(hydra.core.IntegerValue v1) {
    return ((v1)).accept(new hydra.core.IntegerValue.Visitor<>() {
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Bigint instance) {
        return new hydra.core.IntegerType.Bigint();
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Int8 instance) {
        return new hydra.core.IntegerType.Int8();
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Int16 instance) {
        return new hydra.core.IntegerType.Int16();
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Int32 instance) {
        return new hydra.core.IntegerType.Int32();
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Int64 instance) {
        return new hydra.core.IntegerType.Int64();
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Uint8 instance) {
        return new hydra.core.IntegerType.Uint8();
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Uint16 instance) {
        return new hydra.core.IntegerType.Uint16();
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Uint32 instance) {
        return new hydra.core.IntegerType.Uint32();
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Uint64 instance) {
        return new hydra.core.IntegerType.Uint64();
      }
    });
  }
  
  static hydra.core.LiteralType literalType(hydra.core.Literal v1) {
    return ((v1)).accept(new hydra.core.Literal.Visitor<>() {
      @Override
      public hydra.core.LiteralType visit(hydra.core.Literal.Binary instance) {
        return new hydra.core.LiteralType.Binary();
      }
      
      @Override
      public hydra.core.LiteralType visit(hydra.core.Literal.Boolean_ instance) {
        return new hydra.core.LiteralType.Boolean_();
      }
      
      @Override
      public hydra.core.LiteralType visit(hydra.core.Literal.Float_ instance) {
        return new hydra.core.LiteralType.Float_(hydra.basics.Basics.floatValueType((instance.value)));
      }
      
      @Override
      public hydra.core.LiteralType visit(hydra.core.Literal.Integer_ instance) {
        return new hydra.core.LiteralType.Integer_(hydra.basics.Basics.integerValueType((instance.value)));
      }
      
      @Override
      public hydra.core.LiteralType visit(hydra.core.Literal.String_ instance) {
        return new hydra.core.LiteralType.String_();
      }
    });
  }
  
  static hydra.mantle.LiteralVariant literalTypeVariant(hydra.core.LiteralType v1) {
    return ((v1)).accept(new hydra.core.LiteralType.Visitor<>() {
      @Override
      public hydra.mantle.LiteralVariant visit(hydra.core.LiteralType.Binary instance) {
        return new hydra.mantle.LiteralVariant.Binary();
      }
      
      @Override
      public hydra.mantle.LiteralVariant visit(hydra.core.LiteralType.Boolean_ instance) {
        return new hydra.mantle.LiteralVariant.Boolean_();
      }
      
      @Override
      public hydra.mantle.LiteralVariant visit(hydra.core.LiteralType.Float_ instance) {
        return new hydra.mantle.LiteralVariant.Float_();
      }
      
      @Override
      public hydra.mantle.LiteralVariant visit(hydra.core.LiteralType.Integer_ instance) {
        return new hydra.mantle.LiteralVariant.Integer_();
      }
      
      @Override
      public hydra.mantle.LiteralVariant visit(hydra.core.LiteralType.String_ instance) {
        return new hydra.mantle.LiteralVariant.String_();
      }
    });
  }
  
  static hydra.mantle.LiteralVariant literalVariant(hydra.core.Literal x) {
    return hydra.basics.Basics.literalTypeVariant(hydra.basics.Basics.literalType((x)));
  }
  
  java.util.List<hydra.mantle.LiteralVariant> literalVariants = java.util.Arrays.asList(
    new hydra.mantle.LiteralVariant.Binary(),
    new hydra.mantle.LiteralVariant.Boolean_(),
    new hydra.mantle.LiteralVariant.Float_(),
    new hydra.mantle.LiteralVariant.Integer_(),
    new hydra.mantle.LiteralVariant.String_());
  
  static <A> java.util.function.Function<hydra.core.Term<A>, A> termMeta(hydra.graph.Graph<A> x) {
    return (((x)).annotations).termAnnotation;
  }
  
  static <A> hydra.mantle.TermVariant termVariant(hydra.core.Term<A> v1) {
    return ((v1)).accept(new hydra.core.Term.Visitor<>() {
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Annotated<A> instance) {
        return new hydra.mantle.TermVariant.Annotated();
      }
      
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Application<A> instance) {
        return new hydra.mantle.TermVariant.Application();
      }
      
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Function<A> instance) {
        return new hydra.mantle.TermVariant.Function();
      }
      
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Let<A> instance) {
        return new hydra.mantle.TermVariant.Let();
      }
      
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.List<A> instance) {
        return new hydra.mantle.TermVariant.List();
      }
      
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Literal<A> instance) {
        return new hydra.mantle.TermVariant.Literal();
      }
      
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Map<A> instance) {
        return new hydra.mantle.TermVariant.Map();
      }
      
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Optional<A> instance) {
        return new hydra.mantle.TermVariant.Optional();
      }
      
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Product<A> instance) {
        return new hydra.mantle.TermVariant.Product();
      }
      
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Record<A> instance) {
        return new hydra.mantle.TermVariant.Record();
      }
      
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Set<A> instance) {
        return new hydra.mantle.TermVariant.Set();
      }
      
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Stream<A> instance) {
        return new hydra.mantle.TermVariant.Stream();
      }
      
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Sum<A> instance) {
        return new hydra.mantle.TermVariant.Sum();
      }
      
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Union<A> instance) {
        return new hydra.mantle.TermVariant.Union();
      }
      
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Variable<A> instance) {
        return new hydra.mantle.TermVariant.Variable();
      }
      
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Wrap<A> instance) {
        return new hydra.mantle.TermVariant.Wrap();
      }
    });
  }
  
  java.util.List<hydra.mantle.TermVariant> termVariants = java.util.Arrays.asList(
    new hydra.mantle.TermVariant.Annotated(),
    new hydra.mantle.TermVariant.Application(),
    new hydra.mantle.TermVariant.Literal(),
    new hydra.mantle.TermVariant.Function(),
    new hydra.mantle.TermVariant.List(),
    new hydra.mantle.TermVariant.Map(),
    new hydra.mantle.TermVariant.Optional(),
    new hydra.mantle.TermVariant.Product(),
    new hydra.mantle.TermVariant.Record(),
    new hydra.mantle.TermVariant.Set(),
    new hydra.mantle.TermVariant.Stream(),
    new hydra.mantle.TermVariant.Sum(),
    new hydra.mantle.TermVariant.Union(),
    new hydra.mantle.TermVariant.Variable(),
    new hydra.mantle.TermVariant.Wrap());
  
  static <A> hydra.mantle.TypeVariant typeVariant(hydra.core.Type<A> v1) {
    return ((v1)).accept(new hydra.core.Type.Visitor<>() {
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Annotated<A> instance) {
        return new hydra.mantle.TypeVariant.Annotated();
      }
      
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Application<A> instance) {
        return new hydra.mantle.TypeVariant.Application();
      }
      
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Function<A> instance) {
        return new hydra.mantle.TypeVariant.Function();
      }
      
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Lambda<A> instance) {
        return new hydra.mantle.TypeVariant.Lambda();
      }
      
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.List<A> instance) {
        return new hydra.mantle.TypeVariant.List();
      }
      
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Literal<A> instance) {
        return new hydra.mantle.TypeVariant.Literal();
      }
      
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Map<A> instance) {
        return new hydra.mantle.TypeVariant.Map();
      }
      
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Optional<A> instance) {
        return new hydra.mantle.TypeVariant.Optional();
      }
      
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Product<A> instance) {
        return new hydra.mantle.TypeVariant.Product();
      }
      
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Record<A> instance) {
        return new hydra.mantle.TypeVariant.Record();
      }
      
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Set<A> instance) {
        return new hydra.mantle.TypeVariant.Set();
      }
      
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Stream<A> instance) {
        return new hydra.mantle.TypeVariant.Stream();
      }
      
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Sum<A> instance) {
        return new hydra.mantle.TypeVariant.Sum();
      }
      
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Union<A> instance) {
        return new hydra.mantle.TypeVariant.Union();
      }
      
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Variable<A> instance) {
        return new hydra.mantle.TypeVariant.Variable();
      }
      
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Wrap<A> instance) {
        return new hydra.mantle.TypeVariant.Wrap();
      }
    });
  }
  
  java.util.List<hydra.mantle.TypeVariant> typeVariants = java.util.Arrays.asList(
    new hydra.mantle.TypeVariant.Annotated(),
    new hydra.mantle.TypeVariant.Application(),
    new hydra.mantle.TypeVariant.Function(),
    new hydra.mantle.TypeVariant.Lambda(),
    new hydra.mantle.TypeVariant.List(),
    new hydra.mantle.TypeVariant.Literal(),
    new hydra.mantle.TypeVariant.Map(),
    new hydra.mantle.TypeVariant.Wrap(),
    new hydra.mantle.TypeVariant.Optional(),
    new hydra.mantle.TypeVariant.Product(),
    new hydra.mantle.TypeVariant.Record(),
    new hydra.mantle.TypeVariant.Set(),
    new hydra.mantle.TypeVariant.Stream(),
    new hydra.mantle.TypeVariant.Sum(),
    new hydra.mantle.TypeVariant.Union(),
    new hydra.mantle.TypeVariant.Variable());
  
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
            return (hydra.basics.Basics.floatEqual((instance.value))).apply((instance.value));
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
            return (hydra.basics.Basics.integerEqual((instance.value))).apply((instance.value));
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
  
  static <A, X> java.util.function.Function<X, X> skipAnnotations(java.util.function.Function<X, java.util.Optional<hydra.core.Annotated<X, A>>> getAnn) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<X, X>> skip = new java.util.concurrent.atomic.AtomicReference<>();
    skip.set((java.util.function.Function<X, X>) (t1 -> ((((getAnn)).apply((t1))).map((java.util.function.Function<hydra.core.Annotated<X, A>, X>) (ann -> (skip.get()).apply(((ann)).subject)))).orElse((t1))));
    return (java.util.function.Function<X, X>) (t -> (skip.get()).apply((t)));
  }
  
  static <A> hydra.core.Term<A> stripTerm(hydra.core.Term<A> x) {
    return (hydra.basics.Basics.skipAnnotations((java.util.function.Function<hydra.core.Term<A>, java.util.Optional<hydra.core.Annotated<hydra.core.Term<A>, A>>>) (v1 -> ((v1)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public java.util.Optional<hydra.core.Annotated<hydra.core.Term<A>, A>> otherwise(hydra.core.Term<A> instance) {
        return java.util.Optional.empty();
      }
      
      @Override
      public java.util.Optional<hydra.core.Annotated<hydra.core.Term<A>, A>> visit(hydra.core.Term.Annotated<A> instance) {
        return java.util.Optional.of((instance.value));
      }
    })))).apply((x));
  }
  
  static <A> hydra.core.Type<A> stripType(hydra.core.Type<A> x) {
    return (hydra.basics.Basics.skipAnnotations((java.util.function.Function<hydra.core.Type<A>, java.util.Optional<hydra.core.Annotated<hydra.core.Type<A>, A>>>) (v1 -> ((v1)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.Optional<hydra.core.Annotated<hydra.core.Type<A>, A>> otherwise(hydra.core.Type<A> instance) {
        return java.util.Optional.empty();
      }
      
      @Override
      public java.util.Optional<hydra.core.Annotated<hydra.core.Type<A>, A>> visit(hydra.core.Type.Annotated<A> instance) {
        return java.util.Optional.of((instance.value));
      }
    })))).apply((x));
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
            return (hydra.basics.Basics.literalEqual((instance.value))).apply((instance.value));
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
  
  static hydra.core.Name unqualifyName(hydra.module.QualifiedName qname) {
    String prefix = ((((qname)).namespace).map((java.util.function.Function<hydra.module.Namespace, String>) (n -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      ((n)).value,
      "."))))).orElse("");
    return new hydra.core.Name(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      (prefix),
      ((qname)).local)));
  }
}