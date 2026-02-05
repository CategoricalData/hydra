// Note: this is an automatically generated file. Do not edit.

package hydra.reflect;

/**
 * Reflection functions for working with term, type, and literal type variants, as well as numeric precision.
 */
public interface Reflect {
  static hydra.variants.EliminationVariant eliminationVariant(hydra.core.Elimination v1) {
    return ((v1)).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public hydra.variants.EliminationVariant visit(hydra.core.Elimination.Record ignored) {
        return new hydra.variants.EliminationVariant.Record();
      }
      
      @Override
      public hydra.variants.EliminationVariant visit(hydra.core.Elimination.Union ignored) {
        return new hydra.variants.EliminationVariant.Union();
      }
      
      @Override
      public hydra.variants.EliminationVariant visit(hydra.core.Elimination.Wrap ignored) {
        return new hydra.variants.EliminationVariant.Wrap();
      }
    });
  }
  
  static java.util.List<hydra.variants.EliminationVariant> eliminationVariants() {
    return java.util.List.of(
      new hydra.variants.EliminationVariant.Record(),
      new hydra.variants.EliminationVariant.Union(),
      new hydra.variants.EliminationVariant.Wrap());
  }
  
  static hydra.util.Precision floatTypePrecision(hydra.core.FloatType v1) {
    return ((v1)).accept(new hydra.core.FloatType.PartialVisitor<>() {
      @Override
      public hydra.util.Precision visit(hydra.core.FloatType.Bigfloat ignored) {
        return new hydra.util.Precision.Arbitrary();
      }
      
      @Override
      public hydra.util.Precision visit(hydra.core.FloatType.Float32 ignored) {
        return new hydra.util.Precision.Bits(32);
      }
      
      @Override
      public hydra.util.Precision visit(hydra.core.FloatType.Float64 ignored) {
        return new hydra.util.Precision.Bits(64);
      }
    });
  }
  
  static java.util.List<hydra.core.FloatType> floatTypes() {
    return java.util.List.of(
      new hydra.core.FloatType.Bigfloat(),
      new hydra.core.FloatType.Float32(),
      new hydra.core.FloatType.Float64());
  }
  
  static hydra.core.FloatType floatValueType(hydra.core.FloatValue v1) {
    return ((v1)).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.core.FloatType visit(hydra.core.FloatValue.Bigfloat ignored) {
        return new hydra.core.FloatType.Bigfloat();
      }
      
      @Override
      public hydra.core.FloatType visit(hydra.core.FloatValue.Float32 ignored) {
        return new hydra.core.FloatType.Float32();
      }
      
      @Override
      public hydra.core.FloatType visit(hydra.core.FloatValue.Float64 ignored) {
        return new hydra.core.FloatType.Float64();
      }
    });
  }
  
  static hydra.variants.FunctionVariant functionVariant(hydra.core.Function v1) {
    return ((v1)).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.variants.FunctionVariant visit(hydra.core.Function.Elimination ignored) {
        return new hydra.variants.FunctionVariant.Elimination();
      }
      
      @Override
      public hydra.variants.FunctionVariant visit(hydra.core.Function.Lambda ignored) {
        return new hydra.variants.FunctionVariant.Lambda();
      }
      
      @Override
      public hydra.variants.FunctionVariant visit(hydra.core.Function.Primitive ignored) {
        return new hydra.variants.FunctionVariant.Primitive();
      }
    });
  }
  
  static java.util.List<hydra.variants.FunctionVariant> functionVariants() {
    return java.util.List.of(
      new hydra.variants.FunctionVariant.Elimination(),
      new hydra.variants.FunctionVariant.Lambda(),
      new hydra.variants.FunctionVariant.Primitive());
  }
  
  static Boolean integerTypeIsSigned(hydra.core.IntegerType v1) {
    return ((v1)).accept(new hydra.core.IntegerType.PartialVisitor<>() {
      @Override
      public Boolean visit(hydra.core.IntegerType.Bigint ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.IntegerType.Int8 ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.IntegerType.Int16 ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.IntegerType.Int32 ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.IntegerType.Int64 ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.IntegerType.Uint8 ignored) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.IntegerType.Uint16 ignored) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.IntegerType.Uint32 ignored) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.IntegerType.Uint64 ignored) {
        return false;
      }
    });
  }
  
  static hydra.util.Precision integerTypePrecision(hydra.core.IntegerType v1) {
    return ((v1)).accept(new hydra.core.IntegerType.PartialVisitor<>() {
      @Override
      public hydra.util.Precision visit(hydra.core.IntegerType.Bigint ignored) {
        return new hydra.util.Precision.Arbitrary();
      }
      
      @Override
      public hydra.util.Precision visit(hydra.core.IntegerType.Int8 ignored) {
        return new hydra.util.Precision.Bits(8);
      }
      
      @Override
      public hydra.util.Precision visit(hydra.core.IntegerType.Int16 ignored) {
        return new hydra.util.Precision.Bits(16);
      }
      
      @Override
      public hydra.util.Precision visit(hydra.core.IntegerType.Int32 ignored) {
        return new hydra.util.Precision.Bits(32);
      }
      
      @Override
      public hydra.util.Precision visit(hydra.core.IntegerType.Int64 ignored) {
        return new hydra.util.Precision.Bits(64);
      }
      
      @Override
      public hydra.util.Precision visit(hydra.core.IntegerType.Uint8 ignored) {
        return new hydra.util.Precision.Bits(8);
      }
      
      @Override
      public hydra.util.Precision visit(hydra.core.IntegerType.Uint16 ignored) {
        return new hydra.util.Precision.Bits(16);
      }
      
      @Override
      public hydra.util.Precision visit(hydra.core.IntegerType.Uint32 ignored) {
        return new hydra.util.Precision.Bits(32);
      }
      
      @Override
      public hydra.util.Precision visit(hydra.core.IntegerType.Uint64 ignored) {
        return new hydra.util.Precision.Bits(64);
      }
    });
  }
  
  static java.util.List<hydra.core.IntegerType> integerTypes() {
    return java.util.List.of(
      new hydra.core.IntegerType.Bigint(),
      new hydra.core.IntegerType.Int8(),
      new hydra.core.IntegerType.Int16(),
      new hydra.core.IntegerType.Int32(),
      new hydra.core.IntegerType.Int64(),
      new hydra.core.IntegerType.Uint8(),
      new hydra.core.IntegerType.Uint16(),
      new hydra.core.IntegerType.Uint32(),
      new hydra.core.IntegerType.Uint64());
  }
  
  static hydra.core.IntegerType integerValueType(hydra.core.IntegerValue v1) {
    return ((v1)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Bigint ignored) {
        return new hydra.core.IntegerType.Bigint();
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Int8 ignored) {
        return new hydra.core.IntegerType.Int8();
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Int16 ignored) {
        return new hydra.core.IntegerType.Int16();
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Int32 ignored) {
        return new hydra.core.IntegerType.Int32();
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Int64 ignored) {
        return new hydra.core.IntegerType.Int64();
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Uint8 ignored) {
        return new hydra.core.IntegerType.Uint8();
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Uint16 ignored) {
        return new hydra.core.IntegerType.Uint16();
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Uint32 ignored) {
        return new hydra.core.IntegerType.Uint32();
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Uint64 ignored) {
        return new hydra.core.IntegerType.Uint64();
      }
    });
  }
  
  static hydra.core.LiteralType literalType(hydra.core.Literal v1) {
    return ((v1)).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.core.LiteralType visit(hydra.core.Literal.Binary ignored) {
        return new hydra.core.LiteralType.Binary();
      }
      
      @Override
      public hydra.core.LiteralType visit(hydra.core.Literal.Boolean_ ignored) {
        return new hydra.core.LiteralType.Boolean_();
      }
      
      @Override
      public hydra.core.LiteralType visit(hydra.core.Literal.Float_ arg_) {
        return new hydra.core.LiteralType.Float_(hydra.reflect.Reflect.floatValueType(((arg_)).value));
      }
      
      @Override
      public hydra.core.LiteralType visit(hydra.core.Literal.Integer_ arg_) {
        return new hydra.core.LiteralType.Integer_(hydra.reflect.Reflect.integerValueType(((arg_)).value));
      }
      
      @Override
      public hydra.core.LiteralType visit(hydra.core.Literal.String_ ignored) {
        return new hydra.core.LiteralType.String_();
      }
    });
  }
  
  static hydra.variants.LiteralVariant literalTypeVariant(hydra.core.LiteralType v1) {
    return ((v1)).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.variants.LiteralVariant visit(hydra.core.LiteralType.Binary ignored) {
        return new hydra.variants.LiteralVariant.Binary();
      }
      
      @Override
      public hydra.variants.LiteralVariant visit(hydra.core.LiteralType.Boolean_ ignored) {
        return new hydra.variants.LiteralVariant.Boolean_();
      }
      
      @Override
      public hydra.variants.LiteralVariant visit(hydra.core.LiteralType.Float_ ignored) {
        return new hydra.variants.LiteralVariant.Float_();
      }
      
      @Override
      public hydra.variants.LiteralVariant visit(hydra.core.LiteralType.Integer_ ignored) {
        return new hydra.variants.LiteralVariant.Integer_();
      }
      
      @Override
      public hydra.variants.LiteralVariant visit(hydra.core.LiteralType.String_ ignored) {
        return new hydra.variants.LiteralVariant.String_();
      }
    });
  }
  
  static java.util.List<hydra.core.LiteralType> literalTypes() {
    return hydra.lib.lists.Concat.apply(java.util.List.of(
      java.util.List.of(
        new hydra.core.LiteralType.Binary(),
        new hydra.core.LiteralType.Boolean_()),
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.FloatType, hydra.core.LiteralType>) (x -> new hydra.core.LiteralType.Float_((x))),
        hydra.reflect.Reflect.floatTypes()),
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.IntegerType, hydra.core.LiteralType>) (x -> new hydra.core.LiteralType.Integer_((x))),
        hydra.reflect.Reflect.integerTypes()),
      java.util.List.of(new hydra.core.LiteralType.String_())));
  }
  
  static hydra.variants.LiteralVariant literalVariant(hydra.core.Literal arg_) {
    return hydra.reflect.Reflect.literalTypeVariant(hydra.reflect.Reflect.literalType((arg_)));
  }
  
  static java.util.List<hydra.variants.LiteralVariant> literalVariants() {
    return java.util.List.of(
      new hydra.variants.LiteralVariant.Binary(),
      new hydra.variants.LiteralVariant.Boolean_(),
      new hydra.variants.LiteralVariant.Float_(),
      new hydra.variants.LiteralVariant.Integer_(),
      new hydra.variants.LiteralVariant.String_());
  }
  
  static hydra.variants.TermVariant termVariant(hydra.core.Term v1) {
    return ((v1)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Annotated ignored) {
        return new hydra.variants.TermVariant.Annotated();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Application ignored) {
        return new hydra.variants.TermVariant.Application();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Either ignored) {
        return new hydra.variants.TermVariant.Either();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Function ignored) {
        return new hydra.variants.TermVariant.Function();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Let ignored) {
        return new hydra.variants.TermVariant.Let();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.List ignored) {
        return new hydra.variants.TermVariant.List();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Literal ignored) {
        return new hydra.variants.TermVariant.Literal();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Map ignored) {
        return new hydra.variants.TermVariant.Map();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Maybe ignored) {
        return new hydra.variants.TermVariant.Maybe();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Pair ignored) {
        return new hydra.variants.TermVariant.Pair();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Record ignored) {
        return new hydra.variants.TermVariant.Record();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Set ignored) {
        return new hydra.variants.TermVariant.Set();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.TypeApplication ignored) {
        return new hydra.variants.TermVariant.TypeApplication();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.TypeLambda ignored) {
        return new hydra.variants.TermVariant.TypeLambda();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Union ignored) {
        return new hydra.variants.TermVariant.Union();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Unit ignored) {
        return new hydra.variants.TermVariant.Unit();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Variable ignored) {
        return new hydra.variants.TermVariant.Variable();
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Wrap ignored) {
        return new hydra.variants.TermVariant.Wrap();
      }
    });
  }
  
  static java.util.List<hydra.variants.TermVariant> termVariants() {
    return java.util.List.of(
      new hydra.variants.TermVariant.Annotated(),
      new hydra.variants.TermVariant.Application(),
      new hydra.variants.TermVariant.Either(),
      new hydra.variants.TermVariant.Function(),
      new hydra.variants.TermVariant.List(),
      new hydra.variants.TermVariant.Literal(),
      new hydra.variants.TermVariant.Map(),
      new hydra.variants.TermVariant.Maybe(),
      new hydra.variants.TermVariant.Pair(),
      new hydra.variants.TermVariant.Record(),
      new hydra.variants.TermVariant.Set(),
      new hydra.variants.TermVariant.TypeLambda(),
      new hydra.variants.TermVariant.TypeApplication(),
      new hydra.variants.TermVariant.Union(),
      new hydra.variants.TermVariant.Unit(),
      new hydra.variants.TermVariant.Variable(),
      new hydra.variants.TermVariant.Wrap());
  }
  
  static hydra.variants.TypeVariant typeVariant(hydra.core.Type v1) {
    return ((v1)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Annotated ignored) {
        return new hydra.variants.TypeVariant.Annotated();
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Application ignored) {
        return new hydra.variants.TypeVariant.Application();
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Either ignored) {
        return new hydra.variants.TypeVariant.Either();
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Function ignored) {
        return new hydra.variants.TypeVariant.Function();
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Forall ignored) {
        return new hydra.variants.TypeVariant.Forall();
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.List ignored) {
        return new hydra.variants.TypeVariant.List();
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Literal ignored) {
        return new hydra.variants.TypeVariant.Literal();
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Map ignored) {
        return new hydra.variants.TypeVariant.Map();
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Maybe ignored) {
        return new hydra.variants.TypeVariant.Maybe();
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Pair ignored) {
        return new hydra.variants.TypeVariant.Pair();
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Record ignored) {
        return new hydra.variants.TypeVariant.Record();
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Set ignored) {
        return new hydra.variants.TypeVariant.Set();
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Union ignored) {
        return new hydra.variants.TypeVariant.Union();
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Unit ignored) {
        return new hydra.variants.TypeVariant.Unit();
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Variable ignored) {
        return new hydra.variants.TypeVariant.Variable();
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Wrap ignored) {
        return new hydra.variants.TypeVariant.Wrap();
      }
    });
  }
  
  static java.util.List<hydra.variants.TypeVariant> typeVariants() {
    return java.util.List.of(
      new hydra.variants.TypeVariant.Annotated(),
      new hydra.variants.TypeVariant.Application(),
      new hydra.variants.TypeVariant.Either(),
      new hydra.variants.TypeVariant.Function(),
      new hydra.variants.TypeVariant.Forall(),
      new hydra.variants.TypeVariant.List(),
      new hydra.variants.TypeVariant.Literal(),
      new hydra.variants.TypeVariant.Map(),
      new hydra.variants.TypeVariant.Wrap(),
      new hydra.variants.TypeVariant.Maybe(),
      new hydra.variants.TypeVariant.Pair(),
      new hydra.variants.TypeVariant.Record(),
      new hydra.variants.TypeVariant.Set(),
      new hydra.variants.TypeVariant.Union(),
      new hydra.variants.TypeVariant.Unit(),
      new hydra.variants.TypeVariant.Variable());
  }
}
