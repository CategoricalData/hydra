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
        return new hydra.variants.EliminationVariant.Record(true);
      }
      
      @Override
      public hydra.variants.EliminationVariant visit(hydra.core.Elimination.Union ignored) {
        return new hydra.variants.EliminationVariant.Union(true);
      }
      
      @Override
      public hydra.variants.EliminationVariant visit(hydra.core.Elimination.Wrap ignored) {
        return new hydra.variants.EliminationVariant.Wrap(true);
      }
    });
  }
  
  java.util.List<hydra.variants.EliminationVariant> eliminationVariants = java.util.List.of(
    new hydra.variants.EliminationVariant.Record(true),
    new hydra.variants.EliminationVariant.Union(true),
    new hydra.variants.EliminationVariant.Wrap(true));
  
  static hydra.util.Precision floatTypePrecision(hydra.core.FloatType v1) {
    return ((v1)).accept(new hydra.core.FloatType.PartialVisitor<>() {
      @Override
      public hydra.util.Precision visit(hydra.core.FloatType.Bigfloat ignored) {
        return new hydra.util.Precision.Arbitrary(true);
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
  
  java.util.List<hydra.core.FloatType> floatTypes = java.util.List.of(
    new hydra.core.FloatType.Bigfloat(true),
    new hydra.core.FloatType.Float32(true),
    new hydra.core.FloatType.Float64(true));
  
  static hydra.core.FloatType floatValueType(hydra.core.FloatValue v1) {
    return ((v1)).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.core.FloatType visit(hydra.core.FloatValue.Bigfloat ignored) {
        return new hydra.core.FloatType.Bigfloat(true);
      }
      
      @Override
      public hydra.core.FloatType visit(hydra.core.FloatValue.Float32 ignored) {
        return new hydra.core.FloatType.Float32(true);
      }
      
      @Override
      public hydra.core.FloatType visit(hydra.core.FloatValue.Float64 ignored) {
        return new hydra.core.FloatType.Float64(true);
      }
    });
  }
  
  static hydra.variants.FunctionVariant functionVariant(hydra.core.Function v1) {
    return ((v1)).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.variants.FunctionVariant visit(hydra.core.Function.Elimination ignored) {
        return new hydra.variants.FunctionVariant.Elimination(true);
      }
      
      @Override
      public hydra.variants.FunctionVariant visit(hydra.core.Function.Lambda ignored) {
        return new hydra.variants.FunctionVariant.Lambda(true);
      }
      
      @Override
      public hydra.variants.FunctionVariant visit(hydra.core.Function.Primitive ignored) {
        return new hydra.variants.FunctionVariant.Primitive(true);
      }
    });
  }
  
  java.util.List<hydra.variants.FunctionVariant> functionVariants = java.util.List.of(
    new hydra.variants.FunctionVariant.Elimination(true),
    new hydra.variants.FunctionVariant.Lambda(true),
    new hydra.variants.FunctionVariant.Primitive(true));
  
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
        return new hydra.util.Precision.Arbitrary(true);
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
  
  java.util.List<hydra.core.IntegerType> integerTypes = java.util.List.of(
    new hydra.core.IntegerType.Bigint(true),
    new hydra.core.IntegerType.Int8(true),
    new hydra.core.IntegerType.Int16(true),
    new hydra.core.IntegerType.Int32(true),
    new hydra.core.IntegerType.Int64(true),
    new hydra.core.IntegerType.Uint8(true),
    new hydra.core.IntegerType.Uint16(true),
    new hydra.core.IntegerType.Uint32(true),
    new hydra.core.IntegerType.Uint64(true));
  
  static hydra.core.IntegerType integerValueType(hydra.core.IntegerValue v1) {
    return ((v1)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Bigint ignored) {
        return new hydra.core.IntegerType.Bigint(true);
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Int8 ignored) {
        return new hydra.core.IntegerType.Int8(true);
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Int16 ignored) {
        return new hydra.core.IntegerType.Int16(true);
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Int32 ignored) {
        return new hydra.core.IntegerType.Int32(true);
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Int64 ignored) {
        return new hydra.core.IntegerType.Int64(true);
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Uint8 ignored) {
        return new hydra.core.IntegerType.Uint8(true);
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Uint16 ignored) {
        return new hydra.core.IntegerType.Uint16(true);
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Uint32 ignored) {
        return new hydra.core.IntegerType.Uint32(true);
      }
      
      @Override
      public hydra.core.IntegerType visit(hydra.core.IntegerValue.Uint64 ignored) {
        return new hydra.core.IntegerType.Uint64(true);
      }
    });
  }
  
  static hydra.core.LiteralType literalType(hydra.core.Literal v1) {
    return ((v1)).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.core.LiteralType visit(hydra.core.Literal.Binary ignored) {
        return new hydra.core.LiteralType.Binary(true);
      }
      
      @Override
      public hydra.core.LiteralType visit(hydra.core.Literal.Boolean_ ignored) {
        return new hydra.core.LiteralType.Boolean_(true);
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
        return new hydra.core.LiteralType.String_(true);
      }
    });
  }
  
  static hydra.variants.LiteralVariant literalTypeVariant(hydra.core.LiteralType v1) {
    return ((v1)).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.variants.LiteralVariant visit(hydra.core.LiteralType.Binary ignored) {
        return new hydra.variants.LiteralVariant.Binary(true);
      }
      
      @Override
      public hydra.variants.LiteralVariant visit(hydra.core.LiteralType.Boolean_ ignored) {
        return new hydra.variants.LiteralVariant.Boolean_(true);
      }
      
      @Override
      public hydra.variants.LiteralVariant visit(hydra.core.LiteralType.Float_ ignored) {
        return new hydra.variants.LiteralVariant.Float_(true);
      }
      
      @Override
      public hydra.variants.LiteralVariant visit(hydra.core.LiteralType.Integer_ ignored) {
        return new hydra.variants.LiteralVariant.Integer_(true);
      }
      
      @Override
      public hydra.variants.LiteralVariant visit(hydra.core.LiteralType.String_ ignored) {
        return new hydra.variants.LiteralVariant.String_(true);
      }
    });
  }
  
  java.util.List<hydra.core.LiteralType> literalTypes = hydra.lib.lists.Concat.apply(java.util.List.of(
    java.util.List.of(
      new hydra.core.LiteralType.Binary(true),
      new hydra.core.LiteralType.Boolean_(true)),
    hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.FloatType, hydra.core.LiteralType>) (x -> new hydra.core.LiteralType.Float_((x))),
      (hydra.reflect.Reflect.floatTypes)),
    hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.IntegerType, hydra.core.LiteralType>) (x -> new hydra.core.LiteralType.Integer_((x))),
      (hydra.reflect.Reflect.integerTypes)),
    java.util.List.of(new hydra.core.LiteralType.String_(true))));
  
  static hydra.variants.LiteralVariant literalVariant(hydra.core.Literal arg_) {
    return hydra.reflect.Reflect.literalTypeVariant(hydra.reflect.Reflect.literalType((arg_)));
  }
  
  java.util.List<hydra.variants.LiteralVariant> literalVariants = java.util.List.of(
    new hydra.variants.LiteralVariant.Binary(true),
    new hydra.variants.LiteralVariant.Boolean_(true),
    new hydra.variants.LiteralVariant.Float_(true),
    new hydra.variants.LiteralVariant.Integer_(true),
    new hydra.variants.LiteralVariant.String_(true));
  
  static hydra.variants.TermVariant termVariant(hydra.core.Term v1) {
    return ((v1)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Annotated ignored) {
        return new hydra.variants.TermVariant.Annotated(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Application ignored) {
        return new hydra.variants.TermVariant.Application(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Either ignored) {
        return new hydra.variants.TermVariant.Either(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Function ignored) {
        return new hydra.variants.TermVariant.Function(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Let ignored) {
        return new hydra.variants.TermVariant.Let(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.List ignored) {
        return new hydra.variants.TermVariant.List(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Literal ignored) {
        return new hydra.variants.TermVariant.Literal(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Map ignored) {
        return new hydra.variants.TermVariant.Map(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Maybe ignored) {
        return new hydra.variants.TermVariant.Maybe(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Pair ignored) {
        return new hydra.variants.TermVariant.Pair(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Record ignored) {
        return new hydra.variants.TermVariant.Record(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Set ignored) {
        return new hydra.variants.TermVariant.Set(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.TypeApplication ignored) {
        return new hydra.variants.TermVariant.TypeApplication(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.TypeLambda ignored) {
        return new hydra.variants.TermVariant.TypeLambda(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Union ignored) {
        return new hydra.variants.TermVariant.Union(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Unit ignored) {
        return new hydra.variants.TermVariant.Unit(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Variable ignored) {
        return new hydra.variants.TermVariant.Variable(true);
      }
      
      @Override
      public hydra.variants.TermVariant visit(hydra.core.Term.Wrap ignored) {
        return new hydra.variants.TermVariant.Wrap(true);
      }
    });
  }
  
  java.util.List<hydra.variants.TermVariant> termVariants = java.util.List.of(
    new hydra.variants.TermVariant.Annotated(true),
    new hydra.variants.TermVariant.Application(true),
    new hydra.variants.TermVariant.Either(true),
    new hydra.variants.TermVariant.Function(true),
    new hydra.variants.TermVariant.List(true),
    new hydra.variants.TermVariant.Literal(true),
    new hydra.variants.TermVariant.Map(true),
    new hydra.variants.TermVariant.Maybe(true),
    new hydra.variants.TermVariant.Pair(true),
    new hydra.variants.TermVariant.Record(true),
    new hydra.variants.TermVariant.Set(true),
    new hydra.variants.TermVariant.TypeLambda(true),
    new hydra.variants.TermVariant.TypeApplication(true),
    new hydra.variants.TermVariant.Union(true),
    new hydra.variants.TermVariant.Unit(true),
    new hydra.variants.TermVariant.Variable(true),
    new hydra.variants.TermVariant.Wrap(true));
  
  static hydra.variants.TypeVariant typeVariant(hydra.core.Type v1) {
    return ((v1)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Annotated ignored) {
        return new hydra.variants.TypeVariant.Annotated(true);
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Application ignored) {
        return new hydra.variants.TypeVariant.Application(true);
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Either ignored) {
        return new hydra.variants.TypeVariant.Either(true);
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Function ignored) {
        return new hydra.variants.TypeVariant.Function(true);
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Forall ignored) {
        return new hydra.variants.TypeVariant.Forall(true);
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.List ignored) {
        return new hydra.variants.TypeVariant.List(true);
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Literal ignored) {
        return new hydra.variants.TypeVariant.Literal(true);
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Map ignored) {
        return new hydra.variants.TypeVariant.Map(true);
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Maybe ignored) {
        return new hydra.variants.TypeVariant.Maybe(true);
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Pair ignored) {
        return new hydra.variants.TypeVariant.Pair(true);
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Record ignored) {
        return new hydra.variants.TypeVariant.Record(true);
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Set ignored) {
        return new hydra.variants.TypeVariant.Set(true);
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Union ignored) {
        return new hydra.variants.TypeVariant.Union(true);
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Unit ignored) {
        return new hydra.variants.TypeVariant.Unit(true);
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Variable ignored) {
        return new hydra.variants.TypeVariant.Variable(true);
      }
      
      @Override
      public hydra.variants.TypeVariant visit(hydra.core.Type.Wrap ignored) {
        return new hydra.variants.TypeVariant.Wrap(true);
      }
    });
  }
  
  java.util.List<hydra.variants.TypeVariant> typeVariants = java.util.List.of(
    new hydra.variants.TypeVariant.Annotated(true),
    new hydra.variants.TypeVariant.Application(true),
    new hydra.variants.TypeVariant.Either(true),
    new hydra.variants.TypeVariant.Function(true),
    new hydra.variants.TypeVariant.Forall(true),
    new hydra.variants.TypeVariant.List(true),
    new hydra.variants.TypeVariant.Literal(true),
    new hydra.variants.TypeVariant.Map(true),
    new hydra.variants.TypeVariant.Wrap(true),
    new hydra.variants.TypeVariant.Maybe(true),
    new hydra.variants.TypeVariant.Pair(true),
    new hydra.variants.TypeVariant.Record(true),
    new hydra.variants.TypeVariant.Set(true),
    new hydra.variants.TypeVariant.Union(true),
    new hydra.variants.TypeVariant.Unit(true),
    new hydra.variants.TypeVariant.Variable(true));
}
