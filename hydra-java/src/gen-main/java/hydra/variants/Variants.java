// Note: this is an automatically generated file. Do not edit.

package hydra.variants;

/**
 * Functions for working with term, type, and literal type variants, as well as numeric precision.
 */
public interface Variants {
  static hydra.meta.EliminationVariant eliminationVariant(hydra.core.Elimination v1) {
    return ((v1)).accept(new hydra.core.Elimination.Visitor<>() {
      @Override
      public hydra.meta.EliminationVariant visit(hydra.core.Elimination.Product ignored) {
        return new hydra.meta.EliminationVariant.Product(true);
      }
      
      @Override
      public hydra.meta.EliminationVariant visit(hydra.core.Elimination.Record ignored) {
        return new hydra.meta.EliminationVariant.Record(true);
      }
      
      @Override
      public hydra.meta.EliminationVariant visit(hydra.core.Elimination.Union ignored) {
        return new hydra.meta.EliminationVariant.Union(true);
      }
      
      @Override
      public hydra.meta.EliminationVariant visit(hydra.core.Elimination.Wrap ignored) {
        return new hydra.meta.EliminationVariant.Wrap(true);
      }
    });
  }
  
  java.util.List<hydra.meta.EliminationVariant> eliminationVariants = java.util.List.of(
    new hydra.meta.EliminationVariant.Product(true),
    new hydra.meta.EliminationVariant.Record(true),
    new hydra.meta.EliminationVariant.Union(true),
    new hydra.meta.EliminationVariant.Wrap(true));
  
  static hydra.util.Precision floatTypePrecision(hydra.core.FloatType v1) {
    return ((v1)).accept(new hydra.core.FloatType.Visitor<>() {
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
    return ((v1)).accept(new hydra.core.FloatValue.Visitor<>() {
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
  
  static hydra.meta.FunctionVariant functionVariant(hydra.core.Function v1) {
    return ((v1)).accept(new hydra.core.Function.Visitor<>() {
      @Override
      public hydra.meta.FunctionVariant visit(hydra.core.Function.Elimination ignored) {
        return new hydra.meta.FunctionVariant.Elimination(true);
      }
      
      @Override
      public hydra.meta.FunctionVariant visit(hydra.core.Function.Lambda ignored) {
        return new hydra.meta.FunctionVariant.Lambda(true);
      }
      
      @Override
      public hydra.meta.FunctionVariant visit(hydra.core.Function.Primitive ignored) {
        return new hydra.meta.FunctionVariant.Primitive(true);
      }
    });
  }
  
  java.util.List<hydra.meta.FunctionVariant> functionVariants = java.util.List.of(
    new hydra.meta.FunctionVariant.Elimination(true),
    new hydra.meta.FunctionVariant.Lambda(true),
    new hydra.meta.FunctionVariant.Primitive(true));
  
  static Boolean integerTypeIsSigned(hydra.core.IntegerType v1) {
    return ((v1)).accept(new hydra.core.IntegerType.Visitor<>() {
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
    return ((v1)).accept(new hydra.core.IntegerType.Visitor<>() {
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
    return ((v1)).accept(new hydra.core.IntegerValue.Visitor<>() {
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
    return ((v1)).accept(new hydra.core.Literal.Visitor<>() {
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
        return ((java.util.function.Function<hydra.core.FloatType, hydra.core.LiteralType>) (injected_ -> new hydra.core.LiteralType.Float_((injected_)))).apply(hydra.variants.Variants.floatValueType(((arg_)).value));
      }
      
      @Override
      public hydra.core.LiteralType visit(hydra.core.Literal.Integer_ arg_) {
        return ((java.util.function.Function<hydra.core.IntegerType, hydra.core.LiteralType>) (injected_ -> new hydra.core.LiteralType.Integer_((injected_)))).apply(hydra.variants.Variants.integerValueType(((arg_)).value));
      }
      
      @Override
      public hydra.core.LiteralType visit(hydra.core.Literal.String_ ignored) {
        return new hydra.core.LiteralType.String_(true);
      }
    });
  }
  
  static hydra.meta.LiteralVariant literalTypeVariant(hydra.core.LiteralType v1) {
    return ((v1)).accept(new hydra.core.LiteralType.Visitor<>() {
      @Override
      public hydra.meta.LiteralVariant visit(hydra.core.LiteralType.Binary ignored) {
        return new hydra.meta.LiteralVariant.Binary(true);
      }
      
      @Override
      public hydra.meta.LiteralVariant visit(hydra.core.LiteralType.Boolean_ ignored) {
        return new hydra.meta.LiteralVariant.Boolean_(true);
      }
      
      @Override
      public hydra.meta.LiteralVariant visit(hydra.core.LiteralType.Float_ ignored) {
        return new hydra.meta.LiteralVariant.Float_(true);
      }
      
      @Override
      public hydra.meta.LiteralVariant visit(hydra.core.LiteralType.Integer_ ignored) {
        return new hydra.meta.LiteralVariant.Integer_(true);
      }
      
      @Override
      public hydra.meta.LiteralVariant visit(hydra.core.LiteralType.String_ ignored) {
        return new hydra.meta.LiteralVariant.String_(true);
      }
    });
  }
  
  java.util.List<hydra.core.LiteralType> literalTypes = hydra.lib.lists.Concat.apply(java.util.List.of(
    java.util.List.of(
      new hydra.core.LiteralType.Binary(true),
      new hydra.core.LiteralType.Boolean_(true)),
    hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.FloatType, hydra.core.LiteralType>) (x -> new hydra.core.LiteralType.Float_((x))),
      (hydra.variants.Variants.floatTypes)),
    hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.IntegerType, hydra.core.LiteralType>) (x -> new hydra.core.LiteralType.Integer_((x))),
      (hydra.variants.Variants.integerTypes)),
    java.util.List.of(new hydra.core.LiteralType.String_(true))));
  
  static hydra.meta.LiteralVariant literalVariant(hydra.core.Literal arg_) {
    return hydra.variants.Variants.literalTypeVariant(hydra.variants.Variants.literalType((arg_)));
  }
  
  java.util.List<hydra.meta.LiteralVariant> literalVariants = java.util.List.of(
    new hydra.meta.LiteralVariant.Binary(true),
    new hydra.meta.LiteralVariant.Boolean_(true),
    new hydra.meta.LiteralVariant.Float_(true),
    new hydra.meta.LiteralVariant.Integer_(true),
    new hydra.meta.LiteralVariant.String_(true));
  
  static hydra.meta.TermVariant termVariant(hydra.core.Term v1) {
    return ((v1)).accept(new hydra.core.Term.Visitor<>() {
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Annotated ignored) {
        return new hydra.meta.TermVariant.Annotated(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Application ignored) {
        return new hydra.meta.TermVariant.Application(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Either ignored) {
        return new hydra.meta.TermVariant.Either(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Function ignored) {
        return new hydra.meta.TermVariant.Function(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Let ignored) {
        return new hydra.meta.TermVariant.Let(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.List ignored) {
        return new hydra.meta.TermVariant.List(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Literal ignored) {
        return new hydra.meta.TermVariant.Literal(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Map ignored) {
        return new hydra.meta.TermVariant.Map(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Maybe ignored) {
        return new hydra.meta.TermVariant.Maybe(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Pair ignored) {
        return new hydra.meta.TermVariant.Pair(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Product ignored) {
        return new hydra.meta.TermVariant.Product(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Record ignored) {
        return new hydra.meta.TermVariant.Record(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Set ignored) {
        return new hydra.meta.TermVariant.Set(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Sum ignored) {
        return new hydra.meta.TermVariant.Sum(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.TypeApplication ignored) {
        return new hydra.meta.TermVariant.TypeApplication(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.TypeLambda ignored) {
        return new hydra.meta.TermVariant.TypeLambda(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Union ignored) {
        return new hydra.meta.TermVariant.Union(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Unit ignored) {
        return new hydra.meta.TermVariant.Unit(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Variable ignored) {
        return new hydra.meta.TermVariant.Variable(true);
      }
      
      @Override
      public hydra.meta.TermVariant visit(hydra.core.Term.Wrap ignored) {
        return new hydra.meta.TermVariant.Wrap(true);
      }
    });
  }
  
  java.util.List<hydra.meta.TermVariant> termVariants = java.util.List.of(
    new hydra.meta.TermVariant.Annotated(true),
    new hydra.meta.TermVariant.Application(true),
    new hydra.meta.TermVariant.Either(true),
    new hydra.meta.TermVariant.Function(true),
    new hydra.meta.TermVariant.List(true),
    new hydra.meta.TermVariant.Literal(true),
    new hydra.meta.TermVariant.Map(true),
    new hydra.meta.TermVariant.Maybe(true),
    new hydra.meta.TermVariant.Pair(true),
    new hydra.meta.TermVariant.Product(true),
    new hydra.meta.TermVariant.Record(true),
    new hydra.meta.TermVariant.Set(true),
    new hydra.meta.TermVariant.Sum(true),
    new hydra.meta.TermVariant.TypeLambda(true),
    new hydra.meta.TermVariant.TypeApplication(true),
    new hydra.meta.TermVariant.Union(true),
    new hydra.meta.TermVariant.Unit(true),
    new hydra.meta.TermVariant.Variable(true),
    new hydra.meta.TermVariant.Wrap(true));
  
  static hydra.meta.TypeVariant typeVariant(hydra.core.Type v1) {
    return ((v1)).accept(new hydra.core.Type.Visitor<>() {
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Annotated ignored) {
        return new hydra.meta.TypeVariant.Annotated(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Application ignored) {
        return new hydra.meta.TypeVariant.Application(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Either ignored) {
        return new hydra.meta.TypeVariant.Either(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Function ignored) {
        return new hydra.meta.TypeVariant.Function(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Forall ignored) {
        return new hydra.meta.TypeVariant.Forall(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.List ignored) {
        return new hydra.meta.TypeVariant.List(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Literal ignored) {
        return new hydra.meta.TypeVariant.Literal(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Map ignored) {
        return new hydra.meta.TypeVariant.Map(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Maybe ignored) {
        return new hydra.meta.TypeVariant.Maybe(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Pair ignored) {
        return new hydra.meta.TypeVariant.Pair(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Product ignored) {
        return new hydra.meta.TypeVariant.Product(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Record ignored) {
        return new hydra.meta.TypeVariant.Record(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Set ignored) {
        return new hydra.meta.TypeVariant.Set(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Sum ignored) {
        return new hydra.meta.TypeVariant.Sum(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Union ignored) {
        return new hydra.meta.TypeVariant.Union(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Unit ignored) {
        return new hydra.meta.TypeVariant.Unit(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Variable ignored) {
        return new hydra.meta.TypeVariant.Variable(true);
      }
      
      @Override
      public hydra.meta.TypeVariant visit(hydra.core.Type.Wrap ignored) {
        return new hydra.meta.TypeVariant.Wrap(true);
      }
    });
  }
  
  java.util.List<hydra.meta.TypeVariant> typeVariants = java.util.List.of(
    new hydra.meta.TypeVariant.Annotated(true),
    new hydra.meta.TypeVariant.Application(true),
    new hydra.meta.TypeVariant.Either(true),
    new hydra.meta.TypeVariant.Function(true),
    new hydra.meta.TypeVariant.Forall(true),
    new hydra.meta.TypeVariant.List(true),
    new hydra.meta.TypeVariant.Literal(true),
    new hydra.meta.TypeVariant.Map(true),
    new hydra.meta.TypeVariant.Wrap(true),
    new hydra.meta.TypeVariant.Maybe(true),
    new hydra.meta.TypeVariant.Pair(true),
    new hydra.meta.TypeVariant.Product(true),
    new hydra.meta.TypeVariant.Record(true),
    new hydra.meta.TypeVariant.Set(true),
    new hydra.meta.TypeVariant.Sum(true),
    new hydra.meta.TypeVariant.Union(true),
    new hydra.meta.TypeVariant.Unit(true),
    new hydra.meta.TypeVariant.Variable(true));
}
