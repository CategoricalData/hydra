package hydra.basics;

import hydra.core.Type;


/**
 * Basic functions for working with types and terms
 */
public interface Basics {
  static <A> hydra.mantle.EliminationVariant eliminationVariant(hydra.core.Elimination<A> v1) {
    return ((v1)).accept(new hydra.core.Elimination.Visitor<hydra.mantle.EliminationVariant>() {
      @Override
      public hydra.mantle.EliminationVariant visit(hydra.core.Elimination.Element instance) {
        return new hydra.mantle.EliminationVariant.Element();
      }

      @Override
      public hydra.mantle.EliminationVariant visit(hydra.core.Elimination.List instance) {
        return new hydra.mantle.EliminationVariant.List();
      }

      @Override
      public hydra.mantle.EliminationVariant visit(hydra.core.Elimination.Optional instance) {
        return new hydra.mantle.EliminationVariant.Optional();
      }

      @Override
      public hydra.mantle.EliminationVariant visit(hydra.core.Elimination.Record instance) {
        return new hydra.mantle.EliminationVariant.Record();
      }

      @Override
      public hydra.mantle.EliminationVariant visit(hydra.core.Elimination.Union instance) {
        return new hydra.mantle.EliminationVariant.Union();
      }

      @Override
      public hydra.mantle.EliminationVariant visit(hydra.core.Elimination.Wrap instance) {
        return new hydra.mantle.EliminationVariant.Wrap();
      }
    });
  }

  java.util.List<hydra.mantle.EliminationVariant> eliminationVariants = java.util.Arrays.asList(
    new hydra.mantle.EliminationVariant.Element(),
    new hydra.mantle.EliminationVariant.List(),
    new hydra.mantle.EliminationVariant.Wrap(),
    new hydra.mantle.EliminationVariant.Optional(),
    new hydra.mantle.EliminationVariant.Record(),
    new hydra.mantle.EliminationVariant.Union());

  static hydra.mantle.Precision floatTypePrecision(hydra.core.FloatType v1) {
    return ((v1)).accept(new hydra.core.FloatType.Visitor<hydra.mantle.Precision>() {
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
    return ((v1)).accept(new hydra.core.FloatValue.Visitor<hydra.core.FloatType>() {
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
    return ((v1)).accept(new hydra.core.Function.Visitor<hydra.mantle.FunctionVariant>() {
      @Override
      public hydra.mantle.FunctionVariant visit(hydra.core.Function.Elimination instance) {
        return new hydra.mantle.FunctionVariant.Elimination();
      }

      @Override
      public hydra.mantle.FunctionVariant visit(hydra.core.Function.Lambda instance) {
        return new hydra.mantle.FunctionVariant.Lambda();
      }

      @Override
      public hydra.mantle.FunctionVariant visit(hydra.core.Function.Primitive instance) {
        return new hydra.mantle.FunctionVariant.Primitive();
      }
    });
  }

  java.util.List<hydra.mantle.FunctionVariant> functionVariants = java.util.Arrays.asList(
    new hydra.mantle.FunctionVariant.Elimination(),
    new hydra.mantle.FunctionVariant.Lambda(),
    new hydra.mantle.FunctionVariant.Primitive());

  static Boolean integerTypeIsSigned(hydra.core.IntegerType v1) {
    return ((v1)).accept(new hydra.core.IntegerType.Visitor<Boolean>() {
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
    return ((v1)).accept(new hydra.core.IntegerType.Visitor<hydra.mantle.Precision>() {
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
    return ((v1)).accept(new hydra.core.IntegerValue.Visitor<hydra.core.IntegerType>() {
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
    return ((v1)).accept(new hydra.core.Literal.Visitor<hydra.core.LiteralType>() {
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
    return ((v1)).accept(new hydra.core.LiteralType.Visitor<hydra.mantle.LiteralVariant>() {
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

  static hydra.mantle.LiteralVariant literalVariant(hydra.core.Literal x1) {
    return hydra.basics.Basics.literalTypeVariant(hydra.basics.Basics.literalType((x1)));
  }

  java.util.List<hydra.mantle.LiteralVariant> literalVariants = java.util.Arrays.asList(
    new hydra.mantle.LiteralVariant.Binary(),
    new hydra.mantle.LiteralVariant.Boolean_(),
    new hydra.mantle.LiteralVariant.Float_(),
    new hydra.mantle.LiteralVariant.Integer_(),
    new hydra.mantle.LiteralVariant.String_());

  static java.util.function.Function<String, hydra.core.Name> qname(hydra.module.Namespace ns) {
    return name -> new hydra.core.Name(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      ((ns)).value,
      ".",
      (name))));
  }

  static <A> hydra.mantle.TermVariant termVariant(hydra.core.Term<A> term) {
    return ((term)).accept(new hydra.core.Term.Visitor<hydra.mantle.TermVariant>() {
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Annotated instance) {
        return new hydra.mantle.TermVariant.Annotated();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Application instance) {
        return new hydra.mantle.TermVariant.Application();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Element instance) {
        return new hydra.mantle.TermVariant.Element();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Function instance) {
        return new hydra.mantle.TermVariant.Function();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Let instance) {
        return new hydra.mantle.TermVariant.Let();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.List instance) {
        return new hydra.mantle.TermVariant.List();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Literal instance) {
        return new hydra.mantle.TermVariant.Literal();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Map instance) {
        return new hydra.mantle.TermVariant.Map();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Optional instance) {
        return new hydra.mantle.TermVariant.Optional();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Product instance) {
        return new hydra.mantle.TermVariant.Product();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Record instance) {
        return new hydra.mantle.TermVariant.Record();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Set instance) {
        return new hydra.mantle.TermVariant.Set();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Stream instance) {
        return new hydra.mantle.TermVariant.Stream();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Sum instance) {
        return new hydra.mantle.TermVariant.Sum();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Union instance) {
        return new hydra.mantle.TermVariant.Union();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Variable instance) {
        return new hydra.mantle.TermVariant.Variable();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Wrap instance) {
        return new hydra.mantle.TermVariant.Wrap();
      }
    });
  }

  java.util.List<hydra.mantle.TermVariant> termVariants = java.util.Arrays.asList(
    new hydra.mantle.TermVariant.Annotated(),
    new hydra.mantle.TermVariant.Application(),
    new hydra.mantle.TermVariant.Literal(),
    new hydra.mantle.TermVariant.Element(),
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

  static <A> Integer testLists(java.util.List<java.util.List<A>> els) {
    return hydra.lib.lists.Length.apply(hydra.lib.lists.Concat.apply((els)));
  }
  
  static <A> Integer typeArity(hydra.core.Type<A> v1) {
    return ((v1)).accept(new Type.PartialVisitor<Integer>() {
      @Override
      public Integer otherwise(hydra.core.Type instance) {
        return 0;
      }

      @Override
      public Integer visit(hydra.core.Type.Annotated instance) {
        return hydra.basics.Basics.typeArity((hydra.core.Type<A>) (((instance.value)).subject));
      }

      @Override
      public Integer visit(hydra.core.Type.Application instance) {
        return hydra.basics.Basics.typeArity((hydra.core.Type<A>) (((instance.value)).function));
      }

      @Override
      public Integer visit(hydra.core.Type.Lambda instance) {
        return hydra.basics.Basics.typeArity((hydra.core.Type<A>) (((instance.value)).body));
      }

      @Override
      public Integer visit(hydra.core.Type.Function instance) {
        return hydra.lib.math.Add.apply(
          1,
          hydra.basics.Basics.typeArity((hydra.core.Type<A>) (((instance.value)).codomain)));
      }
    });
  }

  static <A> hydra.mantle.TypeVariant typeVariant(hydra.core.Type<A> typ) {
    return ((typ)).accept(new hydra.core.Type.Visitor<hydra.mantle.TypeVariant>() {
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Annotated instance) {
        return new hydra.mantle.TypeVariant.Annotated();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Application instance) {
        return new hydra.mantle.TypeVariant.Application();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Element instance) {
        return new hydra.mantle.TypeVariant.Element();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Function instance) {
        return new hydra.mantle.TypeVariant.Function();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Lambda instance) {
        return new hydra.mantle.TypeVariant.Lambda();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.List instance) {
        return new hydra.mantle.TypeVariant.List();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Literal instance) {
        return new hydra.mantle.TypeVariant.Literal();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Map instance) {
        return new hydra.mantle.TypeVariant.Map();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Optional instance) {
        return new hydra.mantle.TypeVariant.Optional();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Product instance) {
        return new hydra.mantle.TypeVariant.Product();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Record instance) {
        return new hydra.mantle.TypeVariant.Record();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Set instance) {
        return new hydra.mantle.TypeVariant.Set();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Stream instance) {
        return new hydra.mantle.TypeVariant.Stream();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Sum instance) {
        return new hydra.mantle.TypeVariant.Sum();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Union instance) {
        return new hydra.mantle.TypeVariant.Union();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Variable instance) {
        return new hydra.mantle.TypeVariant.Variable();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Wrap instance) {
        return new hydra.mantle.TypeVariant.Wrap();
      }
    });
  }

  java.util.List<hydra.mantle.TypeVariant> typeVariants = java.util.Arrays.asList(
    new hydra.mantle.TypeVariant.Annotated(),
    new hydra.mantle.TypeVariant.Application(),
    new hydra.mantle.TypeVariant.Element(),
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
}