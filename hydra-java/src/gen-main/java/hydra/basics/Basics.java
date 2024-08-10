// Note: this is an automatically generated file. Do not edit.

package hydra.basics;

/**
 * A tier-2 module of basic functions for working with types and terms.
 */
public interface Basics {
  static hydra.mantle.EliminationVariant eliminationVariant(hydra.core.Elimination v1) {
    return ((v1)).accept(new hydra.core.Elimination.Visitor<>() {
      @Override
      public hydra.mantle.EliminationVariant visit(hydra.core.Elimination.List instance) {
        return new hydra.mantle.EliminationVariant.List();
      }

      @Override
      public hydra.mantle.EliminationVariant visit(hydra.core.Elimination.Optional instance) {
        return new hydra.mantle.EliminationVariant.Optional();
      }

      @Override
      public hydra.mantle.EliminationVariant visit(hydra.core.Elimination.Product instance) {
        return new hydra.mantle.EliminationVariant.Product();
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

  java.util.List<hydra.mantle.EliminationVariant> eliminationVariants =  java.util.Arrays.asList(
      new hydra.mantle.EliminationVariant.List(),
      new hydra.mantle.EliminationVariant.Wrap(),
      new hydra.mantle.EliminationVariant.Optional(),
      new hydra.mantle.EliminationVariant.Product(),
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

  static hydra.mantle.FunctionVariant functionVariant(hydra.core.Function v1) {
    return ((v1)).accept(new hydra.core.Function.Visitor<>() {
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

  static <A> A id(A x) {
    return (x);
  }

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

  static hydra.mantle.TermVariant termVariant(hydra.core.Term v1) {
    return ((v1)).accept(new hydra.core.Term.Visitor<>() {
      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Annotated instance) {
        return new hydra.mantle.TermVariant.Annotated();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Application instance) {
        return new hydra.mantle.TermVariant.Application();
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
      public hydra.mantle.TermVariant visit(hydra.core.Term.Sum instance) {
        return new hydra.mantle.TermVariant.Sum();
      }

      @Override
      public hydra.mantle.TermVariant visit(hydra.core.Term.Typed instance) {
        return new hydra.mantle.TermVariant.Typed();
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
      new hydra.mantle.TermVariant.Function(),
      new hydra.mantle.TermVariant.List(),
      new hydra.mantle.TermVariant.Map(),
      new hydra.mantle.TermVariant.Optional(),
      new hydra.mantle.TermVariant.Product(),
      new hydra.mantle.TermVariant.Record(),
      new hydra.mantle.TermVariant.Set(),
      new hydra.mantle.TermVariant.Sum(),
      new hydra.mantle.TermVariant.Typed(),
      new hydra.mantle.TermVariant.Union(),
      new hydra.mantle.TermVariant.Variable(),
      new hydra.mantle.TermVariant.Wrap());

  static hydra.mantle.TypeVariant typeVariant(hydra.core.Type v1) {
    return ((v1)).accept(new hydra.core.Type.Visitor<>() {
      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Annotated instance) {
        return new hydra.mantle.TypeVariant.Annotated();
      }

      @Override
      public hydra.mantle.TypeVariant visit(hydra.core.Type.Application instance) {
        return new hydra.mantle.TypeVariant.Application();
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
      new hydra.mantle.TypeVariant.Sum(),
      new hydra.mantle.TypeVariant.Union(),
      new hydra.mantle.TypeVariant.Variable());

  static String capitalize(String v) {
    return hydra.basics.Basics.mapFirstLetter((java.util.function.Function<String, String>) (v1 -> hydra.lib.strings.ToUpper.apply((v1)))).apply(v);
  }

  static String decapitalize(String v) {
    return hydra.basics.Basics.mapFirstLetter((java.util.function.Function<String, String>) (v1 -> hydra.lib.strings.ToLower.apply((v1)))).apply(v);
  }

  static java.util.function.Function<String, String> mapFirstLetter(java.util.function.Function<String, String> mapping) {
    return (java.util.function.Function<String, String>) (s -> {
      java.util.List<Integer> list = hydra.lib.strings.ToList.apply((s));
      String firstLetter = ((mapping)).apply(hydra.lib.strings.FromList.apply(hydra.lib.lists.Pure.apply(hydra.lib.lists.Head.apply((list)))));
      return hydra.lib.logic.IfElse.apply(
        (s),
        hydra.lib.strings.Cat2.apply(
          (firstLetter),
          hydra.lib.strings.FromList.apply(hydra.lib.lists.Tail.apply((list)))),
        hydra.lib.strings.IsEmpty.apply((s)));
    });
  }

  static java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap(java.util.List<hydra.core.Field> fields) {
    java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>> toPair = (java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>) (f -> new hydra.util.Tuple.Tuple2(((f)).name, ((f)).term));
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (toPair),
      (fields)));
  }

  static java.util.Map<hydra.core.Name, hydra.core.Type> fieldTypeMap(java.util.List<hydra.core.FieldType> fields) {
    java.util.function.Function<hydra.core.FieldType, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>> toPair = (java.util.function.Function<hydra.core.FieldType, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>) (f -> new hydra.util.Tuple.Tuple2(((f)).name, ((f)).type));
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (toPair),
      (fields)));
  }

  static Boolean isEncodedType(hydra.core.Term t) {
    return (hydra.strip.Strip.stripTerm((t))).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Term.Application instance) {
        return hydra.basics.Basics.isEncodedType(((instance.value)).function);
      }

      @Override
      public Boolean visit(hydra.core.Term.Union instance) {
        return hydra.lib.equality.EqualString.apply(
          "hydra/core.Type",
          (((instance.value)).typeName).value);
      }
    });
  }

  static Boolean isType(hydra.core.Type t) {
    return (hydra.strip.Strip.stripType((t))).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Type.Application instance) {
        return hydra.basics.Basics.isType(((instance.value)).function);
      }

      @Override
      public Boolean visit(hydra.core.Type.Lambda instance) {
        return hydra.basics.Basics.isType(((instance.value)).body);
      }

      @Override
      public Boolean visit(hydra.core.Type.Union instance) {
        return hydra.lib.equality.EqualString.apply(
          "hydra/core.Type",
          (((instance.value)).typeName).value);
      }
    });
  }

  static Boolean isUnitTerm(hydra.core.Term t) {
    return hydra.lib.equality.EqualTerm.apply(
      hydra.strip.Strip.fullyStripTerm((t)),
      new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())));
  }

  static Boolean isUnitType(hydra.core.Type t) {
    return hydra.lib.equality.EqualType.apply(
      hydra.strip.Strip.stripType((t)),
      new hydra.core.Type.Record(new hydra.core.RowType(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())));
  }

  static java.util.function.Function<hydra.util.Opt<hydra.graph.Graph>, java.util.function.Function<java.util.List<hydra.graph.Element>, hydra.graph.Graph>> elementsToGraph(hydra.graph.Graph parent) {
    return (java.util.function.Function<hydra.util.Opt<hydra.graph.Graph>, java.util.function.Function<java.util.List<hydra.graph.Element>, hydra.graph.Graph>>) (schema -> (java.util.function.Function<java.util.List<hydra.graph.Element>, hydra.graph.Graph>) (elements -> {
      java.util.function.Function<hydra.graph.Element, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.graph.Element>> toPair = (java.util.function.Function<hydra.graph.Element, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.graph.Element>>) (el -> new hydra.util.Tuple.Tuple2(((el)).name, (el)));
      return new hydra.graph.Graph(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (toPair),
        (elements))), ((parent)).environment, ((parent)).types, ((parent)).body, ((parent)).primitives, (schema));
    }));
  }

  static String localNameOfEager(hydra.core.Name x) {
    return (hydra.basics.Basics.qualifyNameEager((x))).local;
  }

  static String localNameOfLazy(hydra.core.Name x) {
    return (hydra.basics.Basics.qualifyNameLazy((x))).local;
  }

  static hydra.util.Opt<hydra.module.Namespace> namespaceOfEager(hydra.core.Name x) {
    return (hydra.basics.Basics.qualifyNameEager((x))).namespace;
  }

  static hydra.util.Opt<hydra.module.Namespace> namespaceOfLazy(hydra.core.Name x) {
    return (hydra.basics.Basics.qualifyNameLazy((x))).namespace;
  }

  static java.util.function.Function<hydra.module.FileExtension, java.util.function.Function<hydra.module.Namespace, String>> namespaceToFilePath(Boolean caps) {
    return (java.util.function.Function<hydra.module.FileExtension, java.util.function.Function<hydra.module.Namespace, String>>) (ext -> (java.util.function.Function<hydra.module.Namespace, String>) (ns -> {
      java.util.List<String> parts = hydra.lib.lists.Map.apply(
          hydra.lib.logic.IfElse.apply(
              (hydra.basics.Basics::capitalize),
              (hydra.basics.Basics::id),
              (caps)),
          hydra.lib.strings.SplitOn.apply(
              "/",
              ((ns)).value));
      return hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              hydra.lib.strings.Intercalate.apply(
                  "/",
                  (parts)),
              ".")),
          ((ext)).value));
    }));
  }

  static hydra.module.QualifiedName qualifyNameEager(hydra.core.Name name) {
    java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
      ".",
      ((name)).value);
    return hydra.lib.logic.IfElse.apply(
      new hydra.module.QualifiedName(hydra.util.Opt.empty(), ((name)).value),
      new hydra.module.QualifiedName(hydra.util.Opt.of(new hydra.module.Namespace(hydra.lib.lists.Head.apply((parts)))), hydra.lib.strings.Intercalate.apply(
        ".",
        hydra.lib.lists.Tail.apply((parts)))),
      hydra.lib.equality.EqualInt32.apply(
        1,
        hydra.lib.lists.Length.apply((parts))));
  }

  static hydra.module.QualifiedName qualifyNameLazy(hydra.core.Name name) {
    java.util.List<String> parts = hydra.lib.lists.Reverse.apply(hydra.lib.strings.SplitOn.apply(
      ".",
      ((name)).value));
    return hydra.lib.logic.IfElse.apply(
      new hydra.module.QualifiedName(hydra.util.Opt.empty(), ((name)).value),
      new hydra.module.QualifiedName(hydra.util.Opt.of(new hydra.module.Namespace(hydra.lib.strings.Intercalate.apply(
        ".",
        hydra.lib.lists.Reverse.apply(hydra.lib.lists.Tail.apply((parts)))))), hydra.lib.lists.Head.apply((parts))),
      hydra.lib.equality.EqualInt32.apply(
        1,
        hydra.lib.lists.Length.apply((parts))));
  }
}