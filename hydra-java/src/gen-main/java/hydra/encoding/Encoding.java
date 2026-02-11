// Note: this is an automatically generated file. Do not edit.

package hydra.encoding;

/**
 * Functions for generating term encoders from type modules
 */
public interface Encoding {
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Binding> encodeBinding(hydra.core.Binding b) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Binding>>) (cx -> hydra.lib.flows.Bind.apply(
        hydra.monads.Monads.eitherToFlow(
          wrapped -> (wrapped).value,
          hydra.decode.core.Core.type(
            cx,
            (b).term)),
        (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Binding>>) (typ -> hydra.lib.flows.Pure.apply(new hydra.core.Binding(hydra.encoding.Encoding.encodeBindingName((b).name), hydra.encoding.Encoding.encodeType(typ), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())))))));
  }
  
  static hydra.core.Name encodeBindingName(hydra.core.Name n) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.lists.Tail.apply(hydra.lib.strings.SplitOn.apply(
        ".",
        (n).value)))),
      () -> new hydra.core.Name(hydra.lib.strings.Intercalate.apply(
        ".",
        hydra.lib.lists.Concat2.apply(
          java.util.List.of(
            "hydra",
            "encode"),
          hydra.lib.lists.Concat2.apply(
            hydra.lib.lists.Tail.apply(hydra.lib.lists.Init.apply(hydra.lib.strings.SplitOn.apply(
              ".",
              (n).value))),
            java.util.List.of(hydra.formatting.Formatting.decapitalize(hydra.names.Names.localNameOf(n))))))),
      () -> new hydra.core.Name(hydra.formatting.Formatting.decapitalize(hydra.names.Names.localNameOf(n))));
  }
  
  static hydra.core.Term encodeFieldValue(hydra.core.Name typeName, hydra.core.Name fieldName, hydra.core.Type fieldType) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("union"), hydra.encoding.Encoding.encodeInjection(
      typeName,
      fieldName,
      new hydra.core.Term.Application(new hydra.core.Application(hydra.encoding.Encoding.encodeType(fieldType), new hydra.core.Term.Variable(new hydra.core.Name("y")))))))))));
  }
  
  static hydra.core.Term encodeFloatValue(hydra.core.FloatType floatType, hydra.core.Term valTerm) {
    return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.FloatValue"), new hydra.core.Field((floatType).accept(new hydra.core.FloatType.PartialVisitor<>() {
      @Override
      public hydra.core.Name visit(hydra.core.FloatType.Bigfloat ignored) {
        return new hydra.core.Name("bigfloat");
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.FloatType.Float32 ignored) {
        return new hydra.core.Name("float32");
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.FloatType.Float64 ignored) {
        return new hydra.core.Name("float64");
      }
    }), valTerm)));
  }
  
  static hydra.core.Term encodeInjection(hydra.core.Name typeName, hydra.core.Name fieldName, hydra.core.Term fieldTerm) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Injection"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.encoding.Encoding.encodeName(typeName)),
      new hydra.core.Field(new hydra.core.Name("field"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Field"), java.util.List.of(
        new hydra.core.Field(new hydra.core.Name("name"), hydra.encoding.Encoding.encodeName(fieldName)),
        new hydra.core.Field(new hydra.core.Name("term"), fieldTerm))))))));
  }
  
  static hydra.core.Term encodeIntegerValue(hydra.core.IntegerType intType, hydra.core.Term valTerm) {
    return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field((intType).accept(new hydra.core.IntegerType.PartialVisitor<>() {
      @Override
      public hydra.core.Name visit(hydra.core.IntegerType.Bigint ignored) {
        return new hydra.core.Name("bigint");
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.IntegerType.Int8 ignored) {
        return new hydra.core.Name("int8");
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.IntegerType.Int16 ignored) {
        return new hydra.core.Name("int16");
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.IntegerType.Int32 ignored) {
        return new hydra.core.Name("int32");
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.IntegerType.Int64 ignored) {
        return new hydra.core.Name("int64");
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.IntegerType.Uint8 ignored) {
        return new hydra.core.Name("uint8");
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.IntegerType.Uint16 ignored) {
        return new hydra.core.Name("uint16");
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.IntegerType.Uint32 ignored) {
        return new hydra.core.Name("uint32");
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.IntegerType.Uint64 ignored) {
        return new hydra.core.Name("uint64");
      }
    }), valTerm)));
  }
  
  static hydra.core.Term encodeListType(hydra.core.Type elemType) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("xs"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("list"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.map"))), hydra.encoding.Encoding.encodeType(elemType))), new hydra.core.Term.Variable(new hydra.core.Name("xs"))))))))));
  }
  
  static hydra.core.Term encodeLiteralType(hydra.core.LiteralType v1) {
    return (v1).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.LiteralType instance) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Binary ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("binary"), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Boolean_ ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("boolean"), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.String_ ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("string"), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Integer_ intType) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("integer"), hydra.encoding.Encoding.encodeIntegerValue(
          (intType).value,
          new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Float_ floatType) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("float"), hydra.encoding.Encoding.encodeFloatValue(
          (floatType).value,
          new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))));
      }
    });
  }
  
  static hydra.core.Term encodeEitherType(hydra.core.EitherType et) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("e"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("either"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.bimap"))), hydra.encoding.Encoding.encodeType((et).left))), hydra.encoding.Encoding.encodeType((et).right))), new hydra.core.Term.Variable(new hydra.core.Name("e"))))))))));
  }
  
  static hydra.core.Term encodeForallType(hydra.core.ForallType ft) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(hydra.encoding.Encoding.encodeBindingName((ft).parameter), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), hydra.encoding.Encoding.encodeType((ft).body))));
  }
  
  static hydra.core.Term encodeMapType(hydra.core.MapType mt) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("m"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("map"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maps.bimap"))), hydra.encoding.Encoding.encodeType((mt).keys))), hydra.encoding.Encoding.encodeType((mt).values))), new hydra.core.Term.Variable(new hydra.core.Name("m"))))))))));
  }
  
  static hydra.core.Term encodeOptionalType(hydra.core.Type elemType) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("opt"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("maybe"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maybes.map"))), hydra.encoding.Encoding.encodeType(elemType))), new hydra.core.Term.Variable(new hydra.core.Name("opt"))))))))));
  }
  
  static hydra.core.Term encodePairType(hydra.core.PairType pt) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("p"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("pair"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.bimap"))), hydra.encoding.Encoding.encodeType((pt).first))), hydra.encoding.Encoding.encodeType((pt).second))), new hydra.core.Term.Variable(new hydra.core.Name("p"))))))))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.module.Module>> encodeModule(hydra.module.Module mod) {
    return hydra.lib.flows.Bind.apply(
      hydra.encoding.Encoding.filterTypeBindings((mod).elements),
      (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.module.Module>>>) (typeBindings -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(typeBindings),
        () -> hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.module.Module>) (hydra.util.Maybe.<hydra.module.Module>nothing())),
        () -> hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            hydra.encoding.Encoding::encodeBinding,
            typeBindings),
          (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.module.Module>>>) (encodedBindings -> hydra.lib.flows.Pure.apply(hydra.util.Maybe.just(new hydra.module.Module(hydra.encoding.Encoding.encodeNamespace((mod).namespace), encodedBindings, hydra.lib.lists.Map.apply(
            hydra.encoding.Encoding::encodeNamespace,
            (mod).typeDependencies), java.util.List.of((mod).namespace), hydra.util.Maybe.just(hydra.lib.strings.Cat.apply(java.util.List.of(
            "Term encoders for ",
            ((mod).namespace).value)))))))))));
  }
  
  static hydra.core.Term encodeName(hydra.core.Name n) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((n).value))));
  }
  
  static hydra.module.Namespace encodeNamespace(hydra.module.Namespace ns) {
    return new hydra.module.Namespace(hydra.lib.strings.Cat.apply(java.util.List.of(
      "hydra.encode.",
      hydra.lib.strings.Intercalate.apply(
        ".",
        hydra.lib.lists.Tail.apply(hydra.lib.strings.SplitOn.apply(
          ".",
          (ns).value))))));
  }
  
  static hydra.core.Term encodeRecordType(hydra.core.RowType rt) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("record"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Record"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.encoding.Encoding.encodeName((rt).typeName)),
      new hydra.core.Field(new hydra.core.Name("fields"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.FieldType, hydra.core.Term>) (ft -> new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Field"), java.util.List.of(
          new hydra.core.Field(new hydra.core.Name("name"), hydra.encoding.Encoding.encodeName((ft).name)),
          new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(hydra.encoding.Encoding.encodeType((ft).type), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection((rt).typeName, (ft).name)))), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))),
        (rt).fields))))))))))));
  }
  
  static hydra.core.Term encodeSetType(hydra.core.Type elemType) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("s"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("set"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.sets.map"))), hydra.encoding.Encoding.encodeType(elemType))), new hydra.core.Term.Variable(new hydra.core.Name("s"))))))))));
  }
  
  static hydra.core.Term encodeType(hydra.core.Type v1) {
    return (v1).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Type instance) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Annotated at) {
        return hydra.encoding.Encoding.encodeType(((at).value).body);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Application appType) {
        return new hydra.core.Term.Application(new hydra.core.Application(hydra.encoding.Encoding.encodeType(((appType).value).function), hydra.encoding.Encoding.encodeType(((appType).value).argument)));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Either et) {
        return hydra.encoding.Encoding.encodeEitherType((et).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Forall ft) {
        return hydra.encoding.Encoding.encodeForallType((ft).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Function ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.List elemType) {
        return hydra.encoding.Encoding.encodeListType((elemType).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Literal lt) {
        return hydra.encoding.Encoding.encodeLiteralType((lt).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Map mt) {
        return hydra.encoding.Encoding.encodeMapType((mt).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Maybe elemType) {
        return hydra.encoding.Encoding.encodeOptionalType((elemType).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Pair pt) {
        return hydra.encoding.Encoding.encodePairType((pt).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Record rt) {
        return hydra.encoding.Encoding.encodeRecordType((rt).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Set elemType) {
        return hydra.encoding.Encoding.encodeSetType((elemType).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Union rt) {
        return hydra.encoding.Encoding.encodeUnionType((rt).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Wrap wt) {
        return hydra.encoding.Encoding.encodeWrappedType((wt).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Unit ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("_"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("unit"), new hydra.core.Term.Unit()))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Variable typeName) {
        return new hydra.core.Term.Variable(hydra.encoding.Encoding.encodeBindingName((typeName).value));
      }
    });
  }
  
  static hydra.core.Term encodeUnionType(hydra.core.RowType rt) {
    return new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement((rt).typeName, (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.FieldType, hydra.core.Field>) (ft -> new hydra.core.Field((ft).name, hydra.encoding.Encoding.encodeFieldValue(
        (rt).typeName,
        (ft).name,
        (ft).type))),
      (rt).fields)))));
  }
  
  static hydra.core.Term encodeWrappedType(hydra.core.WrappedType wt) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("wrap"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.WrappedTerm"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.encoding.Encoding.encodeName((wt).typeName)),
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(hydra.encoding.Encoding.encodeType((wt).body), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap((wt).typeName))), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))))))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Binding>> filterTypeBindings(java.util.List<hydra.core.Binding> bindings) {
    return hydra.lib.flows.Map.apply(
      (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.core.Binding>>, java.util.List<hydra.core.Binding>>) (hydra.lib.maybes.Cat::apply),
      hydra.lib.flows.MapList.apply(
        hydra.encoding.Encoding::isEncodableBinding,
        hydra.lib.lists.Filter.apply(
          hydra.annotations.Annotations::isNativeType,
          bindings)));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Binding>> isEncodableBinding(hydra.core.Binding b) {
    return hydra.lib.flows.Map.apply(
      (java.util.function.Function<Boolean, hydra.util.Maybe<hydra.core.Binding>>) (serializable -> hydra.lib.logic.IfElse.lazy(
        serializable,
        () -> hydra.util.Maybe.just(b),
        () -> (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing()))),
      hydra.schemas.Schemas.isSerializableByName((b).name));
  }
  
  static Boolean isUnitType(hydra.core.Type v1) {
    return (v1).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Unit ignored) {
        return true;
      }
    });
  }
}
