// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Functions for generating term encoders from type modules
 */
public interface Encoding {
  static hydra.util.Either<hydra.context.InContext<hydra.errors.DecodingError>, hydra.core.Binding> encodeBinding(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Binding b) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.errors.DecodingError, hydra.context.InContext<hydra.errors.DecodingError>>) (_wc_e -> (hydra.context.InContext<hydra.errors.DecodingError>) (new hydra.context.InContext<hydra.errors.DecodingError>(_wc_e, cx))),
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_wc_a -> _wc_a),
        hydra.decode.Core.type(
          graph,
          (b).term)),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.DecodingError>, hydra.core.Binding>>) (typ -> hydra.util.Either.<hydra.context.InContext<hydra.errors.DecodingError>, hydra.core.Binding>right(new hydra.core.Binding(hydra.Encoding.encodeBindingName((b).name), hydra.Encoding.encodeTypeNamed(
        (b).name,
        typ), hydra.util.Maybe.just(hydra.Encoding.encoderTypeSchemeNamed(
        (b).name,
        typ))))));
  }

  static hydra.core.Name encodeBindingName(hydra.core.Name n) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.lists.Tail.apply(hydra.lib.strings.SplitOn.apply(
        ".",
        (n).value)))),
      () -> new hydra.core.Name(hydra.lib.strings.Intercalate.apply(
        ".",
        hydra.lib.lists.Concat2.apply(
          hydra.util.ConsList.of(
            "hydra",
            "encode"),
          hydra.lib.lists.Concat2.apply(
            hydra.lib.lists.Tail.apply(hydra.lib.lists.Init.apply(hydra.lib.strings.SplitOn.apply(
              ".",
              (n).value))),
            hydra.util.ConsList.of(hydra.Formatting.decapitalize(hydra.Names.localNameOf(n))))))),
      () -> new hydra.core.Name(hydra.Formatting.decapitalize(hydra.Names.localNameOf(n))));
  }

  static hydra.core.Term encodeEitherType(hydra.core.EitherType et) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("e"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("either"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.bimap"))), hydra.Encoding.encodeType((et).left))), hydra.Encoding.encodeType((et).right))), new hydra.core.Term.Variable(new hydra.core.Name("e"))))))))));
  }

  static hydra.core.Term encodeFieldValue(hydra.core.Name typeName, hydra.core.Name fieldName, hydra.core.Type fieldType) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("union"), hydra.Encoding.encodeInjection(
      typeName,
      fieldName,
      new hydra.core.Term.Application(new hydra.core.Application(hydra.Encoding.encodeType(fieldType), new hydra.core.Term.Variable(new hydra.core.Name("y")))))))))));
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

  static hydra.core.Term encodeForallType(hydra.core.ForallType ft) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(hydra.Encoding.encodeBindingName((ft).parameter), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), hydra.Encoding.encodeType((ft).body))));
  }

  static hydra.core.Term encodeInjection(hydra.core.Name typeName, hydra.core.Name fieldName, hydra.core.Term fieldTerm) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Injection"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.Encoding.encodeName(typeName)),
      new hydra.core.Field(new hydra.core.Name("field"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Field"), hydra.util.ConsList.of(
        new hydra.core.Field(new hydra.core.Name("name"), hydra.Encoding.encodeName(fieldName)),
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
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("xs"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("list"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.map"))), hydra.Encoding.encodeType(elemType))), new hydra.core.Term.Variable(new hydra.core.Name("xs"))))))))));
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
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("integer"), hydra.Encoding.encodeIntegerValue(
          (intType).value,
          new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Float_ floatType) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("float"), hydra.Encoding.encodeFloatValue(
          (floatType).value,
          new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))));
      }
    });
  }

  static hydra.core.Term encodeMapType(hydra.core.MapType mt) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("m"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("map"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maps.bimap"))), hydra.Encoding.encodeType((mt).keys))), hydra.Encoding.encodeType((mt).values))), new hydra.core.Term.Variable(new hydra.core.Name("m"))))))))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.module.Module>> encodeModule(hydra.context.Context cx, hydra.graph.Graph graph, hydra.module.Module mod) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Encoding.filterTypeBindings(
        cx,
        graph,
        hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.core.Binding>>) (d -> (d).accept(new hydra.module.Definition.PartialVisitor<>() {
            @Override
            public hydra.util.Maybe<hydra.core.Binding> otherwise(hydra.module.Definition instance) {
              return (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing());
            }

            @Override
            public hydra.util.Maybe<hydra.core.Binding> visit(hydra.module.Definition.Type td) {
              return hydra.util.Maybe.just(hydra.Annotations.typeElement(
                (td).value.name,
                (td).value.type));
            }
          })),
          (mod).definitions))),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Binding>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.module.Module>>>) (typeBindings -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(typeBindings),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.module.Module>>right((hydra.util.Maybe<hydra.module.Module>) (hydra.util.Maybe.<hydra.module.Module>nothing())),
        () -> hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Binding>>) (b -> hydra.lib.eithers.Bimap.apply(
              (java.util.function.Function<hydra.context.InContext<hydra.errors.DecodingError>, hydra.context.InContext<hydra.errors.Error_>>) (ic -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(((java.util.function.Function<hydra.context.InContext<hydra.errors.DecodingError>, hydra.errors.DecodingError>) (projected -> projected.object)).apply(ic).value)), ((java.util.function.Function<hydra.context.InContext<hydra.errors.DecodingError>, hydra.context.Context>) (projected -> projected.context)).apply(ic)))),
              (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (x -> x),
              hydra.Encoding.encodeBinding(
                cx,
                graph,
                b))),
            typeBindings),
          (java.util.function.Function<hydra.util.ConsList<hydra.core.Binding>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.module.Module>>>) (encodedBindings -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.module.Module>>right(hydra.util.Maybe.just(new hydra.module.Module(hydra.Encoding.encodeNamespace((mod).namespace), hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Binding, hydra.module.Definition>) (b -> new hydra.module.Definition.Term(new hydra.module.TermDefinition((b).name, (b).term, (b).type))),
            encodedBindings), hydra.lib.lists.Nub.apply(hydra.lib.lists.Concat2.apply(
            hydra.lib.lists.Map.apply(
              hydra.Encoding::encodeNamespace,
              (mod).typeDependencies),
            hydra.lib.lists.Map.apply(
              hydra.Encoding::encodeNamespace,
              (mod).termDependencies))), hydra.util.ConsList.of((mod).namespace), hydra.util.Maybe.just(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
            "Term encoders for ",
            (mod).namespace.value)))))))))));
  }

  static hydra.core.Term encodeName(hydra.core.Name n) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((n).value))));
  }

  static hydra.module.Namespace encodeNamespace(hydra.module.Namespace ns) {
    return new hydra.module.Namespace(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "hydra.encode.",
      hydra.lib.strings.Intercalate.apply(
        ".",
        hydra.lib.lists.Tail.apply(hydra.lib.strings.SplitOn.apply(
          ".",
          (ns).value))))));
  }

  static hydra.core.Term encodeOptionalType(hydra.core.Type elemType) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("opt"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("maybe"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maybes.map"))), hydra.Encoding.encodeType(elemType))), new hydra.core.Term.Variable(new hydra.core.Name("opt"))))))))));
  }

  static hydra.core.Term encodePairType(hydra.core.PairType pt) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("p"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("pair"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.bimap"))), hydra.Encoding.encodeType((pt).first))), hydra.Encoding.encodeType((pt).second))), new hydra.core.Term.Variable(new hydra.core.Name("p"))))))))));
  }

  static hydra.core.Term encodeRecordType(hydra.util.ConsList<hydra.core.FieldType> rt) {
    return hydra.Encoding.encodeRecordTypeNamed(
      new hydra.core.Name("unknown"),
      rt);
  }

  static hydra.core.Term encodeRecordTypeNamed(hydra.core.Name ename, hydra.util.ConsList<hydra.core.FieldType> rt) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("record"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Record"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.Encoding.encodeName(ename)),
      new hydra.core.Field(new hydra.core.Name("fields"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.FieldType, hydra.core.Term>) (ft -> new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Field"), hydra.util.ConsList.of(
          new hydra.core.Field(new hydra.core.Name("name"), hydra.Encoding.encodeName((ft).name)),
          new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(hydra.Encoding.encodeType((ft).type), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(ename, (ft).name)))), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))),
        rt))))))))))));
  }

  static hydra.core.Term encodeSetType(hydra.core.Type elemType) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("s"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("set"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.sets.map"))), hydra.Encoding.encodeType(elemType))), new hydra.core.Term.Variable(new hydra.core.Name("s"))))))))));
  }

  static hydra.core.Term encodeType(hydra.core.Type v1) {
    return (v1).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Type instance) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Annotated at) {
        return hydra.Encoding.encodeType((at).value.body);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Application appType) {
        return new hydra.core.Term.Application(new hydra.core.Application(hydra.Encoding.encodeType((appType).value.function), hydra.Encoding.encodeType((appType).value.argument)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Either et) {
        return hydra.Encoding.encodeEitherType((et).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Forall ft) {
        return hydra.Encoding.encodeForallType((ft).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Function ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.List elemType) {
        return hydra.Encoding.encodeListType((elemType).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Literal lt) {
        return hydra.Encoding.encodeLiteralType((lt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Map mt) {
        return hydra.Encoding.encodeMapType((mt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Maybe elemType) {
        return hydra.Encoding.encodeOptionalType((elemType).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Pair pt) {
        return hydra.Encoding.encodePairType((pt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Record rt) {
        return hydra.Encoding.encodeRecordType((rt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Set elemType) {
        return hydra.Encoding.encodeSetType((elemType).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Union rt) {
        return hydra.Encoding.encodeUnionType((rt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Wrap wt) {
        return hydra.Encoding.encodeWrappedType((wt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Unit ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("_"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("unit"), new hydra.core.Term.Unit()))))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Void_ ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("_"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("unit"), new hydra.core.Term.Unit()))))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Variable typeName) {
        return new hydra.core.Term.Variable(hydra.Encoding.encodeBindingName((typeName).value));
      }
    });
  }

  static hydra.core.Term encodeTypeNamed(hydra.core.Name ename, hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Type instance) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Annotated at) {
        return hydra.Encoding.encodeTypeNamed(
          ename,
          (at).value.body);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Application appType) {
        return new hydra.core.Term.Application(new hydra.core.Application(hydra.Encoding.encodeType((appType).value.function), hydra.Encoding.encodeType((appType).value.argument)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Either et) {
        return hydra.Encoding.encodeEitherType((et).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Forall ft) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(hydra.Encoding.encodeBindingName((ft).value.parameter), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), hydra.Encoding.encodeTypeNamed(
          ename,
          (ft).value.body))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Function ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.List elemType) {
        return hydra.Encoding.encodeListType((elemType).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Literal lt) {
        return hydra.Encoding.encodeLiteralType((lt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Map mt) {
        return hydra.Encoding.encodeMapType((mt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Maybe elemType) {
        return hydra.Encoding.encodeOptionalType((elemType).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Pair pt) {
        return hydra.Encoding.encodePairType((pt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Record rt) {
        return hydra.Encoding.encodeRecordTypeNamed(
          ename,
          (rt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Set elemType) {
        return hydra.Encoding.encodeSetType((elemType).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Union rt) {
        return hydra.Encoding.encodeUnionTypeNamed(
          ename,
          (rt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Wrap wt) {
        return hydra.Encoding.encodeWrappedTypeNamed(
          ename,
          (wt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Unit ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("_"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("unit"), new hydra.core.Term.Unit()))))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Void_ ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("_"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("unit"), new hydra.core.Term.Unit()))))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Variable typeName) {
        return new hydra.core.Term.Variable(hydra.Encoding.encodeBindingName((typeName).value));
      }
    });
  }

  static hydra.core.Term encodeUnionType(hydra.util.ConsList<hydra.core.FieldType> rt) {
    return hydra.Encoding.encodeUnionTypeNamed(
      new hydra.core.Name("unknown"),
      rt);
  }

  static hydra.core.Term encodeUnionTypeNamed(hydra.core.Name ename, hydra.util.ConsList<hydra.core.FieldType> rt) {
    return new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(ename, (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()), hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.FieldType, hydra.core.Field>) (ft -> new hydra.core.Field((ft).name, hydra.Encoding.encodeFieldValue(
        ename,
        (ft).name,
        (ft).type))),
      rt)))));
  }

  static hydra.core.Term encodeWrappedType(hydra.core.Type wt) {
    return hydra.Encoding.encodeWrappedTypeNamed(
      new hydra.core.Name("unknown"),
      wt);
  }

  static hydra.core.Term encodeWrappedTypeNamed(hydra.core.Name ename, hydra.core.Type wt) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("wrap"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.WrappedTerm"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.Encoding.encodeName(ename)),
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(hydra.Encoding.encodeType(wt), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(ename))), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))))))));
  }

  static hydra.util.ConsList<hydra.core.Name> encoderCollectForallVariables(hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty());
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Annotated at) {
        return hydra.Encoding.encoderCollectForallVariables((at).value.body);
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.lib.lists.Cons.apply(
          (ft).value.parameter,
          hydra.Encoding.encoderCollectForallVariables((ft).value.body));
      }
    });
  }

  static hydra.util.ConsList<hydra.core.Name> encoderCollectOrdVars(hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty());
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Annotated at) {
        return hydra.Encoding.encoderCollectOrdVars((at).value.body);
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Application appType) {
        return hydra.lib.lists.Concat2.apply(
          hydra.Encoding.encoderCollectOrdVars((appType).value.function),
          hydra.Encoding.encoderCollectOrdVars((appType).value.argument));
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Either et) {
        return hydra.lib.lists.Concat2.apply(
          hydra.Encoding.encoderCollectOrdVars((et).value.left),
          hydra.Encoding.encoderCollectOrdVars((et).value.right));
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.Encoding.encoderCollectOrdVars((ft).value.body);
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.List elemType) {
        return hydra.Encoding.encoderCollectOrdVars((elemType).value);
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Map mt) {
        return hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.Encoding.encoderCollectTypeVarsFromType((mt).value.keys),
          hydra.Encoding.encoderCollectOrdVars((mt).value.keys),
          hydra.Encoding.encoderCollectOrdVars((mt).value.values)));
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Maybe elemType) {
        return hydra.Encoding.encoderCollectOrdVars((elemType).value);
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Pair pt) {
        return hydra.lib.lists.Concat2.apply(
          hydra.Encoding.encoderCollectOrdVars((pt).value.first),
          hydra.Encoding.encoderCollectOrdVars((pt).value.second));
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Record rt) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.FieldType, hydra.util.ConsList<hydra.core.Name>>) (ft -> hydra.Encoding.encoderCollectOrdVars((ft).type)),
          (rt).value));
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Set elemType) {
        return hydra.lib.lists.Concat2.apply(
          hydra.Encoding.encoderCollectTypeVarsFromType((elemType).value),
          hydra.Encoding.encoderCollectOrdVars((elemType).value));
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Union rt) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.FieldType, hydra.util.ConsList<hydra.core.Name>>) (ft -> hydra.Encoding.encoderCollectOrdVars((ft).type)),
          (rt).value));
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Wrap wt) {
        return hydra.Encoding.encoderCollectOrdVars((wt).value);
      }
    });
  }

  static hydra.util.ConsList<hydra.core.Name> encoderCollectTypeVarsFromType(hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty());
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Annotated at) {
        return hydra.Encoding.encoderCollectTypeVarsFromType((at).value.body);
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Application appType) {
        return hydra.lib.lists.Concat2.apply(
          hydra.Encoding.encoderCollectTypeVarsFromType((appType).value.function),
          hydra.Encoding.encoderCollectTypeVarsFromType((appType).value.argument));
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.Encoding.encoderCollectTypeVarsFromType((ft).value.body);
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.List elemType) {
        return hydra.Encoding.encoderCollectTypeVarsFromType((elemType).value);
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Map mt) {
        return hydra.lib.lists.Concat2.apply(
          hydra.Encoding.encoderCollectTypeVarsFromType((mt).value.keys),
          hydra.Encoding.encoderCollectTypeVarsFromType((mt).value.values));
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Maybe elemType) {
        return hydra.Encoding.encoderCollectTypeVarsFromType((elemType).value);
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Pair pt) {
        return hydra.lib.lists.Concat2.apply(
          hydra.Encoding.encoderCollectTypeVarsFromType((pt).value.first),
          hydra.Encoding.encoderCollectTypeVarsFromType((pt).value.second));
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Record rt) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.FieldType, hydra.util.ConsList<hydra.core.Name>>) (ft -> hydra.Encoding.encoderCollectTypeVarsFromType((ft).type)),
          (rt).value));
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Set elemType) {
        return hydra.Encoding.encoderCollectTypeVarsFromType((elemType).value);
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Union rt) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.FieldType, hydra.util.ConsList<hydra.core.Name>>) (ft -> hydra.Encoding.encoderCollectTypeVarsFromType((ft).type)),
          (rt).value));
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Variable name) {
        return hydra.util.ConsList.of((name).value);
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Wrap wt) {
        return hydra.Encoding.encoderCollectTypeVarsFromType((wt).value);
      }
    });
  }

  static hydra.core.Type encoderFullResultType(hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return hydra.Encoding.encoderFullResultType((at).value.body);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Application appType) {
        return new hydra.core.Type.Application(new hydra.core.ApplicationType(hydra.Encoding.encoderFullResultType((appType).value.function), (appType).value.argument));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Either et) {
        return new hydra.core.Type.Either(new hydra.core.EitherType(hydra.Encoding.encoderFullResultType((et).value.left), hydra.Encoding.encoderFullResultType((et).value.right)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return new hydra.core.Type.Application(new hydra.core.ApplicationType(hydra.Encoding.encoderFullResultType((ft).value.body), new hydra.core.Type.Variable((ft).value.parameter)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.List elemType) {
        return new hydra.core.Type.List(hydra.Encoding.encoderFullResultType((elemType).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Literal ignored) {
        return new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Literal"));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Map mt) {
        return new hydra.core.Type.Map(new hydra.core.MapType(hydra.Encoding.encoderFullResultType((mt).value.keys), hydra.Encoding.encoderFullResultType((mt).value.values)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Maybe elemType) {
        return new hydra.core.Type.Maybe(hydra.Encoding.encoderFullResultType((elemType).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Pair pt) {
        return new hydra.core.Type.Pair(new hydra.core.PairType(hydra.Encoding.encoderFullResultType((pt).value.first), hydra.Encoding.encoderFullResultType((pt).value.second)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Record ignored) {
        return new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Set elemType) {
        return new hydra.core.Type.Set(hydra.Encoding.encoderFullResultType((elemType).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Union ignored) {
        return new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Unit ignored) {
        return new hydra.core.Type.Unit();
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable name) {
        return new hydra.core.Type.Variable((name).value);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Void_ ignored) {
        return new hydra.core.Type.Void_();
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Wrap ignored) {
        return new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"));
      }
    });
  }

  static hydra.core.Type encoderFullResultTypeNamed(hydra.core.Name ename, hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return hydra.Encoding.encoderFullResultTypeNamed(
          ename,
          (at).value.body);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Application appType) {
        return new hydra.core.Type.Application(new hydra.core.ApplicationType(hydra.Encoding.encoderFullResultType((appType).value.function), (appType).value.argument));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Either et) {
        return new hydra.core.Type.Either(new hydra.core.EitherType(hydra.Encoding.encoderFullResultType((et).value.left), hydra.Encoding.encoderFullResultType((et).value.right)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return new hydra.core.Type.Application(new hydra.core.ApplicationType(hydra.Encoding.encoderFullResultTypeNamed(
          ename,
          (ft).value.body), new hydra.core.Type.Variable((ft).value.parameter)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.List elemType) {
        return new hydra.core.Type.List(hydra.Encoding.encoderFullResultType((elemType).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Literal ignored) {
        return new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Literal"));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Map mt) {
        return new hydra.core.Type.Map(new hydra.core.MapType(hydra.Encoding.encoderFullResultType((mt).value.keys), hydra.Encoding.encoderFullResultType((mt).value.values)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Maybe elemType) {
        return new hydra.core.Type.Maybe(hydra.Encoding.encoderFullResultType((elemType).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Pair pt) {
        return new hydra.core.Type.Pair(new hydra.core.PairType(hydra.Encoding.encoderFullResultType((pt).value.first), hydra.Encoding.encoderFullResultType((pt).value.second)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Record ignored) {
        return new hydra.core.Type.Variable(ename);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Set elemType) {
        return new hydra.core.Type.Set(hydra.Encoding.encoderFullResultType((elemType).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Union ignored) {
        return new hydra.core.Type.Variable(ename);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Unit ignored) {
        return new hydra.core.Type.Unit();
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable name) {
        return new hydra.core.Type.Variable((name).value);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Void_ ignored) {
        return new hydra.core.Type.Void_();
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Wrap ignored) {
        return new hydra.core.Type.Variable(ename);
      }
    });
  }

  static hydra.core.Type encoderType(hydra.core.Type typ) {
    hydra.core.Type resultType = hydra.Encoding.encoderFullResultType(typ);
    hydra.core.Type baseType = new hydra.core.Type.Function(new hydra.core.FunctionType(resultType, new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"))));
    return hydra.Encoding.prependForallEncoders(
      baseType,
      typ);
  }

  static hydra.core.Type encoderTypeNamed(hydra.core.Name ename, hydra.core.Type typ) {
    hydra.core.Type resultType = hydra.Encoding.encoderFullResultTypeNamed(
      ename,
      typ);
    hydra.core.Type baseType = new hydra.core.Type.Function(new hydra.core.FunctionType(resultType, new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"))));
    return hydra.Encoding.prependForallEncoders(
      baseType,
      typ);
  }

  static hydra.core.TypeScheme encoderTypeScheme(hydra.core.Type typ) {
    hydra.util.ConsList<hydra.core.Name> allOrdVars = hydra.Encoding.encoderCollectOrdVars(typ);
    hydra.util.ConsList<hydra.core.Name> typeVars = hydra.Encoding.encoderCollectForallVariables(typ);
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> ordVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.lists.Elem.apply(
        v,
        typeVars)),
      allOrdVars));
    hydra.util.Lazy<hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(ordVars.get()),
      () -> (hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()),
      () -> hydra.util.Maybe.just(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (v -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>(v, new hydra.core.TypeVariableMetadata(hydra.lib.sets.Singleton.apply(new hydra.core.Name("ordering"))))))),
        ordVars.get())))));
    hydra.core.Type encoderFunType = hydra.Encoding.encoderType(typ);
    return new hydra.core.TypeScheme(typeVars, encoderFunType, constraints.get());
  }

  static hydra.core.TypeScheme encoderTypeSchemeNamed(hydra.core.Name ename, hydra.core.Type typ) {
    hydra.util.ConsList<hydra.core.Name> allOrdVars = hydra.Encoding.encoderCollectOrdVars(typ);
    hydra.util.ConsList<hydra.core.Name> typeVars = hydra.Encoding.encoderCollectForallVariables(typ);
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> ordVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.lists.Elem.apply(
        v,
        typeVars)),
      allOrdVars));
    hydra.util.Lazy<hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(ordVars.get()),
      () -> (hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()),
      () -> hydra.util.Maybe.just(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (v -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>(v, new hydra.core.TypeVariableMetadata(hydra.lib.sets.Singleton.apply(new hydra.core.Name("ordering"))))))),
        ordVars.get())))));
    hydra.core.Type encoderFunType = hydra.Encoding.encoderTypeNamed(
      ename,
      typ);
    return new hydra.core.TypeScheme(typeVars, encoderFunType, constraints.get());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Binding>> filterTypeBindings(hydra.context.Context cx, hydra.graph.Graph graph, hydra.util.ConsList<hydra.core.Binding> bindings) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.util.ConsList<hydra.util.Maybe<hydra.core.Binding>>, hydra.util.ConsList<hydra.core.Binding>>) (hydra.lib.maybes.Cat::apply),
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.Binding>>>) (v1 -> hydra.Encoding.isEncodableBinding(
          cx,
          graph,
          v1)),
        hydra.lib.lists.Filter.apply(
          hydra.Annotations::isNativeType,
          bindings)));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.Binding>> isEncodableBinding(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Binding b) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Schemas.isSerializableByName(
        cx,
        graph,
        (b).name),
      (java.util.function.Function<Boolean, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.Binding>>>) (serializable -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.Binding>>right(hydra.lib.logic.IfElse.lazy(
        serializable,
        () -> hydra.util.Maybe.just(b),
        () -> (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing())))));
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

  static hydra.core.Type prependForallEncoders(hydra.core.Type baseType, hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return baseType;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return hydra.Encoding.prependForallEncoders(
          baseType,
          (at).value.body);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable((ft).value.parameter), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")))), hydra.Encoding.prependForallEncoders(
          baseType,
          (ft).value.body)));
      }
    });
  }
}
