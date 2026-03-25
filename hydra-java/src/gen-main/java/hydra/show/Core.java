// Note: this is an automatically generated file. Do not edit.

package hydra.show;

/**
 * String representations of hydra.core types
 */
public interface Core {
  static String binding(hydra.core.Binding el) {
    String name = (el).name.value;
    hydra.core.Term t = (el).term;
    hydra.util.Lazy<String> typeStr = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> "",
      (java.util.function.Function<hydra.core.TypeScheme, String>) (ts -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
        ":(",
        hydra.show.Core.typeScheme(ts),
        ")"))),
      (el).type));
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      name,
      typeStr.get(),
      " = ",
      hydra.show.Core.term(t)));
  }

  static String elimination(hydra.core.Elimination elm) {
    return (elm).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public String visit(hydra.core.Elimination.Record proj) {
        String fname = (proj).value.field.value;
        String tname = (proj).value.typeName.value;
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "project(",
          tname,
          "){",
          fname,
          "}"));
      }

      @Override
      public String visit(hydra.core.Elimination.Union cs) {
        hydra.util.ConsList<hydra.core.Field> cases = (cs).value.cases;
        hydra.util.Maybe<hydra.core.Term> mdef = (cs).value.default_;
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Field>> defaultField = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
          () -> (hydra.util.ConsList<hydra.core.Field>) (hydra.util.ConsList.<hydra.core.Field>empty()),
          (java.util.function.Function<hydra.core.Term, hydra.util.ConsList<hydra.core.Field>>) (d -> hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("[default]"), d))),
          mdef));
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Field>> allFields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          cases,
          defaultField.get())));
        String tname = (cs).value.typeName.value;
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "case(",
          tname,
          ")",
          hydra.show.Core.fields(allFields.get())));
      }

      @Override
      public String visit(hydra.core.Elimination.Wrap tname) {
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "unwrap(",
          (tname).value.value,
          ")"));
      }
    });
  }

  static String field(hydra.core.Field field) {
    String fname = (field).name.value;
    hydra.core.Term fterm = (field).term;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      fname,
      "=",
      hydra.show.Core.term(fterm)));
  }

  static String fieldType(hydra.core.FieldType ft) {
    String fname = (ft).name.value;
    hydra.core.Type ftyp = (ft).type;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      fname,
      ":",
      hydra.show.Core.type(ftyp)));
  }

  static String fields(hydra.util.ConsList<hydra.core.Field> flds) {
    hydra.util.Lazy<hydra.util.ConsList<String>> fieldStrs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.show.Core::field,
      flds));
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "{",
      hydra.lib.strings.Intercalate.apply(
        ", ",
        fieldStrs.get()),
      "}"));
  }

  static String float_(hydra.core.FloatValue fv) {
    return (fv).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public String visit(hydra.core.FloatValue.Bigfloat v) {
        return hydra.lib.strings.Cat2.apply(
          hydra.lib.literals.ShowBigfloat.apply((v).value),
          ":bigfloat");
      }

      @Override
      public String visit(hydra.core.FloatValue.Float32 v) {
        return hydra.lib.strings.Cat2.apply(
          hydra.lib.literals.ShowFloat32.apply((v).value),
          ":float32");
      }

      @Override
      public String visit(hydra.core.FloatValue.Float64 v) {
        return hydra.lib.strings.Cat2.apply(
          hydra.lib.literals.ShowFloat64.apply((v).value),
          ":float64");
      }
    });
  }

  static String floatType(hydra.core.FloatType ft) {
    return (ft).accept(new hydra.core.FloatType.PartialVisitor<>() {
      @Override
      public String visit(hydra.core.FloatType.Bigfloat ignored) {
        return "bigfloat";
      }

      @Override
      public String visit(hydra.core.FloatType.Float32 ignored) {
        return "float32";
      }

      @Override
      public String visit(hydra.core.FloatType.Float64 ignored) {
        return "float64";
      }
    });
  }

  static String function(hydra.core.Function f) {
    return (f).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public String visit(hydra.core.Function.Elimination v1) {
        return hydra.show.Core.elimination((v1).value);
      }

      @Override
      public String visit(hydra.core.Function.Lambda v1) {
        return hydra.show.Core.lambda((v1).value);
      }

      @Override
      public String visit(hydra.core.Function.Primitive name) {
        return hydra.lib.strings.Cat2.apply(
          (name).value.value,
          "!");
      }
    });
  }

  static String injection(hydra.core.Injection inj) {
    hydra.core.Field f = (inj).field;
    hydra.core.Name tname = (inj).typeName;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "inject(",
      (tname).value,
      ")",
      hydra.show.Core.fields(hydra.util.ConsList.of(f))));
  }

  static String integer(hydra.core.IntegerValue iv) {
    return (iv).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public String visit(hydra.core.IntegerValue.Bigint v) {
        return hydra.lib.strings.Cat2.apply(
          hydra.lib.literals.ShowBigint.apply((v).value),
          ":bigint");
      }

      @Override
      public String visit(hydra.core.IntegerValue.Int8 v) {
        return hydra.lib.strings.Cat2.apply(
          hydra.lib.literals.ShowInt8.apply((v).value),
          ":int8");
      }

      @Override
      public String visit(hydra.core.IntegerValue.Int16 v) {
        return hydra.lib.strings.Cat2.apply(
          hydra.lib.literals.ShowInt16.apply((v).value),
          ":int16");
      }

      @Override
      public String visit(hydra.core.IntegerValue.Int32 v) {
        return hydra.lib.strings.Cat2.apply(
          hydra.lib.literals.ShowInt32.apply((v).value),
          ":int32");
      }

      @Override
      public String visit(hydra.core.IntegerValue.Int64 v) {
        return hydra.lib.strings.Cat2.apply(
          hydra.lib.literals.ShowInt64.apply((v).value),
          ":int64");
      }

      @Override
      public String visit(hydra.core.IntegerValue.Uint8 v) {
        return hydra.lib.strings.Cat2.apply(
          hydra.lib.literals.ShowUint8.apply((v).value),
          ":uint8");
      }

      @Override
      public String visit(hydra.core.IntegerValue.Uint16 v) {
        return hydra.lib.strings.Cat2.apply(
          hydra.lib.literals.ShowUint16.apply((v).value),
          ":uint16");
      }

      @Override
      public String visit(hydra.core.IntegerValue.Uint32 v) {
        return hydra.lib.strings.Cat2.apply(
          hydra.lib.literals.ShowUint32.apply((v).value),
          ":uint32");
      }

      @Override
      public String visit(hydra.core.IntegerValue.Uint64 v) {
        return hydra.lib.strings.Cat2.apply(
          hydra.lib.literals.ShowUint64.apply((v).value),
          ":uint64");
      }
    });
  }

  static String integerType(hydra.core.IntegerType it) {
    return (it).accept(new hydra.core.IntegerType.PartialVisitor<>() {
      @Override
      public String visit(hydra.core.IntegerType.Bigint ignored) {
        return "bigint";
      }

      @Override
      public String visit(hydra.core.IntegerType.Int8 ignored) {
        return "int8";
      }

      @Override
      public String visit(hydra.core.IntegerType.Int16 ignored) {
        return "int16";
      }

      @Override
      public String visit(hydra.core.IntegerType.Int32 ignored) {
        return "int32";
      }

      @Override
      public String visit(hydra.core.IntegerType.Int64 ignored) {
        return "int64";
      }

      @Override
      public String visit(hydra.core.IntegerType.Uint8 ignored) {
        return "uint8";
      }

      @Override
      public String visit(hydra.core.IntegerType.Uint16 ignored) {
        return "uint16";
      }

      @Override
      public String visit(hydra.core.IntegerType.Uint32 ignored) {
        return "uint32";
      }

      @Override
      public String visit(hydra.core.IntegerType.Uint64 ignored) {
        return "uint64";
      }
    });
  }

  static String lambda(hydra.core.Lambda l) {
    hydra.core.Term body = (l).body;
    hydra.util.Maybe<hydra.core.Type> mt = (l).domain;
    hydra.util.Lazy<String> typeStr = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> "",
      (java.util.function.Function<hydra.core.Type, String>) (t -> hydra.lib.strings.Cat2.apply(
        ":",
        hydra.show.Core.type(t))),
      mt));
    String v = (l).parameter.value;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "\u03BB",
      v,
      typeStr.get(),
      ".",
      hydra.show.Core.term(body)));
  }

  static String let(hydra.core.Let l) {
    hydra.util.ConsList<hydra.core.Binding> bindings = (l).bindings;
    hydra.util.Lazy<hydra.util.ConsList<String>> bindingStrs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.show.Core::binding,
      bindings));
    hydra.core.Term env = (l).body;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "let ",
      hydra.lib.strings.Intercalate.apply(
        ", ",
        bindingStrs.get()),
      " in ",
      hydra.show.Core.term(env)));
  }

  static <T0> String list(java.util.function.Function<T0, String> f, hydra.util.ConsList<T0> xs) {
    hydra.util.Lazy<hydra.util.ConsList<String>> elementStrs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      f,
      xs));
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "[",
      hydra.lib.strings.Intercalate.apply(
        ", ",
        elementStrs.get()),
      "]"));
  }

  static String literal(hydra.core.Literal l) {
    return (l).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public String visit(hydra.core.Literal.Binary ignored) {
        return "[binary]";
      }

      @Override
      public String visit(hydra.core.Literal.Boolean_ b) {
        return hydra.lib.logic.IfElse.lazy(
          (b).value,
          () -> "true",
          () -> "false");
      }

      @Override
      public String visit(hydra.core.Literal.Float_ fv) {
        return hydra.show.Core.float_((fv).value);
      }

      @Override
      public String visit(hydra.core.Literal.Integer_ iv) {
        return hydra.show.Core.integer((iv).value);
      }

      @Override
      public String visit(hydra.core.Literal.String_ s) {
        return hydra.lib.literals.ShowString.apply((s).value);
      }
    });
  }

  static String literalType(hydra.core.LiteralType lt) {
    return (lt).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public String visit(hydra.core.LiteralType.Binary ignored) {
        return "binary";
      }

      @Override
      public String visit(hydra.core.LiteralType.Boolean_ ignored) {
        return "boolean";
      }

      @Override
      public String visit(hydra.core.LiteralType.Float_ ft) {
        return hydra.show.Core.floatType((ft).value);
      }

      @Override
      public String visit(hydra.core.LiteralType.Integer_ it) {
        return hydra.show.Core.integerType((it).value);
      }

      @Override
      public String visit(hydra.core.LiteralType.String_ ignored) {
        return "string";
      }
    });
  }

  static hydra.util.Maybe<hydra.core.Term> readTerm(String s) {
    return hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_(s)));
  }

  static String term(hydra.core.Term t) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, java.util.function.Function<hydra.core.Application, hydra.util.ConsList<hydra.core.Term>>>> gatherTerms = new java.util.concurrent.atomic.AtomicReference<>();
    gatherTerms.set((java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, java.util.function.Function<hydra.core.Application, hydra.util.ConsList<hydra.core.Term>>>) (prev -> (java.util.function.Function<hydra.core.Application, hydra.util.ConsList<hydra.core.Term>>) (app -> {
      hydra.core.Term lhs = (app).function;
      hydra.core.Term rhs = (app).argument;
      return (lhs).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.ConsList<hydra.core.Term> otherwise(hydra.core.Term instance) {
          return hydra.lib.lists.Cons.apply(
            lhs,
            hydra.lib.lists.Cons.apply(
              rhs,
              prev));
        }

        @Override
        public hydra.util.ConsList<hydra.core.Term> visit(hydra.core.Term.Application app2) {
          return gatherTerms.get().apply(hydra.lib.lists.Cons.apply(
            rhs,
            prev)).apply((app2).value);
        }
      });
    })));
    return (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public String visit(hydra.core.Term.Annotated at) {
        return hydra.show.Core.term((at).value.body);
      }

      @Override
      public String visit(hydra.core.Term.Application app) {
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> terms = new hydra.util.Lazy<>(() -> gatherTerms.get().apply((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())).apply((app).value));
        hydra.util.Lazy<hydra.util.ConsList<String>> termStrs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          hydra.show.Core::term,
          terms.get()));
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "(",
          hydra.lib.strings.Intercalate.apply(
            " @ ",
            termStrs.get()),
          ")"));
      }

      @Override
      public String visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, String>) (l -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
            "left(",
            hydra.show.Core.term(l),
            ")"))),
          (java.util.function.Function<hydra.core.Term, String>) (r -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
            "right(",
            hydra.show.Core.term(r),
            ")"))),
          (e).value);
      }

      @Override
      public String visit(hydra.core.Term.Function v1) {
        return hydra.show.Core.function((v1).value);
      }

      @Override
      public String visit(hydra.core.Term.Let l) {
        return hydra.show.Core.let((l).value);
      }

      @Override
      public String visit(hydra.core.Term.List els) {
        hydra.util.Lazy<hydra.util.ConsList<String>> termStrs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          hydra.show.Core::term,
          (els).value));
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "[",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            termStrs.get()),
          "]"));
      }

      @Override
      public String visit(hydra.core.Term.Literal lit) {
        return hydra.show.Core.literal((lit).value);
      }

      @Override
      public String visit(hydra.core.Term.Map m) {
        java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, String> entry = (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, String>) (p -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          hydra.show.Core.term(hydra.lib.pairs.First.apply(p)),
          "=",
          hydra.show.Core.term(hydra.lib.pairs.Second.apply(p)))));
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              entry,
              hydra.lib.maps.ToList.apply((m).value))),
          "}"));
      }

      @Override
      public String visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> "nothing",
          (java.util.function.Function<hydra.core.Term, String>) (t2 -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
            "just(",
            hydra.show.Core.term(t2),
            ")"))),
          (mt).value);
      }

      @Override
      public String visit(hydra.core.Term.Pair p) {
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "(",
          hydra.show.Core.term(hydra.lib.pairs.First.apply((p).value)),
          ", ",
          hydra.show.Core.term(hydra.lib.pairs.Second.apply((p).value)),
          ")"));
      }

      @Override
      public String visit(hydra.core.Term.Record rec) {
        hydra.util.ConsList<hydra.core.Field> flds = (rec).value.fields;
        String tname = (rec).value.typeName.value;
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "record(",
          tname,
          ")",
          hydra.show.Core.fields(flds)));
      }

      @Override
      public String visit(hydra.core.Term.Set s) {
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              hydra.show.Core::term,
              hydra.lib.sets.ToList.apply((s).value))),
          "}"));
      }

      @Override
      public String visit(hydra.core.Term.TypeLambda ta) {
        hydra.core.Term body = (ta).value.body;
        String param = (ta).value.parameter.value;
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "\u039B",
          param,
          ".",
          hydra.show.Core.term(body)));
      }

      @Override
      public String visit(hydra.core.Term.TypeApplication tt) {
        hydra.core.Term t2 = (tt).value.body;
        hydra.core.Type typ = (tt).value.type;
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          hydra.show.Core.term(t2),
          "\u27E8",
          hydra.show.Core.type(typ),
          "\u27E9"));
      }

      @Override
      public String visit(hydra.core.Term.Union v1) {
        return hydra.show.Core.injection((v1).value);
      }

      @Override
      public String visit(hydra.core.Term.Unit ignored) {
        return "unit";
      }

      @Override
      public String visit(hydra.core.Term.Variable name) {
        return (name).value.value;
      }

      @Override
      public String visit(hydra.core.Term.Wrap wt) {
        hydra.core.Term term1 = (wt).value.body;
        String tname = (wt).value.typeName.value;
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "wrap(",
          tname,
          "){",
          hydra.show.Core.term(term1),
          "}"));
      }
    });
  }

  static String type(hydra.core.Type typ) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>>> gatherFunctionTypes = new java.util.concurrent.atomic.AtomicReference<>();
    gatherFunctionTypes.set((java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>>) (prev -> (java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
          t,
          prev));
      }

      @Override
      public hydra.util.ConsList<hydra.core.Type> visit(hydra.core.Type.Function ft) {
        hydra.core.Type cod = (ft).value.codomain;
        hydra.core.Type dom = (ft).value.domain;
        return gatherFunctionTypes.get().apply(hydra.lib.lists.Cons.apply(
          dom,
          prev)).apply(cod);
      }
    }))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, java.util.function.Function<hydra.core.ApplicationType, hydra.util.ConsList<hydra.core.Type>>>> gatherTypes = new java.util.concurrent.atomic.AtomicReference<>();
    gatherTypes.set((java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, java.util.function.Function<hydra.core.ApplicationType, hydra.util.ConsList<hydra.core.Type>>>) (prev -> (java.util.function.Function<hydra.core.ApplicationType, hydra.util.ConsList<hydra.core.Type>>) (app -> {
      hydra.core.Type lhs = (app).function;
      hydra.core.Type rhs = (app).argument;
      return (lhs).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.util.ConsList<hydra.core.Type> otherwise(hydra.core.Type instance) {
          return hydra.lib.lists.Cons.apply(
            lhs,
            hydra.lib.lists.Cons.apply(
              rhs,
              prev));
        }

        @Override
        public hydra.util.ConsList<hydra.core.Type> visit(hydra.core.Type.Application app2) {
          return gatherTypes.get().apply(hydra.lib.lists.Cons.apply(
            rhs,
            prev)).apply((app2).value);
        }
      });
    })));
    java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, String> showRowType = (java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, String>) (flds -> {
      hydra.util.Lazy<hydra.util.ConsList<String>> fieldStrs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
        hydra.show.Core::fieldType,
        flds));
      return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
        "{",
        hydra.lib.strings.Intercalate.apply(
          ", ",
          fieldStrs.get()),
        "}"));
    });
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public String visit(hydra.core.Type.Annotated at) {
        return hydra.show.Core.type((at).value.body);
      }

      @Override
      public String visit(hydra.core.Type.Application app) {
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> types = new hydra.util.Lazy<>(() -> gatherTypes.get().apply((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty())).apply((app).value));
        hydra.util.Lazy<hydra.util.ConsList<String>> typeStrs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          hydra.show.Core::type,
          types.get()));
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "(",
          hydra.lib.strings.Intercalate.apply(
            " @ ",
            typeStrs.get()),
          ")"));
      }

      @Override
      public String visit(hydra.core.Type.Either et) {
        hydra.core.Type leftTyp = (et).value.left;
        hydra.core.Type rightTyp = (et).value.right;
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "either<",
          hydra.show.Core.type(leftTyp),
          ", ",
          hydra.show.Core.type(rightTyp),
          ">"));
      }

      @Override
      public String visit(hydra.core.Type.Forall ft) {
        hydra.core.Type body = (ft).value.body;
        String var = (ft).value.parameter.value;
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "(\u2200",
          var,
          ".",
          hydra.show.Core.type(body),
          ")"));
      }

      @Override
      public String visit(hydra.core.Type.Function ft) {
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> types = new hydra.util.Lazy<>(() -> gatherFunctionTypes.get().apply((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty())).apply(typ));
        hydra.util.Lazy<hydra.util.ConsList<String>> typeStrs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          hydra.show.Core::type,
          types.get()));
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "(",
          hydra.lib.strings.Intercalate.apply(
            " \u2192 ",
            typeStrs.get()),
          ")"));
      }

      @Override
      public String visit(hydra.core.Type.List etyp) {
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "list<",
          hydra.show.Core.type((etyp).value),
          ">"));
      }

      @Override
      public String visit(hydra.core.Type.Literal lt) {
        return hydra.show.Core.literalType((lt).value);
      }

      @Override
      public String visit(hydra.core.Type.Map mt) {
        hydra.core.Type keyTyp = (mt).value.keys;
        hydra.core.Type valTyp = (mt).value.values;
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "map<",
          hydra.show.Core.type(keyTyp),
          ", ",
          hydra.show.Core.type(valTyp),
          ">"));
      }

      @Override
      public String visit(hydra.core.Type.Maybe etyp) {
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "maybe<",
          hydra.show.Core.type((etyp).value),
          ">"));
      }

      @Override
      public String visit(hydra.core.Type.Pair pt) {
        hydra.core.Type firstTyp = (pt).value.first;
        hydra.core.Type secondTyp = (pt).value.second;
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "(",
          hydra.show.Core.type(firstTyp),
          ", ",
          hydra.show.Core.type(secondTyp),
          ")"));
      }

      @Override
      public String visit(hydra.core.Type.Record rt) {
        return hydra.lib.strings.Cat2.apply(
          "record",
          (showRowType).apply((rt).value));
      }

      @Override
      public String visit(hydra.core.Type.Set etyp) {
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "set<",
          hydra.show.Core.type((etyp).value),
          ">"));
      }

      @Override
      public String visit(hydra.core.Type.Union rt) {
        return hydra.lib.strings.Cat2.apply(
          "union",
          (showRowType).apply((rt).value));
      }

      @Override
      public String visit(hydra.core.Type.Unit ignored) {
        return "unit";
      }

      @Override
      public String visit(hydra.core.Type.Variable name) {
        return (name).value.value;
      }

      @Override
      public String visit(hydra.core.Type.Void_ ignored) {
        return "void";
      }

      @Override
      public String visit(hydra.core.Type.Wrap wt) {
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "wrap(",
          hydra.show.Core.type((wt).value),
          ")"));
      }
    });
  }

  static String typeScheme(hydra.core.TypeScheme ts) {
    hydra.core.Type body = (ts).type;
    hydra.util.ConsList<hydra.core.Name> vars = (ts).variables;
    hydra.util.Lazy<hydra.util.ConsList<String>> varNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      wrapped -> (wrapped).value,
      vars));
    hydra.util.Lazy<String> fa = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(vars),
      () -> "",
      () -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
        "forall ",
        hydra.lib.strings.Intercalate.apply(
          ",",
          varNames.get()),
        ". "))));
    java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Name, String>> toConstraintPair = (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Name, String>>) (v -> (java.util.function.Function<hydra.core.Name, String>) (c -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      (c).value,
      " ",
      (v).value))));
    java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>, hydra.util.ConsList<String>> toConstraintPairs = (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>, hydra.util.ConsList<String>>) (p -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Name, String>) (v1 -> (toConstraintPair).apply(hydra.lib.pairs.First.apply(p)).apply(v1)),
      hydra.lib.sets.ToList.apply(hydra.lib.pairs.Second.apply(p).classes)));
    hydra.util.Lazy<hydra.util.ConsList<String>> tc = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty()),
      (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>, hydra.util.ConsList<String>>) (m -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        toConstraintPairs,
        hydra.lib.maps.ToList.apply(m)))),
      (ts).constraints));
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "(",
      fa.get(),
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(tc.get()),
        () -> "",
        () -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "(",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            tc.get()),
          ") => "))),
      hydra.show.Core.type(body),
      ")"));
  }
}
