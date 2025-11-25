// Note: this is an automatically generated file. Do not edit.

package hydra.show.core;

/**
 * String representations of hydra.core types
 */
public interface Core {
  static hydra.util.Maybe<hydra.core.Term> readTerm(String s) {
    return hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_((s))));
  }
  
  static String binding(hydra.core.Binding el) {
    String name = (((hydra.core.Binding) ((el))).name).value;
    hydra.core.Term t = ((hydra.core.Binding) ((el))).term;
    String typeStr = hydra.lib.maybes.Maybe.apply(
      "",
      (java.util.function.Function<hydra.core.TypeScheme, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.List.of(
        ":(",
        hydra.show.core.Core.typeScheme((ts)),
        ")"))),
      ((hydra.core.Binding) ((el))).type);
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      (name),
      (typeStr),
      " = ",
      hydra.show.core.Core.term((t))));
  }
  
  static String elimination(hydra.core.Elimination elm) {
    return ((elm)).accept(new hydra.core.Elimination.Visitor<>() {
      @Override
      public String visit(hydra.core.Elimination.Product tp) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "[",
          hydra.lib.literals.ShowInt32.apply(((hydra.core.TupleProjection) (((tp)).value)).index),
          "/",
          hydra.lib.literals.ShowInt32.apply(((hydra.core.TupleProjection) (((tp)).value)).arity),
          "]"));
      }
      
      @Override
      public String visit(hydra.core.Elimination.Record proj) {
        String fname = (((hydra.core.Projection) (((proj)).value)).field).value;
        String tname = (((hydra.core.Projection) (((proj)).value)).typeName).value;
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "project(",
          (tname),
          "){",
          (fname),
          "}"));
      }
      
      @Override
      public String visit(hydra.core.Elimination.Union cs) {
        java.util.List<hydra.core.Field> cases = ((hydra.core.CaseStatement) (((cs)).value)).cases;
        hydra.util.Maybe<hydra.core.Term> mdef = ((hydra.core.CaseStatement) (((cs)).value)).default_;
        java.util.List<hydra.core.Field> defaultField = hydra.lib.maybes.Maybe.apply(
          (java.util.List<hydra.core.Field>) ((java.util.List) (java.util.List.<hydra.core.Field>of())),
          (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Field>>) (d -> java.util.List.of(new hydra.core.Field(new hydra.core.Name("[default]"), (d)))),
          (mdef));
        java.util.List<hydra.core.Field> allFields = hydra.lib.lists.Concat.apply(java.util.List.of(
          (cases),
          (defaultField)));
        String tname = (((hydra.core.CaseStatement) (((cs)).value)).typeName).value;
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "case(",
          (tname),
          ")",
          hydra.show.core.Core.fields((allFields))));
      }
      
      @Override
      public String visit(hydra.core.Elimination.Wrap tname) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "unwrap(",
          (((tname)).value).value,
          ")"));
      }
    });
  }
  
  static String field(hydra.core.Field field) {
    String fname = (((hydra.core.Field) ((field))).name).value;
    hydra.core.Term fterm = ((hydra.core.Field) ((field))).term;
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      (fname),
      "=",
      hydra.show.core.Core.term((fterm))));
  }
  
  static String fieldType(hydra.core.FieldType ft) {
    String fname = (((hydra.core.FieldType) ((ft))).name).value;
    hydra.core.Type ftyp = ((hydra.core.FieldType) ((ft))).type;
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      (fname),
      ":",
      hydra.show.core.Core.type((ftyp))));
  }
  
  static String fields(java.util.List<hydra.core.Field> flds) {
    java.util.List<String> fieldStrs = hydra.lib.lists.Map.apply(
      (hydra.show.core.Core::field),
      (flds));
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      "{",
      hydra.lib.strings.Intercalate.apply(
        ", ",
        (fieldStrs)),
      "}"));
  }
  
  static String float_(hydra.core.FloatValue fv) {
    return ((fv)).accept(new hydra.core.FloatValue.Visitor<>() {
      @Override
      public String visit(hydra.core.FloatValue.Bigfloat v) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          hydra.lib.literals.ShowBigfloat.apply(((v)).value),
          ":bigfloat"));
      }
      
      @Override
      public String visit(hydra.core.FloatValue.Float32 v) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          hydra.lib.literals.ShowFloat32.apply(((v)).value),
          ":float32"));
      }
      
      @Override
      public String visit(hydra.core.FloatValue.Float64 v) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          hydra.lib.literals.ShowFloat64.apply(((v)).value),
          ":float64"));
      }
    });
  }
  
  static String floatType(hydra.core.FloatType ft) {
    return ((ft)).accept(new hydra.core.FloatType.Visitor<>() {
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
    return ((f)).accept(new hydra.core.Function.Visitor<>() {
      @Override
      public String visit(hydra.core.Function.Elimination v1) {
        return hydra.show.core.Core.elimination(((v1)).value);
      }
      
      @Override
      public String visit(hydra.core.Function.Lambda v1) {
        return hydra.show.core.Core.lambda(((v1)).value);
      }
      
      @Override
      public String visit(hydra.core.Function.Primitive name) {
        return hydra.lib.strings.Cat2.apply(
          (((name)).value).value,
          "!");
      }
    });
  }
  
  static String injection(hydra.core.Injection inj) {
    hydra.core.Field f = ((hydra.core.Injection) ((inj))).field;
    hydra.core.Name tname = ((hydra.core.Injection) ((inj))).typeName;
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      "inject(",
      ((tname)).value,
      ")",
      hydra.show.core.Core.fields(java.util.List.of((f)))));
  }
  
  static String integer(hydra.core.IntegerValue iv) {
    return ((iv)).accept(new hydra.core.IntegerValue.Visitor<>() {
      @Override
      public String visit(hydra.core.IntegerValue.Bigint v) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          hydra.lib.literals.ShowBigint.apply(((v)).value),
          ":bigint"));
      }
      
      @Override
      public String visit(hydra.core.IntegerValue.Int8 v) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          hydra.lib.literals.ShowInt8.apply(((v)).value),
          ":int8"));
      }
      
      @Override
      public String visit(hydra.core.IntegerValue.Int16 v) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          hydra.lib.literals.ShowInt16.apply(((v)).value),
          ":int16"));
      }
      
      @Override
      public String visit(hydra.core.IntegerValue.Int32 v) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          hydra.lib.literals.ShowInt32.apply(((v)).value),
          ":int32"));
      }
      
      @Override
      public String visit(hydra.core.IntegerValue.Int64 v) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          hydra.lib.literals.ShowInt64.apply(((v)).value),
          ":int64"));
      }
      
      @Override
      public String visit(hydra.core.IntegerValue.Uint8 v) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          hydra.lib.literals.ShowUint8.apply(((v)).value),
          ":uint8"));
      }
      
      @Override
      public String visit(hydra.core.IntegerValue.Uint16 v) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          hydra.lib.literals.ShowUint16.apply(((v)).value),
          ":uint16"));
      }
      
      @Override
      public String visit(hydra.core.IntegerValue.Uint32 v) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          hydra.lib.literals.ShowUint32.apply(((v)).value),
          ":uint32"));
      }
      
      @Override
      public String visit(hydra.core.IntegerValue.Uint64 v) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          hydra.lib.literals.ShowUint64.apply(((v)).value),
          ":uint64"));
      }
    });
  }
  
  static String integerType(hydra.core.IntegerType it) {
    return ((it)).accept(new hydra.core.IntegerType.Visitor<>() {
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
    hydra.core.Term body = ((hydra.core.Lambda) ((l))).body;
    hydra.util.Maybe<hydra.core.Type> mt = ((hydra.core.Lambda) ((l))).domain;
    String typeStr = hydra.lib.maybes.Maybe.apply(
      "",
      (java.util.function.Function<hydra.core.Type, String>) (t -> hydra.lib.strings.Cat2.apply(
        ":",
        hydra.show.core.Core.type((t)))),
      (mt));
    String v = (((hydra.core.Lambda) ((l))).parameter).value;
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      "\u03BB",
      (v),
      (typeStr),
      ".",
      hydra.show.core.Core.term((body))));
  }
  
  static <T0> String list(java.util.function.Function<T0, String> f, java.util.List<T0> xs) {
    java.util.List<String> elementStrs = hydra.lib.lists.Map.apply(
      (f),
      (xs));
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      "[",
      hydra.lib.strings.Intercalate.apply(
        ", ",
        (elementStrs)),
      "]"));
  }
  
  static String literal(hydra.core.Literal l) {
    return ((l)).accept(new hydra.core.Literal.Visitor<>() {
      @Override
      public String visit(hydra.core.Literal.Binary ignored) {
        return "[binary]";
      }
      
      @Override
      public String visit(hydra.core.Literal.Boolean_ b) {
        return hydra.lib.logic.IfElse.apply(
          ((b)).value,
          "true",
          "false");
      }
      
      @Override
      public String visit(hydra.core.Literal.Float_ fv) {
        return hydra.show.core.Core.float_(((fv)).value);
      }
      
      @Override
      public String visit(hydra.core.Literal.Integer_ iv) {
        return hydra.show.core.Core.integer(((iv)).value);
      }
      
      @Override
      public String visit(hydra.core.Literal.String_ s) {
        return hydra.lib.literals.ShowString.apply(((s)).value);
      }
    });
  }
  
  static String literalType(hydra.core.LiteralType lt) {
    return ((lt)).accept(new hydra.core.LiteralType.Visitor<>() {
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
        return hydra.show.core.Core.floatType(((ft)).value);
      }
      
      @Override
      public String visit(hydra.core.LiteralType.Integer_ it) {
        return hydra.show.core.Core.integerType(((it)).value);
      }
      
      @Override
      public String visit(hydra.core.LiteralType.String_ ignored) {
        return "string";
      }
    });
  }
  
  static String term(hydra.core.Term t) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Term>, java.util.function.Function<hydra.core.Application, java.util.List<hydra.core.Term>>>> gatherTerms = new java.util.concurrent.atomic.AtomicReference<>();
    gatherTerms.set((java.util.function.Function<java.util.List<hydra.core.Term>, java.util.function.Function<hydra.core.Application, java.util.List<hydra.core.Term>>>) (prev -> (java.util.function.Function<hydra.core.Application, java.util.List<hydra.core.Term>>) (app -> {
      hydra.core.Term lhs = ((hydra.core.Application) ((app))).function;
      hydra.core.Term rhs = ((hydra.core.Application) ((app))).argument;
      return ((lhs)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public java.util.List<hydra.core.Term> otherwise(hydra.core.Term instance) {
          return hydra.lib.lists.Cons.apply(
            (lhs),
            hydra.lib.lists.Cons.apply(
              (rhs),
              (prev)));
        }
        
        @Override
        public java.util.List<hydra.core.Term> visit(hydra.core.Term.Application app2) {
          return (java.util.List<hydra.core.Term>) ((java.util.List) (((java.util.function.Function<hydra.core.Application, java.util.List<hydra.core.Term>>) ((java.util.function.Function) ((gatherTerms.get()).apply(hydra.lib.lists.Cons.apply(
            (rhs),
            (prev)))))).apply(((app2)).value)));
        }
      });
    })));
    return ((t)).accept(new hydra.core.Term.Visitor<>() {
      @Override
      public String visit(hydra.core.Term.Annotated at) {
        return hydra.show.core.Core.term(((hydra.core.AnnotatedTerm) (((at)).value)).body);
      }
      
      @Override
      public String visit(hydra.core.Term.Application app) {
        java.util.List<hydra.core.Term> terms = (java.util.List<hydra.core.Term>) ((java.util.List) (((java.util.function.Function<hydra.core.Application, java.util.List<hydra.core.Term>>) ((java.util.function.Function) ((gatherTerms.get()).apply((java.util.List<hydra.core.Term>) ((java.util.List) (java.util.List.<hydra.core.Term>of())))))).apply(((app)).value)));
        java.util.List<String> termStrs = hydra.lib.lists.Map.apply(
          (hydra.show.core.Core::term),
          (terms));
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "(",
          hydra.lib.strings.Intercalate.apply(
            " @ ",
            (termStrs)),
          ")"));
      }
      
      @Override
      public String visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, String>) (l -> hydra.lib.strings.Cat.apply(java.util.List.of(
            "left(",
            hydra.show.core.Core.term((l)),
            ")"))),
          (java.util.function.Function<hydra.core.Term, String>) (r -> hydra.lib.strings.Cat.apply(java.util.List.of(
            "right(",
            hydra.show.core.Core.term((r)),
            ")"))),
          ((e)).value);
      }
      
      @Override
      public String visit(hydra.core.Term.Function v1) {
        return hydra.show.core.Core.function(((v1)).value);
      }
      
      @Override
      public String visit(hydra.core.Term.Let l) {
        java.util.List<hydra.core.Binding> bindings = ((hydra.core.Let) (((l)).value)).bindings;
        java.util.List<String> bindingStrs = hydra.lib.lists.Map.apply(
          (hydra.show.core.Core::binding),
          (bindings));
        hydra.core.Term env = ((hydra.core.Let) (((l)).value)).body;
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "let ",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            (bindingStrs)),
          " in ",
          hydra.show.core.Core.term((env))));
      }
      
      @Override
      public String visit(hydra.core.Term.List els) {
        java.util.List<String> termStrs = hydra.lib.lists.Map.apply(
          (hydra.show.core.Core::term),
          ((els)).value);
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "[",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            (termStrs)),
          "]"));
      }
      
      @Override
      public String visit(hydra.core.Term.Literal lit) {
        return hydra.show.core.Core.literal(((lit)).value);
      }
      
      @Override
      public String visit(hydra.core.Term.Map m) {
        java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, String> entry = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.List.of(
          hydra.show.core.Core.term(((p)).object1),
          "=",
          hydra.show.core.Core.term(((p)).object2))));
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (entry),
              hydra.lib.maps.ToList.apply(((m)).value))),
          "}"));
      }
      
      @Override
      public String visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Maybe.apply(
          "nothing",
          (java.util.function.Function<hydra.core.Term, String>) (t2 -> hydra.lib.strings.Cat.apply(java.util.List.of(
            "just(",
            hydra.show.core.Core.term((t2)),
            ")"))),
          ((mt)).value);
      }
      
      @Override
      public String visit(hydra.core.Term.Pair p) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "(",
          hydra.show.core.Core.term((((p)).value).object1),
          ", ",
          hydra.show.core.Core.term((((p)).value).object2),
          ")"));
      }
      
      @Override
      public String visit(hydra.core.Term.Product els) {
        java.util.List<String> termStrs = hydra.lib.lists.Map.apply(
          (hydra.show.core.Core::term),
          ((els)).value);
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "(",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            (termStrs)),
          ")"));
      }
      
      @Override
      public String visit(hydra.core.Term.Record rec) {
        java.util.List<hydra.core.Field> flds = ((hydra.core.Record) (((rec)).value)).fields;
        String tname = (((hydra.core.Record) (((rec)).value)).typeName).value;
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "record(",
          (tname),
          ")",
          hydra.show.core.Core.fields((flds))));
      }
      
      @Override
      public String visit(hydra.core.Term.Set s) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (hydra.show.core.Core::term),
              hydra.lib.sets.ToList.apply(((s)).value))),
          "}"));
      }
      
      @Override
      public String visit(hydra.core.Term.Sum s) {
        Integer index = ((hydra.core.Sum) (((s)).value)).index;
        Integer size = ((hydra.core.Sum) (((s)).value)).size;
        hydra.core.Term t2 = ((hydra.core.Sum) (((s)).value)).term;
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "(",
          hydra.lib.literals.ShowInt32.apply((index)),
          "/",
          hydra.lib.literals.ShowInt32.apply((size)),
          "=",
          hydra.show.core.Core.term((t2)),
          ")"));
      }
      
      @Override
      public String visit(hydra.core.Term.TypeLambda ta) {
        hydra.core.Term body = ((hydra.core.TypeLambda) (((ta)).value)).body;
        String param = (((hydra.core.TypeLambda) (((ta)).value)).parameter).value;
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "\u039B",
          (param),
          ".",
          hydra.show.core.Core.term((body))));
      }
      
      @Override
      public String visit(hydra.core.Term.TypeApplication tt) {
        hydra.core.Term t2 = ((hydra.core.TypeApplicationTerm) (((tt)).value)).body;
        hydra.core.Type typ = ((hydra.core.TypeApplicationTerm) (((tt)).value)).type;
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          hydra.show.core.Core.term((t2)),
          "\u27E8",
          hydra.show.core.Core.type((typ)),
          "\u27E9"));
      }
      
      @Override
      public String visit(hydra.core.Term.Union v1) {
        return hydra.show.core.Core.injection(((v1)).value);
      }
      
      @Override
      public String visit(hydra.core.Term.Unit ignored) {
        return "unit";
      }
      
      @Override
      public String visit(hydra.core.Term.Variable name) {
        return (((name)).value).value;
      }
      
      @Override
      public String visit(hydra.core.Term.Wrap wt) {
        hydra.core.Term term1 = ((hydra.core.WrappedTerm) (((wt)).value)).body;
        String tname = (((hydra.core.WrappedTerm) (((wt)).value)).typeName).value;
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "wrap(",
          (tname),
          "){",
          hydra.show.core.Core.term((term1)),
          "}"));
      }
    });
  }
  
  static String type(hydra.core.Type typ) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>>> gatherFunctionTypes = new java.util.concurrent.atomic.AtomicReference<>();
    gatherFunctionTypes.set((java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>>) (prev -> (java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>) (t -> ((t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
          (t),
          (prev)));
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Function ft) {
        hydra.core.Type cod = ((hydra.core.FunctionType) (((ft)).value)).codomain;
        hydra.core.Type dom = ((hydra.core.FunctionType) (((ft)).value)).domain;
        return (java.util.List<hydra.core.Type>) ((java.util.List) (((java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>) ((java.util.function.Function) ((gatherFunctionTypes.get()).apply(hydra.lib.lists.Cons.apply(
          (dom),
          (prev)))))).apply((cod))));
      }
    }))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.ApplicationType, java.util.List<hydra.core.Type>>>> gatherTypes = new java.util.concurrent.atomic.AtomicReference<>();
    gatherTypes.set((java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.ApplicationType, java.util.List<hydra.core.Type>>>) (prev -> (java.util.function.Function<hydra.core.ApplicationType, java.util.List<hydra.core.Type>>) (app -> {
      hydra.core.Type lhs = ((hydra.core.ApplicationType) ((app))).function;
      hydra.core.Type rhs = ((hydra.core.ApplicationType) ((app))).argument;
      return ((lhs)).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public java.util.List<hydra.core.Type> otherwise(hydra.core.Type instance) {
          return hydra.lib.lists.Cons.apply(
            (lhs),
            hydra.lib.lists.Cons.apply(
              (rhs),
              (prev)));
        }
        
        @Override
        public java.util.List<hydra.core.Type> visit(hydra.core.Type.Application app2) {
          return (java.util.List<hydra.core.Type>) ((java.util.List) (((java.util.function.Function<hydra.core.ApplicationType, java.util.List<hydra.core.Type>>) ((java.util.function.Function) ((gatherTypes.get()).apply(hydra.lib.lists.Cons.apply(
            (rhs),
            (prev)))))).apply(((app2)).value)));
        }
      });
    })));
    java.util.function.Function<hydra.core.RowType, String> showRowType = (java.util.function.Function<hydra.core.RowType, String>) (rt -> {
      java.util.List<hydra.core.FieldType> flds = ((hydra.core.RowType) ((rt))).fields;
      java.util.List<String> fieldStrs = hydra.lib.lists.Map.apply(
        (hydra.show.core.Core::fieldType),
        (flds));
      return hydra.lib.strings.Cat.apply(java.util.List.of(
        "{",
        hydra.lib.strings.Intercalate.apply(
          ", ",
          (fieldStrs)),
        "}"));
    });
    return ((typ)).accept(new hydra.core.Type.Visitor<>() {
      @Override
      public String visit(hydra.core.Type.Annotated at) {
        return hydra.show.core.Core.type(((hydra.core.AnnotatedType) (((at)).value)).body);
      }
      
      @Override
      public String visit(hydra.core.Type.Application app) {
        java.util.List<hydra.core.Type> types = (java.util.List<hydra.core.Type>) ((java.util.List) (((java.util.function.Function<hydra.core.ApplicationType, java.util.List<hydra.core.Type>>) ((java.util.function.Function) ((gatherTypes.get()).apply((java.util.List<hydra.core.Type>) ((java.util.List) (java.util.List.<hydra.core.Type>of())))))).apply(((app)).value)));
        java.util.List<String> typeStrs = hydra.lib.lists.Map.apply(
          (hydra.show.core.Core::type),
          (types));
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "(",
          hydra.lib.strings.Intercalate.apply(
            " @ ",
            (typeStrs)),
          ")"));
      }
      
      @Override
      public String visit(hydra.core.Type.Either et) {
        hydra.core.Type leftTyp = ((hydra.core.EitherType) (((et)).value)).left;
        hydra.core.Type rightTyp = ((hydra.core.EitherType) (((et)).value)).right;
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "either<",
          hydra.show.core.Core.type((leftTyp)),
          ", ",
          hydra.show.core.Core.type((rightTyp)),
          ">"));
      }
      
      @Override
      public String visit(hydra.core.Type.Forall ft) {
        hydra.core.Type body = ((hydra.core.ForallType) (((ft)).value)).body;
        String var = (((hydra.core.ForallType) (((ft)).value)).parameter).value;
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "(\u2200",
          (var),
          ".",
          hydra.show.core.Core.type((body)),
          ")"));
      }
      
      @Override
      public String visit(hydra.core.Type.Function ft) {
        java.util.List<hydra.core.Type> types = (java.util.List<hydra.core.Type>) ((java.util.List) (((java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>) ((java.util.function.Function) ((gatherFunctionTypes.get()).apply((java.util.List<hydra.core.Type>) ((java.util.List) (java.util.List.<hydra.core.Type>of())))))).apply((typ))));
        java.util.List<String> typeStrs = hydra.lib.lists.Map.apply(
          (hydra.show.core.Core::type),
          (types));
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "(",
          hydra.lib.strings.Intercalate.apply(
            " \u2192 ",
            (typeStrs)),
          ")"));
      }
      
      @Override
      public String visit(hydra.core.Type.List etyp) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "list<",
          hydra.show.core.Core.type(((etyp)).value),
          ">"));
      }
      
      @Override
      public String visit(hydra.core.Type.Literal lt) {
        return hydra.show.core.Core.literalType(((lt)).value);
      }
      
      @Override
      public String visit(hydra.core.Type.Map mt) {
        hydra.core.Type keyTyp = ((hydra.core.MapType) (((mt)).value)).keys;
        hydra.core.Type valTyp = ((hydra.core.MapType) (((mt)).value)).values;
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "map<",
          hydra.show.core.Core.type((keyTyp)),
          ", ",
          hydra.show.core.Core.type((valTyp)),
          ">"));
      }
      
      @Override
      public String visit(hydra.core.Type.Maybe etyp) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "maybe<",
          hydra.show.core.Core.type(((etyp)).value),
          ">"));
      }
      
      @Override
      public String visit(hydra.core.Type.Pair pt) {
        hydra.core.Type firstTyp = ((hydra.core.PairType) (((pt)).value)).first;
        hydra.core.Type secondTyp = ((hydra.core.PairType) (((pt)).value)).second;
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "(",
          hydra.show.core.Core.type((firstTyp)),
          ", ",
          hydra.show.core.Core.type((secondTyp)),
          ")"));
      }
      
      @Override
      public String visit(hydra.core.Type.Product types) {
        java.util.List<String> typeStrs = hydra.lib.lists.Map.apply(
          (hydra.show.core.Core::type),
          ((types)).value);
        return hydra.lib.strings.Intercalate.apply(
          "\u00D7",
          (typeStrs));
      }
      
      @Override
      public String visit(hydra.core.Type.Record rt) {
        return hydra.lib.strings.Cat2.apply(
          "record",
          (String) ((String) (((showRowType)).apply(((rt)).value))));
      }
      
      @Override
      public String visit(hydra.core.Type.Set etyp) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "set<",
          hydra.show.core.Core.type(((etyp)).value),
          ">"));
      }
      
      @Override
      public String visit(hydra.core.Type.Sum types) {
        java.util.List<String> typeStrs = hydra.lib.lists.Map.apply(
          (hydra.show.core.Core::type),
          ((types)).value);
        return hydra.lib.strings.Intercalate.apply(
          "+",
          (typeStrs));
      }
      
      @Override
      public String visit(hydra.core.Type.Union rt) {
        return hydra.lib.strings.Cat2.apply(
          "union",
          (String) ((String) (((showRowType)).apply(((rt)).value))));
      }
      
      @Override
      public String visit(hydra.core.Type.Unit ignored) {
        return "unit";
      }
      
      @Override
      public String visit(hydra.core.Type.Variable name) {
        return (((name)).value).value;
      }
      
      @Override
      public String visit(hydra.core.Type.Wrap wt) {
        String tname = (((hydra.core.WrappedType) (((wt)).value)).typeName).value;
        hydra.core.Type typ1 = ((hydra.core.WrappedType) (((wt)).value)).body;
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "wrap[",
          (tname),
          "](",
          hydra.show.core.Core.type((typ1)),
          ")"));
      }
    });
  }
  
  static String typeScheme(hydra.core.TypeScheme ts) {
    hydra.core.Type body = ((hydra.core.TypeScheme) ((ts))).type;
    java.util.List<hydra.core.Name> vars = ((hydra.core.TypeScheme) ((ts))).variables;
    java.util.List<String> varNames = hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Name, String>) (v1 -> ((v1)).value),
      (vars));
    String fa = hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((vars)),
      "",
      hydra.lib.strings.Cat.apply(java.util.List.of(
        "\u2200[",
        hydra.lib.strings.Intercalate.apply(
          ",",
          (varNames)),
        "].")));
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      "(",
      (fa),
      hydra.show.core.Core.type((body)),
      ")"));
  }
}
