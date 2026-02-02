// Note: this is an automatically generated file. Do not edit.

package hydra.encode.core;

/**
 * Term encoders for hydra.core
 */
public interface Core {
  static hydra.core.Term annotatedTerm(hydra.core.AnnotatedTerm x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.AnnotatedTerm"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("body"), hydra.encode.core.Core.term(((x)).body)),
      new hydra.core.Field(new hydra.core.Name("annotation"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        (hydra.encode.core.Core::name),
        (hydra.encode.core.Core::term),
        ((x)).annotation))))));
  }
  
  static hydra.core.Term annotatedType(hydra.core.AnnotatedType x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.AnnotatedType"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("body"), hydra.encode.core.Core.type(((x)).body)),
      new hydra.core.Field(new hydra.core.Name("annotation"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        (hydra.encode.core.Core::name),
        (hydra.encode.core.Core::term),
        ((x)).annotation))))));
  }
  
  static hydra.core.Term application(hydra.core.Application x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Application"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("function"), hydra.encode.core.Core.term(((x)).function)),
      new hydra.core.Field(new hydra.core.Name("argument"), hydra.encode.core.Core.term(((x)).argument)))));
  }
  
  static hydra.core.Term applicationType(hydra.core.ApplicationType x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.ApplicationType"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("function"), hydra.encode.core.Core.type(((x)).function)),
      new hydra.core.Field(new hydra.core.Name("argument"), hydra.encode.core.Core.type(((x)).argument)))));
  }
  
  static hydra.core.Term binding(hydra.core.Binding x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Binding"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.core.Core.name(((x)).name)),
      new hydra.core.Field(new hydra.core.Name("term"), hydra.encode.core.Core.term(((x)).term)),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        (hydra.encode.core.Core::typeScheme),
        ((x)).type))))));
  }
  
  static hydra.core.Term caseStatement(hydra.core.CaseStatement x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.CaseStatement"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.encode.core.Core.name(((x)).typeName)),
      new hydra.core.Field(new hydra.core.Name("default"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        (hydra.encode.core.Core::term),
        ((x)).default_))),
      new hydra.core.Field(new hydra.core.Name("cases"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.encode.core.Core::field),
        ((x)).cases))))));
  }
  
  static hydra.core.Term eitherType(hydra.core.EitherType x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.EitherType"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("left"), hydra.encode.core.Core.type(((x)).left)),
      new hydra.core.Field(new hydra.core.Name("right"), hydra.encode.core.Core.type(((x)).right)))));
  }
  
  static hydra.core.Term pairType(hydra.core.PairType x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.PairType"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("first"), hydra.encode.core.Core.type(((x)).first)),
      new hydra.core.Field(new hydra.core.Name("second"), hydra.encode.core.Core.type(((x)).second)))));
  }
  
  static hydra.core.Term elimination(hydra.core.Elimination v1) {
    return ((v1)).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.Elimination.Record y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Elimination"), new hydra.core.Field(new hydra.core.Name("record"), hydra.encode.core.Core.projection(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Elimination.Union y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Elimination"), new hydra.core.Field(new hydra.core.Name("union"), hydra.encode.core.Core.caseStatement(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Elimination.Wrap y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Elimination"), new hydra.core.Field(new hydra.core.Name("wrap"), hydra.encode.core.Core.name(((y)).value))));
      }
    });
  }
  
  static hydra.core.Term field(hydra.core.Field x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Field"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.core.Core.name(((x)).name)),
      new hydra.core.Field(new hydra.core.Name("term"), hydra.encode.core.Core.term(((x)).term)))));
  }
  
  static hydra.core.Term fieldType(hydra.core.FieldType x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.FieldType"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.core.Core.name(((x)).name)),
      new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.core.Core.type(((x)).type)))));
  }
  
  static hydra.core.Term floatType(hydra.core.FloatType v1) {
    return ((v1)).accept(new hydra.core.FloatType.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.FloatType.Bigfloat y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.FloatType"), new hydra.core.Field(new hydra.core.Name("bigfloat"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.FloatType.Float32 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.FloatType"), new hydra.core.Field(new hydra.core.Name("float32"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.FloatType.Float64 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.FloatType"), new hydra.core.Field(new hydra.core.Name("float64"), new hydra.core.Term.Unit(true))));
      }
    });
  }
  
  static hydra.core.Term floatValue(hydra.core.FloatValue v1) {
    return ((v1)).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.FloatValue.Bigfloat y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.FloatValue"), new hydra.core.Field(new hydra.core.Name("bigfloat"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.FloatValue.Float32 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.FloatValue"), new hydra.core.Field(new hydra.core.Name("float32"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.FloatValue.Float64 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.FloatValue"), new hydra.core.Field(new hydra.core.Name("float64"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(((y)).value))))));
      }
    });
  }
  
  static hydra.core.Term forallType(hydra.core.ForallType x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.ForallType"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("parameter"), hydra.encode.core.Core.name(((x)).parameter)),
      new hydra.core.Field(new hydra.core.Name("body"), hydra.encode.core.Core.type(((x)).body)))));
  }
  
  static hydra.core.Term function(hydra.core.Function v1) {
    return ((v1)).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.Function.Elimination y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Function"), new hydra.core.Field(new hydra.core.Name("elimination"), hydra.encode.core.Core.elimination(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Function.Lambda y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Function"), new hydra.core.Field(new hydra.core.Name("lambda"), hydra.encode.core.Core.lambda(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Function.Primitive y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Function"), new hydra.core.Field(new hydra.core.Name("primitive"), hydra.encode.core.Core.name(((y)).value))));
      }
    });
  }
  
  static hydra.core.Term functionType(hydra.core.FunctionType x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.FunctionType"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("domain"), hydra.encode.core.Core.type(((x)).domain)),
      new hydra.core.Field(new hydra.core.Name("codomain"), hydra.encode.core.Core.type(((x)).codomain)))));
  }
  
  static hydra.core.Term injection(hydra.core.Injection x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Injection"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.encode.core.Core.name(((x)).typeName)),
      new hydra.core.Field(new hydra.core.Name("field"), hydra.encode.core.Core.field(((x)).field)))));
  }
  
  static hydra.core.Term integerType(hydra.core.IntegerType v1) {
    return ((v1)).accept(new hydra.core.IntegerType.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Bigint y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("bigint"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Int8 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("int8"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Int16 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("int16"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Int32 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("int32"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Int64 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("int64"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Uint8 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("uint8"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Uint16 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("uint16"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Uint32 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("uint32"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Uint64 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("uint64"), new hydra.core.Term.Unit(true))));
      }
    });
  }
  
  static hydra.core.Term integerValue(hydra.core.IntegerValue v1) {
    return ((v1)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Bigint y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("bigint"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Bigint(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Int8 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("int8"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int8(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Int16 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("int16"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int16(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Int32 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("int32"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Int64 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("int64"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Uint8 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("uint8"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint8(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Uint16 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("uint16"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint16(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Uint32 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("uint32"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint32(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Uint64 y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("uint64"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint64(((y)).value))))));
      }
    });
  }
  
  static hydra.core.Term lambda(hydra.core.Lambda x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Lambda"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("parameter"), hydra.encode.core.Core.name(((x)).parameter)),
      new hydra.core.Field(new hydra.core.Name("domain"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        (hydra.encode.core.Core::type),
        ((x)).domain))),
      new hydra.core.Field(new hydra.core.Name("body"), hydra.encode.core.Core.term(((x)).body)))));
  }
  
  static hydra.core.Term let(hydra.core.Let x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Let"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("bindings"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.encode.core.Core::binding),
        ((x)).bindings))),
      new hydra.core.Field(new hydra.core.Name("body"), hydra.encode.core.Core.term(((x)).body)))));
  }
  
  static hydra.core.Term literal(hydra.core.Literal v1) {
    return ((v1)).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.Literal.Binary y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("binary"), new hydra.core.Term.Literal(new hydra.core.Literal.Binary(((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Literal.Boolean_ y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("boolean"), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Literal.Float_ y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("float"), hydra.encode.core.Core.floatValue(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Literal.Integer_ y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("integer"), hydra.encode.core.Core.integerValue(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Literal.String_ y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("string"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((y)).value)))));
      }
    });
  }
  
  static hydra.core.Term literalType(hydra.core.LiteralType v1) {
    return ((v1)).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Binary y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.LiteralType"), new hydra.core.Field(new hydra.core.Name("binary"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Boolean_ y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.LiteralType"), new hydra.core.Field(new hydra.core.Name("boolean"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Float_ y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.LiteralType"), new hydra.core.Field(new hydra.core.Name("float"), hydra.encode.core.Core.floatType(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Integer_ y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.LiteralType"), new hydra.core.Field(new hydra.core.Name("integer"), hydra.encode.core.Core.integerType(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.String_ y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.LiteralType"), new hydra.core.Field(new hydra.core.Name("string"), new hydra.core.Term.Unit(true))));
      }
    });
  }
  
  static hydra.core.Term mapType(hydra.core.MapType x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.MapType"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("keys"), hydra.encode.core.Core.type(((x)).keys)),
      new hydra.core.Field(new hydra.core.Name("values"), hydra.encode.core.Core.type(((x)).values)))));
  }
  
  static hydra.core.Term name(hydra.core.Name x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).value))));
  }
  
  static hydra.core.Term projection(hydra.core.Projection x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Projection"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.encode.core.Core.name(((x)).typeName)),
      new hydra.core.Field(new hydra.core.Name("field"), hydra.encode.core.Core.name(((x)).field)))));
  }
  
  static hydra.core.Term record(hydra.core.Record x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Record"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.encode.core.Core.name(((x)).typeName)),
      new hydra.core.Field(new hydra.core.Name("fields"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.encode.core.Core::field),
        ((x)).fields))))));
  }
  
  static hydra.core.Term rowType(hydra.core.RowType x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.RowType"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.encode.core.Core.name(((x)).typeName)),
      new hydra.core.Field(new hydra.core.Name("fields"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.encode.core.Core::fieldType),
        ((x)).fields))))));
  }
  
  static hydra.core.Term term(hydra.core.Term v1) {
    return ((v1)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("annotated"), hydra.encode.core.Core.annotatedTerm(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Application y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("application"), hydra.encode.core.Core.application(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Either y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("either"), new hydra.core.Term.Either(hydra.lib.eithers.Bimap.apply(
          (hydra.encode.core.Core::term),
          (hydra.encode.core.Core::term),
          ((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Function y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("function"), hydra.encode.core.Core.function(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Let y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("let"), hydra.encode.core.Core.let(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.List y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("list"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (hydra.encode.core.Core::term),
          ((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Literal y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("literal"), hydra.encode.core.Core.literal(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Map y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("map"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
          (hydra.encode.core.Core::term),
          (hydra.encode.core.Core::term),
          ((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Maybe y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("maybe"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
          (hydra.encode.core.Core::term),
          ((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Pair y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("pair"), new hydra.core.Term.Pair(hydra.lib.pairs.Bimap.apply(
          (hydra.encode.core.Core::term),
          (hydra.encode.core.Core::term),
          ((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Record y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("record"), hydra.encode.core.Core.record(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Set y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("set"), new hydra.core.Term.Set(hydra.lib.sets.Map.apply(
          (hydra.encode.core.Core::term),
          ((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("typeApplication"), hydra.encode.core.Core.typeApplicationTerm(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("typeLambda"), hydra.encode.core.Core.typeLambda(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Union y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("union"), hydra.encode.core.Core.injection(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Unit y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("unit"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("variable"), hydra.encode.core.Core.name(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Wrap y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("wrap"), hydra.encode.core.Core.wrappedTerm(((y)).value))));
      }
    });
  }
  
  static hydra.core.Term type(hydra.core.Type v1) {
    return ((v1)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.Type.Annotated y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("annotated"), hydra.encode.core.Core.annotatedType(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Application y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("application"), hydra.encode.core.Core.applicationType(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Either y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("either"), hydra.encode.core.Core.eitherType(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Forall y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("forall"), hydra.encode.core.Core.forallType(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Function y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("function"), hydra.encode.core.Core.functionType(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.List y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("list"), hydra.encode.core.Core.type(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Literal y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("literal"), hydra.encode.core.Core.literalType(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Map y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("map"), hydra.encode.core.Core.mapType(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Maybe y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("maybe"), hydra.encode.core.Core.type(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Pair y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("pair"), hydra.encode.core.Core.pairType(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Record y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("record"), hydra.encode.core.Core.rowType(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Set y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("set"), hydra.encode.core.Core.type(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Union y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("union"), hydra.encode.core.Core.rowType(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Unit y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("unit"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Variable y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("variable"), hydra.encode.core.Core.name(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Wrap y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("wrap"), hydra.encode.core.Core.wrappedType(((y)).value))));
      }
    });
  }
  
  static hydra.core.Term typeApplicationTerm(hydra.core.TypeApplicationTerm x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.TypeApplicationTerm"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("body"), hydra.encode.core.Core.term(((x)).body)),
      new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.core.Core.type(((x)).type)))));
  }
  
  static hydra.core.Term typeLambda(hydra.core.TypeLambda x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.TypeLambda"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("parameter"), hydra.encode.core.Core.name(((x)).parameter)),
      new hydra.core.Field(new hydra.core.Name("body"), hydra.encode.core.Core.term(((x)).body)))));
  }
  
  static hydra.core.Term typeScheme(hydra.core.TypeScheme x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.TypeScheme"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("variables"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.encode.core.Core::name),
        ((x)).variables))),
      new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.core.Core.type(((x)).type)),
      new hydra.core.Field(new hydra.core.Name("constraints"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, hydra.core.Term>) (m -> new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
          (hydra.encode.core.Core::name),
          (hydra.encode.core.Core::typeVariableMetadata),
          (m)))),
        ((x)).constraints))))));
  }
  
  static hydra.core.Term typeVariableMetadata(hydra.core.TypeVariableMetadata x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.TypeVariableMetadata"), java.util.List.of(new hydra.core.Field(new hydra.core.Name("classes"), new hydra.core.Term.Set(hydra.lib.sets.Map.apply(
      (hydra.encode.core.Core::name),
      ((x)).classes))))));
  }
  
  static hydra.core.Term wrappedTerm(hydra.core.WrappedTerm x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.WrappedTerm"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.encode.core.Core.name(((x)).typeName)),
      new hydra.core.Field(new hydra.core.Name("body"), hydra.encode.core.Core.term(((x)).body)))));
  }
  
  static hydra.core.Term wrappedType(hydra.core.WrappedType x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.WrappedType"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.encode.core.Core.name(((x)).typeName)),
      new hydra.core.Field(new hydra.core.Name("body"), hydra.encode.core.Core.type(((x)).body)))));
  }
}
