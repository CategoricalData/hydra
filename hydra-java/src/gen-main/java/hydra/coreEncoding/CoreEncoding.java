// Note: this is an automatically generated file. Do not edit.

package hydra.coreEncoding;

/**
 * Mapping of hydra/core constructs in a host language like Haskell or Java  to their native Hydra counterparts as terms.  This includes an implementation of LambdaGraph's epsilon encoding (types to terms).
 */
public interface CoreEncoding {
  static hydra.core.Term coreEncodeAnnotatedTerm(hydra.core.AnnotatedTerm a) {
    return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.coreEncoding.CoreEncoding.coreEncodeTerm(((a)).subject), ((a)).annotation));
  }
  
  static hydra.core.Term coreEncodeAnnotatedType(hydra.core.AnnotatedType at) {
    return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.coreEncoding.CoreEncoding.coreEncodeType(((at)).subject), ((at)).annotation));
  }
  
  static hydra.core.Term coreEncodeApplication(hydra.core.Application app) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Application"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("function"), hydra.coreEncoding.CoreEncoding.coreEncodeTerm(((app)).function)),
      new hydra.core.Field(new hydra.core.Name("argument"), hydra.coreEncoding.CoreEncoding.coreEncodeTerm(((app)).argument)))));
  }
  
  static hydra.core.Term coreEncodeApplicationType(hydra.core.ApplicationType at) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.ApplicationType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("function"), hydra.coreEncoding.CoreEncoding.coreEncodeType(((at)).function)),
      new hydra.core.Field(new hydra.core.Name("argument"), hydra.coreEncoding.CoreEncoding.coreEncodeType(((at)).argument)))));
  }
  
  static hydra.core.Term coreEncodeCaseStatement(hydra.core.CaseStatement cs) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.CaseStatement"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.coreEncoding.CoreEncoding.coreEncodeName(((cs)).typeName)),
      new hydra.core.Field(new hydra.core.Name("default"), new hydra.core.Term.Optional(hydra.lib.optionals.Map.apply(
        (hydra.coreEncoding.CoreEncoding::coreEncodeTerm),
        ((cs)).default_))),
      new hydra.core.Field(new hydra.core.Name("cases"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.coreEncoding.CoreEncoding::coreEncodeField),
        ((cs)).cases))))));
  }
  
  static hydra.core.Term coreEncodeElimination(hydra.core.Elimination v1) {
    return ((v1)).accept(new hydra.core.Elimination.Visitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.Elimination.List instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Elimination"), new hydra.core.Field(new hydra.core.Name("list"), hydra.coreEncoding.CoreEncoding.coreEncodeTerm((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Elimination.Optional instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Elimination"), new hydra.core.Field(new hydra.core.Name("optional"), hydra.coreEncoding.CoreEncoding.coreEncodeOptionalCases((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Elimination.Product instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Elimination"), new hydra.core.Field(new hydra.core.Name("product"), hydra.coreEncoding.CoreEncoding.coreEncodeTupleProjection((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Elimination.Record instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Elimination"), new hydra.core.Field(new hydra.core.Name("record"), hydra.coreEncoding.CoreEncoding.coreEncodeProjection((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Elimination.Union instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Elimination"), new hydra.core.Field(new hydra.core.Name("union"), hydra.coreEncoding.CoreEncoding.coreEncodeCaseStatement((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Elimination.Wrap instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Elimination"), new hydra.core.Field(new hydra.core.Name("wrap"), hydra.coreEncoding.CoreEncoding.coreEncodeName((instance.value)))));
      }
    });
  }
  
  static hydra.core.Term coreEncodeField(hydra.core.Field f) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Field"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra/core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((((f)).name).value))))),
      new hydra.core.Field(new hydra.core.Name("term"), hydra.coreEncoding.CoreEncoding.coreEncodeTerm(((f)).term)))));
  }
  
  static hydra.core.Term coreEncodeFieldType(hydra.core.FieldType ft) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.FieldType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.coreEncoding.CoreEncoding.coreEncodeName(((ft)).name)),
      new hydra.core.Field(new hydra.core.Name("type"), hydra.coreEncoding.CoreEncoding.coreEncodeType(((ft)).type)))));
  }
  
  static hydra.core.Term coreEncodeFloatType(hydra.core.FloatType v1) {
    return ((v1)).accept(new hydra.core.FloatType.Visitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.FloatType.Bigfloat instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.FloatType"), new hydra.core.Field(new hydra.core.Name("bigfloat"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.FloatType.Float32 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.FloatType"), new hydra.core.Field(new hydra.core.Name("float32"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.FloatType.Float64 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.FloatType"), new hydra.core.Field(new hydra.core.Name("float64"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())))));
      }
    });
  }
  
  static hydra.core.Term coreEncodeFloatValue(hydra.core.FloatValue v1) {
    return ((v1)).accept(new hydra.core.FloatValue.Visitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.FloatValue.Bigfloat instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.FloatValue"), new hydra.core.Field(new hydra.core.Name("bigfloat"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat((instance.value)))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.FloatValue.Float32 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.FloatValue"), new hydra.core.Field(new hydra.core.Name("float32"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((instance.value)))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.FloatValue.Float64 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.FloatValue"), new hydra.core.Field(new hydra.core.Name("float64"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64((instance.value)))))));
      }
    });
  }
  
  static hydra.core.Term coreEncodeFunction(hydra.core.Function v1) {
    return ((v1)).accept(new hydra.core.Function.Visitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.Function.Elimination instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Function"), new hydra.core.Field(new hydra.core.Name("elimination"), hydra.coreEncoding.CoreEncoding.coreEncodeElimination((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Function.Lambda instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Function"), new hydra.core.Field(new hydra.core.Name("lambda"), hydra.coreEncoding.CoreEncoding.coreEncodeLambda((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Function.Primitive instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Function"), new hydra.core.Field(new hydra.core.Name("primitive"), hydra.coreEncoding.CoreEncoding.coreEncodeName((instance.value)))));
      }
    });
  }
  
  static hydra.core.Term coreEncodeFunctionType(hydra.core.FunctionType ft) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.FunctionType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("domain"), hydra.coreEncoding.CoreEncoding.coreEncodeType(((ft)).domain)),
      new hydra.core.Field(new hydra.core.Name("codomain"), hydra.coreEncoding.CoreEncoding.coreEncodeType(((ft)).codomain)))));
  }
  
  static hydra.core.Term coreEncodeInjection(hydra.core.Injection i) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Injection"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.coreEncoding.CoreEncoding.coreEncodeName(((i)).typeName)),
      new hydra.core.Field(new hydra.core.Name("field"), hydra.coreEncoding.CoreEncoding.coreEncodeField(((i)).field)))));
  }
  
  static hydra.core.Term coreEncodeIntegerType(hydra.core.IntegerType v1) {
    return ((v1)).accept(new hydra.core.IntegerType.Visitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Bigint instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerType"), new hydra.core.Field(new hydra.core.Name("bigint"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Int8 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerType"), new hydra.core.Field(new hydra.core.Name("int8"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Int16 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerType"), new hydra.core.Field(new hydra.core.Name("int16"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Int32 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerType"), new hydra.core.Field(new hydra.core.Name("int32"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Int64 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerType"), new hydra.core.Field(new hydra.core.Name("int64"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Uint8 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerType"), new hydra.core.Field(new hydra.core.Name("uint8"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Uint16 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerType"), new hydra.core.Field(new hydra.core.Name("uint16"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Uint32 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerType"), new hydra.core.Field(new hydra.core.Name("uint32"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerType.Uint64 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerType"), new hydra.core.Field(new hydra.core.Name("uint64"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())))));
      }
    });
  }
  
  static hydra.core.Term coreEncodeIntegerValue(hydra.core.IntegerValue v1) {
    return ((v1)).accept(new hydra.core.IntegerValue.Visitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Bigint instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("bigint"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Bigint((instance.value)))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Int8 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("int8"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int8((instance.value)))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Int16 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("int16"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int16((instance.value)))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Int32 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("int32"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32((instance.value)))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Int64 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("int64"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((instance.value)))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Uint8 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("uint8"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint8((instance.value)))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Uint16 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("uint16"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint16((instance.value)))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Uint32 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("uint32"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint32((instance.value)))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.IntegerValue.Uint64 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("uint64"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint64((instance.value)))))));
      }
    });
  }
  
  static hydra.core.Term coreEncodeLambda(hydra.core.Lambda l) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Lambda"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("parameter"), hydra.coreEncoding.CoreEncoding.coreEncodeName(((l)).parameter)),
      new hydra.core.Field(new hydra.core.Name("body"), hydra.coreEncoding.CoreEncoding.coreEncodeTerm(((l)).body)))));
  }
  
  static hydra.core.Term coreEncodeLambdaType(hydra.core.LambdaType lt) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.LambdaType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("parameter"), hydra.coreEncoding.CoreEncoding.coreEncodeName(((lt)).parameter)),
      new hydra.core.Field(new hydra.core.Name("body"), hydra.coreEncoding.CoreEncoding.coreEncodeType(((lt)).body)))));
  }
  
  static hydra.core.Term coreEncodeLet(hydra.core.Let l) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Let"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("bindings"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.coreEncoding.CoreEncoding::coreEncodeLetBinding),
        ((l)).bindings))),
      new hydra.core.Field(new hydra.core.Name("environment"), hydra.coreEncoding.CoreEncoding.coreEncodeTerm(((l)).environment)))));
  }
  
  static hydra.core.Term coreEncodeLetBinding(hydra.core.LetBinding b) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.LetBinding"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.coreEncoding.CoreEncoding.coreEncodeName(((b)).name)),
      new hydra.core.Field(new hydra.core.Name("term"), hydra.coreEncoding.CoreEncoding.coreEncodeTerm(((b)).term)),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Optional(hydra.lib.optionals.Map.apply(
        (hydra.coreEncoding.CoreEncoding::coreEncodeTypeScheme),
        ((b)).type))))));
  }
  
  static hydra.core.Term coreEncodeLiteral(hydra.core.Literal v1) {
    return ((v1)).accept(new hydra.core.Literal.Visitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.Literal.Binary instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Literal"), new hydra.core.Field(new hydra.core.Name("binary"), new hydra.core.Term.Literal(new hydra.core.Literal.Binary((instance.value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Literal.Boolean_ instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Literal"), new hydra.core.Field(new hydra.core.Name("boolean"), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_((instance.value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Literal.Float_ instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Literal"), new hydra.core.Field(new hydra.core.Name("float"), hydra.coreEncoding.CoreEncoding.coreEncodeFloatValue((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Literal.Integer_ instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Literal"), new hydra.core.Field(new hydra.core.Name("integer"), hydra.coreEncoding.CoreEncoding.coreEncodeIntegerValue((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Literal.String_ instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Literal"), new hydra.core.Field(new hydra.core.Name("string"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((instance.value))))));
      }
    });
  }
  
  static hydra.core.Term coreEncodeLiteralType(hydra.core.LiteralType v1) {
    return ((v1)).accept(new hydra.core.LiteralType.Visitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Binary instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.LiteralType"), new hydra.core.Field(new hydra.core.Name("binary"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Boolean_ instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.LiteralType"), new hydra.core.Field(new hydra.core.Name("boolean"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Float_ instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.LiteralType"), new hydra.core.Field(new hydra.core.Name("float"), hydra.coreEncoding.CoreEncoding.coreEncodeFloatType((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Integer_ instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.LiteralType"), new hydra.core.Field(new hydra.core.Name("integer"), hydra.coreEncoding.CoreEncoding.coreEncodeIntegerType((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.String_ instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.LiteralType"), new hydra.core.Field(new hydra.core.Name("string"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList())))));
      }
    });
  }
  
  static hydra.core.Term coreEncodeMapType(hydra.core.MapType mt) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.MapType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("keys"), hydra.coreEncoding.CoreEncoding.coreEncodeType(((mt)).keys)),
      new hydra.core.Field(new hydra.core.Name("values"), hydra.coreEncoding.CoreEncoding.coreEncodeType(((mt)).values)))));
  }
  
  static hydra.core.Term coreEncodeName(hydra.core.Name fn) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra/core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((fn)).value))));
  }
  
  static hydra.core.Term coreEncodeOptionalCases(hydra.core.OptionalCases oc) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.OptionalCases"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("nothing"), hydra.coreEncoding.CoreEncoding.coreEncodeTerm(((oc)).nothing)),
      new hydra.core.Field(new hydra.core.Name("just"), hydra.coreEncoding.CoreEncoding.coreEncodeTerm(((oc)).just)))));
  }
  
  static hydra.core.Term coreEncodeProjection(hydra.core.Projection p) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Projection"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.coreEncoding.CoreEncoding.coreEncodeName(((p)).typeName)),
      new hydra.core.Field(new hydra.core.Name("field"), hydra.coreEncoding.CoreEncoding.coreEncodeName(((p)).field)))));
  }
  
  static hydra.core.Term coreEncodeRecord(hydra.core.Record r) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Record"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.coreEncoding.CoreEncoding.coreEncodeName(((r)).typeName)),
      new hydra.core.Field(new hydra.core.Name("fields"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.coreEncoding.CoreEncoding::coreEncodeField),
        ((r)).fields))))));
  }
  
  static hydra.core.Term coreEncodeRowType(hydra.core.RowType rt) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.RowType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.coreEncoding.CoreEncoding.coreEncodeName(((rt)).typeName)),
      new hydra.core.Field(new hydra.core.Name("extends"), new hydra.core.Term.Optional(hydra.lib.optionals.Map.apply(
        (hydra.coreEncoding.CoreEncoding::coreEncodeName),
        ((rt)).extends_))),
      new hydra.core.Field(new hydra.core.Name("fields"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.coreEncoding.CoreEncoding::coreEncodeFieldType),
        ((rt)).fields))))));
  }
  
  static hydra.core.Term coreEncodeSum(hydra.core.Sum s) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Sum"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("index"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((s)).index)))),
      new hydra.core.Field(new hydra.core.Name("size"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((s)).size)))),
      new hydra.core.Field(new hydra.core.Name("term"), hydra.coreEncoding.CoreEncoding.coreEncodeTerm(((s)).term)))));
  }
  
  static hydra.core.Term coreEncodeTerm(hydra.core.Term v1) {
    return ((v1)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return new hydra.core.Term.Literal(new hydra.core.Literal.String_("not implemented"));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.Name("annotated"), hydra.coreEncoding.CoreEncoding.coreEncodeAnnotatedTerm((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Application instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.Name("application"), hydra.coreEncoding.CoreEncoding.coreEncodeApplication((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Function instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.Name("function"), hydra.coreEncoding.CoreEncoding.coreEncodeFunction((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Let instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.Name("let"), hydra.coreEncoding.CoreEncoding.coreEncodeLet((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Literal instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.Name("literal"), hydra.coreEncoding.CoreEncoding.coreEncodeLiteral((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.List instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.Name("list"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (hydra.coreEncoding.CoreEncoding::coreEncodeTerm),
          (instance.value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Optional instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.Name("optional"), new hydra.core.Term.Optional(hydra.lib.optionals.Map.apply(
          (hydra.coreEncoding.CoreEncoding::coreEncodeTerm),
          (instance.value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Product instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.Name("product"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (hydra.coreEncoding.CoreEncoding::coreEncodeTerm),
          (instance.value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Record instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.Name("record"), hydra.coreEncoding.CoreEncoding.coreEncodeRecord((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Set instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.Name("set"), new hydra.core.Term.Set(hydra.lib.sets.Map.apply(
          (hydra.coreEncoding.CoreEncoding::coreEncodeTerm),
          (instance.value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Sum instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.Name("sum"), hydra.coreEncoding.CoreEncoding.coreEncodeSum((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Union instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.Name("union"), hydra.coreEncoding.CoreEncoding.coreEncodeInjection((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.Name("variable"), hydra.coreEncoding.CoreEncoding.coreEncodeName((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Wrap instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.Name("wrap"), hydra.coreEncoding.CoreEncoding.coreEncodeWrappedTerm((instance.value)))));
      }
    });
  }
  
  static hydra.core.Term coreEncodeTupleProjection(hydra.core.TupleProjection tp) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.TupleProjection"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("arity"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((tp)).arity)))),
      new hydra.core.Field(new hydra.core.Name("index"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((tp)).index)))))));
  }
  
  static hydra.core.Term coreEncodeType(hydra.core.Type v1) {
    return ((v1)).accept(new hydra.core.Type.Visitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.Type.Annotated instance) {
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.coreEncoding.CoreEncoding.coreEncodeType(((instance.value)).subject), ((instance.value)).annotation));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Application instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Type"), new hydra.core.Field(new hydra.core.Name("application"), hydra.coreEncoding.CoreEncoding.coreEncodeApplicationType((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Function instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Type"), new hydra.core.Field(new hydra.core.Name("function"), hydra.coreEncoding.CoreEncoding.coreEncodeFunctionType((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Lambda instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Type"), new hydra.core.Field(new hydra.core.Name("lambda"), hydra.coreEncoding.CoreEncoding.coreEncodeLambdaType((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.List instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Type"), new hydra.core.Field(new hydra.core.Name("list"), hydra.coreEncoding.CoreEncoding.coreEncodeType((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Literal instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Type"), new hydra.core.Field(new hydra.core.Name("literal"), hydra.coreEncoding.CoreEncoding.coreEncodeLiteralType((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Map instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Type"), new hydra.core.Field(new hydra.core.Name("map"), hydra.coreEncoding.CoreEncoding.coreEncodeMapType((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Optional instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Type"), new hydra.core.Field(new hydra.core.Name("optional"), hydra.coreEncoding.CoreEncoding.coreEncodeType((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Product instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Type"), new hydra.core.Field(new hydra.core.Name("product"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (hydra.coreEncoding.CoreEncoding::coreEncodeType),
          (instance.value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Record instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Type"), new hydra.core.Field(new hydra.core.Name("record"), hydra.coreEncoding.CoreEncoding.coreEncodeRowType((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Set instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Type"), new hydra.core.Field(new hydra.core.Name("set"), hydra.coreEncoding.CoreEncoding.coreEncodeType((instance.value)))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Sum instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Type"), new hydra.core.Field(new hydra.core.Name("sum"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (hydra.coreEncoding.CoreEncoding::coreEncodeType),
          (instance.value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Union instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Type"), new hydra.core.Field(new hydra.core.Name("union"), hydra.coreEncoding.CoreEncoding.coreEncodeRowType((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Variable instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Type"), new hydra.core.Field(new hydra.core.Name("variable"), hydra.coreEncoding.CoreEncoding.coreEncodeName((instance.value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Wrap instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Type"), new hydra.core.Field(new hydra.core.Name("wrap"), hydra.coreEncoding.CoreEncoding.coreEncodeWrappedType((instance.value)))));
      }
    });
  }
  
  static hydra.core.Term coreEncodeTypeScheme(hydra.core.TypeScheme ts) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.TypeScheme"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("variables"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.coreEncoding.CoreEncoding::coreEncodeName),
        ((ts)).variables))),
      new hydra.core.Field(new hydra.core.Name("type"), hydra.coreEncoding.CoreEncoding.coreEncodeType(((ts)).type)))));
  }
  
  static hydra.core.Term coreEncodeWrappedTerm(hydra.core.WrappedTerm n) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.WrappedTerm"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.coreEncoding.CoreEncoding.coreEncodeName(((n)).typeName)),
      new hydra.core.Field(new hydra.core.Name("object"), hydra.coreEncoding.CoreEncoding.coreEncodeTerm(((n)).object)))));
  }
  
  static hydra.core.Term coreEncodeWrappedType(hydra.core.WrappedType nt) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.WrappedType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.coreEncoding.CoreEncoding.coreEncodeName(((nt)).typeName)),
      new hydra.core.Field(new hydra.core.Name("object"), hydra.coreEncoding.CoreEncoding.coreEncodeType(((nt)).object)))));
  }
}