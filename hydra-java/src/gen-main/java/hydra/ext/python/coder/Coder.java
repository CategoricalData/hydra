// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.coder;

/**
 * Python code generator: converts Hydra modules to Python source code
 */
public interface Coder {
  static Boolean useInlineTypeParamsFor(hydra.ext.python.helpers.PythonVersion version) {
    return hydra.lib.equality.Equal.apply(
      version,
      new hydra.ext.python.helpers.PythonVersion.Python312());
  }
  
  static Boolean useInlineTypeParams() {
    return hydra.ext.python.coder.Coder.useInlineTypeParamsFor(hydra.ext.python.utils.Utils.targetPythonVersion());
  }
  
  static hydra.ext.python.syntax.Statement typeAliasStatementFor(hydra.ext.python.helpers.PythonEnvironment env, hydra.ext.python.syntax.Name name, java.util.List<hydra.ext.python.syntax.TypeParameter> tparams, hydra.util.Maybe<String> mcomment, hydra.ext.python.syntax.Expression tyexpr) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.ext.python.coder.Coder.useInlineTypeParamsFor((env).version),
      () -> hydra.ext.python.utils.Utils.typeAliasStatement(
        name,
        tparams,
        mcomment,
        tyexpr),
      () -> hydra.ext.python.utils.Utils.typeAliasStatement310(
        name,
        tparams,
        mcomment,
        tyexpr));
  }
  
  static java.util.List<hydra.ext.python.syntax.Statement> unionTypeStatementsFor(hydra.ext.python.helpers.PythonEnvironment env, hydra.ext.python.syntax.Name name, java.util.List<hydra.ext.python.syntax.TypeParameter> tparams, hydra.util.Maybe<String> mcomment, hydra.ext.python.syntax.Expression tyexpr) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.ext.python.coder.Coder.useInlineTypeParamsFor((env).version),
      () -> java.util.List.of(hydra.ext.python.utils.Utils.typeAliasStatement(
        name,
        tparams,
        mcomment,
        tyexpr)),
      () -> hydra.ext.python.utils.Utils.unionTypeClassStatements310(
        name,
        mcomment,
        tyexpr));
  }
  
  static hydra.ext.python.syntax.Expression wrapInNullaryLambda(hydra.ext.python.syntax.Expression expr) {
    return new hydra.ext.python.syntax.Expression.Lambda(new hydra.ext.python.syntax.Lambda(new hydra.ext.python.syntax.LambdaParameters((hydra.util.Maybe<hydra.ext.python.syntax.LambdaSlashNoDefault>) (hydra.util.Maybe.<hydra.ext.python.syntax.LambdaSlashNoDefault>nothing()), (java.util.List<hydra.ext.python.syntax.LambdaParamNoDefault>) (java.util.List.<hydra.ext.python.syntax.LambdaParamNoDefault>of()), (java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault>) (java.util.List.<hydra.ext.python.syntax.LambdaParamWithDefault>of()), (hydra.util.Maybe<hydra.ext.python.syntax.LambdaStarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.LambdaStarEtc>nothing())), expr));
  }
  
  static java.util.List<hydra.ext.python.syntax.Expression> wrapLazyArguments(hydra.core.Name name, java.util.List<hydra.ext.python.syntax.Expression> args) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        hydra.lib.equality.Equal.apply(
          name,
          new hydra.core.Name("hydra.lib.logic.ifElse")),
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(args),
          3)),
      () -> java.util.List.of(
        hydra.lib.lists.At.apply(
          0,
          args),
        hydra.ext.python.coder.Coder.wrapInNullaryLambda(hydra.lib.lists.At.apply(
          1,
          args)),
        hydra.ext.python.coder.Coder.wrapInNullaryLambda(hydra.lib.lists.At.apply(
          2,
          args))),
      () -> args);
  }
  
  static hydra.ext.python.syntax.Expression pyInt(java.math.BigInteger n) {
    return hydra.ext.python.utils.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Number_(new hydra.ext.python.syntax.Number_.Integer_(n)));
  }
  
  static hydra.ext.python.syntax.NamedExpression lruCacheDecorator() {
    return new hydra.ext.python.syntax.NamedExpression.Simple(hydra.ext.python.utils.Utils.functionCall(
      new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("lru_cache"))),
      java.util.List.of(hydra.ext.python.coder.Coder.pyInt(new java.math.BigInteger("1")))));
  }
  
  static hydra.ext.python.syntax.Expression makeThunk(hydra.ext.python.syntax.Expression pbody) {
    return hydra.ext.python.utils.Utils.functionCall(
      hydra.ext.python.utils.Utils.pyExpressionToPyPrimary(hydra.ext.python.utils.Utils.functionCall(
        new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("lru_cache"))),
        java.util.List.of(hydra.ext.python.coder.Coder.pyInt(new java.math.BigInteger("1"))))),
      java.util.List.of(hydra.ext.python.coder.Coder.wrapInNullaryLambda(pbody)));
  }
  
  static hydra.ext.python.syntax.Expression makeCurriedLambda(java.util.List<hydra.ext.python.syntax.Name> params, hydra.ext.python.syntax.Expression body) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.ext.python.syntax.Expression, java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ext.python.syntax.Expression>>) (acc -> (java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ext.python.syntax.Expression>) (p -> new hydra.ext.python.syntax.Expression.Lambda(new hydra.ext.python.syntax.Lambda(new hydra.ext.python.syntax.LambdaParameters((hydra.util.Maybe<hydra.ext.python.syntax.LambdaSlashNoDefault>) (hydra.util.Maybe.<hydra.ext.python.syntax.LambdaSlashNoDefault>nothing()), java.util.List.of(new hydra.ext.python.syntax.LambdaParamNoDefault(p)), (java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault>) (java.util.List.<hydra.ext.python.syntax.LambdaParamWithDefault>of()), (hydra.util.Maybe<hydra.ext.python.syntax.LambdaStarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.LambdaStarEtc>nothing())), acc)))),
      body,
      hydra.lib.lists.Reverse.apply(params));
  }
  
  static hydra.util.Maybe<hydra.ext.python.syntax.Expression> genericArg(java.util.List<hydra.core.Name> tparamList) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(tparamList),
      () -> (hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing()),
      () -> hydra.util.Maybe.just(hydra.ext.python.utils.Utils.pyPrimaryToPyExpression(hydra.ext.python.utils.Utils.primaryWithExpressionSlices(
        new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("Generic"))),
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Name, hydra.ext.python.syntax.Expression>) (n -> new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(hydra.ext.python.names.Names.encodeTypeVariable(n)))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of()))))))))),
          tparamList)))));
  }
  
  static hydra.ext.python.syntax.Args variantArgs(hydra.ext.python.syntax.Expression ptype, java.util.List<hydra.core.Name> tparams) {
    return hydra.ext.python.utils.Utils.pyExpressionsToPyArgs(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.ext.python.utils.Utils.pyPrimaryToPyExpression(hydra.ext.python.utils.Utils.primaryWithExpressionSlices(
        new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("Node"))),
        java.util.List.of(ptype)))),
      hydra.ext.python.coder.Coder.genericArg(tparams))));
  }
  
  static java.util.List<hydra.ext.python.syntax.TypeParameter> environmentTypeParameters(hydra.ext.python.helpers.PythonEnvironment env) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.ext.python.syntax.TypeParameter>) (arg_ -> hydra.ext.python.utils.Utils.pyNameToPyTypeParameter(hydra.ext.python.names.Names.encodeTypeVariable(arg_))),
      hydra.lib.pairs.First.apply((env).boundTypeVariables));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> encodeFloatValue(hydra.core.FloatValue fv) {
    return (fv).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.FloatValue.Bigfloat f) {
        return hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.functionCall(
          hydra.ext.python.utils.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("Decimal")),
          java.util.List.of(hydra.ext.python.utils.Utils.singleQuotedString(hydra.lib.literals.ShowBigfloat.apply((f).value)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.FloatValue.Float32 f) {
        return hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Number_(new hydra.ext.python.syntax.Number_.Float_(hydra.lib.literals.Float32ToBigfloat.apply((f).value)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.FloatValue.Float64 f) {
        return hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Number_(new hydra.ext.python.syntax.Number_.Float_(hydra.lib.literals.Float64ToBigfloat.apply((f).value)))));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> encodeIntegerValue(hydra.core.IntegerValue iv) {
    return (iv).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Bigint i) {
        return ((java.util.function.Function<java.math.BigInteger, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (v1 -> hydra.ext.python.coder.Coder.encodeIntegerValue_toPyInt(
          hydra.ext.python.utils.Utils::pyAtomToPyExpression,
          v1))).apply((i).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Int8 i) {
        return ((java.util.function.Function<java.math.BigInteger, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (v1 -> hydra.ext.python.coder.Coder.encodeIntegerValue_toPyInt(
          hydra.ext.python.utils.Utils::pyAtomToPyExpression,
          v1))).apply(hydra.lib.literals.Int8ToBigint.apply((i).value));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Int16 i) {
        return ((java.util.function.Function<java.math.BigInteger, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (v1 -> hydra.ext.python.coder.Coder.encodeIntegerValue_toPyInt(
          hydra.ext.python.utils.Utils::pyAtomToPyExpression,
          v1))).apply(hydra.lib.literals.Int16ToBigint.apply((i).value));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Int32 i) {
        return ((java.util.function.Function<java.math.BigInteger, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (v1 -> hydra.ext.python.coder.Coder.encodeIntegerValue_toPyInt(
          hydra.ext.python.utils.Utils::pyAtomToPyExpression,
          v1))).apply(hydra.lib.literals.Int32ToBigint.apply((i).value));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Int64 i) {
        return ((java.util.function.Function<java.math.BigInteger, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (v1 -> hydra.ext.python.coder.Coder.encodeIntegerValue_toPyInt(
          hydra.ext.python.utils.Utils::pyAtomToPyExpression,
          v1))).apply(hydra.lib.literals.Int64ToBigint.apply((i).value));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Uint8 i) {
        return ((java.util.function.Function<java.math.BigInteger, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (v1 -> hydra.ext.python.coder.Coder.encodeIntegerValue_toPyInt(
          hydra.ext.python.utils.Utils::pyAtomToPyExpression,
          v1))).apply(hydra.lib.literals.Uint8ToBigint.apply((i).value));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Uint16 i) {
        return ((java.util.function.Function<java.math.BigInteger, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (v1 -> hydra.ext.python.coder.Coder.encodeIntegerValue_toPyInt(
          hydra.ext.python.utils.Utils::pyAtomToPyExpression,
          v1))).apply(hydra.lib.literals.Uint16ToBigint.apply((i).value));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Uint32 i) {
        return ((java.util.function.Function<java.math.BigInteger, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (v1 -> hydra.ext.python.coder.Coder.encodeIntegerValue_toPyInt(
          hydra.ext.python.utils.Utils::pyAtomToPyExpression,
          v1))).apply(hydra.lib.literals.Uint32ToBigint.apply((i).value));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Uint64 i) {
        return ((java.util.function.Function<java.math.BigInteger, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (v1 -> hydra.ext.python.coder.Coder.encodeIntegerValue_toPyInt(
          hydra.ext.python.utils.Utils::pyAtomToPyExpression,
          v1))).apply(hydra.lib.literals.Uint64ToBigint.apply((i).value));
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T1, T0> encodeIntegerValue_toPyInt(java.util.function.Function<hydra.ext.python.syntax.Atom, T0> hydra_ext_python_utils_pyAtomToPyExpression2, java.math.BigInteger n) {
    return hydra.lib.flows.Pure.apply((hydra_ext_python_utils_pyAtomToPyExpression2).apply(new hydra.ext.python.syntax.Atom.Number_(new hydra.ext.python.syntax.Number_.Integer_(n))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> encodeLiteral(hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Literal.Binary bs) {
        java.util.List<Integer> byteValues = hydra.lib.literals.BinaryToBytes.apply((bs).value);
        return hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.functionCall(
          new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("bytes"))),
          java.util.List.of(hydra.ext.python.utils.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.List(hydra.ext.python.utils.Utils.pyList(hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, hydra.ext.python.syntax.Expression>) (byteVal -> hydra.ext.python.utils.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Number_(new hydra.ext.python.syntax.Number_.Integer_(hydra.lib.literals.Int32ToBigint.apply(byteVal))))),
            byteValues)))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Literal.Boolean_ b) {
        return hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.pyAtomToPyExpression(hydra.lib.logic.IfElse.lazy(
          (b).value,
          () -> new hydra.ext.python.syntax.Atom.True(),
          () -> new hydra.ext.python.syntax.Atom.False())));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Literal.Float_ f) {
        return hydra.ext.python.coder.Coder.<T0>encodeFloatValue((f).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Literal.Integer_ i) {
        return hydra.ext.python.coder.Coder.<T0>encodeIntegerValue((i).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Literal.String_ s) {
        return hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.stringToPyExpression(
          new hydra.ext.python.syntax.QuoteStyle.Double_(),
          (s).value));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> encodeLiteralType(hydra.core.LiteralType lt) {
    String findName = (lt).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public String visit(hydra.core.LiteralType.Binary ignored) {
        return "bytes";
      }
      
      @Override
      public String visit(hydra.core.LiteralType.Boolean_ ignored) {
        return "bool";
      }
      
      @Override
      public String visit(hydra.core.LiteralType.Float_ ft) {
        return ((ft).value).accept(new hydra.core.FloatType.PartialVisitor<>() {
          @Override
          public String visit(hydra.core.FloatType.Bigfloat ignored) {
            return "Decimal";
          }
          
          @Override
          public String visit(hydra.core.FloatType.Float32 ignored) {
            return "float";
          }
          
          @Override
          public String visit(hydra.core.FloatType.Float64 ignored) {
            return "float";
          }
        });
      }
      
      @Override
      public String visit(hydra.core.LiteralType.Integer_ ignored) {
        return "int";
      }
      
      @Override
      public String visit(hydra.core.LiteralType.String_ ignored) {
        return "str";
      }
    });
    return hydra.lib.flows.Pure.apply(new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name(findName)))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of())))))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> encodeApplicationType(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.ApplicationType at) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, java.util.function.Function<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>>>> gatherParams = new java.util.concurrent.atomic.AtomicReference<>();
    gatherParams.set((java.util.function.Function<hydra.core.Type, java.util.function.Function<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>>>) (t -> (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>>) (ps -> (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Application appT) {
        return ((gatherParams.get()).apply(((appT).value).function)).apply(hydra.lib.lists.Cons.apply(
          ((appT).value).argument,
          ps));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Annotated ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Function ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Forall ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.List ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Literal ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Map ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Maybe ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Either ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Pair ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Record ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Set ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Union ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Unit ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Variable ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Wrap ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }
    }))));
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Type>>> bodyAndArgs = new hydra.util.Lazy<>(() -> ((gatherParams.get()).apply(new hydra.core.Type.Application(at))).apply((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of())));
    hydra.util.Lazy<java.util.List<hydra.core.Type>> args = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bodyAndArgs.get()));
    hydra.util.Lazy<hydra.core.Type> body = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bodyAndArgs.get()));
    return hydra.lib.flows.Bind.apply(
      hydra.ext.python.coder.Coder.<T0>encodeType(
        env,
        body.get()),
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (pyBody -> hydra.lib.flows.Bind.apply(
        hydra.lib.flows.MapList.apply(
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (v1 -> hydra.ext.python.coder.Coder.<T0>encodeType(
            env,
            v1)),
          args.get()),
        (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Expression>, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (pyArgs -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.primaryAndParams(
          hydra.ext.python.utils.Utils.pyExpressionToPyPrimary(pyBody),
          pyArgs))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> encodeForallType(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.ForallType lt) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, java.util.function.Function<java.util.List<hydra.core.Name>, hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>>>> gatherParams = new java.util.concurrent.atomic.AtomicReference<>();
    gatherParams.set((java.util.function.Function<hydra.core.Type, java.util.function.Function<java.util.List<hydra.core.Name>, hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>>>) (t -> (java.util.function.Function<java.util.List<hydra.core.Name>, hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>>) (ps -> (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Forall forallT) {
        return ((gatherParams.get()).apply(((forallT).value).body)).apply(hydra.lib.lists.Cons.apply(
          ((forallT).value).parameter,
          ps));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Annotated ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Application ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Function ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.List ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Literal ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Map ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Maybe ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Either ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Pair ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Record ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Set ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Union ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Unit ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Variable ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Wrap ignored) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }
    }))));
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<hydra.core.Type, java.util.List<hydra.core.Name>>> bodyAndParams = new hydra.util.Lazy<>(() -> ((gatherParams.get()).apply(new hydra.core.Type.Forall(lt))).apply((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of())));
    hydra.util.Lazy<hydra.core.Type> body = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bodyAndParams.get()));
    hydra.util.Lazy<java.util.List<hydra.core.Name>> params = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bodyAndParams.get()));
    return hydra.lib.flows.Bind.apply(
      hydra.ext.python.coder.Coder.<T0>encodeType(
        env,
        body.get()),
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (pyBody -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.primaryAndParams(
        hydra.ext.python.utils.Utils.pyExpressionToPyPrimary(pyBody),
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Name, hydra.ext.python.syntax.Expression>) (n -> new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name((n).value)))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of()))))))))),
          params.get())))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> encodeFunctionType(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.FunctionType ft) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.FunctionType, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>>>> gatherParams = new java.util.concurrent.atomic.AtomicReference<>();
    gatherParams.set((java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.FunctionType, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>>>) (rdoms -> (java.util.function.Function<hydra.core.FunctionType, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>>) (ftype -> {
      hydra.core.Type dom = (ftype).domain;
      hydra.core.Type innerCod = (ftype).codomain;
      return (hydra.rewriting.Rewriting.deannotateType(innerCod)).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Function ft2) {
          return ((gatherParams.get()).apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms))).apply((ft2).value);
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Annotated ignored) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Application ignored) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Forall ignored) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.List ignored) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Literal ignored) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Map ignored) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Maybe ignored) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Either ignored) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Pair ignored) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Record ignored) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Set ignored) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Union ignored) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Unit ignored) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Variable ignored) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Wrap ignored) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }
      });
    })));
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>> domsAndCod = new hydra.util.Lazy<>(() -> ((gatherParams.get()).apply((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()))).apply(ft));
    hydra.util.Lazy<hydra.core.Type> cod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(domsAndCod.get()));
    hydra.util.Lazy<java.util.List<hydra.core.Type>> doms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(domsAndCod.get()));
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapList.apply(
        (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (v1 -> hydra.ext.python.coder.Coder.<T0>encodeType(
          env,
          v1)),
        doms.get()),
      (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Expression>, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (pydoms -> hydra.lib.flows.Bind.apply(
        hydra.ext.python.coder.Coder.<T0>encodeType(
          env,
          cod.get()),
        (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (pycod -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.pyPrimaryToPyExpression(hydra.ext.python.utils.Utils.primaryWithSlices(
          new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("Callable"))),
          hydra.ext.python.utils.Utils.pyPrimaryToPySlice(new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.List(hydra.ext.python.utils.Utils.pyList(pydoms)))),
          java.util.List.of(new hydra.ext.python.syntax.SliceOrStarredExpression.Slice(hydra.ext.python.utils.Utils.pyExpressionToPySlice(pycod))))))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> encodeType(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Type typ) {
    return (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Application at) {
        return hydra.ext.python.coder.Coder.<T0>encodeApplicationType(
          env,
          (at).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Function ft) {
        return hydra.ext.python.coder.Coder.<T0>encodeFunctionType(
          env,
          (ft).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Forall lt) {
        return hydra.ext.python.coder.Coder.<T0>encodeForallType(
          env,
          (lt).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.List et) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.python.coder.Coder.<T0>encodeType(
            env,
            (et).value),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (pyet -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.nameAndParams(
            new hydra.ext.python.syntax.Name("frozenlist"),
            java.util.List.of(pyet)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Map mt) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.python.coder.Coder.<T0>encodeType(
            env,
            ((mt).value).keys),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (pykt -> hydra.lib.flows.Bind.apply(
            hydra.ext.python.coder.Coder.<T0>encodeType(
              env,
              ((mt).value).values),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (pyvt -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.nameAndParams(
              new hydra.ext.python.syntax.Name("FrozenDict"),
              java.util.List.of(
                pykt,
                pyvt)))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Literal lt) {
        return hydra.ext.python.coder.Coder.<T0>encodeLiteralType((lt).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Maybe et) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.python.coder.Coder.<T0>encodeType(
            env,
            (et).value),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (ptype -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.pyPrimaryToPyExpression(hydra.ext.python.utils.Utils.primaryWithExpressionSlices(
            new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("Maybe"))),
            java.util.List.of(ptype))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Either eitherT) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.python.coder.Coder.<T0>encodeType(
            env,
            ((eitherT).value).left),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (pyleft -> hydra.lib.flows.Bind.apply(
            hydra.ext.python.coder.Coder.<T0>encodeType(
              env,
              ((eitherT).value).right),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (pyright -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.pyPrimaryToPyExpression(hydra.ext.python.utils.Utils.primaryWithExpressionSlices(
              new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("Either"))),
              java.util.List.of(
                pyleft,
                pyright))))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Pair pairT) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.python.coder.Coder.<T0>encodeType(
            env,
            ((pairT).value).first),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (pyFirst -> hydra.lib.flows.Bind.apply(
            hydra.ext.python.coder.Coder.<T0>encodeType(
              env,
              ((pairT).value).second),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (pySecond -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.nameAndParams(
              new hydra.ext.python.syntax.Name("tuple"),
              java.util.List.of(
                pyFirst,
                pySecond)))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Record rt) {
        return hydra.lib.flows.Pure.apply(hydra.ext.python.names.Names.typeVariableReference(
          env,
          ((rt).value).typeName));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Set et) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.python.coder.Coder.<T0>encodeType(
            env,
            (et).value),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (pyet -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.nameAndParams(
            new hydra.ext.python.syntax.Name("frozenset"),
            java.util.List.of(pyet)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Union rt) {
        return hydra.lib.flows.Pure.apply(hydra.ext.python.names.Names.typeVariableReference(
          env,
          ((rt).value).typeName));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Unit ignored) {
        return hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.pyNameToPyExpression(hydra.ext.python.utils.Utils.pyNone()));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Variable name) {
        return hydra.lib.flows.Pure.apply(hydra.ext.python.names.Names.typeVariableReference(
          env,
          (name).value));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Wrap wt) {
        return hydra.lib.flows.Pure.apply(hydra.ext.python.names.Names.typeVariableReference(
          env,
          ((wt).value).typeName));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Annotated ignored) {
        return hydra.ext.python.coder.Coder.encodeType_dflt(
          hydra.ext.python.utils.Utils::doubleQuotedString,
          hydra.rewriting.Rewriting::deannotateType,
          hydra.show.core.Core::type,
          typ);
      }
    });
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T3, T0> encodeType_dflt(java.util.function.Function<String, T0> hydra_ext_python_utils_doubleQuotedString2, java.util.function.Function<T1, T2> hydra_rewriting_deannotateType2, java.util.function.Function<T2, String> hydra_show_core_type2, T1 typ) {
    return hydra.lib.flows.Pure.apply((hydra_ext_python_utils_doubleQuotedString2).apply(hydra.lib.strings.Cat2.apply(
      "type = ",
      (hydra_show_core_type2).apply((hydra_rewriting_deannotateType2).apply(typ)))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression> encodeTypeQuoted(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Type typ) {
    return hydra.lib.flows.Bind.apply(
      hydra.ext.python.coder.Coder.<T0>encodeType(
        env,
        typ),
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<T0, hydra.ext.python.syntax.Expression>>) (pytype -> hydra.lib.flows.Pure.apply(hydra.lib.logic.IfElse.lazy(
        hydra.lib.sets.Null.apply(hydra.rewriting.Rewriting.freeVariablesInType(typ)),
        () -> pytype,
        () -> hydra.ext.python.utils.Utils.doubleQuotedString(hydra.serialization.Serialization.printExpr(hydra.ext.python.serde.Serde.encodeExpression(pytype)))))));
  }
  
  static java.util.List<hydra.ext.python.syntax.Statement> encodeNameConstants(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name name, hydra.core.Type typ) {
    java.util.function.Function<hydra.core.FieldType, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> fieldPair = (java.util.function.Function<hydra.core.FieldType, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>) (field -> (hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>) ((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>) (new hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>(hydra.ext.python.names.Names.encodeConstantForFieldName(
      env,
      name,
      (field).name), (field).name))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>>> fieldPairs = new java.util.concurrent.atomic.AtomicReference<>();
    fieldPairs.set((java.util.function.Function<hydra.core.Type, java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>>) (t -> (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> visit(hydra.core.Type.Forall ft) {
        return (fieldPairs.get()).apply(((ft).value).body);
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> visit(hydra.core.Type.Record rt) {
        return hydra.lib.lists.Map.apply(
          fieldPair,
          ((rt).value).fields);
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> visit(hydra.core.Type.Union rt) {
        return hydra.lib.lists.Map.apply(
          fieldPair,
          ((rt).value).fields);
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> visit(hydra.core.Type.Annotated ignored) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> visit(hydra.core.Type.Application ignored) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> visit(hydra.core.Type.Function ignored) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> visit(hydra.core.Type.List ignored) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> visit(hydra.core.Type.Literal ignored) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> visit(hydra.core.Type.Map ignored) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> visit(hydra.core.Type.Maybe ignored) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> visit(hydra.core.Type.Either ignored) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> visit(hydra.core.Type.Pair ignored) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> visit(hydra.core.Type.Set ignored) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> visit(hydra.core.Type.Unit ignored) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> visit(hydra.core.Type.Variable ignored) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> visit(hydra.core.Type.Wrap ignored) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>>of());
      }
    })));
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>> namePair = new hydra.util.Lazy<>(() -> (hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>) ((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>) (new hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>(hydra.ext.python.names.Names.encodeConstantForTypeName(
      env,
      name), name))));
    java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>, hydra.ext.python.syntax.Statement> toStmt = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, hydra.core.Name>, hydra.ext.python.syntax.Statement>) (pair -> hydra.ext.python.utils.Utils.assignmentStatement(
      hydra.lib.pairs.First.apply(pair),
      hydra.ext.python.utils.Utils.functionCall(
        hydra.ext.python.utils.Utils.pyNameToPyPrimary(hydra.ext.python.names.Names.encodeName(
          true,
          new hydra.util.CaseConvention.Pascal(),
          env,
          new hydra.core.Name("hydra.core.Name"))),
        java.util.List.of(hydra.ext.python.utils.Utils.doubleQuotedString((hydra.lib.pairs.Second.apply(pair)).value)))));
    return hydra.lib.lists.Map.apply(
      toStmt,
      hydra.lib.lists.Cons.apply(
        namePair.get(),
        (fieldPairs.get()).apply(typ)));
  }
  
  static java.util.List<hydra.core.Name> findTypeParams(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Type typ) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> boundVars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((env).boundTypeVariables));
    java.util.function.Function<hydra.core.Name, Boolean> isBound = (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.maybes.IsJust.apply(hydra.lib.maps.Lookup.apply(
      v,
      boundVars.get())));
    return hydra.lib.lists.Filter.apply(
      isBound,
      hydra.lib.sets.ToList.apply(hydra.rewriting.Rewriting.freeVariablesInType(typ)));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.python.syntax.Statement> encodeWrappedType(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name name, hydra.core.Type typ, hydra.util.Maybe<String> comment) {
    hydra.util.Lazy<java.util.List<hydra.core.Name>> tparamList = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((env).boundTypeVariables));
    return hydra.lib.flows.Bind.apply(
      hydra.ext.python.coder.Coder.<T0>encodeTypeQuoted(
        env,
        typ),
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<T0, hydra.ext.python.syntax.Statement>>) (ptypeQuoted -> {
        hydra.util.Lazy<hydra.ext.python.syntax.Block> body = new hydra.util.Lazy<>(() -> hydra.ext.python.utils.Utils.indentedBlock(
          comment,
          (java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>) (java.util.List.<java.util.List<hydra.ext.python.syntax.Statement>>of())));
        return hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.pyClassDefinitionToPyStatement(new hydra.ext.python.syntax.ClassDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), hydra.ext.python.names.Names.encodeName(
          false,
          new hydra.util.CaseConvention.Pascal(),
          env,
          name), hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Name, hydra.ext.python.syntax.TypeParameter>) (arg_ -> hydra.ext.python.utils.Utils.pyNameToPyTypeParameter(hydra.ext.python.names.Names.encodeTypeVariable(arg_))),
          hydra.ext.python.coder.Coder.findTypeParams(
            env,
            typ)), hydra.util.Maybe.just(hydra.ext.python.coder.Coder.variantArgs(
          ptypeQuoted,
          tparamList.get())), body.get())));
      }));
  }
  
  static hydra.ext.python.helpers.PythonEnvironment extendEnvWithTypeVar(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name var_) {
    hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> oldBound = (env).boundTypeVariables;
    hydra.util.Lazy<java.util.List<hydra.core.Name>> tparamList = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(oldBound));
    hydra.util.Lazy<java.util.List<hydra.core.Name>> newList = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
      tparamList.get(),
      java.util.List.of(var_)));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> tparamMap = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(oldBound));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> newMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.Insert.apply(
      var_,
      hydra.ext.python.names.Names.encodeTypeVariable(var_),
      tparamMap.get()));
    return new hydra.ext.python.helpers.PythonEnvironment((env).namespaces, (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>(newList.get(), newMap.get()))), (env).typeContext, (env).nullaryBindings, (env).version, (env).skipCasts, (env).inlineVariables);
  }
  
  static hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Term> gatherLambdas(hydra.core.Term term) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Term>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Term>>>) (params -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Term>>) (t -> (hydra.rewriting.Rewriting.deannotateAndDetypeTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Term>(params, t)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Term> visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Term> otherwise(hydra.core.Function instance) {
            return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Term>(params, t)));
          }
          
          @Override
          public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Term> visit(hydra.core.Function.Lambda l) {
            return ((go.get()).apply(hydra.lib.lists.Concat2.apply(
              params,
              java.util.List.of(((l).value).parameter)))).apply(((l).value).body);
          }
        });
      }
    }))));
    return ((go.get()).apply((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()))).apply(term);
  }
  
  static hydra.ext.python.helpers.PythonEnvironment extendEnvWithLambdaParams(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Term term) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, java.util.function.Function<hydra.core.Term, hydra.ext.python.helpers.PythonEnvironment>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, java.util.function.Function<hydra.core.Term, hydra.ext.python.helpers.PythonEnvironment>>) (e -> (java.util.function.Function<hydra.core.Term, hydra.ext.python.helpers.PythonEnvironment>) (t -> (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.ext.python.helpers.PythonEnvironment otherwise(hydra.core.Term instance) {
        return e;
      }
      
      @Override
      public hydra.ext.python.helpers.PythonEnvironment visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.ext.python.helpers.PythonEnvironment otherwise(hydra.core.Function instance) {
            return e;
          }
          
          @Override
          public hydra.ext.python.helpers.PythonEnvironment visit(hydra.core.Function.Lambda lam) {
            hydra.typing.TypeContext newTc = hydra.schemas.Schemas.extendTypeContextForLambda(
              hydra.ext.python.coder.Coder.pythonEnvironmentGetTypeContext(e),
              (lam).value);
            hydra.ext.python.helpers.PythonEnvironment newEnv = hydra.ext.python.coder.Coder.pythonEnvironmentSetTypeContext(
              newTc,
              e);
            return ((go.get()).apply(newEnv)).apply(((lam).value).body);
          }
        });
      }
    }))));
    return ((go.get()).apply(env)).apply(term);
  }
  
  static hydra.ext.python.syntax.Expression makeSimpleLambda(Integer arity, hydra.ext.python.syntax.Expression lhs) {
    hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Name>> args = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<Integer, hydra.ext.python.syntax.Name>) (i -> new hydra.ext.python.syntax.Name(hydra.lib.strings.Cat2.apply(
        "x",
        hydra.lib.literals.ShowInt32.apply(i)))),
      hydra.lib.math.Range.apply(
        1,
        arity)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        arity,
        0),
      () -> lhs,
      () -> new hydra.ext.python.syntax.Expression.Lambda(new hydra.ext.python.syntax.Lambda(new hydra.ext.python.syntax.LambdaParameters((hydra.util.Maybe<hydra.ext.python.syntax.LambdaSlashNoDefault>) (hydra.util.Maybe.<hydra.ext.python.syntax.LambdaSlashNoDefault>nothing()), hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ext.python.syntax.LambdaParamNoDefault>) (a -> new hydra.ext.python.syntax.LambdaParamNoDefault(a)),
        args.get()), (java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault>) (java.util.List.<hydra.ext.python.syntax.LambdaParamWithDefault>of()), (hydra.util.Maybe<hydra.ext.python.syntax.LambdaStarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.LambdaStarEtc>nothing())), hydra.ext.python.utils.Utils.functionCall(
        hydra.ext.python.utils.Utils.pyExpressionToPyPrimary(lhs),
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ext.python.syntax.Expression>) (a -> new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(a))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of()))))))))),
          args.get())))));
  }
  
  static hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>> isCaseStatementApplication(hydra.core.Term term) {
    hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term> gathered = hydra.coderUtils.CoderUtils.gatherApplications(term);
    hydra.util.Lazy<java.util.List<hydra.core.Term>> args = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(gathered));
    hydra.util.Lazy<hydra.core.Term> body = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(gathered));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply(args.get()),
        1)),
      () -> (hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>>) (hydra.util.Maybe.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>>nothing()),
      () -> ((java.util.function.Supplier<hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>>>) (() -> {
        hydra.util.Lazy<hydra.core.Term> arg = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(args.get()));
        return (hydra.rewriting.Rewriting.deannotateAndDetypeTerm(body.get())).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>> otherwise(hydra.core.Term instance) {
            return (hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>>) (hydra.util.Maybe.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>>nothing());
          }
          
          @Override
          public hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>> visit(hydra.core.Term.Function f) {
            return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
              @Override
              public hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>> otherwise(hydra.core.Function instance) {
                return (hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>>) (hydra.util.Maybe.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>>nothing());
              }
              
              @Override
              public hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>> visit(hydra.core.Function.Elimination e) {
                return ((e).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
                  @Override
                  public hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>> otherwise(hydra.core.Elimination instance) {
                    return (hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>>) (hydra.util.Maybe.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>>nothing());
                  }
                  
                  @Override
                  public hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>> visit(hydra.core.Elimination.Union cs) {
                    return hydra.util.Maybe.just((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>(((cs).value).typeName, (hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>) ((hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>) (new hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>(((cs).value).default_, (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>(((cs).value).cases, arg.get()))))))))));
                  }
                });
              }
            });
          }
        });
      })).get());
  }
  
  static Boolean isVariantUnitType(hydra.core.RowType rowType, hydra.core.Name fieldName) {
    java.util.List<hydra.core.FieldType> fields = (rowType).fields;
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.FieldType>> mfield = new hydra.util.Lazy<>(() -> hydra.lib.lists.Find.apply(
      (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.lib.equality.Equal.apply(
        (ft).name,
        fieldName)),
      fields));
    return hydra.lib.maybes.FromMaybe.apply(
      false,
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.schemas.Schemas.isUnitType(hydra.rewriting.Rewriting.deannotateType((ft).type))),
        mfield.get()));
  }
  
  static hydra.ext.python.syntax.CaseBlock wildcardCaseBlock(hydra.ext.python.syntax.Statement stmt) {
    return new hydra.ext.python.syntax.CaseBlock(hydra.ext.python.utils.Utils.pyClosedPatternToPyPatterns(new hydra.ext.python.syntax.ClosedPattern.Wildcard()), (hydra.util.Maybe<hydra.ext.python.syntax.Guard>) (hydra.util.Maybe.<hydra.ext.python.syntax.Guard>nothing()), hydra.ext.python.utils.Utils.indentedBlock(
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      java.util.List.of(java.util.List.of(stmt))));
  }
  
  static hydra.ext.python.syntax.ClosedPattern enumVariantPattern(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name typeName, hydra.core.Name fieldName) {
    return new hydra.ext.python.syntax.ClosedPattern.Value(new hydra.ext.python.syntax.ValuePattern(new hydra.ext.python.syntax.Attribute(java.util.List.of(
      hydra.ext.python.names.Names.encodeName(
        true,
        new hydra.util.CaseConvention.Pascal(),
        env,
        typeName),
      hydra.ext.python.names.Names.encodeEnumValue(
        env,
        fieldName)))));
  }
  
  static hydra.ext.python.syntax.ClosedPattern classVariantPatternUnit(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name typeName, hydra.core.Name fieldName) {
    return new hydra.ext.python.syntax.ClosedPattern.Class_(new hydra.ext.python.syntax.ClassPattern(new hydra.ext.python.syntax.NameOrAttribute(java.util.List.of(hydra.ext.python.names.Names.variantName(
      true,
      env,
      typeName,
      fieldName))), (hydra.util.Maybe<hydra.ext.python.syntax.PositionalPatterns>) (hydra.util.Maybe.<hydra.ext.python.syntax.PositionalPatterns>nothing()), (hydra.util.Maybe<hydra.ext.python.syntax.KeywordPatterns>) (hydra.util.Maybe.<hydra.ext.python.syntax.KeywordPatterns>nothing())));
  }
  
  static hydra.ext.python.syntax.ClosedPattern classVariantPatternWithCapture(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name typeName, hydra.core.Name fieldName, hydra.core.Name varName) {
    hydra.ext.python.syntax.ClosedPattern capturePattern = new hydra.ext.python.syntax.ClosedPattern.Capture(new hydra.ext.python.syntax.CapturePattern(new hydra.ext.python.syntax.PatternCaptureTarget(hydra.ext.python.names.Names.encodeName(
      false,
      new hydra.util.CaseConvention.LowerSnake(),
      env,
      varName))));
    hydra.ext.python.syntax.KeywordPattern keywordPattern = new hydra.ext.python.syntax.KeywordPattern(new hydra.ext.python.syntax.Name("value"), new hydra.ext.python.syntax.Pattern.Or(new hydra.ext.python.syntax.OrPattern(java.util.List.of(capturePattern))));
    hydra.ext.python.syntax.NameOrAttribute pyVarName = new hydra.ext.python.syntax.NameOrAttribute(java.util.List.of(hydra.ext.python.names.Names.variantName(
      true,
      env,
      typeName,
      fieldName)));
    return new hydra.ext.python.syntax.ClosedPattern.Class_(new hydra.ext.python.syntax.ClassPattern(pyVarName, (hydra.util.Maybe<hydra.ext.python.syntax.PositionalPatterns>) (hydra.util.Maybe.<hydra.ext.python.syntax.PositionalPatterns>nothing()), hydra.util.Maybe.just(new hydra.ext.python.syntax.KeywordPatterns(java.util.List.of(keywordPattern)))));
  }
  
  static <T0> Boolean isCasesFull(hydra.core.RowType rowType, java.util.List<T0> cases_) {
    hydra.util.Lazy<Integer> numCases = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(cases_));
    hydra.util.Lazy<Integer> numFields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply((rowType).fields));
    return hydra.lib.logic.Not.apply(hydra.lib.equality.Lt.apply(
      numCases.get(),
      numFields.get()));
  }
  
  static <T0> hydra.ext.python.syntax.ClosedPattern variantClosedPattern(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name typeName, hydra.core.Name fieldName, T0 rowType, Boolean isEnum, hydra.core.Name varName, Boolean shouldCapture) {
    return hydra.lib.logic.IfElse.lazy(
      isEnum,
      () -> hydra.ext.python.coder.Coder.enumVariantPattern(
        env,
        typeName,
        fieldName),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.logic.Not.apply(shouldCapture),
        () -> hydra.ext.python.coder.Coder.classVariantPatternUnit(
          env,
          typeName,
          fieldName),
        () -> hydra.ext.python.coder.Coder.classVariantPatternWithCapture(
          env,
          typeName,
          fieldName,
          varName)));
  }
  
  static java.util.List<hydra.core.Field> deduplicateCaseVariables(java.util.List<hydra.core.Field> cases_) {
    java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>, java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>>> rewriteCase = (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>, java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>>>) (state -> (java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>>) (field -> {
      hydra.util.Lazy<java.util.Map<hydra.core.Name, Integer>> countByName = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(state));
      hydra.util.Lazy<java.util.List<hydra.core.Field>> done = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(state));
      hydra.core.Name fname = (field).name;
      hydra.core.Term fterm = (field).term;
      return (hydra.rewriting.Rewriting.deannotateTerm(fterm)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) ((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) (new hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>(countByName.get(), hydra.lib.lists.Cons.apply(
            field,
            done.get()))));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>> visit(hydra.core.Term.Function f) {
          return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>> otherwise(hydra.core.Function instance) {
              return (hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) ((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) (new hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>(countByName.get(), hydra.lib.lists.Cons.apply(
                field,
                done.get()))));
            }
            
            @Override
            public hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>> visit(hydra.core.Function.Lambda lam) {
              hydra.core.Term body = ((lam).value).body;
              hydra.util.Maybe<hydra.core.Type> mdom = ((lam).value).domain;
              hydra.core.Name v = ((lam).value).parameter;
              return hydra.lib.maybes.Maybe.apply(
                (hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) ((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) (new hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>(hydra.lib.maps.Insert.apply(
                  v,
                  1,
                  countByName.get()), hydra.lib.lists.Cons.apply(
                  field,
                  done.get())))),
                (java.util.function.Function<Integer, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>>) (count -> {
                  Integer count2 = hydra.lib.math.Add.apply(
                    count,
                    1);
                  hydra.core.Name v2 = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
                    (v).value,
                    hydra.lib.literals.ShowInt32.apply(count2)));
                  hydra.core.Term newBody = hydra.reduction.Reduction.alphaConvert(
                    v,
                    v2,
                    body);
                  hydra.core.Lambda newLam = new hydra.core.Lambda(v2, mdom, newBody);
                  hydra.core.Term newTerm = new hydra.core.Term.Function(new hydra.core.Function.Lambda(newLam));
                  hydra.core.Field newField = new hydra.core.Field(fname, newTerm);
                  return (hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) ((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) (new hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>(hydra.lib.maps.Insert.apply(
                    v,
                    count2,
                    countByName.get()), hydra.lib.lists.Cons.apply(
                    newField,
                    done.get()))));
                }),
                hydra.lib.maps.Lookup.apply(
                  v,
                  countByName.get()));
            }
          });
        }
      });
    }));
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>> result = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      rewriteCase,
      (hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) ((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) (new hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>((java.util.Map<hydra.core.Name, Integer>) ((java.util.Map<hydra.core.Name, Integer>) (hydra.lib.maps.Empty.<hydra.core.Name, Integer>apply())), (java.util.List<hydra.core.Field>) (java.util.List.<hydra.core.Field>of())))),
      cases_));
    return hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(result.get()));
  }
  
  static hydra.core.Term eliminateUnitVar(hydra.core.Name v, hydra.core.Term term0) {
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Binding, hydra.core.Binding>> rewriteBinding = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Binding, hydra.core.Binding>>) (rewrite2 -> (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (bnd -> new hydra.core.Binding((bnd).name, (rewrite2).apply((bnd).term), (bnd).type)));
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Field, hydra.core.Field>> rewriteField = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Field, hydra.core.Field>>) (rewrite2 -> (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (fld -> new hydra.core.Field((fld).name, (rewrite2).apply((fld).term))));
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> rewrite = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return term;
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable n) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (n).value,
            v),
          () -> new hydra.core.Term.Unit(),
          () -> term);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm((recurse).apply(((at).value).body), ((at).value).annotation));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Application app) {
        return new hydra.core.Term.Application(new hydra.core.Application((recurse).apply(((app).value).function), (recurse).apply(((app).value).argument)));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return term;
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda lam) {
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                ((lam).value).parameter,
                v),
              () -> term,
              () -> new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(((lam).value).parameter, ((lam).value).domain, (recurse).apply(((lam).value).body)))));
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.Function.Elimination e) {
            return ((e).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public hydra.core.Term otherwise(hydra.core.Elimination instance) {
                return term;
              }
              
              @Override
              public hydra.core.Term visit(hydra.core.Elimination.Union cs) {
                return new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(((cs).value).typeName, hydra.lib.maybes.Map.apply(
                  recurse,
                  ((cs).value).default_), hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (v1 -> ((rewriteField).apply(recurse)).apply(v1)),
                  ((cs).value).cases)))));
              }
            });
          }
        });
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        return new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (v1 -> ((rewriteBinding).apply(recurse)).apply(v1)),
          ((lt).value).bindings), (recurse).apply(((lt).value).body)));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.List ts) {
        return new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          recurse,
          (ts).value));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Map m) {
        return new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>) (kv -> (hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>((recurse).apply(hydra.lib.pairs.First.apply(kv)), (recurse).apply(hydra.lib.pairs.Second.apply(kv)))))),
          hydra.lib.maps.ToList.apply((m).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Record rec) {
        return new hydra.core.Term.Record(new hydra.core.Record(((rec).value).typeName, hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (v1 -> ((rewriteField).apply(recurse)).apply(v1)),
          ((rec).value).fields)));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Set s) {
        return new hydra.core.Term.Set(hydra.lib.sets.Map.apply(
          recurse,
          (s).value));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Union inj) {
        return new hydra.core.Term.Union(new hydra.core.Injection(((inj).value).typeName, ((rewriteField).apply(recurse)).apply(((inj).value).field)));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Maybe mt) {
        return new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
          recurse,
          (mt).value));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Pair p) {
        return new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>((recurse).apply(hydra.lib.pairs.First.apply((p).value)), (recurse).apply(hydra.lib.pairs.Second.apply((p).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Wrap wt) {
        return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(((wt).value).typeName, (recurse).apply(((wt).value).body)));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Either e) {
        return new hydra.core.Term.Either(hydra.lib.eithers.Bimap.apply(
          recurse,
          recurse,
          (e).value));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication ta) {
        return new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm((recurse).apply(((ta).value).body), ((ta).value).type));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
        return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(((tl).value).parameter, (recurse).apply(((tl).value).body)));
      }
    })));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, hydra.core.Term>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> ((rewrite).apply(go.get())).apply(term)));
    return (go.get()).apply(term0);
  }
  
  static <T0, T1> hydra.compute.Flow<T1, java.util.List<hydra.ext.python.syntax.CaseBlock>> encodeDefaultCaseBlock(java.util.function.Function<T0, hydra.compute.Flow<T1, hydra.ext.python.syntax.Expression>> encodeTerm, Boolean isFull, hydra.util.Maybe<T0> mdflt, hydra.core.Name tname) {
    return hydra.lib.flows.Bind.apply(
      hydra.lib.maybes.Maybe.apply(
        hydra.lib.flows.Pure.apply(hydra.lib.logic.IfElse.lazy(
          isFull,
          () -> hydra.ext.python.utils.Utils.raiseAssertionError("Unreachable: all variants handled"),
          () -> hydra.ext.python.utils.Utils.raiseTypeError(hydra.lib.strings.Cat2.apply(
            "Unsupported ",
            hydra.names.Names.localNameOf(tname))))),
        (java.util.function.Function<T0, hydra.compute.Flow<T1, hydra.ext.python.syntax.Statement>>) (d -> hydra.lib.flows.Bind.apply(
          (encodeTerm).apply(d),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<T1, hydra.ext.python.syntax.Statement>>) (pyexpr -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.returnSingle(pyexpr))))),
        mdflt),
      (java.util.function.Function<hydra.ext.python.syntax.Statement, hydra.compute.Flow<T1, java.util.List<hydra.ext.python.syntax.CaseBlock>>>) (stmt -> {
        hydra.util.Lazy<hydra.ext.python.syntax.Block> body = new hydra.util.Lazy<>(() -> hydra.ext.python.utils.Utils.indentedBlock(
          (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
          java.util.List.of(java.util.List.of(stmt))));
        hydra.ext.python.syntax.Patterns patterns = hydra.ext.python.utils.Utils.pyClosedPatternToPyPatterns(new hydra.ext.python.syntax.ClosedPattern.Wildcard());
        return hydra.lib.flows.Pure.apply(java.util.List.of(new hydra.ext.python.syntax.CaseBlock(patterns, (hydra.util.Maybe<hydra.ext.python.syntax.Guard>) (hydra.util.Maybe.<hydra.ext.python.syntax.Guard>nothing()), body.get())));
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.python.syntax.CaseBlock> encodeCaseBlock(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name tname, hydra.core.RowType rowType, Boolean isEnum, java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, java.util.List<hydra.ext.python.syntax.Statement>>>> encodeBody, hydra.core.Field field) {
    hydra.core.Name fname = (field).name;
    hydra.core.Term fterm = (field).term;
    return (hydra.rewriting.Rewriting.deannotateTerm(fterm)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.ext.python.syntax.CaseBlock> visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, hydra.ext.python.syntax.CaseBlock> visit(hydra.core.Function.Lambda lam) {
            Boolean isUnitVariant = hydra.ext.python.coder.Coder.isVariantUnitType(
              rowType,
              fname);
            hydra.core.Term rawBody = ((lam).value).body;
            hydra.core.Name v = ((lam).value).parameter;
            hydra.util.Lazy<hydra.core.Term> effectiveBody = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              isUnitVariant,
              () -> hydra.ext.python.coder.Coder.eliminateUnitVar(
                v,
                rawBody),
              () -> rawBody));
            Boolean shouldCapture = hydra.lib.logic.Not.apply(hydra.lib.logic.Or.apply(
              isUnitVariant,
              hydra.lib.logic.Or.apply(
                hydra.rewriting.Rewriting.isFreeVariableInTerm(
                  v,
                  rawBody),
                hydra.schemas.Schemas.isUnitTerm(rawBody))));
            return hydra.lib.flows.Bind.apply(
              hydra.lib.flows.Pure.apply(hydra.ext.python.coder.Coder.pythonEnvironmentSetTypeContext(
                hydra.schemas.Schemas.extendTypeContextForLambda(
                  hydra.ext.python.coder.Coder.pythonEnvironmentGetTypeContext(env),
                  (lam).value),
                env)),
              (java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, hydra.compute.Flow<T0, hydra.ext.python.syntax.CaseBlock>>) (env2 -> {
                hydra.util.Lazy<hydra.ext.python.syntax.ClosedPattern> pattern = new hydra.util.Lazy<>(() -> hydra.ext.python.coder.Coder.variantClosedPattern(
                  env2,
                  tname,
                  fname,
                  rowType,
                  isEnum,
                  v,
                  shouldCapture));
                return hydra.lib.flows.Bind.apply(
                  ((encodeBody).apply(env2)).apply(effectiveBody.get()),
                  (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.compute.Flow<T0, hydra.ext.python.syntax.CaseBlock>>) (stmts -> {
                    hydra.util.Lazy<hydra.ext.python.syntax.Block> pyBody = new hydra.util.Lazy<>(() -> hydra.ext.python.utils.Utils.indentedBlock(
                      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
                      java.util.List.of(stmts)));
                    return hydra.lib.flows.Pure.apply(new hydra.ext.python.syntax.CaseBlock(hydra.ext.python.utils.Utils.pyClosedPatternToPyPatterns(pattern.get()), (hydra.util.Maybe<hydra.ext.python.syntax.Guard>) (hydra.util.Maybe.<hydra.ext.python.syntax.Guard>nothing()), pyBody.get()));
                  }));
              }));
          }
        });
      }
    });
  }
  
  static hydra.graph.Graph pyGraphGraph(hydra.ext.python.helpers.PyGraph pyg) {
    return (pyg).graph;
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata pyGraphMetadata(hydra.ext.python.helpers.PyGraph pyg) {
    return (pyg).metadata;
  }
  
  static hydra.ext.python.helpers.PyGraph makePyGraph(hydra.graph.Graph g, hydra.ext.python.helpers.PythonModuleMetadata m) {
    return new hydra.ext.python.helpers.PyGraph(g, m);
  }
  
  static <T0> hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, T0> inGraphContext(hydra.compute.Flow<hydra.graph.Graph, T0> graphFlow) {
    return hydra.coderUtils.CoderUtils.inCoderGraphContext(
      hydra.ext.python.coder.Coder::pyGraphGraph,
      hydra.ext.python.coder.Coder::pyGraphMetadata,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, hydra.ext.python.helpers.PyGraph>>) (p0 -> p1 -> hydra.ext.python.coder.Coder.makePyGraph(
        p0,
        p1)),
      graphFlow);
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement> encodeFieldType(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.FieldType fieldType) {
    hydra.core.Name fname = (fieldType).name;
    hydra.core.Type ftype = (fieldType).type;
    return hydra.lib.flows.Bind.apply(
      hydra.ext.python.coder.Coder.inGraphContext(hydra.annotations.Annotations.getTypeDescription(ftype)),
      (java.util.function.Function<hydra.util.Maybe<String>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (comment -> {
        hydra.ext.python.syntax.SingleTarget pyName = new hydra.ext.python.syntax.SingleTarget.Name(hydra.ext.python.names.Names.encodeFieldName(
          env,
          fname));
        return hydra.lib.flows.Bind.apply(
          hydra.ext.python.coder.Coder.encodeType(
            env,
            ftype),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (pyType -> {
            hydra.ext.python.syntax.Expression annotatedPyType = hydra.ext.python.utils.Utils.annotatedExpression(
              comment,
              pyType);
            return hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.pyAssignmentToPyStatement(new hydra.ext.python.syntax.Assignment.Typed(new hydra.ext.python.syntax.TypedAssignment(pyName, annotatedPyType, (hydra.util.Maybe<hydra.ext.python.syntax.AnnotatedRhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.AnnotatedRhs>nothing())))));
          }));
      }));
  }
  
  static hydra.ext.python.syntax.NamedExpression dataclassDecorator() {
    return new hydra.ext.python.syntax.NamedExpression.Simple(hydra.ext.python.utils.Utils.pyPrimaryToPyExpression(hydra.ext.python.utils.Utils.primaryWithRhs(
      new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("dataclass"))),
      new hydra.ext.python.syntax.PrimaryRhs.Call(new hydra.ext.python.syntax.Args((java.util.List<hydra.ext.python.syntax.PosArg>) (java.util.List.<hydra.ext.python.syntax.PosArg>of()), java.util.List.of(new hydra.ext.python.syntax.KwargOrStarred.Kwarg(new hydra.ext.python.syntax.Kwarg(new hydra.ext.python.syntax.Name("frozen"), hydra.ext.python.utils.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.True())))), (java.util.List<hydra.ext.python.syntax.KwargOrDoubleStarred>) (java.util.List.<hydra.ext.python.syntax.KwargOrDoubleStarred>of()))))));
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement> encodeRecordType(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name name, hydra.core.RowType rowType, hydra.util.Maybe<String> comment) {
    java.util.List<hydra.core.FieldType> tfields = (rowType).fields;
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapList.apply(
        (java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (v1 -> hydra.ext.python.coder.Coder.encodeFieldType(
          env,
          v1)),
        tfields),
      (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (pyFields -> {
        hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> boundVars = (env).boundTypeVariables;
        hydra.util.Lazy<java.util.List<hydra.core.Name>> tparamList = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(boundVars));
        hydra.util.Maybe<hydra.ext.python.syntax.Expression> mGenericArg = hydra.ext.python.coder.Coder.genericArg(tparamList.get());
        hydra.util.Lazy<hydra.util.Maybe<hydra.ext.python.syntax.Args>> args = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.apply(
          (hydra.util.Maybe<hydra.ext.python.syntax.Args>) (hydra.util.Maybe.<hydra.ext.python.syntax.Args>nothing()),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Maybe<hydra.ext.python.syntax.Args>>) (a -> hydra.util.Maybe.just(hydra.ext.python.utils.Utils.pyExpressionsToPyArgs(java.util.List.of(a)))),
          mGenericArg));
        hydra.ext.python.syntax.Block body = hydra.ext.python.utils.Utils.indentedBlock(
          comment,
          java.util.List.of(pyFields));
        hydra.util.Maybe<hydra.ext.python.syntax.Decorators> decs = hydra.util.Maybe.just(new hydra.ext.python.syntax.Decorators(java.util.List.of(hydra.ext.python.coder.Coder.dataclassDecorator())));
        hydra.ext.python.syntax.Name pyName = hydra.ext.python.names.Names.encodeName(
          false,
          new hydra.util.CaseConvention.Pascal(),
          env,
          name);
        return hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.pyClassDefinitionToPyStatement(new hydra.ext.python.syntax.ClassDefinition(decs, pyName, hydra.ext.python.coder.Coder.<hydra.ext.python.syntax.TypeParameter>encodeRecordType_noTypeParams(), args.get(), body)));
      }));
  }
  
  static <T0> java.util.List<T0> encodeRecordType_noTypeParams() {
    return (java.util.List<T0>) (java.util.List.<T0>of());
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>> encodeEnumValueAssignment(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.FieldType fieldType) {
    hydra.core.Name fname = (fieldType).name;
    hydra.core.Type ftype = (fieldType).type;
    return hydra.lib.flows.Bind.apply(
      hydra.ext.python.coder.Coder.inGraphContext(hydra.annotations.Annotations.getTypeDescription(ftype)),
      (java.util.function.Function<hydra.util.Maybe<String>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (mcomment -> {
        hydra.ext.python.syntax.Name pyName = hydra.ext.python.names.Names.encodeEnumValue(
          env,
          fname);
        String fnameStr = (fname).value;
        hydra.ext.python.syntax.Expression pyValue = hydra.ext.python.utils.Utils.doubleQuotedString(fnameStr);
        hydra.ext.python.syntax.Statement assignStmt = hydra.ext.python.utils.Utils.assignmentStatement(
          pyName,
          pyValue);
        return hydra.lib.flows.Pure.apply(hydra.lib.maybes.Maybe.apply(
          java.util.List.of(assignStmt),
          (java.util.function.Function<String, java.util.List<hydra.ext.python.syntax.Statement>>) (c -> java.util.List.of(
            assignStmt,
            hydra.ext.python.utils.Utils.pyExpressionToPyStatement(hydra.ext.python.utils.Utils.tripleQuotedString(c)))),
          mcomment));
      }));
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement> encodeUnionField(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name unionName, hydra.core.FieldType fieldType) {
    hydra.core.Name fname = (fieldType).name;
    hydra.core.Type ftype = (fieldType).type;
    return hydra.lib.flows.Bind.apply(
      hydra.ext.python.coder.Coder.inGraphContext(hydra.annotations.Annotations.getTypeDescription(ftype)),
      (java.util.function.Function<hydra.util.Maybe<String>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (fcomment -> {
        hydra.util.Lazy<Boolean> isUnit = new hydra.util.Lazy<>(() -> hydra.lib.equality.Equal.apply(
          hydra.rewriting.Rewriting.deannotateType(ftype),
          new hydra.core.Type.Unit()));
        hydra.ext.python.syntax.Name varName = hydra.ext.python.names.Names.variantName(
          false,
          env,
          unionName,
          fname);
        hydra.util.Lazy<hydra.ext.python.syntax.Block> body = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          isUnit.get(),
          () -> hydra.ext.python.utils.Utils.indentedBlock(
            fcomment,
            java.util.List.of(hydra.ext.python.utils.Utils.unitVariantMethods(varName))),
          () -> hydra.ext.python.utils.Utils.indentedBlock(
            fcomment,
            (java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>) (java.util.List.<java.util.List<hydra.ext.python.syntax.Statement>>of()))));
        java.util.List<hydra.core.Name> tparamNames = hydra.ext.python.coder.Coder.findTypeParams(
          env,
          ftype);
        hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Name>> tparamPyNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          hydra.ext.python.names.Names::encodeTypeVariable,
          tparamNames));
        hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.TypeParameter>> fieldParams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          hydra.ext.python.utils.Utils::pyNameToPyTypeParameter,
          tparamPyNames.get()));
        return hydra.lib.flows.Bind.apply(
          hydra.lib.logic.IfElse.lazy(
            isUnit.get(),
            () -> hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.ext.python.syntax.Args>) (hydra.util.Maybe.<hydra.ext.python.syntax.Args>nothing())),
            () -> hydra.lib.flows.Bind.apply(
              hydra.ext.python.coder.Coder.encodeTypeQuoted(
                env,
                ftype),
              (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Maybe<hydra.ext.python.syntax.Args>>>) (quotedType -> hydra.lib.flows.Pure.apply(hydra.util.Maybe.just(hydra.ext.python.coder.Coder.variantArgs(
                quotedType,
                (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()))))))),
          (java.util.function.Function<hydra.util.Maybe<hydra.ext.python.syntax.Args>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (margs -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.pyClassDefinitionToPyStatement(new hydra.ext.python.syntax.ClassDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), varName, fieldParams.get(), margs, body.get())))));
      }));
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>> encodeUnionType(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name name, hydra.core.RowType rowType, hydra.util.Maybe<String> comment) {
    java.util.List<hydra.core.FieldType> tfields = (rowType).fields;
    return hydra.lib.logic.IfElse.lazy(
      hydra.schemas.Schemas.isEnumRowType(rowType),
      () -> hydra.lib.flows.Bind.apply(
        hydra.lib.flows.MapList.apply(
          (java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (v1 -> hydra.ext.python.coder.Coder.encodeEnumValueAssignment(
            env,
            v1)),
          tfields),
        (java.util.function.Function<java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (vals -> {
          hydra.ext.python.syntax.Name enumName = new hydra.ext.python.syntax.Name("Enum");
          hydra.util.Maybe<hydra.ext.python.syntax.Args> args = hydra.util.Maybe.just(hydra.ext.python.utils.Utils.pyExpressionsToPyArgs(java.util.List.of(hydra.ext.python.utils.Utils.pyNameToPyExpression(enumName))));
          hydra.ext.python.syntax.Block body = hydra.ext.python.utils.Utils.indentedBlock(
            comment,
            vals);
          hydra.ext.python.syntax.Name pyName = hydra.ext.python.names.Names.encodeName(
            false,
            new hydra.util.CaseConvention.Pascal(),
            env,
            name);
          return hydra.lib.flows.Pure.apply(java.util.List.of(hydra.ext.python.utils.Utils.pyClassDefinitionToPyStatement(new hydra.ext.python.syntax.ClassDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), pyName, (java.util.List<hydra.ext.python.syntax.TypeParameter>) (java.util.List.<hydra.ext.python.syntax.TypeParameter>of()), args, body))));
        })),
      () -> hydra.lib.flows.Bind.apply(
        hydra.lib.flows.MapList.apply(
          (java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (v1 -> hydra.ext.python.coder.Coder.encodeUnionField(
            env,
            name,
            v1)),
          tfields),
        (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (fieldStmts -> {
          java.util.List<hydra.ext.python.syntax.TypeParameter> tparams = hydra.ext.python.coder.Coder.environmentTypeParameters(env);
          hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Primary>> unionAlts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.FieldType, hydra.ext.python.syntax.Primary>) (v1 -> hydra.ext.python.coder.Coder.encodeUnionFieldAlt(
              env,
              name,
              v1)),
            tfields));
          java.util.List<hydra.ext.python.syntax.Statement> unionStmts = hydra.ext.python.coder.Coder.unionTypeStatementsFor(
            env,
            hydra.ext.python.names.Names.encodeName(
              false,
              new hydra.util.CaseConvention.Pascal(),
              env,
              name),
            tparams,
            comment,
            hydra.ext.python.utils.Utils.orExpression(unionAlts.get()));
          return hydra.lib.flows.Pure.apply(hydra.lib.lists.Concat2.apply(
            fieldStmts,
            unionStmts));
        })));
  }
  
  static hydra.ext.python.syntax.Primary encodeUnionFieldAlt(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name unionName, hydra.core.FieldType fieldType) {
    hydra.core.Name fname = (fieldType).name;
    hydra.core.Type ftype = (fieldType).type;
    hydra.ext.python.syntax.Primary namePrim = hydra.ext.python.utils.Utils.pyNameToPyPrimary(hydra.ext.python.names.Names.variantName(
      false,
      env,
      unionName,
      fname));
    java.util.List<hydra.core.Name> tparamNames = hydra.ext.python.coder.Coder.findTypeParams(
      env,
      ftype);
    hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Name>> tparams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.ext.python.names.Names::encodeTypeVariable,
      tparamNames));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(tparams.get()),
      () -> namePrim,
      () -> ((java.util.function.Supplier<hydra.ext.python.syntax.Primary>) (() -> {
        hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> tparamExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          hydra.ext.python.utils.Utils::pyNameToPyExpression,
          tparams.get()));
        return hydra.ext.python.utils.Utils.primaryWithExpressionSlices(
          namePrim,
          tparamExprs.get());
      })).get());
  }
  
  static java.util.List<hydra.ext.python.syntax.Statement> encodeTypeDefSingle(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name name, hydra.util.Maybe<String> comment, hydra.ext.python.syntax.Expression typeExpr) {
    hydra.ext.python.syntax.Name pyName = hydra.ext.python.names.Names.encodeName(
      false,
      new hydra.util.CaseConvention.Pascal(),
      env,
      name);
    java.util.List<hydra.ext.python.syntax.TypeParameter> tparams = hydra.ext.python.coder.Coder.environmentTypeParameters(env);
    return java.util.List.of(hydra.ext.python.coder.Coder.typeAliasStatementFor(
      env,
      pyName,
      tparams,
      comment,
      typeExpr));
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>> encodeTypeAssignment(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name name, hydra.core.Type typ, hydra.util.Maybe<String> comment) {
    return hydra.lib.flows.Bind.apply(
      hydra.ext.python.coder.Coder.encodeTypeAssignmentInner(
        env,
        name,
        typ,
        comment),
      (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>>>) (defStmts -> {
        java.util.List<hydra.ext.python.syntax.Statement> constStmts = hydra.ext.python.coder.Coder.encodeNameConstants(
          env,
          name,
          typ);
        return hydra.lib.flows.Pure.apply(hydra.lib.lists.Concat2.apply(
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.ext.python.syntax.Statement, java.util.List<hydra.ext.python.syntax.Statement>>) (s -> java.util.List.of(s)),
            defStmts),
          java.util.List.of(constStmts)));
      }));
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>> encodeTypeAssignmentInner(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name name, hydra.core.Type typ, hydra.util.Maybe<String> comment) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>> otherwise(hydra.core.Type instance) {
        return hydra.ext.python.coder.Coder.encodeTypeAssignmentInner_dflt(
          comment,
          env,
          (java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.util.Maybe<String>, java.util.function.Function<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Statement>>>>>) (p0 -> p1 -> p2 -> p3 -> hydra.ext.python.coder.Coder.encodeTypeDefSingle(
            p0,
            p1,
            p2,
            p3)),
          name,
          typ);
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Type.Forall ft) {
        hydra.core.Type body = ((ft).value).body;
        hydra.core.Name tvar = ((ft).value).parameter;
        hydra.ext.python.helpers.PythonEnvironment newEnv = hydra.ext.python.coder.Coder.extendEnvWithTypeVar(
          env,
          tvar);
        return hydra.ext.python.coder.Coder.encodeTypeAssignmentInner(
          newEnv,
          name,
          body,
          comment);
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Type.Record rt) {
        return hydra.lib.flows.Map.apply(
          (java.util.function.Function<hydra.ext.python.syntax.Statement, java.util.List<hydra.ext.python.syntax.Statement>>) (s -> java.util.List.of(s)),
          hydra.ext.python.coder.Coder.encodeRecordType(
            env,
            name,
            (rt).value,
            comment));
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Type.Union rt) {
        return hydra.ext.python.coder.Coder.encodeUnionType(
          env,
          name,
          (rt).value,
          comment);
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Type.Wrap wt) {
        hydra.core.Type innerType = ((wt).value).body;
        return hydra.lib.flows.Map.apply(
          (java.util.function.Function<hydra.ext.python.syntax.Statement, java.util.List<hydra.ext.python.syntax.Statement>>) (s -> java.util.List.of(s)),
          hydra.ext.python.coder.Coder.encodeWrappedType(
            env,
            name,
            innerType,
            comment));
      }
    });
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T3, T2> encodeTypeAssignmentInner_dflt(T0 comment, hydra.ext.python.helpers.PythonEnvironment env, java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, java.util.function.Function<T1, java.util.function.Function<T0, java.util.function.Function<hydra.ext.python.syntax.Expression, T2>>>> hydra_ext_python_coder_encodeTypeDefSingle2, T1 name, hydra.core.Type typ) {
    return hydra.lib.flows.Bind.apply(
      hydra.ext.python.coder.Coder.<T3>encodeType(
        env,
        typ),
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<T3, T2>>) (typeExpr -> hydra.lib.flows.Pure.apply(((((hydra_ext_python_coder_encodeTypeDefSingle2).apply(env)).apply(name)).apply(comment)).apply(typeExpr))));
  }
  
  static hydra.ext.python.syntax.Expression unsupportedExpression(String msg) {
    return hydra.ext.python.utils.Utils.functionCall(
      hydra.ext.python.utils.Utils.pyExpressionToPyPrimary(hydra.ext.python.utils.Utils.projectFromExpression(
        hydra.ext.python.utils.Utils.projectFromExpression(
          hydra.ext.python.utils.Utils.projectFromExpression(
            new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("hydra")))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of())))))))),
            new hydra.ext.python.syntax.Name("dsl")),
          new hydra.ext.python.syntax.Name("python")),
        new hydra.ext.python.syntax.Name("unsupported"))),
      java.util.List.of(hydra.ext.python.utils.Utils.stringToPyExpression(
        new hydra.ext.python.syntax.QuoteStyle.Double_(),
        msg)));
  }
  
  static hydra.ext.python.syntax.Expression makeUncurriedLambda(java.util.List<hydra.ext.python.syntax.Name> params, hydra.ext.python.syntax.Expression body) {
    return new hydra.ext.python.syntax.Expression.Lambda(new hydra.ext.python.syntax.Lambda(new hydra.ext.python.syntax.LambdaParameters((hydra.util.Maybe<hydra.ext.python.syntax.LambdaSlashNoDefault>) (hydra.util.Maybe.<hydra.ext.python.syntax.LambdaSlashNoDefault>nothing()), hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ext.python.syntax.LambdaParamNoDefault>) (p -> new hydra.ext.python.syntax.LambdaParamNoDefault(p)),
      params), (java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault>) (java.util.List.<hydra.ext.python.syntax.LambdaParamWithDefault>of()), (hydra.util.Maybe<hydra.ext.python.syntax.LambdaStarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.LambdaStarEtc>nothing())), body));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, T1>> encodeField(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Field field, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, T1>> encodeTerm) {
    hydra.core.Name fname = (field).name;
    hydra.core.Term fterm = (field).term;
    return hydra.lib.flows.Bind.apply(
      (encodeTerm).apply(fterm),
      (java.util.function.Function<T1, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, T1>>>) (pterm -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, T1>) ((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, T1>) (new hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Name, T1>(hydra.ext.python.names.Names.encodeFieldName(
        env,
        fname), pterm))))));
  }
  
  static hydra.util.Maybe<hydra.core.CaseStatement> extractCaseElimination(hydra.core.Term term) {
    return (hydra.rewriting.Rewriting.deannotateAndDetypeTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.CaseStatement> otherwise(hydra.core.Term instance) {
        return (hydra.util.Maybe<hydra.core.CaseStatement>) (hydra.util.Maybe.<hydra.core.CaseStatement>nothing());
      }
      
      @Override
      public hydra.util.Maybe<hydra.core.CaseStatement> visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.core.CaseStatement> otherwise(hydra.core.Function instance) {
            return (hydra.util.Maybe<hydra.core.CaseStatement>) (hydra.util.Maybe.<hydra.core.CaseStatement>nothing());
          }
          
          @Override
          public hydra.util.Maybe<hydra.core.CaseStatement> visit(hydra.core.Function.Elimination e) {
            return ((e).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public hydra.util.Maybe<hydra.core.CaseStatement> otherwise(hydra.core.Elimination instance) {
                return (hydra.util.Maybe<hydra.core.CaseStatement>) (hydra.util.Maybe.<hydra.core.CaseStatement>nothing());
              }
              
              @Override
              public hydra.util.Maybe<hydra.core.CaseStatement> visit(hydra.core.Elimination.Union cs) {
                return hydra.util.Maybe.just((cs).value);
              }
            });
          }
        });
      }
    });
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T2, java.util.List<T3>> encodeBindingsAsDefs(T0 env, java.util.function.Function<T0, java.util.function.Function<T1, hydra.compute.Flow<T2, T3>>> encodeBinding, java.util.List<T1> bindings) {
    return hydra.lib.flows.MapList.apply(
      (java.util.function.Function<T1, hydra.compute.Flow<T2, T3>>) (v1 -> ((encodeBinding).apply(env)).apply(v1)),
      bindings);
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement> encodeBindingAs(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Binding binding) {
    hydra.core.Name name1 = (binding).name;
    hydra.ext.python.syntax.Name fname = hydra.ext.python.names.Names.encodeName(
      true,
      new hydra.util.CaseConvention.LowerSnake(),
      env,
      name1);
    hydra.util.Maybe<hydra.core.TypeScheme> mts = (binding).type;
    hydra.core.Term term1 = (binding).term;
    return hydra.lib.maybes.Maybe.apply(
      ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (() -> {
        hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Term> gathered = hydra.ext.python.coder.Coder.gatherLambdas(term1);
        return ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (() -> {
          hydra.util.Lazy<java.util.List<hydra.core.Name>> lambdaParams = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(gathered));
          return ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (() -> {
            hydra.util.Lazy<hydra.core.Term> innerBody = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(gathered));
            return ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (() -> {
              hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>> mcsa = hydra.ext.python.coder.Coder.isCaseStatementApplication(innerBody.get());
              return hydra.lib.maybes.Maybe.apply(
                ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (() -> {
                  hydra.util.Maybe<hydra.core.CaseStatement> mcs = hydra.ext.python.coder.Coder.extractCaseElimination(term1);
                  return hydra.lib.maybes.Maybe.apply(
                    hydra.lib.flows.Map.apply(
                      (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.ext.python.syntax.Statement>) (stmts -> hydra.lib.lists.Head.apply(stmts)),
                      hydra.ext.python.coder.Coder.encodeTermMultiline(
                        env,
                        term1)),
                    (java.util.function.Function<hydra.core.CaseStatement, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (cs -> {
                      java.util.List<hydra.core.Field> cases_ = (cs).cases;
                      hydra.util.Maybe<hydra.core.Term> dflt = (cs).default_;
                      hydra.core.Name tname = (cs).typeName;
                      return hydra.lib.flows.Bind.apply(
                        hydra.ext.python.coder.Coder.inGraphContext(hydra.schemas.Schemas.requireUnionType(tname)),
                        (java.util.function.Function<hydra.core.RowType, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (rt -> {
                          hydra.util.Lazy<hydra.ext.python.syntax.Param> innerParam = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Param(new hydra.ext.python.syntax.Name("x"), (hydra.util.Maybe<hydra.ext.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.ext.python.syntax.Annotation>nothing())));
                          Boolean isEnum = hydra.schemas.Schemas.isEnumRowType(rt);
                          hydra.util.Lazy<Boolean> isFull = new hydra.util.Lazy<>(() -> hydra.ext.python.coder.Coder.isCasesFull(
                            rt,
                            cases_));
                          hydra.util.Lazy<hydra.ext.python.syntax.ParamNoDefault> param = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.ParamNoDefault(innerParam.get(), (hydra.util.Maybe<hydra.ext.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.TypeComment>nothing())));
                          hydra.util.Lazy<hydra.ext.python.syntax.Parameters> params = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Parameters.ParamNoDefault(new hydra.ext.python.syntax.ParamNoDefaultParameters(java.util.List.of(param.get()), (java.util.List<hydra.ext.python.syntax.ParamWithDefault>) (java.util.List.<hydra.ext.python.syntax.ParamWithDefault>of()), (hydra.util.Maybe<hydra.ext.python.syntax.StarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.StarEtc>nothing()))));
                          return hydra.lib.flows.Bind.apply(
                            hydra.lib.flows.MapList.apply(
                              (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.CaseBlock>>) (v1 -> hydra.ext.python.coder.Coder.encodeCaseBlock(
                                env,
                                tname,
                                rt,
                                isEnum,
                                (java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>>) (e -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (t -> hydra.ext.python.coder.Coder.encodeTermMultiline(
                                  e,
                                  t))),
                                v1)),
                              cases_),
                            (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (pyCases -> hydra.lib.flows.Bind.apply(
                              hydra.ext.python.coder.Coder.encodeDefaultCaseBlock(
                                (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (t -> hydra.ext.python.coder.Coder.encodeTermInline(
                                  env,
                                  false,
                                  t)),
                                isFull.get(),
                                dflt,
                                tname),
                              (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (pyDflt -> {
                                hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.CaseBlock>> allCases = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                                  pyCases,
                                  pyDflt));
                                hydra.ext.python.syntax.SubjectExpression subj = new hydra.ext.python.syntax.SubjectExpression.Simple(new hydra.ext.python.syntax.NamedExpression.Simple(hydra.ext.python.utils.Utils.pyNameToPyExpression(new hydra.ext.python.syntax.Name("x"))));
                                hydra.ext.python.syntax.Statement matchStmt = new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Match(new hydra.ext.python.syntax.MatchStatement(subj, allCases.get())));
                                hydra.util.Lazy<hydra.ext.python.syntax.Block> body = new hydra.util.Lazy<>(() -> hydra.ext.python.utils.Utils.indentedBlock(
                                  (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
                                  java.util.List.of(java.util.List.of(matchStmt))));
                                hydra.util.Lazy<hydra.ext.python.syntax.FunctionDefRaw> funcDefRaw = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.FunctionDefRaw(false, fname, (java.util.List<hydra.ext.python.syntax.TypeParameter>) (java.util.List.<hydra.ext.python.syntax.TypeParameter>of()), hydra.util.Maybe.just(params.get()), (hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing()), (hydra.util.Maybe<hydra.ext.python.syntax.FuncTypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.FuncTypeComment>nothing()), body.get()));
                                return hydra.lib.flows.Pure.apply(new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Function(new hydra.ext.python.syntax.FunctionDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), funcDefRaw.get()))));
                              }))));
                        }));
                    }),
                    mcs);
                })).get(),
                (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (csa -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.lists.Null.apply(lambdaParams.get()),
                  () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (() -> {
                    hydra.util.Maybe<hydra.core.CaseStatement> mcs = hydra.ext.python.coder.Coder.extractCaseElimination(term1);
                    return hydra.lib.maybes.Maybe.apply(
                      hydra.lib.flows.Map.apply(
                        (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.ext.python.syntax.Statement>) (stmts -> hydra.lib.lists.Head.apply(stmts)),
                        hydra.ext.python.coder.Coder.encodeTermMultiline(
                          env,
                          term1)),
                      (java.util.function.Function<hydra.core.CaseStatement, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (cs -> {
                        java.util.List<hydra.core.Field> cases_ = (cs).cases;
                        hydra.util.Maybe<hydra.core.Term> dflt = (cs).default_;
                        hydra.core.Name tname = (cs).typeName;
                        return hydra.lib.flows.Bind.apply(
                          hydra.ext.python.coder.Coder.inGraphContext(hydra.schemas.Schemas.requireUnionType(tname)),
                          (java.util.function.Function<hydra.core.RowType, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (rt -> {
                            hydra.util.Lazy<hydra.ext.python.syntax.Param> innerParam = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Param(new hydra.ext.python.syntax.Name("x"), (hydra.util.Maybe<hydra.ext.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.ext.python.syntax.Annotation>nothing())));
                            Boolean isEnum = hydra.schemas.Schemas.isEnumRowType(rt);
                            hydra.util.Lazy<Boolean> isFull = new hydra.util.Lazy<>(() -> hydra.ext.python.coder.Coder.isCasesFull(
                              rt,
                              cases_));
                            hydra.util.Lazy<hydra.ext.python.syntax.ParamNoDefault> param = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.ParamNoDefault(innerParam.get(), (hydra.util.Maybe<hydra.ext.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.TypeComment>nothing())));
                            hydra.util.Lazy<hydra.ext.python.syntax.Parameters> params = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Parameters.ParamNoDefault(new hydra.ext.python.syntax.ParamNoDefaultParameters(java.util.List.of(param.get()), (java.util.List<hydra.ext.python.syntax.ParamWithDefault>) (java.util.List.<hydra.ext.python.syntax.ParamWithDefault>of()), (hydra.util.Maybe<hydra.ext.python.syntax.StarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.StarEtc>nothing()))));
                            return hydra.lib.flows.Bind.apply(
                              hydra.lib.flows.MapList.apply(
                                (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.CaseBlock>>) (v1 -> hydra.ext.python.coder.Coder.encodeCaseBlock(
                                  env,
                                  tname,
                                  rt,
                                  isEnum,
                                  (java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>>) (e -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (t -> hydra.ext.python.coder.Coder.encodeTermMultiline(
                                    e,
                                    t))),
                                  v1)),
                                cases_),
                              (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (pyCases -> hydra.lib.flows.Bind.apply(
                                hydra.ext.python.coder.Coder.encodeDefaultCaseBlock(
                                  (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (t -> hydra.ext.python.coder.Coder.encodeTermInline(
                                    env,
                                    false,
                                    t)),
                                  isFull.get(),
                                  dflt,
                                  tname),
                                (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (pyDflt -> {
                                  hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.CaseBlock>> allCases = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                                    pyCases,
                                    pyDflt));
                                  hydra.ext.python.syntax.SubjectExpression subj = new hydra.ext.python.syntax.SubjectExpression.Simple(new hydra.ext.python.syntax.NamedExpression.Simple(hydra.ext.python.utils.Utils.pyNameToPyExpression(new hydra.ext.python.syntax.Name("x"))));
                                  hydra.ext.python.syntax.Statement matchStmt = new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Match(new hydra.ext.python.syntax.MatchStatement(subj, allCases.get())));
                                  hydra.util.Lazy<hydra.ext.python.syntax.Block> body = new hydra.util.Lazy<>(() -> hydra.ext.python.utils.Utils.indentedBlock(
                                    (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
                                    java.util.List.of(java.util.List.of(matchStmt))));
                                  hydra.util.Lazy<hydra.ext.python.syntax.FunctionDefRaw> funcDefRaw = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.FunctionDefRaw(false, fname, (java.util.List<hydra.ext.python.syntax.TypeParameter>) (java.util.List.<hydra.ext.python.syntax.TypeParameter>of()), hydra.util.Maybe.just(params.get()), (hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing()), (hydra.util.Maybe<hydra.ext.python.syntax.FuncTypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.FuncTypeComment>nothing()), body.get()));
                                  return hydra.lib.flows.Pure.apply(new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Function(new hydra.ext.python.syntax.FunctionDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), funcDefRaw.get()))));
                                }))));
                          }));
                      }),
                      mcs);
                  })).get(),
                  () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (() -> {
                    hydra.util.Lazy<hydra.core.Name> tname = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(csa));
                    return ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (() -> {
                      hydra.util.Lazy<hydra.util.Tuple.Tuple2<hydra.util.Maybe<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>>> rest1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(csa));
                      return ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (() -> {
                        hydra.util.Lazy<hydra.util.Maybe<hydra.core.Term>> dflt = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(rest1.get()));
                        return ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (() -> {
                          hydra.util.Lazy<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Field>, hydra.core.Term>> rest2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(rest1.get()));
                          return ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (() -> {
                            hydra.util.Lazy<java.util.List<hydra.core.Field>> cases_ = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(rest2.get()));
                            return hydra.lib.flows.Bind.apply(
                              hydra.ext.python.coder.Coder.inGraphContext(hydra.schemas.Schemas.requireUnionType(tname.get())),
                              (java.util.function.Function<hydra.core.RowType, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (rt -> {
                                hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.ParamNoDefault>> capturedParams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                                  (java.util.function.Function<hydra.core.Name, hydra.ext.python.syntax.ParamNoDefault>) (n -> new hydra.ext.python.syntax.ParamNoDefault(new hydra.ext.python.syntax.Param(hydra.ext.python.names.Names.encodeName(
                                    false,
                                    new hydra.util.CaseConvention.LowerSnake(),
                                    env,
                                    n), (hydra.util.Maybe<hydra.ext.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.ext.python.syntax.Annotation>nothing())), (hydra.util.Maybe<hydra.ext.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.TypeComment>nothing()))),
                                  lambdaParams.get()));
                                hydra.ext.python.syntax.Name matchArgName = new hydra.ext.python.syntax.Name("x");
                                hydra.util.Lazy<hydra.ext.python.syntax.ParamNoDefault> matchParam = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.ParamNoDefault(new hydra.ext.python.syntax.Param(matchArgName, (hydra.util.Maybe<hydra.ext.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.ext.python.syntax.Annotation>nothing())), (hydra.util.Maybe<hydra.ext.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.TypeComment>nothing())));
                                hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.ParamNoDefault>> allParams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                                  capturedParams.get(),
                                  java.util.List.of(matchParam.get())));
                                hydra.ext.python.helpers.PythonEnvironment envWithParams = hydra.ext.python.coder.Coder.extendEnvWithLambdaParams(
                                  env,
                                  term1);
                                Boolean isEnum = hydra.schemas.Schemas.isEnumRowType(rt);
                                hydra.util.Lazy<Boolean> isFull = new hydra.util.Lazy<>(() -> hydra.ext.python.coder.Coder.isCasesFull(
                                  rt,
                                  cases_.get()));
                                hydra.util.Lazy<hydra.ext.python.syntax.Parameters> params = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Parameters.ParamNoDefault(new hydra.ext.python.syntax.ParamNoDefaultParameters(allParams.get(), (java.util.List<hydra.ext.python.syntax.ParamWithDefault>) (java.util.List.<hydra.ext.python.syntax.ParamWithDefault>of()), (hydra.util.Maybe<hydra.ext.python.syntax.StarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.StarEtc>nothing()))));
                                return hydra.lib.flows.Bind.apply(
                                  hydra.lib.flows.MapList.apply(
                                    (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.CaseBlock>>) (v1 -> hydra.ext.python.coder.Coder.encodeCaseBlock(
                                      envWithParams,
                                      tname.get(),
                                      rt,
                                      isEnum,
                                      (java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>>) (e -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (t -> hydra.ext.python.coder.Coder.encodeTermMultiline(
                                        e,
                                        t))),
                                      v1)),
                                    cases_.get()),
                                  (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (pyCases -> hydra.lib.flows.Bind.apply(
                                    hydra.ext.python.coder.Coder.encodeDefaultCaseBlock(
                                      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (t -> hydra.ext.python.coder.Coder.encodeTermInline(
                                        envWithParams,
                                        false,
                                        t)),
                                      isFull.get(),
                                      dflt.get(),
                                      tname.get()),
                                    (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (pyDflt -> {
                                      hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.CaseBlock>> allCases = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                                        pyCases,
                                        pyDflt));
                                      hydra.ext.python.syntax.SubjectExpression subj = new hydra.ext.python.syntax.SubjectExpression.Simple(new hydra.ext.python.syntax.NamedExpression.Simple(hydra.ext.python.utils.Utils.pyNameToPyExpression(matchArgName)));
                                      hydra.ext.python.syntax.Statement matchStmt = new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Match(new hydra.ext.python.syntax.MatchStatement(subj, allCases.get())));
                                      hydra.util.Lazy<hydra.ext.python.syntax.Block> body = new hydra.util.Lazy<>(() -> hydra.ext.python.utils.Utils.indentedBlock(
                                        (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
                                        java.util.List.of(java.util.List.of(matchStmt))));
                                      hydra.util.Lazy<hydra.ext.python.syntax.FunctionDefRaw> funcDefRaw = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.FunctionDefRaw(false, fname, (java.util.List<hydra.ext.python.syntax.TypeParameter>) (java.util.List.<hydra.ext.python.syntax.TypeParameter>of()), hydra.util.Maybe.just(params.get()), (hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing()), (hydra.util.Maybe<hydra.ext.python.syntax.FuncTypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.FuncTypeComment>nothing()), body.get()));
                                      return hydra.lib.flows.Pure.apply(new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Function(new hydra.ext.python.syntax.FunctionDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), funcDefRaw.get()))));
                                    }))));
                              }));
                          })).get();
                        })).get();
                      })).get();
                    })).get();
                  })).get())),
                mcsa);
            })).get();
          })).get();
        })).get();
      })).get(),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (ts -> hydra.lib.flows.Bind.apply(
        hydra.ext.python.coder.Coder.inGraphContext(hydra.annotations.Annotations.getTermDescription(term1)),
        (java.util.function.Function<hydra.util.Maybe<String>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (comment -> {
          hydra.util.Lazy<hydra.util.Maybe<String>> normComment = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
            hydra.coderUtils.CoderUtils::normalizeComment,
            comment));
          return hydra.ext.python.coder.Coder.encodeTermAssignment(
            env,
            name1,
            term1,
            ts,
            normComment.get());
        }))),
      mts);
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>> encodeDefinition(hydra.ext.python.helpers.PythonEnvironment env, hydra.module.Definition def_) {
    return (def_).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>> visit(hydra.module.Definition.Term td) {
        hydra.core.Name name = ((td).value).name;
        hydra.core.Term term = ((td).value).term;
        hydra.core.TypeScheme typ = ((td).value).type;
        return hydra.lib.flows.Bind.apply(
          hydra.ext.python.coder.Coder.inGraphContext(hydra.annotations.Annotations.getTermDescription(term)),
          (java.util.function.Function<hydra.util.Maybe<String>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>>>) (comment -> {
            hydra.util.Lazy<hydra.util.Maybe<String>> normComment = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
              hydra.coderUtils.CoderUtils::normalizeComment,
              comment));
            return hydra.lib.flows.Bind.apply(
              hydra.ext.python.coder.Coder.encodeTermAssignment(
                env,
                name,
                term,
                typ,
                normComment.get()),
              (java.util.function.Function<hydra.ext.python.syntax.Statement, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>>>) (stmt -> hydra.lib.flows.Pure.apply(java.util.List.of(java.util.List.of(stmt)))));
          }));
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>> visit(hydra.module.Definition.Type td) {
        hydra.core.Name name = ((td).value).name;
        hydra.core.Type typ = ((td).value).type;
        return hydra.lib.flows.Bind.apply(
          hydra.ext.python.coder.Coder.inGraphContext(hydra.annotations.Annotations.getTypeDescription(typ)),
          (java.util.function.Function<hydra.util.Maybe<String>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>>>) (comment -> {
            hydra.util.Lazy<hydra.util.Maybe<String>> normComment = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
              hydra.coderUtils.CoderUtils::normalizeComment,
              comment));
            return hydra.ext.python.coder.Coder.encodeTypeAssignment(
              env,
              name,
              typ,
              normComment.get());
          }));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.lang.Void> updateMeta(java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, hydra.ext.python.helpers.PythonModuleMetadata> v1) {
    return hydra.coderUtils.CoderUtils.updateCoderMetadata(
      hydra.ext.python.coder.Coder::pyGraphMetadata,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, hydra.ext.python.helpers.PyGraph>>) (p0 -> p1 -> hydra.ext.python.coder.Coder.makePyGraph(
        p0,
        p1)),
      hydra.ext.python.coder.Coder::pyGraphGraph,
      v1);
  }
  
  static <T0> hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, T0> withBindings(java.util.List<hydra.core.Binding> v1, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, T0> v2) {
    return hydra.coderUtils.CoderUtils.withGraphBindings(
      hydra.ext.python.coder.Coder::pyGraphGraph,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, hydra.ext.python.helpers.PyGraph>>) (p0 -> p1 -> hydra.ext.python.coder.Coder.makePyGraph(
        p0,
        p1)),
      hydra.ext.python.coder.Coder::pyGraphMetadata,
      v1,
      v2);
  }
  
  static <T0> hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, T0> withUpdatedGraph(java.util.function.Function<hydra.graph.Graph, hydra.graph.Graph> v1, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, T0> v2) {
    return hydra.coderUtils.CoderUtils.withUpdatedCoderGraph(
      hydra.ext.python.coder.Coder::pyGraphGraph,
      hydra.ext.python.coder.Coder::pyGraphMetadata,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, hydra.ext.python.helpers.PyGraph>>) (p0 -> p1 -> hydra.ext.python.coder.Coder.makePyGraph(
        p0,
        p1)),
      v1,
      v2);
  }
  
  static Integer termArityWithPrimitives(hydra.graph.Graph graph, hydra.core.Term term) {
    return (hydra.rewriting.Rewriting.deannotateAndDetypeTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Term instance) {
        return 0;
      }
      
      @Override
      public Integer visit(hydra.core.Term.Application app) {
        return hydra.lib.math.Max.apply(
          0,
          hydra.lib.math.Sub.apply(
            hydra.ext.python.coder.Coder.termArityWithPrimitives(
              graph,
              ((app).value).function),
            1));
      }
      
      @Override
      public Integer visit(hydra.core.Term.Function f) {
        return hydra.ext.python.coder.Coder.functionArityWithPrimitives(
          graph,
          (f).value);
      }
    });
  }
  
  static Integer functionArityWithPrimitives(hydra.graph.Graph graph, hydra.core.Function f) {
    return (f).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Function instance) {
        return 0;
      }
      
      @Override
      public Integer visit(hydra.core.Function.Elimination ignored) {
        return 1;
      }
      
      @Override
      public Integer visit(hydra.core.Function.Lambda lam) {
        return hydra.lib.math.Add.apply(
          1,
          hydra.ext.python.coder.Coder.termArityWithPrimitives(
            graph,
            ((lam).value).body));
      }
      
      @Override
      public Integer visit(hydra.core.Function.Primitive name) {
        return hydra.lib.maybes.Maybe.apply(
          0,
          (java.util.function.Function<hydra.graph.Primitive, Integer>) (prim -> hydra.arity.Arity.primitiveArity(prim)),
          hydra.lib.maps.Lookup.apply(
            (name).value,
            (graph).primitives));
      }
    });
  }
  
  static hydra.typing.TypeContext pythonEnvironmentGetTypeContext(hydra.ext.python.helpers.PythonEnvironment env) {
    return (env).typeContext;
  }
  
  static hydra.ext.python.helpers.PythonEnvironment pythonEnvironmentSetTypeContext(hydra.typing.TypeContext tc, hydra.ext.python.helpers.PythonEnvironment env) {
    return new hydra.ext.python.helpers.PythonEnvironment((env).namespaces, (env).boundTypeVariables, tc, (env).nullaryBindings, (env).version, (env).skipCasts, (env).inlineVariables);
  }
  
  static <T0> T0 withLambda(hydra.ext.python.helpers.PythonEnvironment v1, hydra.core.Lambda v2, java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, T0> v3) {
    return hydra.schemas.Schemas.withLambdaContext(
      hydra.ext.python.coder.Coder::pythonEnvironmentGetTypeContext,
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, hydra.ext.python.helpers.PythonEnvironment>>) (p0 -> p1 -> hydra.ext.python.coder.Coder.pythonEnvironmentSetTypeContext(
        p0,
        p1)),
      v1,
      v2,
      v3);
  }
  
  static <T0> T0 withTypeLambda(hydra.ext.python.helpers.PythonEnvironment v1, hydra.core.TypeLambda v2, java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, T0> v3) {
    return hydra.schemas.Schemas.withTypeLambdaContext(
      hydra.ext.python.coder.Coder::pythonEnvironmentGetTypeContext,
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, hydra.ext.python.helpers.PythonEnvironment>>) (p0 -> p1 -> hydra.ext.python.coder.Coder.pythonEnvironmentSetTypeContext(
        p0,
        p1)),
      v1,
      v2,
      v3);
  }
  
  static <T0> T0 withLet(hydra.ext.python.helpers.PythonEnvironment v1, hydra.core.Let v2, java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, T0> v3) {
    return hydra.schemas.Schemas.withLetContext(
      hydra.ext.python.coder.Coder::pythonEnvironmentGetTypeContext,
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, hydra.ext.python.helpers.PythonEnvironment>>) (p0 -> p1 -> hydra.ext.python.coder.Coder.pythonEnvironmentSetTypeContext(
        p0,
        p1)),
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (p0 -> p1 -> hydra.coderUtils.CoderUtils.bindingMetadata(
        p0,
        p1)),
      v1,
      v2,
      v3);
  }
  
  static <T0> T0 withLetInline(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Let lt, java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, T0> body) {
    hydra.util.Lazy<java.util.List<hydra.core.Name>> bindingNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.core.Name>) (b -> (b).name),
      (lt).bindings));
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> inlineVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(bindingNames.get()));
    return hydra.schemas.Schemas.withLetContext(
      hydra.ext.python.coder.Coder::pythonEnvironmentGetTypeContext,
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, hydra.ext.python.helpers.PythonEnvironment>>) (p0 -> p1 -> hydra.ext.python.coder.Coder.pythonEnvironmentSetTypeContext(
        p0,
        p1)),
      p0 -> p1 -> hydra.ext.python.coder.Coder.<hydra.typing.TypeContext, hydra.core.Binding, hydra.core.Term>withLetInline_noMetadata(
        p0,
        p1),
      env,
      lt,
      (java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, T0>) (innerEnv -> {
        hydra.util.Lazy<hydra.ext.python.helpers.PythonEnvironment> updatedEnv = new hydra.util.Lazy<>(() -> new hydra.ext.python.helpers.PythonEnvironment((innerEnv).namespaces, (innerEnv).boundTypeVariables, (innerEnv).typeContext, (innerEnv).nullaryBindings, (innerEnv).version, (innerEnv).skipCasts, hydra.lib.sets.Union.apply(
          inlineVars.get(),
          (innerEnv).inlineVariables)));
        return (body).apply(updatedEnv.get());
      }));
  }
  
  static <T0, T1, T2> hydra.util.Maybe<T2> withLetInline_noMetadata(T0 tc, T1 b) {
    return (hydra.util.Maybe<T2>) (hydra.util.Maybe.<T2>nothing());
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata initialMetadata(hydra.module.Namespace ns) {
    hydra.ext.python.syntax.DottedName dottedNs = hydra.ext.python.names.Names.encodeNamespace(ns);
    hydra.util.Lazy<hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>> emptyNs = new hydra.util.Lazy<>(() -> (hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>) (new hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>((hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>) ((hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>) (new hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>(ns, dottedNs))), (java.util.Map<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>) ((java.util.Map<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>) (hydra.lib.maps.Empty.<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>apply())))));
    return new hydra.ext.python.helpers.PythonModuleMetadata(emptyNs.get(), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false);
  }
  
  static hydra.ext.python.helpers.PythonEnvironment initialEnvironment(hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces, hydra.typing.TypeContext tcontext) {
    return new hydra.ext.python.helpers.PythonEnvironment(namespaces, (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()), (java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>) ((java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.ext.python.syntax.Name>apply()))))), tcontext, (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), hydra.ext.python.coder.Coder.targetPythonVersion(), true, (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()));
  }
  
  static hydra.ext.python.helpers.PythonVersion targetPythonVersion() {
    return hydra.ext.python.utils.Utils.targetPythonVersion();
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>> analyzePythonFunction(hydra.ext.python.helpers.PythonEnvironment v1, hydra.core.Term v2) {
    return hydra.coderUtils.CoderUtils.analyzeFunctionTerm(
      hydra.ext.python.coder.Coder::pythonEnvironmentGetTypeContext,
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, hydra.ext.python.helpers.PythonEnvironment>>) (p0 -> p1 -> hydra.ext.python.coder.Coder.pythonEnvironmentSetTypeContext(
        p0,
        p1)),
      v1,
      v2);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>> analyzePythonFunctionInline(hydra.ext.python.helpers.PythonEnvironment v1, hydra.core.Term v2) {
    return hydra.coderUtils.CoderUtils.analyzeFunctionTermInline(
      hydra.ext.python.coder.Coder::pythonEnvironmentGetTypeContext,
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, hydra.ext.python.helpers.PythonEnvironment>>) (p0 -> p1 -> hydra.ext.python.coder.Coder.pythonEnvironmentSetTypeContext(
        p0,
        p1)),
      v1,
      v2);
  }
  
  static <T0> T0 withDefinitions(hydra.ext.python.helpers.PythonEnvironment env, java.util.List<hydra.module.Definition> defs, java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, T0> body) {
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.core.Binding>>) (def_ -> (def_).accept(new hydra.module.Definition.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<hydra.core.Binding> otherwise(hydra.module.Definition instance) {
          return (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing());
        }
        
        @Override
        public hydra.util.Maybe<hydra.core.Binding> visit(hydra.module.Definition.Term td) {
          return hydra.util.Maybe.just(new hydra.core.Binding(((td).value).name, ((td).value).term, hydra.util.Maybe.just(((td).value).type)));
        }
        
        @Override
        public hydra.util.Maybe<hydra.core.Binding> visit(hydra.module.Definition.Type ignored) {
          return (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing());
        }
      })),
      defs)));
    hydra.core.Let dummyLet = new hydra.core.Let(bindings.get(), new hydra.core.Term.Literal(new hydra.core.Literal.String_("dummy")));
    return hydra.ext.python.coder.Coder.<T0>withLet(
      env,
      dummyLet,
      body);
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.NamedExpression> encodeBindingAsAssignment(Boolean allowThunking, hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Binding binding) {
    hydra.util.Maybe<hydra.core.TypeScheme> mts = (binding).type;
    hydra.core.Name name = (binding).name;
    hydra.ext.python.syntax.Name pyName = hydra.ext.python.names.Names.encodeName(
      false,
      new hydra.util.CaseConvention.LowerSnake(),
      env,
      name);
    hydra.core.Term term = (binding).term;
    return hydra.lib.flows.Bind.apply(
      hydra.ext.python.coder.Coder.encodeTermInline(
        env,
        false,
        term),
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.NamedExpression>>) (pbody -> {
        hydra.typing.TypeContext tc = (env).typeContext;
        Boolean isComplexVar = hydra.coderUtils.CoderUtils.isComplexVariable(
          tc,
          name);
        Boolean termIsComplex = hydra.coderUtils.CoderUtils.isComplexTerm(
          tc,
          term);
        hydra.util.Lazy<Boolean> needsThunk = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.apply(
          hydra.lib.logic.And.apply(
            allowThunking,
            hydra.lib.logic.Or.apply(
              isComplexVar,
              termIsComplex)),
          (java.util.function.Function<hydra.core.TypeScheme, Boolean>) (ts -> hydra.lib.logic.And.apply(
            allowThunking,
            hydra.lib.logic.And.apply(
              hydra.lib.equality.Equal.apply(
                hydra.arity.Arity.typeSchemeArity(ts),
                0),
              hydra.lib.logic.Or.apply(
                isComplexVar,
                termIsComplex)))),
          mts));
        hydra.util.Lazy<hydra.ext.python.syntax.Expression> pterm = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          needsThunk.get(),
          () -> hydra.ext.python.coder.Coder.makeThunk(pbody),
          () -> pbody));
        return hydra.lib.flows.Pure.apply(new hydra.ext.python.syntax.NamedExpression.Assignment(new hydra.ext.python.syntax.AssignmentExpression(pyName, pterm.get())));
      }));
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement> encodeFunctionDefinition(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name name, java.util.List<hydra.core.Name> tparams, java.util.List<hydra.core.Name> args, hydra.core.Term body, java.util.List<hydra.core.Type> doms, hydra.util.Maybe<hydra.core.Type> mcod, hydra.util.Maybe<String> comment, java.util.List<hydra.ext.python.syntax.Statement> prefixes) {
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapList.apply(
        (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.ParamNoDefault>>) (pair -> {
          hydra.util.Lazy<hydra.core.Name> argName = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
          hydra.util.Lazy<hydra.core.Type> typ = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
          return hydra.lib.flows.Bind.apply(
            hydra.ext.python.coder.Coder.encodeType(
              env,
              typ.get()),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.ParamNoDefault>>) (pyTyp -> hydra.lib.flows.Pure.apply(new hydra.ext.python.syntax.ParamNoDefault(new hydra.ext.python.syntax.Param(hydra.ext.python.names.Names.encodeName(
              false,
              new hydra.util.CaseConvention.LowerSnake(),
              env,
              argName.get()), hydra.util.Maybe.just(new hydra.ext.python.syntax.Annotation(pyTyp))), (hydra.util.Maybe<hydra.ext.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.TypeComment>nothing())))));
        }),
        hydra.lib.lists.Zip.apply(
          args,
          doms)),
      (java.util.function.Function<java.util.List<hydra.ext.python.syntax.ParamNoDefault>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (pyArgs -> {
        hydra.util.Lazy<hydra.ext.python.syntax.Parameters> pyParams = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Parameters.ParamNoDefault(new hydra.ext.python.syntax.ParamNoDefaultParameters(pyArgs, (java.util.List<hydra.ext.python.syntax.ParamWithDefault>) (java.util.List.<hydra.ext.python.syntax.ParamWithDefault>of()), (hydra.util.Maybe<hydra.ext.python.syntax.StarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.StarEtc>nothing()))));
        return hydra.lib.flows.Bind.apply(
          hydra.ext.python.coder.Coder.encodeTermMultiline(
            env,
            body),
          (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (stmts -> {
            hydra.util.Lazy<hydra.ext.python.syntax.Block> block = new hydra.util.Lazy<>(() -> hydra.ext.python.utils.Utils.indentedBlock(
              comment,
              java.util.List.of(hydra.lib.lists.Concat2.apply(
                prefixes,
                stmts))));
            return hydra.lib.flows.Bind.apply(
              hydra.lib.maybes.Maybe.apply(
                hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing())),
                (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Maybe<hydra.ext.python.syntax.Expression>>>) (cod -> hydra.lib.flows.Bind.apply(
                  hydra.ext.python.coder.Coder.encodeType(
                    env,
                    cod),
                  (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Maybe<hydra.ext.python.syntax.Expression>>>) (pytyp -> hydra.lib.flows.Pure.apply(hydra.util.Maybe.just(pytyp))))),
                mcod),
              (java.util.function.Function<hydra.util.Maybe<hydra.ext.python.syntax.Expression>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (mreturnType -> {
                hydra.util.Lazy<Boolean> isThunk = new hydra.util.Lazy<>(() -> hydra.lib.lists.Null.apply(args));
                hydra.util.Lazy<hydra.util.Maybe<hydra.ext.python.syntax.Decorators>> mDecorators = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                  isThunk.get(),
                  () -> hydra.util.Maybe.just(new hydra.ext.python.syntax.Decorators(java.util.List.of(hydra.ext.python.coder.Coder.lruCacheDecorator()))),
                  () -> (hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing())));
                hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.TypeParameter>> pyTparams = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                  hydra.ext.python.coder.Coder.useInlineTypeParams(),
                  () -> hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.core.Name, hydra.ext.python.syntax.TypeParameter>) (arg_ -> hydra.ext.python.utils.Utils.pyNameToPyTypeParameter(hydra.ext.python.names.Names.encodeTypeVariable(arg_))),
                    tparams),
                  () -> (java.util.List<hydra.ext.python.syntax.TypeParameter>) (java.util.List.<hydra.ext.python.syntax.TypeParameter>of())));
                return hydra.lib.flows.Bind.apply(
                  hydra.lib.logic.IfElse.lazy(
                    isThunk.get(),
                    () -> hydra.ext.python.coder.Coder.updateMeta((java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, hydra.ext.python.helpers.PythonModuleMetadata>) (v1 -> hydra.ext.python.coder.Coder.setMetaUsesLruCache(
                      true,
                      v1))),
                    () -> hydra.lib.flows.Pure.apply(null)),
                  (java.util.function.Function<java.lang.Void, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (unit1 -> {
                    hydra.ext.python.syntax.Name pyName = hydra.ext.python.names.Names.encodeName(
                      false,
                      new hydra.util.CaseConvention.LowerSnake(),
                      env,
                      name);
                    return hydra.lib.flows.Pure.apply(new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Function(new hydra.ext.python.syntax.FunctionDefinition(mDecorators.get(), new hydra.ext.python.syntax.FunctionDefRaw(false, pyName, pyTparams.get(), hydra.util.Maybe.just(pyParams.get()), mreturnType, (hydra.util.Maybe<hydra.ext.python.syntax.FuncTypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.FuncTypeComment>nothing()), block.get())))));
                  }));
              }));
          }));
      }));
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>> encodeTermMultiline(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Term term) {
    hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term> gathered = hydra.coderUtils.CoderUtils.gatherApplications(term);
    hydra.util.Lazy<java.util.List<hydra.core.Term>> args = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(gathered));
    hydra.util.Lazy<hydra.core.Term> body = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(gathered));
    hydra.util.Lazy<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>> dfltLogic = new hydra.util.Lazy<>(() -> hydra.lib.flows.Bind.apply(
      hydra.ext.python.coder.Coder.analyzePythonFunction(
        env,
        term),
      (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (fs -> {
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, java.util.List<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
        hydra.util.Lazy<hydra.ext.python.helpers.PythonEnvironment> env2 = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, hydra.ext.python.helpers.PythonEnvironment>) (projected -> projected.environment)).apply(fs));
        hydra.util.Lazy<hydra.core.Term> innerBody = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
        hydra.util.Lazy<java.util.List<hydra.core.Name>> params = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, java.util.List<hydra.core.Name>>) (projected -> projected.params)).apply(fs));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(params.get())),
          () -> hydra.lib.flows.Fail.apply("Functions currently unsupported in this context"),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply(bindings.get()),
            () -> hydra.lib.flows.Bind.apply(
              hydra.ext.python.coder.Coder.encodeTermInline(
                env,
                false,
                term),
              (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (expr -> hydra.lib.flows.Pure.apply(java.util.List.of(hydra.ext.python.utils.Utils.returnSingle(expr))))),
            () -> hydra.ext.python.coder.Coder.withBindings(
              bindings.get(),
              hydra.lib.flows.Bind.apply(
                hydra.lib.flows.MapList.apply(
                  (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (v1 -> hydra.ext.python.coder.Coder.encodeBindingAs(
                    env2.get(),
                    v1)),
                  bindings.get()),
                (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (bindingStmts -> hydra.lib.flows.Bind.apply(
                  hydra.ext.python.coder.Coder.encodeTermMultiline(
                    env2.get(),
                    innerBody.get()),
                  (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (bodyStmts -> hydra.lib.flows.Pure.apply(hydra.lib.lists.Concat2.apply(
                    bindingStmts,
                    bodyStmts)))))))));
      })));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply(args.get()),
        1),
      () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (() -> {
        hydra.util.Lazy<hydra.core.Term> arg = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(args.get()));
        return (hydra.rewriting.Rewriting.deannotateAndDetypeTerm(body.get())).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>> otherwise(hydra.core.Term instance) {
            return dfltLogic.get();
          }
          
          @Override
          public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Term.Function f) {
            return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
              @Override
              public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>> otherwise(hydra.core.Function instance) {
                return dfltLogic.get();
              }
              
              @Override
              public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Function.Elimination e) {
                return ((e).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
                  @Override
                  public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>> otherwise(hydra.core.Elimination instance) {
                    return dfltLogic.get();
                  }
                  
                  @Override
                  public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Elimination.Union cs) {
                    java.util.List<hydra.core.Field> cases_ = ((cs).value).cases;
                    hydra.util.Maybe<hydra.core.Term> dflt = ((cs).value).default_;
                    hydra.core.Name tname = ((cs).value).typeName;
                    return hydra.lib.flows.Bind.apply(
                      hydra.ext.python.coder.Coder.inGraphContext(hydra.schemas.Schemas.requireUnionType(tname)),
                      (java.util.function.Function<hydra.core.RowType, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (rt -> {
                        Boolean isEnum = hydra.schemas.Schemas.isEnumRowType(rt);
                        hydra.util.Lazy<Boolean> isFull = new hydra.util.Lazy<>(() -> hydra.ext.python.coder.Coder.isCasesFull(
                          rt,
                          cases_));
                        return hydra.lib.flows.Bind.apply(
                          hydra.ext.python.coder.Coder.encodeTermInline(
                            env,
                            false,
                            arg.get()),
                          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (pyArg -> hydra.lib.flows.Bind.apply(
                            hydra.lib.flows.MapList.apply(
                              (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.CaseBlock>>) (v1 -> hydra.ext.python.coder.Coder.encodeCaseBlock(
                                env,
                                tname,
                                rt,
                                isEnum,
                                (java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>>) (e2 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (t -> hydra.ext.python.coder.Coder.encodeTermMultiline(
                                  e2,
                                  t))),
                                v1)),
                              hydra.ext.python.coder.Coder.deduplicateCaseVariables(cases_)),
                            (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (pyCases -> hydra.lib.flows.Bind.apply(
                              hydra.ext.python.coder.Coder.encodeDefaultCaseBlock(
                                (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (t -> hydra.ext.python.coder.Coder.encodeTermInline(
                                  env,
                                  false,
                                  t)),
                                isFull.get(),
                                dflt,
                                tname),
                              (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Statement>>>) (pyDflt -> {
                                hydra.ext.python.syntax.SubjectExpression subj = new hydra.ext.python.syntax.SubjectExpression.Simple(new hydra.ext.python.syntax.NamedExpression.Simple(pyArg));
                                hydra.util.Lazy<hydra.ext.python.syntax.Statement> matchStmt = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Match(new hydra.ext.python.syntax.MatchStatement(subj, hydra.lib.lists.Concat2.apply(
                                  pyCases,
                                  pyDflt)))));
                                return hydra.lib.flows.Pure.apply(java.util.List.of(matchStmt.get()));
                              }))))));
                      }));
                  }
                });
              }
            });
          }
        });
      })).get(),
      () -> dfltLogic.get());
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> encodeFunction(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Function f) {
    return (f).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Function.Lambda lam) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.python.coder.Coder.analyzePythonFunctionInline(
            env,
            new hydra.core.Term.Function(new hydra.core.Function.Lambda((lam).value))),
          (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (fs -> {
            hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, java.util.List<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
            hydra.util.Lazy<hydra.core.Term> innerBody = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
            hydra.util.Lazy<hydra.ext.python.helpers.PythonEnvironment> innerEnv = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, hydra.ext.python.helpers.PythonEnvironment>) (projected -> projected.environment)).apply(fs));
            hydra.util.Lazy<java.util.List<hydra.core.Name>> params = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, java.util.List<hydra.core.Name>>) (projected -> projected.params)).apply(fs));
            return hydra.lib.flows.Bind.apply(
              hydra.ext.python.coder.Coder.encodeTermInline(
                innerEnv.get(),
                false,
                innerBody.get()),
              (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pbody -> {
                hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Name>> pparams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Name, hydra.ext.python.syntax.Name>) (v1 -> hydra.ext.python.names.Names.encodeName(
                    false,
                    new hydra.util.CaseConvention.LowerSnake(),
                    innerEnv.get(),
                    v1)),
                  params.get()));
                return hydra.lib.logic.IfElse.lazy(
                  hydra.lib.lists.Null.apply(bindings.get()),
                  () -> hydra.lib.flows.Pure.apply(hydra.ext.python.coder.Coder.makeUncurriedLambda(
                    pparams.get(),
                    pbody)),
                  () -> hydra.lib.flows.Bind.apply(
                    hydra.lib.flows.MapList.apply(
                      (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.NamedExpression>>) (v1 -> hydra.ext.python.coder.Coder.encodeBindingAsAssignment(
                        false,
                        innerEnv.get(),
                        v1)),
                      bindings.get()),
                    (java.util.function.Function<java.util.List<hydra.ext.python.syntax.NamedExpression>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pbindingExprs -> {
                      hydra.util.Lazy<hydra.ext.python.syntax.Expression> indexValue = new hydra.util.Lazy<>(() -> hydra.ext.python.utils.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Number_(new hydra.ext.python.syntax.Number_.Integer_(hydra.lib.literals.Int32ToBigint.apply(hydra.lib.lists.Length.apply(bindings.get()))))));
                      hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.StarNamedExpression>> pbindingStarExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                        (java.util.function.Function<hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.StarNamedExpression>) (ne -> new hydra.ext.python.syntax.StarNamedExpression.Simple(ne)),
                        pbindingExprs));
                      hydra.ext.python.syntax.StarNamedExpression pbodyStarExpr = hydra.ext.python.utils.Utils.pyExpressionToPyStarNamedExpression(pbody);
                      hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.StarNamedExpression>> tupleElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                        pbindingStarExprs.get(),
                        java.util.List.of(pbodyStarExpr)));
                      hydra.ext.python.syntax.Expression tupleExpr = hydra.ext.python.utils.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Tuple(new hydra.ext.python.syntax.Tuple(tupleElements.get())));
                      hydra.ext.python.syntax.Primary indexedExpr = hydra.ext.python.utils.Utils.primaryWithExpressionSlices(
                        hydra.ext.python.utils.Utils.pyExpressionToPyPrimary(tupleExpr),
                        java.util.List.of(indexValue.get()));
                      return hydra.lib.flows.Pure.apply(hydra.ext.python.coder.Coder.makeUncurriedLambda(
                        pparams.get(),
                        hydra.ext.python.utils.Utils.pyPrimaryToPyExpression(indexedExpr)));
                    })));
              }));
          }));
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Function.Primitive name) {
        return hydra.ext.python.coder.Coder.encodeVariable(
          env,
          (name).value,
          (java.util.List<hydra.ext.python.syntax.Expression>) (java.util.List.<hydra.ext.python.syntax.Expression>of()));
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Function.Elimination e) {
        return ((e).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Elimination.Record proj) {
            hydra.core.Name fname = ((proj).value).field;
            return hydra.lib.flows.Pure.apply(hydra.ext.python.coder.Coder.makeCurriedLambda(
              java.util.List.of(new hydra.ext.python.syntax.Name("v1")),
              hydra.ext.python.utils.Utils.projectFromExpression(
                new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("v1")))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of())))))))),
                hydra.ext.python.names.Names.encodeFieldName(
                  env,
                  fname))));
          }
          
          @Override
          public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Elimination.Wrap ignored) {
            return hydra.lib.flows.Pure.apply(hydra.ext.python.coder.Coder.makeCurriedLambda(
              java.util.List.of(new hydra.ext.python.syntax.Name("v1")),
              hydra.ext.python.utils.Utils.projectFromExpression(
                new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("v1")))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of())))))))),
                new hydra.ext.python.syntax.Name("value"))));
          }
          
          @Override
          public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Elimination.Union ignored) {
            return hydra.lib.flows.Pure.apply(hydra.ext.python.coder.Coder.unsupportedExpression("case expressions as values are not yet supported"));
          }
        });
      }
    });
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement> encodeTermAssignment(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name name, hydra.core.Term term, hydra.core.TypeScheme ts, hydra.util.Maybe<String> comment) {
    return hydra.lib.flows.Bind.apply(
      hydra.ext.python.coder.Coder.analyzePythonFunction(
        env,
        term),
      (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (fs -> {
        hydra.core.Binding binding = new hydra.core.Binding(name, term, hydra.util.Maybe.just(ts));
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, java.util.List<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
        hydra.util.Lazy<hydra.core.Term> body = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
        hydra.util.Lazy<java.util.List<hydra.core.Type>> doms = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, java.util.List<hydra.core.Type>>) (projected -> projected.domains)).apply(fs));
        hydra.util.Lazy<hydra.ext.python.helpers.PythonEnvironment> env2 = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, hydra.ext.python.helpers.PythonEnvironment>) (projected -> projected.environment)).apply(fs));
        hydra.typing.TypeContext tc = (env2.get()).typeContext;
        Boolean isComplex = hydra.coderUtils.CoderUtils.isComplexBinding(
          tc,
          binding);
        hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> mcod = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, hydra.util.Maybe<hydra.core.Type>>) (projected -> projected.codomain)).apply(fs));
        hydra.util.Lazy<java.util.List<hydra.core.Name>> params = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, java.util.List<hydra.core.Name>>) (projected -> projected.params)).apply(fs));
        hydra.util.Lazy<java.util.List<hydra.core.Name>> tparams = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.helpers.PythonEnvironment>, java.util.List<hydra.core.Name>>) (projected -> projected.typeParams)).apply(fs));
        return hydra.lib.logic.IfElse.lazy(
          isComplex,
          () -> hydra.ext.python.coder.Coder.withBindings(
            bindings.get(),
            hydra.lib.flows.Bind.apply(
              hydra.lib.flows.MapList.apply(
                (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (v1 -> hydra.ext.python.coder.Coder.encodeBindingAs(
                  env2.get(),
                  v1)),
                bindings.get()),
              (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (bindingStmts -> hydra.ext.python.coder.Coder.encodeFunctionDefinition(
                env2.get(),
                name,
                tparams.get(),
                params.get(),
                body.get(),
                doms.get(),
                mcod.get(),
                comment,
                bindingStmts)))),
          () -> hydra.lib.flows.Bind.apply(
            hydra.ext.python.coder.Coder.encodeTermInline(
              env2.get(),
              false,
              body.get()),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement>>) (bodyExpr -> {
              hydra.ext.python.syntax.Name pyName = hydra.ext.python.names.Names.encodeName(
                false,
                new hydra.util.CaseConvention.LowerSnake(),
                env2.get(),
                name);
              return hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.annotatedStatement(
                comment,
                hydra.ext.python.utils.Utils.assignmentStatement(
                  pyName,
                  bodyExpr)));
            })));
      }));
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> encodeVariable(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name name, java.util.List<hydra.ext.python.syntax.Expression> args) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.ext.python.helpers.PyGraph>getState(),
      (java.util.function.Function<hydra.ext.python.helpers.PyGraph, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pyg -> {
        hydra.ext.python.syntax.Expression asFunctionCall = hydra.ext.python.utils.Utils.functionCall(
          hydra.ext.python.utils.Utils.pyNameToPyPrimary(hydra.ext.python.names.Names.encodeName(
            true,
            new hydra.util.CaseConvention.LowerSnake(),
            env,
            name)),
          args);
        hydra.ext.python.syntax.Expression asVariable = hydra.ext.python.names.Names.termVariableReference(
          env,
          name);
        hydra.graph.Graph g = hydra.ext.python.coder.Coder.pyGraphGraph(pyg);
        java.util.Set<hydra.core.Name> inlineVars = (env).inlineVariables;
        hydra.typing.TypeContext tc = (env).typeContext;
        java.util.Map<hydra.core.Name, hydra.core.Type> tcTypes = (tc).types;
        hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> mTyp = new hydra.util.Lazy<>(() -> hydra.lib.maps.Lookup.apply(
          name,
          tcTypes));
        java.util.Set<hydra.core.Name> tcLambdaVars = (tc).lambdaVariables;
        java.util.Map<hydra.core.Name, hydra.core.Term> tcMetadata = (tc).metadata;
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(args)),
          () -> hydra.lib.maybes.Maybe.apply(
            hydra.lib.flows.Pure.apply(asFunctionCall),
            (java.util.function.Function<hydra.graph.Primitive, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (prim -> {
              Integer primArity = hydra.arity.Arity.primitiveArity(prim);
              return hydra.lib.logic.IfElse.lazy(
                hydra.lib.equality.Equal.apply(
                  primArity,
                  hydra.lib.lists.Length.apply(args)),
                () -> hydra.lib.flows.Pure.apply(asFunctionCall),
                () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
                  hydra.util.Lazy<Integer> numRemaining = new hydra.util.Lazy<>(() -> hydra.lib.math.Sub.apply(
                    primArity,
                    hydra.lib.lists.Length.apply(args)));
                  return ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
                    hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Name>> remainingParams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                      (java.util.function.Function<Integer, hydra.ext.python.syntax.Name>) (i -> new hydra.ext.python.syntax.Name(hydra.lib.strings.Cat2.apply(
                        "x",
                        hydra.lib.literals.ShowInt32.apply(i)))),
                      hydra.lib.math.Range.apply(
                        1,
                        numRemaining.get())));
                    return ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
                      hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> remainingExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                        (java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ext.python.syntax.Expression>) (n -> new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(n))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of()))))))))),
                        remainingParams.get()));
                      return ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
                        hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> allArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                          args,
                          remainingExprs.get()));
                        return ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
                          hydra.ext.python.syntax.Expression fullCall = hydra.ext.python.utils.Utils.functionCall(
                            hydra.ext.python.utils.Utils.pyNameToPyPrimary(hydra.ext.python.names.Names.encodeName(
                              true,
                              new hydra.util.CaseConvention.LowerSnake(),
                              env,
                              name)),
                            allArgs.get());
                          return hydra.lib.flows.Pure.apply(hydra.ext.python.coder.Coder.makeUncurriedLambda(
                            remainingParams.get(),
                            fullCall));
                        })).get();
                      })).get();
                    })).get();
                  })).get();
                })).get());
            }),
            hydra.lexical.Lexical.lookupPrimitive(
              g,
              name)),
          () -> hydra.lib.maybes.Maybe.apply(
            hydra.lib.logic.IfElse.lazy(
              hydra.lib.sets.Member.apply(
                name,
                tcLambdaVars),
              () -> hydra.lib.flows.Pure.apply(asVariable),
              () -> hydra.lib.maybes.Maybe.apply(
                hydra.lib.maybes.Maybe.apply(
                  hydra.lib.maybes.Maybe.apply(
                    hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
                      "Unknown variable: ",
                      (name).value)),
                    (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (ignored -> hydra.lib.flows.Pure.apply(asFunctionCall)),
                    hydra.lib.maps.Lookup.apply(
                      name,
                      tcMetadata)),
                  (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (el -> hydra.lib.maybes.Maybe.apply(
                    hydra.lib.flows.Pure.apply(asVariable),
                    (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (ts -> hydra.lib.logic.IfElse.lazy(
                      hydra.lib.logic.And.apply(
                        hydra.lib.equality.Equal.apply(
                          hydra.arity.Arity.typeSchemeArity(ts),
                          0),
                        hydra.coderUtils.CoderUtils.isComplexBinding(
                          tc,
                          el)),
                      () -> hydra.lib.flows.Pure.apply(asFunctionCall),
                      () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
                        hydra.util.Lazy<hydra.ext.python.syntax.Expression> asFunctionRef = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                          hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply((ts).variables)),
                          () -> hydra.ext.python.coder.Coder.makeSimpleLambda(
                            hydra.arity.Arity.typeArity((ts).type),
                            asVariable),
                          () -> asVariable));
                        return hydra.lib.flows.Pure.apply(asFunctionRef.get());
                      })).get())),
                    (el).type)),
                  hydra.lexical.Lexical.lookupElement(
                    g,
                    name)),
                (java.util.function.Function<hydra.graph.Primitive, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (prim -> {
                  Integer primArity = hydra.arity.Arity.primitiveArity(prim);
                  return hydra.lib.logic.IfElse.lazy(
                    hydra.lib.equality.Equal.apply(
                      primArity,
                      0),
                    () -> hydra.lib.flows.Pure.apply(asFunctionCall),
                    () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
                      hydra.core.TypeScheme ts = (prim).type;
                      return ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
                        hydra.util.Lazy<hydra.ext.python.syntax.Expression> asFunctionRef = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                          hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply((ts).variables)),
                          () -> hydra.ext.python.coder.Coder.makeSimpleLambda(
                            hydra.arity.Arity.typeArity((ts).type),
                            asVariable),
                          () -> asVariable));
                        return hydra.lib.flows.Pure.apply(asFunctionRef.get());
                      })).get();
                    })).get());
                }),
                hydra.lexical.Lexical.lookupPrimitive(
                  g,
                  name))),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (typ -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.sets.Member.apply(
                name,
                tcLambdaVars),
              () -> hydra.lib.flows.Pure.apply(asVariable),
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.sets.Member.apply(
                  name,
                  inlineVars),
                () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
                  hydra.util.Lazy<hydra.ext.python.syntax.Expression> asFunctionRef = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                    hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.rewriting.Rewriting.freeVariablesInType(typ))),
                    () -> hydra.ext.python.coder.Coder.makeSimpleLambda(
                      hydra.arity.Arity.typeArity(typ),
                      asVariable),
                    () -> asVariable));
                  return hydra.lib.flows.Pure.apply(asFunctionRef.get());
                })).get(),
                () -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.logic.Not.apply(hydra.lib.maps.Member.apply(
                    name,
                    tcMetadata)),
                  () -> hydra.lib.maybes.Maybe.apply(
                    ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
                      hydra.util.Lazy<hydra.ext.python.syntax.Expression> asFunctionRef = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                        hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.rewriting.Rewriting.freeVariablesInType(typ))),
                        () -> hydra.ext.python.coder.Coder.makeSimpleLambda(
                          hydra.arity.Arity.typeArity(typ),
                          asVariable),
                        () -> asVariable));
                      return hydra.lib.flows.Pure.apply(asFunctionRef.get());
                    })).get(),
                    (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (el -> hydra.lib.maybes.Maybe.apply(
                      hydra.lib.logic.IfElse.lazy(
                        hydra.lib.equality.Equal.apply(
                          hydra.arity.Arity.typeArity(typ),
                          0),
                        () -> hydra.lib.flows.Pure.apply(asFunctionCall),
                        () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
                          hydra.util.Lazy<hydra.ext.python.syntax.Expression> asFunctionRef = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                            hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.rewriting.Rewriting.freeVariablesInType(typ))),
                            () -> hydra.ext.python.coder.Coder.makeSimpleLambda(
                              hydra.arity.Arity.typeArity(typ),
                              asVariable),
                            () -> asVariable));
                          return hydra.lib.flows.Pure.apply(asFunctionRef.get());
                        })).get()),
                      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (ts -> hydra.lib.logic.IfElse.lazy(
                        hydra.lib.logic.And.apply(
                          hydra.lib.equality.Equal.apply(
                            hydra.arity.Arity.typeArity(typ),
                            0),
                          hydra.coderUtils.CoderUtils.isComplexBinding(
                            tc,
                            el)),
                        () -> hydra.lib.flows.Pure.apply(asFunctionCall),
                        () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
                          hydra.util.Lazy<hydra.ext.python.syntax.Expression> asFunctionRef = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                            hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.rewriting.Rewriting.freeVariablesInType(typ))),
                            () -> hydra.ext.python.coder.Coder.makeSimpleLambda(
                              hydra.arity.Arity.typeArity(typ),
                              asVariable),
                            () -> asVariable));
                          return hydra.lib.flows.Pure.apply(asFunctionRef.get());
                        })).get())),
                      (el).type)),
                    hydra.lexical.Lexical.lookupElement(
                      g,
                      name)),
                  () -> hydra.lib.logic.IfElse.lazy(
                    hydra.lib.logic.And.apply(
                      hydra.lib.equality.Equal.apply(
                        hydra.arity.Arity.typeArity(typ),
                        0),
                      hydra.coderUtils.CoderUtils.isComplexVariable(
                        tc,
                        name)),
                    () -> hydra.lib.flows.Pure.apply(asFunctionCall),
                    () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
                      hydra.util.Lazy<hydra.ext.python.syntax.Expression> asFunctionRef = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                        hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.rewriting.Rewriting.freeVariablesInType(typ))),
                        () -> hydra.ext.python.coder.Coder.makeSimpleLambda(
                          hydra.arity.Arity.typeArity(typ),
                          asVariable),
                        () -> asVariable));
                      return hydra.lib.flows.Pure.apply(asFunctionRef.get());
                    })).get()))))),
            mTyp.get()));
      }));
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> encodeApplication(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Application app) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.ext.python.helpers.PyGraph>getState(),
      (java.util.function.Function<hydra.ext.python.helpers.PyGraph, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pyg -> {
        hydra.core.Term term = new hydra.core.Term.Application(app);
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Term>>> gathered = new hydra.util.Lazy<>(() -> hydra.coderUtils.CoderUtils.gatherArgs(
          term,
          (java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of())));
        hydra.util.Lazy<java.util.List<hydra.core.Term>> args = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(gathered.get()));
        hydra.util.Lazy<hydra.core.Term> fun = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(gathered.get()));
        hydra.graph.Graph g = hydra.ext.python.coder.Coder.pyGraphGraph(pyg);
        Boolean skipCasts = (env).skipCasts;
        hydra.typing.TypeContext tc = (env).typeContext;
        Integer termArity = hydra.ext.python.coder.Coder.termArityWithPrimitives(
          g,
          fun.get());
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.WithDefault.apply(
            termArity,
            hydra.lib.flows.Map.apply(
              hydra.arity.Arity::typeArity,
              hydra.ext.python.coder.Coder.inGraphContext(hydra.checking.Checking.typeOf(
                tc,
                (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
                fun.get())))),
          (java.util.function.Function<Integer, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (arity -> hydra.lib.flows.Bind.apply(
            hydra.lib.flows.MapList.apply(
              (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (t -> hydra.ext.python.coder.Coder.encodeTermInline(
                env,
                false,
                t)),
              args.get()),
            (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Expression>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pargs -> {
              hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> hargs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Take.apply(
                arity,
                pargs));
              hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> rargs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Drop.apply(
                arity,
                pargs));
              return hydra.lib.flows.Bind.apply(
                hydra.ext.python.coder.Coder.encodeApplicationInner(
                  env,
                  fun.get(),
                  hargs.get(),
                  rargs.get()),
                (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (result -> {
                  hydra.util.Lazy<hydra.ext.python.syntax.Expression> lhs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
                  hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> remainingRargs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result));
                  hydra.util.Lazy<hydra.ext.python.syntax.Expression> pyapp = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                    (java.util.function.Function<hydra.ext.python.syntax.Expression, java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>>) (t -> (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>) (a -> hydra.ext.python.utils.Utils.functionCall(
                      hydra.ext.python.utils.Utils.pyExpressionToPyPrimary(t),
                      java.util.List.of(a)))),
                    lhs.get(),
                    remainingRargs.get()));
                  return hydra.lib.flows.Pure.apply(pyapp.get());
                }));
            }))));
      }));
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> encodeApplicationInner(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Term fun, java.util.List<hydra.ext.python.syntax.Expression> hargs, java.util.List<hydra.ext.python.syntax.Expression> rargs) {
    hydra.util.Lazy<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>> defaultCase = new hydra.util.Lazy<>(() -> hydra.lib.flows.Bind.apply(
      hydra.ext.python.coder.Coder.encodeTermInline(
        env,
        false,
        fun),
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (pfun -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(hydra.ext.python.utils.Utils.functionCall(
        hydra.ext.python.utils.Utils.pyExpressionToPyPrimary(pfun),
        hargs), rargs)))))));
    hydra.util.Lazy<hydra.ext.python.syntax.Expression> firstArg = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(hargs));
    hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> restArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(hargs));
    java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression> withRest = (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>) (e -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(restArgs.get()),
      () -> e,
      () -> hydra.ext.python.utils.Utils.functionCall(
        hydra.ext.python.utils.Utils.pyExpressionToPyPrimary(e),
        restArgs.get())));
    return (hydra.rewriting.Rewriting.deannotateTerm(fun)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> otherwise(hydra.core.Term instance) {
        return defaultCase.get();
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> otherwise(hydra.core.Function instance) {
            return defaultCase.get();
          }
          
          @Override
          public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> visit(hydra.core.Function.Elimination elm) {
            return ((elm).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> otherwise(hydra.core.Elimination instance) {
                return defaultCase.get();
              }
              
              @Override
              public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> visit(hydra.core.Elimination.Record proj) {
                hydra.core.Name fname = ((proj).value).field;
                hydra.ext.python.syntax.Expression fieldExpr = hydra.ext.python.utils.Utils.projectFromExpression(
                  firstArg.get(),
                  hydra.ext.python.names.Names.encodeFieldName(
                    env,
                    fname));
                return hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>((withRest).apply(fieldExpr), rargs))));
              }
              
              @Override
              public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> visit(hydra.core.Elimination.Union ignored) {
                return hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(hydra.ext.python.coder.Coder.unsupportedExpression("inline match expressions are not yet supported"), rargs))));
              }
              
              @Override
              public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> visit(hydra.core.Elimination.Wrap ignored) {
                hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> allArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                  restArgs.get(),
                  rargs));
                hydra.ext.python.syntax.Expression valueExpr = hydra.ext.python.utils.Utils.projectFromExpression(
                  firstArg.get(),
                  new hydra.ext.python.syntax.Name("value"));
                return hydra.lib.logic.IfElse.lazy(
                  hydra.lib.lists.Null.apply(allArgs.get()),
                  () -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(valueExpr, (java.util.List<hydra.ext.python.syntax.Expression>) (java.util.List.<hydra.ext.python.syntax.Expression>of()))))),
                  () -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(hydra.ext.python.utils.Utils.functionCall(
                    hydra.ext.python.utils.Utils.pyExpressionToPyPrimary(valueExpr),
                    allArgs.get()), (java.util.List<hydra.ext.python.syntax.Expression>) (java.util.List.<hydra.ext.python.syntax.Expression>of()))))));
              }
            });
          }
          
          @Override
          public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> visit(hydra.core.Function.Primitive name) {
            java.util.List<hydra.ext.python.syntax.Expression> wrappedArgs = hydra.ext.python.coder.Coder.wrapLazyArguments(
              (name).value,
              hargs);
            return hydra.lib.flows.Bind.apply(
              hydra.ext.python.coder.Coder.encodeVariable(
                env,
                (name).value,
                wrappedArgs),
              (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (expr -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(expr, rargs))))));
          }
          
          @Override
          public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> visit(hydra.core.Function.Lambda ignored) {
            return hydra.lib.flows.Bind.apply(
              hydra.ext.python.coder.Coder.encodeTermInline(
                env,
                false,
                fun),
              (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (pfun -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(hydra.ext.python.utils.Utils.functionCall(
                hydra.ext.python.utils.Utils.pyExpressionToPyPrimary(pfun),
                hargs), rargs))))));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> visit(hydra.core.Term.Variable name) {
        return hydra.lib.flows.Bind.apply(
          hydra.monads.Monads.<hydra.ext.python.helpers.PyGraph>getState(),
          (java.util.function.Function<hydra.ext.python.helpers.PyGraph, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (pyg -> {
            hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> allArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
              hargs,
              rargs));
            hydra.graph.Graph g = hydra.ext.python.coder.Coder.pyGraphGraph(pyg);
            return hydra.lib.maybes.Maybe.apply(
              hydra.lib.flows.Bind.apply(
                hydra.ext.python.coder.Coder.encodeVariable(
                  env,
                  (name).value,
                  hargs),
                (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (expr -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(expr, rargs)))))),
              (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (el -> hydra.lib.maybes.Maybe.apply(
                hydra.lib.flows.Bind.apply(
                  hydra.ext.python.coder.Coder.encodeVariable(
                    env,
                    (name).value,
                    hargs),
                  (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (expr -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(expr, rargs)))))),
                (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (ts -> {
                  Integer elArity = hydra.arity.Arity.typeSchemeArity(ts);
                  hydra.util.Lazy<Integer> consumeCount = new hydra.util.Lazy<>(() -> hydra.lib.math.Min.apply(
                    elArity,
                    hydra.lib.lists.Length.apply(allArgs.get())));
                  hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> consumedArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Take.apply(
                    consumeCount.get(),
                    allArgs.get()));
                  hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> remainingArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Drop.apply(
                    consumeCount.get(),
                    allArgs.get()));
                  return hydra.lib.logic.IfElse.lazy(
                    hydra.lib.lists.Null.apply(consumedArgs.get()),
                    () -> hydra.lib.flows.Bind.apply(
                      hydra.ext.python.coder.Coder.encodeVariable(
                        env,
                        (name).value,
                        (java.util.List<hydra.ext.python.syntax.Expression>) (java.util.List.<hydra.ext.python.syntax.Expression>of())),
                      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (expr -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(expr, rargs)))))),
                    () -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Tuple.Tuple2<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(hydra.ext.python.utils.Utils.functionCall(
                      hydra.ext.python.utils.Utils.pyNameToPyPrimary(hydra.ext.python.names.Names.encodeName(
                        true,
                        new hydra.util.CaseConvention.LowerSnake(),
                        env,
                        (name).value)),
                      consumedArgs.get()), remainingArgs.get())))));
                }),
                (el).type)),
              hydra.lexical.Lexical.lookupElement(
                g,
                (name).value));
          }));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> encodeTermInline(hydra.ext.python.helpers.PythonEnvironment env, Boolean noCast, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>> encode = (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (t -> hydra.ext.python.coder.Coder.encodeTermInline(
      env,
      false,
      t));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, hydra.core.Term>> stripTypeApps = new java.util.concurrent.atomic.AtomicReference<>();
    stripTypeApps.set((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated ann) {
        return (stripTypeApps.get()).apply(((ann).value).body);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication ta) {
        return (stripTypeApps.get()).apply(((ta).value).body);
      }
    })));
    java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>> withCast = (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pyexp -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Or.apply(
        noCast,
        (env).skipCasts),
      () -> hydra.lib.flows.Pure.apply(pyexp),
      () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
        hydra.typing.TypeContext tc = (env).typeContext;
        return hydra.lib.flows.WithDefault.apply(
          pyexp,
          hydra.lib.flows.Bind.apply(
            hydra.checking.Checking.typeOf(
              tc,
              (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
              term),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (typ -> hydra.lib.flows.Bind.apply(
              hydra.ext.python.coder.Coder.encodeType(
                env,
                typ),
              (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pytyp -> hydra.lib.flows.Bind.apply(
                hydra.ext.python.coder.Coder.updateMeta((java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, hydra.ext.python.helpers.PythonModuleMetadata>) (m -> new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, true, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar))),
                (java.util.function.Function<java.lang.Void, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (unit_ -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.castTo(
                  pytyp,
                  pyexp)))))))));
      })).get()));
    return (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Application app) {
        return hydra.ext.python.coder.Coder.encodeApplication(
          env,
          (app).value);
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Either et) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (t1 -> hydra.lib.flows.Bind.apply(
            (encode).apply(t1),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pyexp -> (withCast).apply(hydra.ext.python.utils.Utils.functionCall(
              hydra.ext.python.utils.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("Left")),
              java.util.List.of(pyexp)))))),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (t1 -> hydra.lib.flows.Bind.apply(
            (encode).apply(t1),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pyexp -> (withCast).apply(hydra.ext.python.utils.Utils.functionCall(
              hydra.ext.python.utils.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("Right")),
              java.util.List.of(pyexp)))))),
          (et).value);
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Function f) {
        return hydra.ext.python.coder.Coder.encodeFunction(
          env,
          (f).value);
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Let lt) {
        java.util.List<hydra.core.Binding> bindings = ((lt).value).bindings;
        hydra.core.Term body = ((lt).value).body;
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(bindings),
          () -> hydra.ext.python.coder.Coder.encodeTermInline(
            env,
            false,
            body),
          () -> hydra.ext.python.coder.Coder.withLetInline(
            env,
            (lt).value,
            (java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (innerEnv -> hydra.lib.flows.Bind.apply(
              hydra.lib.flows.MapList.apply(
                (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.NamedExpression>>) (v1 -> hydra.ext.python.coder.Coder.encodeBindingAsAssignment(
                  false,
                  innerEnv,
                  v1)),
                bindings),
              (java.util.function.Function<java.util.List<hydra.ext.python.syntax.NamedExpression>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pbindingExprs -> hydra.lib.flows.Bind.apply(
                hydra.ext.python.coder.Coder.encodeTermInline(
                  innerEnv,
                  false,
                  body),
                (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pbody -> {
                  hydra.util.Lazy<hydra.ext.python.syntax.Expression> indexValue = new hydra.util.Lazy<>(() -> hydra.ext.python.utils.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Number_(new hydra.ext.python.syntax.Number_.Integer_(hydra.lib.literals.Int32ToBigint.apply(hydra.lib.lists.Length.apply(bindings))))));
                  hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.StarNamedExpression>> pbindingStarExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.StarNamedExpression>) (ne -> new hydra.ext.python.syntax.StarNamedExpression.Simple(ne)),
                    pbindingExprs));
                  hydra.ext.python.syntax.StarNamedExpression pbodyStarExpr = hydra.ext.python.utils.Utils.pyExpressionToPyStarNamedExpression(pbody);
                  hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.StarNamedExpression>> tupleElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                    pbindingStarExprs.get(),
                    java.util.List.of(pbodyStarExpr)));
                  hydra.ext.python.syntax.Expression tupleExpr = hydra.ext.python.utils.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Tuple(new hydra.ext.python.syntax.Tuple(tupleElements.get())));
                  hydra.ext.python.syntax.Primary indexedExpr = hydra.ext.python.utils.Utils.primaryWithExpressionSlices(
                    hydra.ext.python.utils.Utils.pyExpressionToPyPrimary(tupleExpr),
                    java.util.List.of(indexValue.get()));
                  return hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.pyPrimaryToPyExpression(indexedExpr));
                })))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.List terms) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            encode,
            (terms).value),
          (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Expression>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pyExprs -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Tuple(new hydra.ext.python.syntax.Tuple(hydra.lib.lists.Map.apply(
            hydra.ext.python.utils.Utils::pyExpressionToPyStarNamedExpression,
            pyExprs)))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Literal lit) {
        return hydra.ext.python.coder.Coder.encodeLiteral((lit).value);
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Map m) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.DoubleStarredKvpair>>) (kv -> {
              hydra.util.Lazy<hydra.core.Term> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kv));
              hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(kv));
              return hydra.lib.flows.Bind.apply(
                (encode).apply(k.get()),
                (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.DoubleStarredKvpair>>) (pyK -> hydra.lib.flows.Bind.apply(
                  (encode).apply(v.get()),
                  (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.DoubleStarredKvpair>>) (pyV -> hydra.lib.flows.Pure.apply(new hydra.ext.python.syntax.DoubleStarredKvpair.Pair(new hydra.ext.python.syntax.Kvpair(pyK, pyV)))))));
            }),
            hydra.lib.maps.ToList.apply((m).value)),
          (java.util.function.Function<java.util.List<hydra.ext.python.syntax.DoubleStarredKvpair>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pairs -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.functionCall(
            hydra.ext.python.utils.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("FrozenDict")),
            java.util.List.of(hydra.ext.python.utils.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Dict(new hydra.ext.python.syntax.Dict(pairs))))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Maybe.apply(
          hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.functionCall(
            hydra.ext.python.utils.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("Nothing")),
            (java.util.List<hydra.ext.python.syntax.Expression>) (java.util.List.<hydra.ext.python.syntax.Expression>of()))),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (t1 -> hydra.lib.flows.Bind.apply(
            (encode).apply(t1),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pyexp -> (withCast).apply(hydra.ext.python.utils.Utils.functionCall(
              hydra.ext.python.utils.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("Just")),
              java.util.List.of(pyexp)))))),
          (mt).value);
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Pair p) {
        hydra.util.Lazy<hydra.core.Term> t1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((p).value));
        hydra.util.Lazy<hydra.core.Term> t2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((p).value));
        return hydra.lib.flows.Bind.apply(
          (encode).apply(t1.get()),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pyExpr1 -> hydra.lib.flows.Bind.apply(
            (encode).apply(t2.get()),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pyExpr2 -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Tuple(new hydra.ext.python.syntax.Tuple(java.util.List.of(
              hydra.ext.python.utils.Utils.pyExpressionToPyStarNamedExpression(pyExpr1),
              hydra.ext.python.utils.Utils.pyExpressionToPyStarNamedExpression(pyExpr2))))))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Record r) {
        java.util.List<hydra.core.Field> fields = ((r).value).fields;
        hydra.core.Name tname = ((r).value).typeName;
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (fld -> (encode).apply((fld).term)),
            fields),
          (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Expression>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pargs -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.functionCall(
            hydra.ext.python.utils.Utils.pyNameToPyPrimary(hydra.ext.python.names.Names.encodeNameQualified(
              env,
              tname)),
            pargs))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Set s) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            encode,
            hydra.lib.sets.ToList.apply((s).value)),
          (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Expression>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pyEls -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.functionCall(
            hydra.ext.python.utils.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("frozenset")),
            java.util.List.of(hydra.ext.python.utils.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Set(new hydra.ext.python.syntax.Set(hydra.lib.lists.Map.apply(
              hydra.ext.python.utils.Utils::pyExpressionToPyStarNamedExpression,
              pyEls)))))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.TypeApplication ta) {
        hydra.core.Term body = ((ta).value).body;
        return hydra.lib.flows.Bind.apply(
          hydra.ext.python.coder.Coder.encodeTermInline(
            env,
            true,
            (stripTypeApps.get()).apply(body)),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (pybase -> (withCast).apply(pybase)));
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.TypeLambda tl) {
        hydra.core.Term body = ((tl).value).body;
        return hydra.ext.python.coder.Coder.withTypeLambda(
          env,
          (tl).value,
          (java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (env2 -> hydra.ext.python.coder.Coder.encodeTermInline(
            env2,
            noCast,
            body)));
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Union inj) {
        hydra.core.Field field = ((inj).value).field;
        hydra.core.Name tname = ((inj).value).typeName;
        return hydra.lib.flows.Bind.apply(
          hydra.ext.python.coder.Coder.inGraphContext(hydra.schemas.Schemas.requireUnionType(tname)),
          (java.util.function.Function<hydra.core.RowType, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (rt -> hydra.lib.logic.IfElse.lazy(
            hydra.schemas.Schemas.isEnumRowType(rt),
            () -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.projectFromExpression(
              hydra.ext.python.utils.Utils.pyNameToPyExpression(hydra.ext.python.names.Names.encodeNameQualified(
                env,
                tname)),
              hydra.ext.python.names.Names.encodeEnumValue(
                env,
                (field).name))),
            () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
              hydra.core.Name fname = (field).name;
              return ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
                java.util.List<hydra.core.FieldType> ftypes = (rt).fields;
                return ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (() -> {
                  hydra.util.Lazy<Boolean> isUnitVariant = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.apply(
                    false,
                    (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.schemas.Schemas.isUnitType(hydra.rewriting.Rewriting.deannotateType((ft).type))),
                    hydra.lib.lists.Find.apply(
                      (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.lib.equality.Equal.apply(
                        ((ft).name).value,
                        (fname).value)),
                      ftypes)));
                  return hydra.lib.flows.Bind.apply(
                    hydra.lib.logic.IfElse.lazy(
                      hydra.lib.logic.Or.apply(
                        hydra.schemas.Schemas.isUnitTerm((field).term),
                        isUnitVariant.get()),
                      () -> hydra.lib.flows.Pure.apply((java.util.List<hydra.ext.python.syntax.Expression>) (java.util.List.<hydra.ext.python.syntax.Expression>of())),
                      () -> hydra.lib.flows.Bind.apply(
                        (encode).apply((field).term),
                        (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<hydra.ext.python.syntax.Expression>>>) (parg -> hydra.lib.flows.Pure.apply(java.util.List.of(parg))))),
                    (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Expression>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (args -> hydra.lib.flows.Bind.apply(
                      hydra.ext.python.coder.Coder.updateMeta((java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, hydra.ext.python.helpers.PythonModuleMetadata>) (v1 -> hydra.ext.python.coder.Coder.setMetaUsesCast(
                        true,
                        v1))),
                      (java.util.function.Function<java.lang.Void, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (unit_ -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.castTo(
                        hydra.ext.python.names.Names.typeVariableReference(
                          env,
                          tname),
                        hydra.ext.python.utils.Utils.functionCall(
                          hydra.ext.python.utils.Utils.pyNameToPyPrimary(hydra.ext.python.names.Names.variantName(
                            true,
                            env,
                            tname,
                            fname)),
                          args)))))));
                })).get();
              })).get();
            })).get())));
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Unit ignored) {
        return hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.pyNameToPyExpression(hydra.ext.python.utils.Utils.pyNone()));
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Variable name) {
        return hydra.ext.python.coder.Coder.encodeVariable(
          env,
          (name).value,
          (java.util.List<hydra.ext.python.syntax.Expression>) (java.util.List.<hydra.ext.python.syntax.Expression>of()));
      }
      
      @Override
      public hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Wrap wrapped) {
        hydra.core.Term inner = ((wrapped).value).body;
        hydra.core.Name tname = ((wrapped).value).typeName;
        return hydra.lib.flows.Bind.apply(
          (encode).apply(inner),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression>>) (parg -> hydra.lib.flows.Pure.apply(hydra.ext.python.utils.Utils.functionCall(
            hydra.ext.python.utils.Utils.pyNameToPyPrimary(hydra.ext.python.names.Names.encodeNameQualified(
              env,
              tname)),
            java.util.List.of(parg)))));
      }
    });
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata extendMetaForTerm(Boolean topLevel, hydra.ext.python.helpers.PythonModuleMetadata meta0, hydra.core.Term term) {
    java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, java.util.function.Function<hydra.core.Term, hydra.ext.python.helpers.PythonModuleMetadata>> step = (java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, java.util.function.Function<hydra.core.Term, hydra.ext.python.helpers.PythonModuleMetadata>>) (meta -> (java.util.function.Function<hydra.core.Term, hydra.ext.python.helpers.PythonModuleMetadata>) (t -> (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata otherwise(hydra.core.Term instance) {
        return meta;
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Term.Either e) {
        hydra.ext.python.helpers.PythonModuleMetadata metaWithCast = hydra.ext.python.coder.Coder.setMetaUsesCast(
          true,
          meta);
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.ext.python.helpers.PythonModuleMetadata>) (ignored -> hydra.ext.python.coder.Coder.setMetaUsesLeft(
            metaWithCast,
            true)),
          (java.util.function.Function<hydra.core.Term, hydra.ext.python.helpers.PythonModuleMetadata>) (ignored -> hydra.ext.python.coder.Coder.setMetaUsesRight(
            metaWithCast,
            true)),
          (e).value);
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.ext.python.helpers.PythonModuleMetadata otherwise(hydra.core.Function instance) {
            return meta;
          }
          
          @Override
          public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Function.Lambda lam) {
            return hydra.lib.maybes.Maybe.apply(
              meta,
              (java.util.function.Function<hydra.core.Type, hydra.ext.python.helpers.PythonModuleMetadata>) (dom -> hydra.lib.logic.IfElse.lazy(
                topLevel,
                () -> hydra.ext.python.coder.Coder.extendMetaForType(
                  true,
                  false,
                  dom,
                  meta),
                () -> meta)),
              ((lam).value).domain);
          }
        });
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Term.Let lt) {
        java.util.List<hydra.core.Binding> bindings = ((lt).value).bindings;
        return hydra.lib.lists.Foldl.apply(
          ((java.util.function.Supplier<java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, java.util.function.Function<hydra.core.Binding, hydra.ext.python.helpers.PythonModuleMetadata>>>) (() -> {
            java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, java.util.function.Function<hydra.core.Binding, hydra.ext.python.helpers.PythonModuleMetadata>> forBinding = (java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, java.util.function.Function<hydra.core.Binding, hydra.ext.python.helpers.PythonModuleMetadata>>) (m -> (java.util.function.Function<hydra.core.Binding, hydra.ext.python.helpers.PythonModuleMetadata>) (b -> hydra.lib.maybes.Maybe.apply(
              m,
              (java.util.function.Function<hydra.core.TypeScheme, hydra.ext.python.helpers.PythonModuleMetadata>) (ts -> {
                hydra.core.Term term1 = (b).term;
                return hydra.lib.logic.IfElse.lazy(
                  hydra.coderUtils.CoderUtils.isSimpleAssignment(term1),
                  () -> m,
                  () -> hydra.ext.python.coder.Coder.extendMetaForType(
                    true,
                    true,
                    (ts).type,
                    m));
              }),
              (b).type)));
            return forBinding;
          })).get(),
          meta,
          bindings);
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Term.Literal l) {
        return ((l).value).accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public hydra.ext.python.helpers.PythonModuleMetadata otherwise(hydra.core.Literal instance) {
            return meta;
          }
          
          @Override
          public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Literal.Float_ fv) {
            return ((fv).value).accept(new hydra.core.FloatValue.PartialVisitor<>() {
              @Override
              public hydra.ext.python.helpers.PythonModuleMetadata otherwise(hydra.core.FloatValue instance) {
                return meta;
              }
              
              @Override
              public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.FloatValue.Bigfloat ignored) {
                return hydra.ext.python.coder.Coder.setMetaUsesDecimal(
                  meta,
                  true);
              }
            });
          }
        });
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Term.Map ignored) {
        return hydra.ext.python.coder.Coder.setMetaUsesFrozenDict(
          meta,
          true);
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Term.Maybe m) {
        return hydra.lib.maybes.Maybe.apply(
          hydra.ext.python.coder.Coder.setMetaUsesNothing(
            meta,
            true),
          (java.util.function.Function<hydra.core.Term, hydra.ext.python.helpers.PythonModuleMetadata>) (ignored -> hydra.ext.python.coder.Coder.setMetaUsesJust(
            meta,
            true)),
          (m).value);
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Term.Union ignored) {
        return hydra.ext.python.coder.Coder.setMetaUsesCast(
          true,
          meta);
      }
    })));
    return hydra.rewriting.Rewriting.foldOverTerm(
      new hydra.coders.TraversalOrder.Pre(),
      step,
      meta0,
      term);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata extendMetaForType(Boolean topLevel, Boolean isTermAnnot, hydra.core.Type typ, hydra.ext.python.helpers.PythonModuleMetadata meta) {
    java.util.Set<hydra.core.Name> currentTvars = (meta).typeVariables;
    java.util.Set<hydra.core.Name> newTvars = hydra.ext.python.coder.Coder.collectTypeVariables(
      currentTvars,
      typ);
    hydra.ext.python.helpers.PythonModuleMetadata metaWithTvars = hydra.ext.python.coder.Coder.setMetaTypeVariables(
      meta,
      newTvars);
    hydra.util.Lazy<hydra.ext.python.helpers.PythonModuleMetadata> metaWithSubtypes = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, java.util.function.Function<hydra.core.Type, hydra.ext.python.helpers.PythonModuleMetadata>>) (m -> (java.util.function.Function<hydra.core.Type, hydra.ext.python.helpers.PythonModuleMetadata>) (t -> hydra.ext.python.coder.Coder.extendMetaForType(
        false,
        isTermAnnot,
        t,
        m))),
      metaWithTvars,
      hydra.rewriting.Rewriting.subtypes(typ)));
    return (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata otherwise(hydra.core.Type instance) {
        return metaWithSubtypes.get();
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Type.Function ft) {
        hydra.core.Type cod = ((ft).value).codomain;
        hydra.core.Type dom = ((ft).value).domain;
        hydra.ext.python.helpers.PythonModuleMetadata meta2 = hydra.ext.python.coder.Coder.extendMetaForType(
          topLevel,
          isTermAnnot,
          cod,
          metaWithSubtypes.get());
        hydra.ext.python.helpers.PythonModuleMetadata meta3 = hydra.ext.python.coder.Coder.extendMetaForType(
          false,
          isTermAnnot,
          dom,
          meta2);
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.And.apply(
            isTermAnnot,
            topLevel),
          () -> meta3,
          () -> hydra.ext.python.coder.Coder.setMetaUsesCallable(
            meta3,
            true));
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Type.List ignored) {
        return hydra.ext.python.coder.Coder.setMetaUsesFrozenList(
          metaWithSubtypes.get(),
          true);
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Type.Map ignored) {
        return hydra.ext.python.coder.Coder.setMetaUsesFrozenDict(
          metaWithSubtypes.get(),
          true);
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Type.Maybe ignored) {
        return hydra.ext.python.coder.Coder.setMetaUsesMaybe(
          metaWithSubtypes.get(),
          true);
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Type.Either ignored) {
        return hydra.ext.python.coder.Coder.setMetaUsesEither(
          metaWithSubtypes.get(),
          true);
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Type.Literal lt) {
        return ((lt).value).accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public hydra.ext.python.helpers.PythonModuleMetadata otherwise(hydra.core.LiteralType instance) {
            return metaWithSubtypes.get();
          }
          
          @Override
          public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.LiteralType.Float_ ft) {
            return ((ft).value).accept(new hydra.core.FloatType.PartialVisitor<>() {
              @Override
              public hydra.ext.python.helpers.PythonModuleMetadata otherwise(hydra.core.FloatType instance) {
                return metaWithSubtypes.get();
              }
              
              @Override
              public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.FloatType.Bigfloat ignored) {
                return hydra.ext.python.coder.Coder.setMetaUsesDecimal(
                  metaWithSubtypes.get(),
                  true);
              }
            });
          }
        });
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Type.Union rt) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.schemas.Schemas.isEnumRowType((rt).value),
          () -> hydra.ext.python.coder.Coder.setMetaUsesEnum(
            metaWithSubtypes.get(),
            true),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(((rt).value).fields)),
            () -> hydra.ext.python.coder.Coder.setMetaUsesNode(
              metaWithSubtypes.get(),
              true),
            () -> metaWithSubtypes.get()));
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Type.Forall ft) {
        hydra.core.Type body = ((ft).value).body;
        hydra.ext.python.helpers.PythonModuleMetadata metaForWrap = hydra.ext.python.coder.Coder.digForWrap(
          isTermAnnot,
          metaWithSubtypes.get(),
          body);
        return (hydra.rewriting.Rewriting.deannotateType(body)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.ext.python.helpers.PythonModuleMetadata otherwise(hydra.core.Type instance) {
            return metaForWrap;
          }
          
          @Override
          public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Type.Record ignored) {
            return hydra.ext.python.coder.Coder.setMetaUsesGeneric(
              metaForWrap,
              true);
          }
        });
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Type.Record rt) {
        java.util.List<hydra.core.FieldType> fields = ((rt).value).fields;
        hydra.util.Lazy<Boolean> hasAnnotated = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.FieldType, Boolean>>) (b -> (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.lib.logic.Or.apply(
            b,
            hydra.annotations.Annotations.hasTypeDescription((ft).type)))),
          false,
          fields));
        hydra.util.Lazy<hydra.ext.python.helpers.PythonModuleMetadata> meta1 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(fields),
          () -> metaWithSubtypes.get(),
          () -> hydra.ext.python.coder.Coder.setMetaUsesDataclass(
            metaWithSubtypes.get(),
            true)));
        return hydra.lib.logic.IfElse.lazy(
          hasAnnotated.get(),
          () -> hydra.ext.python.coder.Coder.setMetaUsesAnnotated(
            meta1.get(),
            true),
          () -> meta1.get());
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Type.Wrap ignored) {
        return hydra.lib.logic.IfElse.lazy(
          isTermAnnot,
          () -> metaWithSubtypes.get(),
          () -> hydra.ext.python.coder.Coder.setMetaUsesNode(
            metaWithSubtypes.get(),
            true));
      }
    });
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata digForWrap(Boolean isTermAnnot, hydra.ext.python.helpers.PythonModuleMetadata meta, hydra.core.Type typ) {
    return (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata otherwise(hydra.core.Type instance) {
        return meta;
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Type.Forall ft) {
        return hydra.ext.python.coder.Coder.digForWrap(
          isTermAnnot,
          meta,
          ((ft).value).body);
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.core.Type.Wrap ignored) {
        return hydra.lib.logic.IfElse.lazy(
          isTermAnnot,
          () -> meta,
          () -> hydra.ext.python.coder.Coder.setMetaUsesNode(
            meta,
            true));
      }
    });
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaNamespaces(hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> ns, hydra.ext.python.helpers.PythonModuleMetadata m) {
    return new hydra.ext.python.helpers.PythonModuleMetadata(ns, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesLeft(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, b, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesRight(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, b, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesDecimal(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, b, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesFrozenDict(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, b, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesNothing(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, b, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesJust(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, b, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesCallable(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, b, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesLruCache(Boolean b, hydra.ext.python.helpers.PythonModuleMetadata m) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, b, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesCast(Boolean b, hydra.ext.python.helpers.PythonModuleMetadata m) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, b, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesGeneric(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, b, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesFrozenList(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, b, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesMaybe(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, b, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesEither(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, b, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesNode(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, b, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesEnum(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, b, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesAnnotated(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, b, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesDataclass(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, b, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaTypeVariables(hydra.ext.python.helpers.PythonModuleMetadata m, java.util.Set<hydra.core.Name> tvars) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, tvars, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static Boolean isTypeVariableName(hydra.core.Name name) {
    return hydra.lib.equality.Equal.apply(
      1,
      hydra.lib.lists.Length.apply(hydra.lib.strings.SplitOn.apply(
        ".",
        (name).value)));
  }
  
  static java.util.Set<hydra.core.Name> collectTypeVariables(java.util.Set<hydra.core.Name> initial, hydra.core.Type typ) {
    return (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.Set<hydra.core.Name> otherwise(hydra.core.Type instance) {
        java.util.Set<hydra.core.Name> freeVars = hydra.rewriting.Rewriting.freeVariablesInType(typ);
        java.util.function.Function<hydra.core.Name, Boolean> isTypeVar = (java.util.function.Function<hydra.core.Name, Boolean>) (n -> hydra.ext.python.coder.Coder.isTypeVariableName(n));
        hydra.util.Lazy<java.util.List<hydra.core.Name>> filteredList = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
          isTypeVar,
          hydra.lib.sets.ToList.apply(freeVars)));
        return hydra.lib.sets.Union.apply(
          initial,
          hydra.lib.sets.FromList.apply(filteredList.get()));
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        hydra.core.Type body = ((ft).value).body;
        hydra.core.Name v = ((ft).value).parameter;
        return hydra.ext.python.coder.Coder.collectTypeVariables(
          hydra.lib.sets.Insert.apply(
            v,
            initial),
          body);
      }
    });
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata extendMetaForTypes(java.util.List<hydra.core.Type> types, hydra.ext.python.helpers.PythonModuleMetadata meta) {
    hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> currentNs = (meta).namespaces;
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> names = new hydra.util.Lazy<>(() -> hydra.lib.sets.Unions.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>) (t -> hydra.rewriting.Rewriting.typeDependencyNames(
        false,
        t)),
      types)));
    hydra.util.Lazy<hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>> updatedNs = new hydra.util.Lazy<>(() -> hydra.schemas.Schemas.addNamesToNamespaces(
      hydra.ext.python.names.Names::encodeNamespace,
      names.get(),
      currentNs));
    hydra.ext.python.helpers.PythonModuleMetadata meta1 = hydra.ext.python.coder.Coder.setMetaNamespaces(
      updatedNs.get(),
      meta);
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, java.util.function.Function<hydra.core.Type, hydra.ext.python.helpers.PythonModuleMetadata>>) (m -> (java.util.function.Function<hydra.core.Type, hydra.ext.python.helpers.PythonModuleMetadata>) (t -> hydra.ext.python.coder.Coder.extendMetaForType(
        true,
        false,
        t,
        m))),
      meta1,
      types);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesName(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, b, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesTypeVar(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, b);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata emptyMetadata(hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> ns) {
    return new hydra.ext.python.helpers.PythonModuleMetadata(ns, (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false);
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata gatherMetadata(hydra.module.Namespace focusNs, java.util.List<hydra.module.Definition> defs) {
    java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, java.util.function.Function<hydra.module.Definition, hydra.ext.python.helpers.PythonModuleMetadata>> addDef = (java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, java.util.function.Function<hydra.module.Definition, hydra.ext.python.helpers.PythonModuleMetadata>>) (meta -> (java.util.function.Function<hydra.module.Definition, hydra.ext.python.helpers.PythonModuleMetadata>) (def -> (def).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.module.Definition.Term termDef) {
        hydra.core.TypeScheme typScheme = ((termDef).value).type;
        hydra.core.Type typ = (typScheme).type;
        hydra.ext.python.helpers.PythonModuleMetadata meta2 = hydra.ext.python.coder.Coder.extendMetaForType(
          true,
          true,
          typ,
          meta);
        hydra.core.Term term = ((termDef).value).term;
        return hydra.ext.python.coder.Coder.extendMetaForTerm(
          true,
          meta2,
          term);
      }
      
      @Override
      public hydra.ext.python.helpers.PythonModuleMetadata visit(hydra.module.Definition.Type typeDef) {
        hydra.ext.python.helpers.PythonModuleMetadata meta2 = hydra.ext.python.coder.Coder.setMetaUsesName(
          meta,
          true);
        hydra.core.Type typ = ((typeDef).value).type;
        return hydra.rewriting.Rewriting.foldOverType(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<hydra.ext.python.helpers.PythonModuleMetadata, java.util.function.Function<hydra.core.Type, hydra.ext.python.helpers.PythonModuleMetadata>>) (m -> (java.util.function.Function<hydra.core.Type, hydra.ext.python.helpers.PythonModuleMetadata>) (t -> hydra.ext.python.coder.Coder.extendMetaForType(
            true,
            false,
            t,
            m))),
          meta2,
          typ);
      }
    })));
    hydra.ext.python.helpers.PythonModuleMetadata start = hydra.ext.python.coder.Coder.emptyMetadata(hydra.ext.python.utils.Utils.findNamespaces(
      focusNs,
      defs));
    hydra.util.Lazy<hydra.ext.python.helpers.PythonModuleMetadata> result = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      addDef,
      start,
      defs));
    java.util.Set<hydra.core.Name> tvars = (result.get()).typeVariables;
    return hydra.ext.python.coder.Coder.setMetaUsesTypeVar(
      result.get(),
      hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(tvars)));
  }
  
  static hydra.ext.python.helpers.PythonModuleMetadata setMetaUsesTypeAlias(hydra.ext.python.helpers.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, b, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }
  
  static Boolean isTypeModuleCheck(java.util.List<hydra.module.Definition> defs) {
    return hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.module.Definition, Boolean>) (d -> (d).accept(new hydra.module.Definition.PartialVisitor<>() {
        @Override
        public Boolean otherwise(hydra.module.Definition instance) {
          return false;
        }
        
        @Override
        public Boolean visit(hydra.module.Definition.Type ignored) {
          return true;
        }
      })),
      defs)));
  }
  
  static java.util.List<hydra.module.Definition> reorderDefs(java.util.List<hydra.module.Definition> defs) {
    hydra.util.Tuple.Tuple2<java.util.List<hydra.module.TypeDefinition>, java.util.List<hydra.module.TermDefinition>> partitioned = hydra.schemas.Schemas.partitionDefinitions(defs);
    hydra.util.Lazy<java.util.List<hydra.module.TypeDefinition>> typeDefsRaw = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(partitioned));
    hydra.util.Lazy<java.util.List<hydra.module.TypeDefinition>> nameFirst = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.module.TypeDefinition, Boolean>) (td -> hydra.lib.equality.Equal.apply(
        (td).name,
        new hydra.core.Name("hydra.core.Name"))),
      typeDefsRaw.get()));
    hydra.util.Lazy<java.util.List<hydra.module.TypeDefinition>> nameRest = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.module.TypeDefinition, Boolean>) (td -> hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
        (td).name,
        new hydra.core.Name("hydra.core.Name")))),
      typeDefsRaw.get()));
    hydra.util.Lazy<java.util.List<hydra.module.TermDefinition>> termDefsRaw = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(partitioned));
    hydra.util.Lazy<java.util.List<hydra.module.Definition>> termDefs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.TermDefinition, hydra.module.Definition>) (td -> new hydra.module.Definition.Term(td)),
      termDefsRaw.get()));
    hydra.util.Lazy<java.util.List<hydra.module.Definition>> sortedTermDefs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.sorting.Sorting.topologicalSortNodes(
      (java.util.function.Function<hydra.module.Definition, hydra.core.Name>) (d -> (d).accept(new hydra.module.Definition.PartialVisitor<>() {
        @Override
        public hydra.core.Name visit(hydra.module.Definition.Term td) {
          return ((td).value).name;
        }
      })),
      (java.util.function.Function<hydra.module.Definition, java.util.List<hydra.core.Name>>) (d -> (d).accept(new hydra.module.Definition.PartialVisitor<>() {
        @Override
        public java.util.List<hydra.core.Name> otherwise(hydra.module.Definition instance) {
          return (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of());
        }
        
        @Override
        public java.util.List<hydra.core.Name> visit(hydra.module.Definition.Term td) {
          return hydra.lib.sets.ToList.apply(hydra.rewriting.Rewriting.freeVariablesInTerm(((td).value).term));
        }
      })),
      termDefs.get())));
    hydra.util.Lazy<java.util.List<hydra.module.Definition>> sortedTypeDefs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(java.util.List.of(
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.TypeDefinition, hydra.module.Definition>) (td -> new hydra.module.Definition.Type(td)),
        nameFirst.get()),
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.TypeDefinition, hydra.module.Definition>) (td -> new hydra.module.Definition.Type(td)),
        nameRest.get()))));
    return hydra.lib.lists.Concat.apply(java.util.List.of(
      sortedTypeDefs.get(),
      sortedTermDefs.get()));
  }
  
  static hydra.ext.python.syntax.Statement tvarStatement(hydra.ext.python.syntax.Name name) {
    return hydra.ext.python.utils.Utils.assignmentStatement(
      name,
      hydra.ext.python.utils.Utils.functionCall(
        new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("TypeVar"))),
        java.util.List.of(hydra.ext.python.utils.Utils.doubleQuotedString((name).value))));
  }
  
  static <T0> hydra.util.Maybe<T0> condImportSymbol(T0 name, Boolean flag) {
    return hydra.lib.logic.IfElse.lazy(
      flag,
      () -> hydra.util.Maybe.just(name),
      () -> (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()));
  }
  
  static java.util.List<hydra.ext.python.syntax.ImportStatement> moduleDomainImports(hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces) {
    hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.DottedName>> names = new hydra.util.Lazy<>(() -> hydra.lib.lists.Sort.apply(hydra.lib.maps.Elems.apply(((java.util.function.Function<hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>, java.util.Map<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>>) (projected -> projected.mapping)).apply(namespaces))));
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.python.syntax.DottedName, hydra.ext.python.syntax.ImportStatement>) (ns -> new hydra.ext.python.syntax.ImportStatement.Name(new hydra.ext.python.syntax.ImportName(java.util.List.of(new hydra.ext.python.syntax.DottedAsName(ns, (hydra.util.Maybe<hydra.ext.python.syntax.Name>) (hydra.util.Maybe.<hydra.ext.python.syntax.Name>nothing())))))),
      names.get());
  }
  
  static hydra.ext.python.syntax.ImportStatement standardImportStatement(String modName, java.util.List<String> symbols) {
    return new hydra.ext.python.syntax.ImportStatement.From(new hydra.ext.python.syntax.ImportFrom((java.util.List<hydra.ext.python.syntax.RelativeImportPrefix>) (java.util.List.<hydra.ext.python.syntax.RelativeImportPrefix>of()), hydra.util.Maybe.just(new hydra.ext.python.syntax.DottedName(java.util.List.of(new hydra.ext.python.syntax.Name(modName)))), new hydra.ext.python.syntax.ImportFromTargets.Simple(hydra.lib.lists.Map.apply(
      (java.util.function.Function<String, hydra.ext.python.syntax.ImportFromAsName>) (s -> new hydra.ext.python.syntax.ImportFromAsName(new hydra.ext.python.syntax.Name(s), (hydra.util.Maybe<hydra.ext.python.syntax.Name>) (hydra.util.Maybe.<hydra.ext.python.syntax.Name>nothing()))),
      symbols))));
  }
  
  static java.util.List<hydra.ext.python.syntax.ImportStatement> moduleStandardImports(hydra.ext.python.helpers.PythonModuleMetadata meta) {
    hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>>> pairs = new hydra.util.Lazy<>(() -> java.util.List.of(
      (hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>) ((hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>) (new hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>("__future__", java.util.List.of(hydra.ext.python.coder.Coder.condImportSymbol(
        "annotations",
        hydra.ext.python.names.Names.useFutureAnnotations()))))),
      (hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>) ((hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>) (new hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>("collections.abc", java.util.List.of(hydra.ext.python.coder.Coder.condImportSymbol(
        "Callable",
        (meta).usesCallable))))),
      (hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>) ((hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>) (new hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>("dataclasses", java.util.List.of(hydra.ext.python.coder.Coder.condImportSymbol(
        "dataclass",
        (meta).usesDataclass))))),
      (hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>) ((hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>) (new hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>("decimal", java.util.List.of(hydra.ext.python.coder.Coder.condImportSymbol(
        "Decimal",
        (meta).usesDecimal))))),
      (hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>) ((hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>) (new hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>("enum", java.util.List.of(hydra.ext.python.coder.Coder.condImportSymbol(
        "Enum",
        (meta).usesEnum))))),
      (hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>) ((hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>) (new hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>("functools", java.util.List.of(hydra.ext.python.coder.Coder.condImportSymbol(
        "lru_cache",
        (meta).usesLruCache))))),
      (hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>) ((hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>) (new hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>("hydra.dsl.python", java.util.List.of(
        hydra.ext.python.coder.Coder.condImportSymbol(
          "Either",
          (meta).usesEither),
        hydra.ext.python.coder.Coder.condImportSymbol(
          "FrozenDict",
          (meta).usesFrozenDict),
        hydra.ext.python.coder.Coder.condImportSymbol(
          "Just",
          (meta).usesJust),
        hydra.ext.python.coder.Coder.condImportSymbol(
          "Left",
          (meta).usesLeft),
        hydra.ext.python.coder.Coder.condImportSymbol(
          "Maybe",
          (meta).usesMaybe),
        hydra.ext.python.coder.Coder.condImportSymbol(
          "Node",
          (meta).usesNode),
        hydra.ext.python.coder.Coder.condImportSymbol(
          "Nothing",
          (meta).usesNothing),
        hydra.ext.python.coder.Coder.condImportSymbol(
          "Right",
          (meta).usesRight),
        hydra.ext.python.coder.Coder.condImportSymbol(
          "frozenlist",
          (meta).usesFrozenList))))),
      (hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>) ((hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>) (new hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>("typing", java.util.List.of(
        hydra.ext.python.coder.Coder.condImportSymbol(
          "Annotated",
          (meta).usesAnnotated),
        hydra.ext.python.coder.Coder.condImportSymbol(
          "Generic",
          (meta).usesGeneric),
        hydra.ext.python.coder.Coder.condImportSymbol(
          "TypeAlias",
          (meta).usesTypeAlias),
        hydra.ext.python.coder.Coder.condImportSymbol(
          "TypeVar",
          (meta).usesTypeVar),
        hydra.ext.python.coder.Coder.condImportSymbol(
          "cast",
          (meta).usesCast)))))));
    hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<String, java.util.List<String>>>> simplified = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<String, java.util.List<hydra.util.Maybe<String>>>, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, java.util.List<String>>>>) (p -> {
        hydra.util.Lazy<String> modName = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
        hydra.util.Lazy<java.util.List<String>> symbols = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.lib.pairs.Second.apply(p)));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(symbols.get()),
          () -> (hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, java.util.List<String>>>) (hydra.util.Maybe.<hydra.util.Tuple.Tuple2<String, java.util.List<String>>>nothing()),
          () -> hydra.util.Maybe.just((hydra.util.Tuple.Tuple2<String, java.util.List<String>>) ((hydra.util.Tuple.Tuple2<String, java.util.List<String>>) (new hydra.util.Tuple.Tuple2<String, java.util.List<String>>(modName.get(), symbols.get())))));
      }),
      pairs.get())));
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<String, java.util.List<String>>, hydra.ext.python.syntax.ImportStatement>) (p -> hydra.ext.python.coder.Coder.standardImportStatement(
        hydra.lib.pairs.First.apply(p),
        hydra.lib.pairs.Second.apply(p))),
      simplified.get());
  }
  
  static java.util.List<hydra.ext.python.syntax.Statement> moduleImports(hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces, hydra.ext.python.helpers.PythonModuleMetadata meta) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.python.syntax.ImportStatement, hydra.ext.python.syntax.Statement>) (imp -> hydra.ext.python.utils.Utils.pySimpleStatementToPyStatement(new hydra.ext.python.syntax.SimpleStatement.Import(imp))),
      hydra.lib.lists.Concat.apply(java.util.List.of(
        hydra.ext.python.coder.Coder.moduleStandardImports(meta),
        hydra.ext.python.coder.Coder.moduleDomainImports(namespaces))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.python.syntax.Module> encodePythonModule(hydra.module.Module mod, java.util.List<hydra.module.Definition> defs0) {
    java.util.List<hydra.module.Definition> defs = hydra.ext.python.coder.Coder.reorderDefs(defs0);
    hydra.ext.python.helpers.PythonModuleMetadata meta0 = hydra.ext.python.coder.Coder.gatherMetadata(
      (mod).namespace,
      defs);
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.python.syntax.Module>>) (g -> hydra.monads.Monads.withState(
        hydra.ext.python.coder.Coder.makePyGraph(
          g,
          meta0),
        ((java.util.function.Supplier<hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Module>>) (() -> {
          hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces0 = (meta0).namespaces;
          return hydra.lib.flows.Bind.apply(
            hydra.inference.Inference.initialTypeContext(g),
            (java.util.function.Function<hydra.typing.TypeContext, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Module>>) (tcontext -> {
              hydra.ext.python.helpers.PythonEnvironment env0 = hydra.ext.python.coder.Coder.initialEnvironment(
                namespaces0,
                tcontext);
              Boolean isTypeMod = hydra.ext.python.coder.Coder.isTypeModuleCheck(defs0);
              return hydra.ext.python.coder.Coder.withDefinitions(
                env0,
                defs,
                (java.util.function.Function<hydra.ext.python.helpers.PythonEnvironment, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Module>>) (env -> hydra.lib.flows.Bind.apply(
                  hydra.lib.flows.Map.apply(
                    (java.util.function.Function<java.util.List<java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>>, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>>) (xs -> hydra.lib.lists.Concat.apply(xs)),
                    hydra.lib.flows.MapList.apply(
                      (java.util.function.Function<hydra.module.Definition, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>>>) (d -> hydra.ext.python.coder.Coder.encodeDefinition(
                        env,
                        d)),
                      defs)),
                  (java.util.function.Function<java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Module>>) (defStmts -> hydra.lib.flows.Bind.apply(
                    hydra.monads.Monads.<hydra.ext.python.helpers.PyGraph>getState(),
                    (java.util.function.Function<hydra.ext.python.helpers.PyGraph, hydra.compute.Flow<hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Module>>) (pyg1 -> {
                      hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Statement>> commentStmts = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.apply(
                        (java.util.List<hydra.ext.python.syntax.Statement>) (java.util.List.<hydra.ext.python.syntax.Statement>of()),
                        (java.util.function.Function<String, java.util.List<hydra.ext.python.syntax.Statement>>) (c -> java.util.List.of(hydra.ext.python.utils.Utils.commentStatement(c))),
                        hydra.lib.maybes.Map.apply(
                          hydra.coderUtils.CoderUtils::normalizeComment,
                          (mod).description)));
                      hydra.ext.python.helpers.PythonModuleMetadata meta1 = hydra.ext.python.coder.Coder.pyGraphMetadata(pyg1);
                      hydra.util.Lazy<hydra.ext.python.helpers.PythonModuleMetadata> meta2 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                        hydra.lib.logic.And.apply(
                          hydra.lib.logic.Not.apply(isTypeMod),
                          hydra.ext.python.coder.Coder.useInlineTypeParams()),
                        () -> hydra.ext.python.coder.Coder.setMetaUsesTypeVar(
                          meta1,
                          false),
                        () -> meta1));
                      hydra.util.Lazy<hydra.ext.python.helpers.PythonModuleMetadata> meta = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                        hydra.lib.logic.And.apply(
                          isTypeMod,
                          hydra.lib.equality.Equal.apply(
                            hydra.ext.python.coder.Coder.targetPythonVersion(),
                            new hydra.ext.python.helpers.PythonVersion.Python310())),
                        () -> hydra.ext.python.coder.Coder.setMetaUsesTypeAlias(
                          meta2.get(),
                          true),
                        () -> meta2.get()));
                      hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces = (meta1).namespaces;
                      java.util.List<hydra.ext.python.syntax.Statement> importStmts = hydra.ext.python.coder.Coder.moduleImports(
                        namespaces,
                        meta.get());
                      hydra.util.Lazy<java.util.Set<hydra.core.Name>> tvars = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                        hydra.lib.logic.Or.apply(
                          isTypeMod,
                          hydra.lib.logic.Not.apply(hydra.ext.python.coder.Coder.useInlineTypeParams())),
                        () -> (meta.get()).typeVariables,
                        () -> (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())));
                      hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Statement>> tvarStmts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                        (java.util.function.Function<hydra.core.Name, hydra.ext.python.syntax.Statement>) (tv -> hydra.ext.python.coder.Coder.tvarStatement(hydra.ext.python.names.Names.encodeTypeVariable(tv))),
                        hydra.lib.sets.ToList.apply(tvars.get())));
                      hydra.util.Lazy<java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>> body = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
                        (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, Boolean>) (group -> hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(group))),
                        hydra.lib.lists.Concat.apply(java.util.List.of(
                          java.util.List.of(
                            commentStmts.get(),
                            importStmts,
                            tvarStmts.get()),
                          defStmts))));
                      return hydra.lib.flows.Pure.apply(new hydra.ext.python.syntax.Module(body.get()));
                    }))))));
            }));
        })).get())));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.Map<String, String>> moduleToPython(hydra.module.Module mod, java.util.List<hydra.module.Definition> defs) {
    return hydra.lib.flows.Bind.apply(
      hydra.ext.python.coder.Coder.encodePythonModule(
        mod,
        defs),
      (java.util.function.Function<hydra.ext.python.syntax.Module, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<String, String>>>) (file -> {
        String path = hydra.names.Names.namespaceToFilePath(
          new hydra.util.CaseConvention.LowerSnake(),
          new hydra.module.FileExtension("py"),
          (mod).namespace);
        String s = hydra.serialization.Serialization.printExpr(hydra.serialization.Serialization.parenthesize(hydra.ext.python.serde.Serde.encodeModule(file)));
        return hydra.lib.flows.Pure.apply(hydra.lib.maps.Singleton.apply(
          path,
          s));
      }));
  }
}
