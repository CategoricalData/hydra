// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.coder;

/**
 * Java code generator: converts Hydra modules to Java source code
 */
public interface Coder {
  static hydra.ext.java.helpers.JavaFeatures java8Features() {
    return new hydra.ext.java.helpers.JavaFeatures(false);
  }
  
  static hydra.ext.java.helpers.JavaFeatures java11Features() {
    return new hydra.ext.java.helpers.JavaFeatures(true);
  }
  
  static hydra.ext.java.helpers.JavaFeatures javaFeatures() {
    return hydra.ext.java.coder.Coder.java11Features();
  }
  
  static java.util.List<hydra.ext.java.syntax.ClassModifier> classModsPublic() {
    return java.util.List.of(new hydra.ext.java.syntax.ClassModifier.Public());
  }
  
  static hydra.ext.java.syntax.ClassBodyDeclarationWithComments noComment(hydra.ext.java.syntax.ClassBodyDeclaration decl) {
    return new hydra.ext.java.syntax.ClassBodyDeclarationWithComments(decl, (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()));
  }
  
  static hydra.ext.java.syntax.TypeArgumentsOrDiamond typeArgsOrDiamond(java.util.List<hydra.ext.java.syntax.TypeArgument> args) {
    return hydra.lib.logic.IfElse.lazy(
      (hydra.ext.java.coder.Coder.javaFeatures()).supportsDiamondOperator,
      () -> new hydra.ext.java.syntax.TypeArgumentsOrDiamond.Diamond(),
      () -> new hydra.ext.java.syntax.TypeArgumentsOrDiamond.Arguments(args));
  }
  
  static String bindingNameToFilePath(hydra.core.Name name) {
    hydra.module.QualifiedName qn = hydra.names.Names.qualifyName(name);
    String local = (qn).local;
    hydra.util.Maybe<hydra.module.Namespace> ns_ = (qn).namespace;
    String sanitized = hydra.formatting.Formatting.sanitizeWithUnderscores(
      hydra.ext.java.language.Language.reservedWords(),
      local);
    hydra.core.Name unq = hydra.names.Names.unqualifyName(new hydra.module.QualifiedName(ns_, sanitized));
    return hydra.adapt.utils.Utils.nameToFilePath(
      new hydra.util.CaseConvention.Camel(),
      new hydra.util.CaseConvention.Pascal(),
      new hydra.module.FileExtension("java"),
      unq);
  }
  
  static String javaIdentifierToString(hydra.ext.java.syntax.Identifier id) {
    return (id).value;
  }
  
  static java.util.List<hydra.core.Name> boundTypeVariables(hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of());
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Annotated at) {
        return hydra.ext.java.coder.Coder.boundTypeVariables(((at).value).body);
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.lib.lists.Cons.apply(
          ((ft).value).parameter,
          hydra.ext.java.coder.Coder.boundTypeVariables(((ft).value).body));
      }
    });
  }
  
  static java.util.List<hydra.core.Type> extractTypeApplicationArgs(hydra.core.Type typ) {
    return hydra.lib.lists.Reverse.apply(hydra.ext.java.coder.Coder.extractTypeApplicationArgs_go(typ));
  }
  
  static java.util.List<hydra.core.Type> extractTypeApplicationArgs_go(hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of());
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Application at) {
        return hydra.lib.lists.Cons.apply(
          ((at).value).argument,
          hydra.ext.java.coder.Coder.extractTypeApplicationArgs_go(((at).value).function));
      }
    });
  }
  
  static java.util.List<hydra.ext.java.syntax.TypeParameter> javaTypeParametersForType(hydra.core.Type typ) {
    java.util.List<hydra.core.Name> boundVars = hydra.ext.java.coder.Coder.javaTypeParametersForType_bvars(typ);
    hydra.util.Lazy<java.util.List<hydra.core.Name>> freeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.ext.java.coder.Coder.isLambdaBoundVariable(v)),
      hydra.lib.sets.ToList.apply(hydra.rewriting.Rewriting.freeVariablesInType(typ))));
    java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.TypeParameter> toParam = (java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.TypeParameter>) (name -> hydra.ext.java.utils.Utils.javaTypeParameter(hydra.formatting.Formatting.capitalize((name).value)));
    hydra.util.Lazy<java.util.List<hydra.core.Name>> vars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Nub.apply(hydra.lib.lists.Concat2.apply(
      boundVars,
      freeVars.get())));
    return hydra.lib.lists.Map.apply(
      toParam,
      vars.get());
  }
  
  static java.util.List<hydra.core.Name> javaTypeParametersForType_bvars(hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of());
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.lib.lists.Cons.apply(
          ((ft).value).parameter,
          hydra.ext.java.coder.Coder.javaTypeParametersForType_bvars(((ft).value).body));
      }
    });
  }
  
  static java.util.List<hydra.ext.java.syntax.TypeArgument> javaTypeArgumentsForType(hydra.core.Type typ) {
    return hydra.lib.lists.Reverse.apply(hydra.lib.lists.Map.apply(
      hydra.ext.java.utils.Utils::typeParameterToTypeArgument,
      hydra.ext.java.coder.Coder.javaTypeParametersForType(typ)));
  }
  
  static Boolean isLambdaBoundVariable(hydra.core.Name name) {
    String v = (name).value;
    return hydra.lib.equality.Lte.apply(
      hydra.lib.strings.Length.apply(v),
      4);
  }
  
  static Boolean isLocalVariable(hydra.core.Name name) {
    return hydra.lib.maybes.IsNothing.apply((hydra.names.Names.qualifyName(name)).namespace);
  }
  
  static java.util.List<hydra.ext.java.syntax.InterfaceType> serializableTypes(Boolean isSer) {
    hydra.util.Lazy<hydra.ext.java.syntax.InterfaceType> javaSerializableType = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.InterfaceType(new hydra.ext.java.syntax.ClassType((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()), new hydra.ext.java.syntax.ClassTypeQualifier.None(), hydra.ext.java.utils.Utils.javaTypeIdentifier("Serializable"), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()))));
    return hydra.lib.logic.IfElse.lazy(
      isSer,
      () -> java.util.List.of(javaSerializableType.get()),
      () -> (java.util.List<hydra.ext.java.syntax.InterfaceType>) (java.util.List.<hydra.ext.java.syntax.InterfaceType>of()));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> encodeLiteralType(hydra.core.LiteralType lt) {
    return (lt).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.LiteralType.Binary ignored) {
        return hydra.lib.flows.Pure.apply(new hydra.ext.java.syntax.Type.Reference(new hydra.ext.java.syntax.ReferenceType.Array(new hydra.ext.java.syntax.ArrayType(new hydra.ext.java.syntax.Dims(java.util.List.of((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()))), new hydra.ext.java.syntax.ArrayType_Variant.Primitive(new hydra.ext.java.syntax.PrimitiveTypeWithAnnotations(new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.Integral(new hydra.ext.java.syntax.IntegralType.Byte_())), (java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of())))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.LiteralType.Boolean_ ignored) {
        return hydra.ext.java.coder.Coder.<T0>encodeLiteralType_simple("Boolean");
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.LiteralType.Float_ ft) {
        return ((ft).value).accept(new hydra.core.FloatType.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.FloatType.Bigfloat ignored) {
            return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaRefType(
              (java.util.List<hydra.ext.java.syntax.ReferenceType>) (java.util.List.<hydra.ext.java.syntax.ReferenceType>of()),
              hydra.util.Maybe.just(hydra.ext.java.names.Names.javaPackageName(java.util.List.of(
                "java",
                "math"))),
              "BigDecimal"));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.FloatType.Float32 ignored) {
            return hydra.ext.java.coder.Coder.<T0>encodeLiteralType_simple("Float");
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.FloatType.Float64 ignored) {
            return hydra.ext.java.coder.Coder.<T0>encodeLiteralType_simple("Double");
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.LiteralType.Integer_ it) {
        return ((it).value).accept(new hydra.core.IntegerType.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Bigint ignored) {
            return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaRefType(
              (java.util.List<hydra.ext.java.syntax.ReferenceType>) (java.util.List.<hydra.ext.java.syntax.ReferenceType>of()),
              hydra.util.Maybe.just(hydra.ext.java.names.Names.javaPackageName(java.util.List.of(
                "java",
                "math"))),
              "BigInteger"));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Int8 ignored) {
            return hydra.ext.java.coder.Coder.<T0>encodeLiteralType_simple("Byte");
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Int16 ignored) {
            return hydra.ext.java.coder.Coder.<T0>encodeLiteralType_simple("Short");
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Int32 ignored) {
            return hydra.ext.java.coder.Coder.<T0>encodeLiteralType_simple("Integer");
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Int64 ignored) {
            return hydra.ext.java.coder.Coder.<T0>encodeLiteralType_simple("Long");
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Uint8 ignored) {
            return hydra.ext.java.coder.Coder.<T0>encodeLiteralType_simple("Short");
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Uint16 ignored) {
            return hydra.ext.java.coder.Coder.<T0>encodeLiteralType_simple("Character");
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Uint32 ignored) {
            return hydra.ext.java.coder.Coder.<T0>encodeLiteralType_simple("Long");
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Uint64 ignored) {
            return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaRefType(
              (java.util.List<hydra.ext.java.syntax.ReferenceType>) (java.util.List.<hydra.ext.java.syntax.ReferenceType>of()),
              hydra.util.Maybe.just(hydra.ext.java.names.Names.javaPackageName(java.util.List.of(
                "java",
                "math"))),
              "BigInteger"));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.core.LiteralType.String_ ignored) {
        return hydra.ext.java.coder.Coder.<T0>encodeLiteralType_simple("String");
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> encodeLiteralType_simple(String n) {
    return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaRefType(
      (java.util.List<hydra.ext.java.syntax.ReferenceType>) (java.util.List.<hydra.ext.java.syntax.ReferenceType>of()),
      (hydra.util.Maybe<hydra.ext.java.syntax.PackageName>) (hydra.util.Maybe.<hydra.ext.java.syntax.PackageName>nothing()),
      n));
  }
  
  static String elementsClassName(hydra.module.Namespace ns) {
    String nsStr = (ns).value;
    java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
      ".",
      nsStr);
    return hydra.formatting.Formatting.sanitizeWithUnderscores(
      hydra.ext.java.language.Language.reservedWords(),
      hydra.formatting.Formatting.capitalize(hydra.lib.lists.Last.apply(parts)));
  }
  
  static Boolean isRecursiveVariable(hydra.ext.java.helpers.Aliases aliases, hydra.core.Name name) {
    return hydra.lib.sets.Member.apply(
      name,
      (aliases).recursiveVars);
  }
  
  static java.util.List<hydra.ext.java.syntax.InterfaceType> interfaceTypes(Boolean isSer, hydra.ext.java.helpers.Aliases aliases, java.util.List<hydra.ext.java.syntax.TypeParameter> tparams, hydra.core.Name elName) {
    hydra.util.Lazy<hydra.ext.java.syntax.TypeArgument> selfTypeArg = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.TypeArgument.Reference(hydra.ext.java.utils.Utils.nameToJavaReferenceType(
      aliases,
      false,
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.ext.java.syntax.TypeParameter, hydra.ext.java.syntax.TypeArgument>) (tp_ -> hydra.ext.java.utils.Utils.typeParameterToTypeArgument(tp_)),
        tparams),
      elName,
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))));
    hydra.util.Lazy<hydra.ext.java.syntax.InterfaceType> javaComparableType = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.InterfaceType(new hydra.ext.java.syntax.ClassType((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()), new hydra.ext.java.syntax.ClassTypeQualifier.None(), hydra.ext.java.utils.Utils.javaTypeIdentifier("Comparable"), java.util.List.of(selfTypeArg.get()))));
    hydra.util.Lazy<hydra.ext.java.syntax.InterfaceType> javaSerializableType = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.InterfaceType(new hydra.ext.java.syntax.ClassType((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()), new hydra.ext.java.syntax.ClassTypeQualifier.None(), hydra.ext.java.utils.Utils.javaTypeIdentifier("Serializable"), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()))));
    return hydra.lib.logic.IfElse.lazy(
      isSer,
      () -> java.util.List.of(
        javaSerializableType.get(),
        javaComparableType.get()),
      () -> (java.util.List<hydra.ext.java.syntax.InterfaceType>) (java.util.List.<hydra.ext.java.syntax.InterfaceType>of()));
  }
  
  static Boolean isNonComparableType(hydra.core.Type typ) {
    return (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.List ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Set ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Map ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Maybe ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Pair ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Either ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Function ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Literal lt) {
        return ((lt).value).accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.LiteralType instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.LiteralType.Binary ignored) {
            return true;
          }
        });
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Forall ft) {
        return hydra.ext.java.coder.Coder.isNonComparableType(((ft).value).body);
      }
    });
  }
  
  static Boolean isBinaryType(hydra.core.Type typ) {
    return (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Literal lt) {
        return ((lt).value).accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.LiteralType instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.LiteralType.Binary ignored) {
            return true;
          }
        });
      }
    });
  }
  
  static Boolean isBigNumericType(hydra.core.Type typ) {
    return (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Literal lt) {
        return ((lt).value).accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.LiteralType instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.LiteralType.Float_ ft) {
            return ((ft).value).accept(new hydra.core.FloatType.PartialVisitor<>() {
              @Override
              public Boolean otherwise(hydra.core.FloatType instance) {
                return false;
              }
              
              @Override
              public Boolean visit(hydra.core.FloatType.Bigfloat ignored) {
                return true;
              }
            });
          }
          
          @Override
          public Boolean visit(hydra.core.LiteralType.Integer_ it) {
            return ((it).value).accept(new hydra.core.IntegerType.PartialVisitor<>() {
              @Override
              public Boolean otherwise(hydra.core.IntegerType instance) {
                return false;
              }
              
              @Override
              public Boolean visit(hydra.core.IntegerType.Bigint ignored) {
                return true;
              }
            });
          }
        });
      }
    });
  }
  
  static hydra.ext.java.syntax.Identifier innerClassRef(hydra.ext.java.helpers.Aliases aliases, hydra.core.Name name, String local) {
    String id = (hydra.ext.java.utils.Utils.nameToJavaName(
      aliases,
      name)).value;
    return new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        id,
        "."),
      local));
  }
  
  static java.util.List<hydra.core.Type> peelExpectedTypes(java.util.Map<hydra.core.Name, hydra.core.Type> subst, Integer n, hydra.core.Type t) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        n,
        0),
      () -> (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
      () -> (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public java.util.List<hydra.core.Type> otherwise(hydra.core.Type instance) {
          return (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of());
        }
        
        @Override
        public java.util.List<hydra.core.Type> visit(hydra.core.Type.Function ft) {
          return hydra.lib.lists.Cons.apply(
            hydra.ext.java.coder.Coder.applySubstFull(
              subst,
              ((ft).value).domain),
            hydra.ext.java.coder.Coder.peelExpectedTypes(
              subst,
              hydra.lib.math.Sub.apply(
                n,
                1),
              ((ft).value).codomain));
        }
      }));
  }
  
  static hydra.core.Type applySubstFull(java.util.Map<hydra.core.Name, hydra.core.Type> s, hydra.core.Type t) {
    return (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return t;
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable v) {
        return hydra.lib.maps.FindWithDefault.apply(
          t,
          (v).value,
          s);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Function ft) {
        return new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.ext.java.coder.Coder.applySubstFull(
          s,
          ((ft).value).domain), hydra.ext.java.coder.Coder.applySubstFull(
          s,
          ((ft).value).codomain)));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Application at) {
        return new hydra.core.Type.Application(new hydra.core.ApplicationType(hydra.ext.java.coder.Coder.applySubstFull(
          s,
          ((at).value).function), hydra.ext.java.coder.Coder.applySubstFull(
          s,
          ((at).value).argument)));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.List inner) {
        return new hydra.core.Type.List(hydra.ext.java.coder.Coder.applySubstFull(
          s,
          (inner).value));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Set inner) {
        return new hydra.core.Type.Set(hydra.ext.java.coder.Coder.applySubstFull(
          s,
          (inner).value));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Maybe inner) {
        return new hydra.core.Type.Maybe(hydra.ext.java.coder.Coder.applySubstFull(
          s,
          (inner).value));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Map mt) {
        return new hydra.core.Type.Map(new hydra.core.MapType(hydra.ext.java.coder.Coder.applySubstFull(
          s,
          ((mt).value).keys), hydra.ext.java.coder.Coder.applySubstFull(
          s,
          ((mt).value).values)));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Pair pt) {
        return new hydra.core.Type.Pair(new hydra.core.PairType(hydra.ext.java.coder.Coder.applySubstFull(
          s,
          ((pt).value).first), hydra.ext.java.coder.Coder.applySubstFull(
          s,
          ((pt).value).second)));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Either et) {
        return new hydra.core.Type.Either(new hydra.core.EitherType(hydra.ext.java.coder.Coder.applySubstFull(
          s,
          ((et).value).left), hydra.ext.java.coder.Coder.applySubstFull(
          s,
          ((et).value).right)));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return new hydra.core.Type.Forall(new hydra.core.ForallType(((ft).value).parameter, hydra.ext.java.coder.Coder.applySubstFull(
          hydra.lib.maps.Delete.apply(
            ((ft).value).parameter,
            s),
          ((ft).value).body)));
      }
    });
  }
  
  static java.util.Set<hydra.core.Name> collectTypeVars(hydra.core.Type typ) {
    return hydra.ext.java.coder.Coder.collectTypeVars_go(hydra.rewriting.Rewriting.deannotateType(typ));
  }
  
  static java.util.Set<hydra.core.Name> collectTypeVars_go(hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.Set<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply());
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Variable name) {
        return hydra.lib.sets.Singleton.apply((name).value);
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Function ft) {
        return hydra.lib.sets.Union.apply(
          hydra.ext.java.coder.Coder.collectTypeVars_go(hydra.rewriting.Rewriting.deannotateType(((ft).value).domain)),
          hydra.ext.java.coder.Coder.collectTypeVars_go(hydra.rewriting.Rewriting.deannotateType(((ft).value).codomain)));
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Application at) {
        return hydra.lib.sets.Union.apply(
          hydra.ext.java.coder.Coder.collectTypeVars_go(hydra.rewriting.Rewriting.deannotateType(((at).value).function)),
          hydra.ext.java.coder.Coder.collectTypeVars_go(hydra.rewriting.Rewriting.deannotateType(((at).value).argument)));
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.List inner) {
        return hydra.ext.java.coder.Coder.collectTypeVars_go(hydra.rewriting.Rewriting.deannotateType((inner).value));
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Set inner) {
        return hydra.ext.java.coder.Coder.collectTypeVars_go(hydra.rewriting.Rewriting.deannotateType((inner).value));
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Maybe inner) {
        return hydra.ext.java.coder.Coder.collectTypeVars_go(hydra.rewriting.Rewriting.deannotateType((inner).value));
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Map mt) {
        return hydra.lib.sets.Union.apply(
          hydra.ext.java.coder.Coder.collectTypeVars_go(hydra.rewriting.Rewriting.deannotateType(((mt).value).keys)),
          hydra.ext.java.coder.Coder.collectTypeVars_go(hydra.rewriting.Rewriting.deannotateType(((mt).value).values)));
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Pair pt) {
        return hydra.lib.sets.Union.apply(
          hydra.ext.java.coder.Coder.collectTypeVars_go(hydra.rewriting.Rewriting.deannotateType(((pt).value).first)),
          hydra.ext.java.coder.Coder.collectTypeVars_go(hydra.rewriting.Rewriting.deannotateType(((pt).value).second)));
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Either et) {
        return hydra.lib.sets.Union.apply(
          hydra.ext.java.coder.Coder.collectTypeVars_go(hydra.rewriting.Rewriting.deannotateType(((et).value).left)),
          hydra.ext.java.coder.Coder.collectTypeVars_go(hydra.rewriting.Rewriting.deannotateType(((et).value).right)));
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.ext.java.coder.Coder.collectTypeVars_go(hydra.rewriting.Rewriting.deannotateType(((ft).value).body));
      }
    });
  }
  
  static hydra.core.Type substituteTypeVarsWithTypes(java.util.Map<hydra.core.Name, hydra.core.Type> subst, hydra.core.Type t) {
    return hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes_go(
      subst,
      hydra.rewriting.Rewriting.deannotateType(t));
  }
  
  static hydra.core.Type substituteTypeVarsWithTypes_go(java.util.Map<hydra.core.Name, hydra.core.Type> subst, hydra.core.Type t) {
    return (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return t;
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable v) {
        return hydra.lib.maybes.Cases.apply(
          hydra.lib.maps.Lookup.apply(
            (v).value,
            subst),
          t,
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (rep -> rep));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Function ft) {
        return new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes_go(
          subst,
          ((ft).value).domain), hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes_go(
          subst,
          ((ft).value).codomain)));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Application at) {
        return new hydra.core.Type.Application(new hydra.core.ApplicationType(hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes_go(
          subst,
          ((at).value).function), hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes_go(
          subst,
          ((at).value).argument)));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.List inner) {
        return new hydra.core.Type.List(hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (inner).value));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Set inner) {
        return new hydra.core.Type.Set(hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (inner).value));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Maybe inner) {
        return new hydra.core.Type.Maybe(hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (inner).value));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Map mt) {
        return new hydra.core.Type.Map(new hydra.core.MapType(hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes_go(
          subst,
          ((mt).value).keys), hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes_go(
          subst,
          ((mt).value).values)));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Pair pt) {
        return new hydra.core.Type.Pair(new hydra.core.PairType(hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes_go(
          subst,
          ((pt).value).first), hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes_go(
          subst,
          ((pt).value).second)));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Either et) {
        return new hydra.core.Type.Either(new hydra.core.EitherType(hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes_go(
          subst,
          ((et).value).left), hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes_go(
          subst,
          ((et).value).right)));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return new hydra.core.Type.Forall(new hydra.core.ForallType(((ft).value).parameter, hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes_go(
          subst,
          ((ft).value).body)));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> addComment(hydra.ext.java.syntax.ClassBodyDeclaration decl, hydra.core.FieldType field) {
    return hydra.lib.flows.Map.apply(
      (java.util.function.Function<hydra.util.Maybe<String>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>) (c -> new hydra.ext.java.syntax.ClassBodyDeclarationWithComments(decl, c)),
      hydra.coderUtils.CoderUtils.commentsFromFieldType(field));
  }
  
  static hydra.ext.java.helpers.JavaEnvironment insertBranchVar(hydra.core.Name name, hydra.ext.java.helpers.JavaEnvironment env) {
    hydra.ext.java.helpers.Aliases aliases = (env).aliases;
    return new hydra.ext.java.helpers.JavaEnvironment(new hydra.ext.java.helpers.Aliases((aliases).currentNamespace, (aliases).packages, hydra.lib.sets.Insert.apply(
      name,
      (aliases).branchVars), (aliases).recursiveVars, (aliases).inScopeTypeParams, (aliases).polymorphicLocals, (aliases).inScopeJavaVars, (aliases).varRenames, (aliases).lambdaVars, (aliases).typeVarSubst, (aliases).trustedTypeVars, (aliases).methodCodomain, (aliases).thunkedVars), (env).typeContext);
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type> getCodomain(java.util.Map<hydra.core.Name, hydra.core.Term> ann) {
    return hydra.lib.flows.Map.apply(
      (java.util.function.Function<hydra.core.FunctionType, hydra.core.Type>) (ft -> (ft).codomain),
      hydra.ext.java.coder.Coder.getFunctionType(ann));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.FunctionType> getFunctionType(java.util.Map<hydra.core.Name, hydra.core.Term> ann) {
    return hydra.lib.flows.Bind.apply(
      hydra.annotations.Annotations.getType(ann),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.FunctionType>>) (mt -> hydra.lib.maybes.Cases.apply(
        mt,
        hydra.lib.flows.Fail.apply("type annotation is required for function and elimination terms in Java"),
        (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.FunctionType>>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<hydra.graph.Graph, hydra.core.FunctionType> otherwise(hydra.core.Type instance) {
            return hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
              "expected function type, got: ",
              hydra.show.core.Core.type(t)));
          }
          
          @Override
          public hydra.compute.Flow<hydra.graph.Graph, hydra.core.FunctionType> visit(hydra.core.Type.Function ft) {
            return hydra.lib.flows.Pure.apply((ft).value);
          }
        })))));
  }
  
  static hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>> wrapLazyArguments(hydra.core.Name name, java.util.List<hydra.ext.java.syntax.Expression> args) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        hydra.lib.equality.Equal.apply(
          name,
          new hydra.core.Name("hydra.lib.logic.ifElse")),
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(args),
          3)),
      () -> (hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>(java.util.List.of(
        hydra.lib.lists.At.apply(
          0,
          args),
        hydra.ext.java.coder.Coder.wrapInSupplierLambda(hydra.lib.lists.At.apply(
          1,
          args)),
        hydra.ext.java.coder.Coder.wrapInSupplierLambda(hydra.lib.lists.At.apply(
          2,
          args))), hydra.util.Maybe.just("lazy")))),
      () -> (hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>(args, (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())))));
  }
  
  static hydra.ext.java.syntax.Expression wrapInSupplierLambda(hydra.ext.java.syntax.Expression expr) {
    return new hydra.ext.java.syntax.Expression.Lambda(new hydra.ext.java.syntax.LambdaExpression(new hydra.ext.java.syntax.LambdaParameters.Tuple((java.util.List<hydra.ext.java.syntax.LambdaParameters>) (java.util.List.<hydra.ext.java.syntax.LambdaParameters>of())), new hydra.ext.java.syntax.LambdaBody.Expression(expr)));
  }
  
  static hydra.ext.java.syntax.Identifier elementJavaIdentifier(Boolean isPrim, Boolean isMethod, hydra.ext.java.helpers.Aliases aliases, hydra.core.Name name) {
    hydra.module.QualifiedName qn = hydra.names.Names.qualifyName(name);
    String local = (qn).local;
    hydra.util.Maybe<hydra.module.Namespace> ns_ = (qn).namespace;
    hydra.util.Lazy<String> sep = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      isMethod,
      () -> "::",
      () -> "."));
    return hydra.lib.logic.IfElse.lazy(
      isPrim,
      () -> new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.ext.java.coder.Coder.elementJavaIdentifier_qualify(
            aliases,
            ns_,
            hydra.formatting.Formatting.capitalize(local)),
          "."),
        hydra.ext.java.names.Names.applyMethodName())),
      () -> hydra.lib.maybes.Cases.apply(
        ns_,
        new hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.Utils.sanitizeJavaName(local)),
        (java.util.function.Function<hydra.module.Namespace, hydra.ext.java.syntax.Identifier>) (n -> new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.ext.java.coder.Coder.elementJavaIdentifier_qualify(
              aliases,
              hydra.util.Maybe.just(n),
              hydra.ext.java.coder.Coder.elementsClassName(n)),
            sep.get()),
          hydra.ext.java.utils.Utils.sanitizeJavaName(local))))));
  }
  
  static String elementJavaIdentifier_qualify(hydra.ext.java.helpers.Aliases aliases, hydra.util.Maybe<hydra.module.Namespace> mns, String s) {
    return (hydra.ext.java.utils.Utils.nameToJavaName(
      aliases,
      hydra.names.Names.unqualifyName(new hydra.module.QualifiedName(mns, s)))).value;
  }
  
  static Boolean isLambdaBoundIn(hydra.core.Name name, java.util.Set<hydra.core.Name> lambdaVars) {
    return hydra.lib.logic.Or.apply(
      hydra.lib.sets.Member.apply(
        name,
        lambdaVars),
      hydra.lib.logic.Or.apply(
        hydra.lib.logic.And.apply(
          hydra.ext.java.coder.Coder.isLambdaBoundIn_isQualified(name),
          hydra.lib.maybes.IsJust.apply(hydra.lib.lists.Find.apply(
            (java.util.function.Function<hydra.core.Name, Boolean>) (lv -> hydra.lib.logic.And.apply(
              hydra.ext.java.coder.Coder.isLambdaBoundIn_isQualified(lv),
              hydra.lib.equality.Equal.apply(
                hydra.names.Names.localNameOf(lv),
                hydra.names.Names.localNameOf(name)))),
            hydra.lib.sets.ToList.apply(lambdaVars)))),
        hydra.lib.logic.And.apply(
          hydra.lib.logic.Not.apply(hydra.ext.java.coder.Coder.isLambdaBoundIn_isQualified(name)),
          hydra.lib.sets.Member.apply(
            new hydra.core.Name(hydra.names.Names.localNameOf(name)),
            lambdaVars))));
  }
  
  static Boolean isLambdaBoundIn_isQualified(hydra.core.Name n) {
    return hydra.lib.maybes.IsJust.apply((hydra.names.Names.qualifyName(n)).namespace);
  }
  
  static hydra.core.Name findMatchingLambdaVar(hydra.core.Name name, java.util.Set<hydra.core.Name> lambdaVars) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        name,
        lambdaVars),
      () -> name,
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.ext.java.coder.Coder.isLambdaBoundIn_isQualified(name),
        () -> hydra.lib.maybes.FromMaybe.apply(
          name,
          hydra.lib.lists.Find.apply(
            (java.util.function.Function<hydra.core.Name, Boolean>) (lv -> hydra.lib.logic.And.apply(
              hydra.ext.java.coder.Coder.isLambdaBoundIn_isQualified(lv),
              hydra.lib.equality.Equal.apply(
                hydra.names.Names.localNameOf(lv),
                hydra.names.Names.localNameOf(name)))),
            hydra.lib.sets.ToList.apply(lambdaVars))),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.sets.Member.apply(
            new hydra.core.Name(hydra.names.Names.localNameOf(name)),
            lambdaVars),
          () -> new hydra.core.Name(hydra.names.Names.localNameOf(name)),
          () -> name)));
  }
  
  static hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit> constructElementsInterface(hydra.module.Module mod, java.util.List<hydra.ext.java.syntax.InterfaceMemberDeclaration> members) {
    hydra.ext.java.syntax.InterfaceBody body = new hydra.ext.java.syntax.InterfaceBody(members);
    String className = hydra.ext.java.coder.Coder.elementsClassName((mod).namespace);
    java.util.List<hydra.ext.java.syntax.InterfaceModifier> mods = java.util.List.of(new hydra.ext.java.syntax.InterfaceModifier.Public());
    hydra.util.Lazy<hydra.ext.java.syntax.TypeDeclaration> itf = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.TypeDeclaration.Interface(new hydra.ext.java.syntax.InterfaceDeclaration.NormalInterface(new hydra.ext.java.syntax.NormalInterfaceDeclaration(mods, hydra.ext.java.utils.Utils.javaTypeIdentifier(className), (java.util.List<hydra.ext.java.syntax.TypeParameter>) (java.util.List.<hydra.ext.java.syntax.TypeParameter>of()), (java.util.List<hydra.ext.java.syntax.InterfaceType>) (java.util.List.<hydra.ext.java.syntax.InterfaceType>of()), body))));
    hydra.ext.java.syntax.TypeDeclarationWithComments decl = new hydra.ext.java.syntax.TypeDeclarationWithComments(itf.get(), (mod).description);
    hydra.core.Name elName = hydra.names.Names.unqualifyName(new hydra.module.QualifiedName(hydra.util.Maybe.just((mod).namespace), className));
    hydra.ext.java.syntax.PackageDeclaration pkg = hydra.ext.java.utils.Utils.javaPackageDeclaration((mod).namespace);
    return (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>(elName, new hydra.ext.java.syntax.CompilationUnit.Ordinary(new hydra.ext.java.syntax.OrdinaryCompilationUnit(hydra.util.Maybe.just(pkg), (java.util.List<hydra.ext.java.syntax.ImportDeclaration>) (java.util.List.<hydra.ext.java.syntax.ImportDeclaration>of()), java.util.List.of(decl))))));
  }
  
  static java.util.List<hydra.ext.java.syntax.InterfaceMemberDeclaration> splitConstantInitializer(hydra.ext.java.syntax.InterfaceMemberDeclaration member) {
    return (member).accept(new hydra.ext.java.syntax.InterfaceMemberDeclaration.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.ext.java.syntax.InterfaceMemberDeclaration> otherwise(hydra.ext.java.syntax.InterfaceMemberDeclaration instance) {
        return java.util.List.of(member);
      }
      
      @Override
      public java.util.List<hydra.ext.java.syntax.InterfaceMemberDeclaration> visit(hydra.ext.java.syntax.InterfaceMemberDeclaration.Constant cd) {
        return hydra.lib.lists.Bind.apply(
          ((cd).value).variables,
          (java.util.function.Function<hydra.ext.java.syntax.VariableDeclarator, java.util.List<hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (v1 -> hydra.ext.java.coder.Coder.splitConstantInitializer_splitVar(
            ((cd).value).modifiers,
            ((cd).value).type,
            v1)));
      }
    });
  }
  
  static java.util.List<hydra.ext.java.syntax.InterfaceMemberDeclaration> splitConstantInitializer_splitVar(java.util.List<hydra.ext.java.syntax.ConstantModifier> mods, hydra.ext.java.syntax.UnannType utype, hydra.ext.java.syntax.VariableDeclarator vd) {
    hydra.util.Maybe<hydra.ext.java.syntax.VariableInitializer> mInit = (vd).initializer;
    hydra.ext.java.syntax.VariableDeclaratorId vid = (vd).id;
    return hydra.lib.maybes.Cases.apply(
      mInit,
      java.util.List.of(new hydra.ext.java.syntax.InterfaceMemberDeclaration.Constant(new hydra.ext.java.syntax.ConstantDeclaration(mods, utype, java.util.List.of(vd)))),
      (java.util.function.Function<hydra.ext.java.syntax.VariableInitializer, java.util.List<hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (init_ -> (init_).accept(new hydra.ext.java.syntax.VariableInitializer.PartialVisitor<>() {
        @Override
        public java.util.List<hydra.ext.java.syntax.InterfaceMemberDeclaration> otherwise(hydra.ext.java.syntax.VariableInitializer instance) {
          return java.util.List.of(new hydra.ext.java.syntax.InterfaceMemberDeclaration.Constant(new hydra.ext.java.syntax.ConstantDeclaration(mods, utype, java.util.List.of(vd))));
        }
        
        @Override
        public java.util.List<hydra.ext.java.syntax.InterfaceMemberDeclaration> visit(hydra.ext.java.syntax.VariableInitializer.Expression expr) {
          String varName = hydra.ext.java.coder.Coder.javaIdentifierToString((vid).identifier);
          String helperName = hydra.lib.strings.Cat2.apply(
            "_init_",
            varName);
          hydra.util.Lazy<hydra.ext.java.syntax.Expression> callExpr = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocation(
            (hydra.util.Maybe<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>>) (hydra.util.Maybe.<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>>nothing()),
            new hydra.ext.java.syntax.Identifier(helperName),
            (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of()))));
          hydra.ext.java.syntax.InterfaceMemberDeclaration field = new hydra.ext.java.syntax.InterfaceMemberDeclaration.Constant(new hydra.ext.java.syntax.ConstantDeclaration(mods, utype, java.util.List.of(new hydra.ext.java.syntax.VariableDeclarator(vid, hydra.util.Maybe.just(new hydra.ext.java.syntax.VariableInitializer.Expression(callExpr.get()))))));
          hydra.ext.java.syntax.Result resultType = new hydra.ext.java.syntax.Result.Type(utype);
          hydra.ext.java.syntax.BlockStatement returnSt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just((expr).value)));
          hydra.util.Lazy<hydra.ext.java.syntax.InterfaceMemberDeclaration> helper = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.interfaceMethodDeclaration(
            java.util.List.of(
              new hydra.ext.java.syntax.InterfaceMethodModifier.Static(),
              new hydra.ext.java.syntax.InterfaceMethodModifier.Private()),
            (java.util.List<hydra.ext.java.syntax.TypeParameter>) (java.util.List.<hydra.ext.java.syntax.TypeParameter>of()),
            helperName,
            (java.util.List<hydra.ext.java.syntax.FormalParameter>) (java.util.List.<hydra.ext.java.syntax.FormalParameter>of()),
            resultType,
            hydra.util.Maybe.just(java.util.List.of(returnSt))));
          return java.util.List.of(
            field,
            helper.get());
        }
      })));
  }
  
  static Boolean isUnresolvedInferenceVar(hydra.core.Name name) {
    java.util.List<Integer> chars = hydra.lib.strings.ToList.apply((name).value);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(chars),
      () -> false,
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Head.apply(chars),
          116)),
        () -> false,
        () -> ((java.util.function.Supplier<Boolean>) (() -> {
          hydra.util.Lazy<java.util.List<Integer>> rest = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(chars));
          return hydra.lib.logic.And.apply(
            hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(rest.get())),
            hydra.lib.lists.Null.apply(hydra.lib.lists.Filter.apply(
              (java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.logic.Not.apply(hydra.ext.java.coder.Coder.isUnresolvedInferenceVar_isDigit(c))),
              rest.get())));
        })).get()));
  }
  
  static Boolean isUnresolvedInferenceVar_isDigit(Integer c) {
    return hydra.lib.logic.And.apply(
      hydra.lib.equality.Gte.apply(
        c,
        48),
      hydra.lib.equality.Lte.apply(
        c,
        57));
  }
  
  static hydra.ext.java.helpers.JavaSymbolClass classifyDataTerm(hydra.core.TypeScheme ts, hydra.core.Term term) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.rewriting.Rewriting.isLambda(term),
      () -> ((java.util.function.Supplier<hydra.ext.java.helpers.JavaSymbolClass>) (() -> {
        Integer n = hydra.ext.java.coder.Coder.classifyDataTerm_countLambdaParams(term);
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Gt.apply(
            n,
            1),
          () -> new hydra.ext.java.helpers.JavaSymbolClass.HoistedLambda(n),
          () -> new hydra.ext.java.helpers.JavaSymbolClass.UnaryFunction());
      })).get(),
      () -> ((java.util.function.Supplier<hydra.ext.java.helpers.JavaSymbolClass>) (() -> {
        hydra.util.Lazy<Boolean> hasTypeParams = new hydra.util.Lazy<>(() -> hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply((ts).variables)));
        return hydra.lib.logic.IfElse.lazy(
          hasTypeParams.get(),
          () -> ((java.util.function.Supplier<hydra.ext.java.helpers.JavaSymbolClass>) (() -> {
            Integer n2 = hydra.ext.java.coder.Coder.classifyDataTerm_countLambdaParams(hydra.ext.java.coder.Coder.classifyDataTerm_stripTypeLambdas(term));
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Gt.apply(
                n2,
                0),
              () -> new hydra.ext.java.helpers.JavaSymbolClass.HoistedLambda(n2),
              () -> new hydra.ext.java.helpers.JavaSymbolClass.NullaryFunction());
          })).get(),
          () -> new hydra.ext.java.helpers.JavaSymbolClass.NullaryFunction());
      })).get());
  }
  
  static Integer classifyDataTerm_countLambdaParams(hydra.core.Term t) {
    return (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Term instance) {
        return 0;
      }
      
      @Override
      public Integer visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public Integer otherwise(hydra.core.Function instance) {
            return 0;
          }
          
          @Override
          public Integer visit(hydra.core.Function.Lambda lam) {
            return hydra.lib.math.Add.apply(
              1,
              hydra.ext.java.coder.Coder.classifyDataTerm_countLambdaParams(((lam).value).body));
          }
        });
      }
      
      @Override
      public Integer visit(hydra.core.Term.Let lt) {
        return hydra.ext.java.coder.Coder.classifyDataTerm_countLambdaParams(((lt).value).body);
      }
    });
  }
  
  static hydra.core.Term classifyDataTerm_stripTypeLambdas(hydra.core.Term t) {
    return (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
        return hydra.ext.java.coder.Coder.classifyDataTerm_stripTypeLambdas(((tl).value).body);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.helpers.JavaSymbolClass> classifyDataReference(hydra.core.Name name) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.dereferenceElement(name),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.helpers.JavaSymbolClass>>) (mel -> hydra.lib.maybes.Cases.apply(
        mel,
        hydra.lib.flows.Pure.apply(new hydra.ext.java.helpers.JavaSymbolClass.LocalVariable()),
        (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.helpers.JavaSymbolClass>>) (el -> hydra.lib.maybes.Cases.apply(
          (el).type,
          hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
            "no type scheme for element ",
            ((el).name).value)),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.helpers.JavaSymbolClass>>) (ts -> hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.classifyDataTerm(
            ts,
            (el).term))))))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> encodeType(hydra.ext.java.helpers.Aliases aliases, java.util.Set<hydra.core.Name> boundVars, hydra.core.Type t) {
    java.util.Set<hydra.core.Name> inScopeTypeParams = (aliases).inScopeTypeParams;
    java.util.Map<hydra.core.Name, hydra.core.Name> typeVarSubst = (aliases).typeVarSubst;
    return (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> otherwise(hydra.core.Type instance) {
        return hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
          "can't encode unsupported type in Java: ",
          hydra.show.core.Core.type(t)));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Application at) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.java.coder.Coder.encodeType(
            aliases,
            boundVars,
            ((at).value).function),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (jlhs -> hydra.lib.flows.Bind.apply(
            hydra.lib.flows.Bind.apply(
              hydra.ext.java.coder.Coder.encodeType(
                aliases,
                boundVars,
                ((at).value).argument),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt_))),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (jrhs -> hydra.ext.java.utils.Utils.addJavaTypeParameter(
              jrhs,
              jlhs)))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Function ft) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.Bind.apply(
            hydra.ext.java.coder.Coder.encodeType(
              aliases,
              boundVars,
              ((ft).value).domain),
            (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt_))),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (jdom -> hydra.lib.flows.Bind.apply(
            hydra.lib.flows.Bind.apply(
              hydra.ext.java.coder.Coder.encodeType(
                aliases,
                boundVars,
                ((ft).value).codomain),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt_))),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (jcod -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaRefType(
              java.util.List.of(
                jdom,
                jcod),
              hydra.ext.java.names.Names.javaUtilFunctionPackageName(),
              "Function"))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Forall fa) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.java.coder.Coder.encodeType(
            aliases,
            hydra.lib.sets.Insert.apply(
              ((fa).value).parameter,
              boundVars),
            ((fa).value).body),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (jbody -> hydra.ext.java.utils.Utils.addJavaTypeParameter(
            hydra.ext.java.utils.Utils.javaTypeVariable((((fa).value).parameter).value),
            jbody)));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> visit(hydra.core.Type.List et) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.java.coder.Coder.encodeType(
            aliases,
            boundVars,
            (et).value),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (jet -> hydra.lib.flows.Bind.apply(
            hydra.lib.flows.Bind.apply(
              hydra.lib.flows.Pure.apply(jet),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt_))),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (rt -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaRefType(
              java.util.List.of(rt),
              hydra.ext.java.names.Names.javaUtilPackageName(),
              "List"))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Literal lt) {
        return hydra.ext.java.coder.Coder.encodeLiteralType((lt).value);
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Either et) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.Bind.apply(
            hydra.ext.java.coder.Coder.encodeType(
              aliases,
              boundVars,
              ((et).value).left),
            (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt_))),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (jlt -> hydra.lib.flows.Bind.apply(
            hydra.lib.flows.Bind.apply(
              hydra.ext.java.coder.Coder.encodeType(
                aliases,
                boundVars,
                ((et).value).right),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt_))),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (jrt -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaRefType(
              java.util.List.of(
                jlt,
                jrt),
              hydra.ext.java.names.Names.hydraUtilPackageName(),
              "Either"))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Map mt) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.Bind.apply(
            hydra.ext.java.coder.Coder.encodeType(
              aliases,
              boundVars,
              ((mt).value).keys),
            (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt_))),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (jkt -> hydra.lib.flows.Bind.apply(
            hydra.lib.flows.Bind.apply(
              hydra.ext.java.coder.Coder.encodeType(
                aliases,
                boundVars,
                ((mt).value).values),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt_))),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (jvt -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaRefType(
              java.util.List.of(
                jkt,
                jvt),
              hydra.ext.java.names.Names.javaUtilPackageName(),
              "Map"))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Pair pt) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.Bind.apply(
            hydra.ext.java.coder.Coder.encodeType(
              aliases,
              boundVars,
              ((pt).value).first),
            (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt_))),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (jfirst -> hydra.lib.flows.Bind.apply(
            hydra.lib.flows.Bind.apply(
              hydra.ext.java.coder.Coder.encodeType(
                aliases,
                boundVars,
                ((pt).value).second),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt_))),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (jsecond -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaRefType(
              java.util.List.of(
                jfirst,
                jsecond),
              hydra.ext.java.names.Names.hydraUtilPackageName(),
              "Tuple.Tuple2"))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Unit ignored) {
        return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaRefType(
          (java.util.List<hydra.ext.java.syntax.ReferenceType>) (java.util.List.<hydra.ext.java.syntax.ReferenceType>of()),
          hydra.ext.java.names.Names.javaLangPackageName(),
          "Void"));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Record rt) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.And.apply(
            hydra.lib.equality.Equal.apply(
              ((rt).value).typeName,
              new hydra.core.Name("hydra.core.Unit")),
            hydra.lib.lists.Null.apply(((rt).value).fields)),
          () -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaRefType(
            (java.util.List<hydra.ext.java.syntax.ReferenceType>) (java.util.List.<hydra.ext.java.syntax.ReferenceType>of()),
            hydra.ext.java.names.Names.javaLangPackageName(),
            "Void")),
          () -> hydra.lib.flows.Pure.apply(new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.utils.Utils.nameToJavaReferenceType(
            aliases,
            true,
            hydra.ext.java.coder.Coder.javaTypeArgumentsForType(t),
            ((rt).value).typeName,
            (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Maybe ot) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.Bind.apply(
            hydra.ext.java.coder.Coder.encodeType(
              aliases,
              boundVars,
              (ot).value),
            (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt_))),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (jot -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaRefType(
            java.util.List.of(jot),
            hydra.ext.java.names.Names.hydraUtilPackageName(),
            "Maybe"))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Set st) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.Bind.apply(
            hydra.ext.java.coder.Coder.encodeType(
              aliases,
              boundVars,
              (st).value),
            (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt_))),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (jst -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaRefType(
            java.util.List.of(jst),
            hydra.ext.java.names.Names.javaUtilPackageName(),
            "Set"))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Union rt) {
        return hydra.lib.flows.Pure.apply(new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.utils.Utils.nameToJavaReferenceType(
          aliases,
          true,
          hydra.ext.java.coder.Coder.javaTypeArgumentsForType(t),
          ((rt).value).typeName,
          (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Variable name0) {
        hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.apply(
          (name0).value,
          hydra.lib.maps.Lookup.apply(
            (name0).value,
            typeVarSubst)));
        return hydra.lib.flows.Bind.apply(
          hydra.ext.java.coder.Coder.encodeType_resolveIfTypedef(
            aliases,
            boundVars,
            inScopeTypeParams,
            name.get()),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (resolved -> hydra.lib.maybes.Cases.apply(
            resolved,
            hydra.lib.flows.Pure.apply(hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.Or.apply(
                hydra.lib.sets.Member.apply(
                  name.get(),
                  boundVars),
                hydra.lib.sets.Member.apply(
                  name.get(),
                  inScopeTypeParams)),
              () -> new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.utils.Utils.javaTypeVariable((name.get()).value)),
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.ext.java.coder.Coder.isLambdaBoundVariable(name.get()),
                () -> new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.utils.Utils.javaTypeVariable((name.get()).value)),
                () -> hydra.lib.logic.IfElse.lazy(
                  hydra.ext.java.coder.Coder.isUnresolvedInferenceVar(name.get()),
                  () -> new hydra.ext.java.syntax.Type.Reference(new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Class_(hydra.ext.java.utils.Utils.javaClassType(
                    (java.util.List<hydra.ext.java.syntax.ReferenceType>) (java.util.List.<hydra.ext.java.syntax.ReferenceType>of()),
                    hydra.ext.java.names.Names.javaLangPackageName(),
                    "Object")))),
                  () -> new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.utils.Utils.nameToJavaReferenceType(
                    aliases,
                    true,
                    (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()),
                    name.get(),
                    (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))))))),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type>>) (resolvedType -> hydra.ext.java.coder.Coder.encodeType(
              aliases,
              boundVars,
              resolvedType)))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Wrap wt) {
        return hydra.lib.flows.Pure.apply(new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.utils.Utils.nameToJavaReferenceType(
          aliases,
          true,
          (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()),
          ((wt).value).typeName,
          (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>> encodeType_resolveIfTypedef(T0 aliases, java.util.Set<hydra.core.Name> boundVars, java.util.Set<hydra.core.Name> inScopeTypeParams, hydra.core.Name name) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Or.apply(
        hydra.lib.sets.Member.apply(
          name,
          boundVars),
        hydra.lib.sets.Member.apply(
          name,
          inScopeTypeParams)),
      () -> hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.ext.java.coder.Coder.isLambdaBoundVariable(name),
        () -> hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
        () -> hydra.lib.flows.Bind.apply(
          hydra.monads.Monads.<hydra.graph.Graph>getState(),
          (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>>>) (g -> hydra.lib.flows.Bind.apply(
            hydra.schemas.Schemas.graphToInferenceContext(g),
            (java.util.function.Function<hydra.typing.InferenceContext, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>>>) (ix -> {
              java.util.Map<hydra.core.Name, hydra.core.TypeScheme> schemaTypes = (ix).schemaTypes;
              return hydra.lib.maybes.Cases.apply(
                hydra.lib.maps.Lookup.apply(
                  name,
                  schemaTypes),
                hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
                (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>>>) (ts -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply((ts).variables)),
                  () -> hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
                  () -> (hydra.rewriting.Rewriting.deannotateType((ts).type)).accept(new hydra.core.Type.PartialVisitor<>() {
                    @Override
                    public hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>> otherwise(hydra.core.Type instance) {
                      return hydra.lib.flows.Pure.apply(hydra.util.Maybe.just((ts).type));
                    }
                    
                    @Override
                    public hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>> visit(hydra.core.Type.Record ignored) {
                      return hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()));
                    }
                    
                    @Override
                    public hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>> visit(hydra.core.Type.Union ignored) {
                      return hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()));
                    }
                    
                    @Override
                    public hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>> visit(hydra.core.Type.Wrap ignored) {
                      return hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()));
                    }
                  }))));
            }))))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>> javaTypeArgumentsForNamedType(hydra.core.Name tname) {
    return hydra.lib.flows.Map.apply(
      (java.util.function.Function<hydra.core.Type, java.util.List<hydra.ext.java.syntax.TypeArgument>>) (typ -> hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.ext.java.syntax.TypeParameter, hydra.ext.java.syntax.TypeArgument>) (tp_ -> hydra.ext.java.utils.Utils.typeParameterToTypeArgument(tp_)),
        hydra.ext.java.coder.Coder.javaTypeParametersForType(typ))),
      hydra.schemas.Schemas.requireType(tname));
  }
  
  static hydra.ext.java.syntax.Expression encodeLiteral(hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.Literal.Binary bs) {
        java.util.List<Integer> byteValues = hydra.lib.literals.BinaryToBytes.apply((bs).value);
        return hydra.ext.java.utils.Utils.javaArrayCreation(
          hydra.ext.java.utils.Utils.javaBytePrimitiveType(),
          hydra.util.Maybe.just(hydra.ext.java.utils.Utils.javaArrayInitializer(hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, hydra.ext.java.syntax.Expression>) (w -> hydra.ext.java.utils.Utils.javaLiteralToJavaExpression(new hydra.ext.java.syntax.Literal.Integer_(new hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.Int32ToBigint.apply(w))))),
            byteValues))));
      }
      
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.Literal.Boolean_ b) {
        return hydra.ext.java.coder.Coder.encodeLiteral_litExp(hydra.ext.java.utils.Utils.javaBoolean((b).value));
      }
      
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.Literal.Float_ f) {
        return hydra.ext.java.coder.Coder.encodeLiteral_encodeFloat((f).value);
      }
      
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.Literal.Integer_ i) {
        return hydra.ext.java.coder.Coder.encodeLiteral_encodeInteger((i).value);
      }
      
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.Literal.String_ s) {
        return hydra.ext.java.coder.Coder.encodeLiteral_litExp(hydra.ext.java.utils.Utils.javaString((s).value));
      }
    });
  }
  
  static hydra.ext.java.syntax.Expression encodeLiteral_litExp(hydra.ext.java.syntax.Literal l) {
    return hydra.ext.java.utils.Utils.javaLiteralToJavaExpression(l);
  }
  
  static hydra.ext.java.syntax.Expression encodeLiteral_primCast(hydra.ext.java.syntax.PrimitiveType pt, hydra.ext.java.syntax.Expression expr) {
    return hydra.ext.java.utils.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.Utils.javaCastPrimitive(
      pt,
      hydra.ext.java.utils.Utils.javaExpressionToJavaUnaryExpression(expr)));
  }
  
  static hydra.ext.java.syntax.Expression encodeLiteral_encodeFloat(hydra.core.FloatValue f) {
    return (f).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.FloatValue.Bigfloat v) {
        return hydra.ext.java.utils.Utils.javaConstructorCall(
          hydra.ext.java.utils.Utils.javaConstructorName(
            new hydra.ext.java.syntax.Identifier("java.math.BigDecimal"),
            (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
          java.util.List.of(hydra.ext.java.coder.Coder.encodeLiteral(new hydra.core.Literal.String_(hydra.lib.literals.ShowBigfloat.apply((v).value)))),
          (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing()));
      }
      
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.FloatValue.Float32 v) {
        return hydra.ext.java.coder.Coder.encodeLiteral_primCast(
          new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.FloatingPoint(new hydra.ext.java.syntax.FloatingPointType.Float_())),
          hydra.ext.java.coder.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.FloatingPoint(new hydra.ext.java.syntax.FloatingPointLiteral(hydra.lib.literals.Float32ToBigfloat.apply((v).value)))));
      }
      
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.FloatValue.Float64 v) {
        return hydra.ext.java.coder.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.FloatingPoint(new hydra.ext.java.syntax.FloatingPointLiteral(hydra.lib.literals.Float64ToBigfloat.apply((v).value))));
      }
    });
  }
  
  static hydra.ext.java.syntax.Expression encodeLiteral_encodeInteger(hydra.core.IntegerValue i) {
    return (i).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Bigint v) {
        return hydra.ext.java.utils.Utils.javaConstructorCall(
          hydra.ext.java.utils.Utils.javaConstructorName(
            new hydra.ext.java.syntax.Identifier("java.math.BigInteger"),
            (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
          java.util.List.of(hydra.ext.java.coder.Coder.encodeLiteral(new hydra.core.Literal.String_(hydra.lib.literals.ShowBigint.apply((v).value)))),
          (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing()));
      }
      
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Int8 v) {
        return hydra.ext.java.coder.Coder.encodeLiteral_primCast(
          new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.Integral(new hydra.ext.java.syntax.IntegralType.Byte_())),
          hydra.ext.java.coder.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.Integer_(new hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.Int8ToBigint.apply((v).value)))));
      }
      
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Int16 v) {
        return hydra.ext.java.coder.Coder.encodeLiteral_primCast(
          new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.Integral(new hydra.ext.java.syntax.IntegralType.Short_())),
          hydra.ext.java.coder.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.Integer_(new hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.Int16ToBigint.apply((v).value)))));
      }
      
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Int32 v) {
        return hydra.ext.java.coder.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.Integer_(new hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.Int32ToBigint.apply((v).value))));
      }
      
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Int64 v) {
        return hydra.ext.java.coder.Coder.encodeLiteral_primCast(
          new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.Integral(new hydra.ext.java.syntax.IntegralType.Long_())),
          hydra.ext.java.coder.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.Integer_(new hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.Int64ToBigint.apply((v).value)))));
      }
      
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Uint8 v) {
        return hydra.ext.java.coder.Coder.encodeLiteral_primCast(
          new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.Integral(new hydra.ext.java.syntax.IntegralType.Short_())),
          hydra.ext.java.coder.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.Integer_(new hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.Uint8ToBigint.apply((v).value)))));
      }
      
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Uint16 v) {
        return hydra.ext.java.coder.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.Character_((v).value));
      }
      
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Uint32 v) {
        return hydra.ext.java.coder.Coder.encodeLiteral_primCast(
          new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.Integral(new hydra.ext.java.syntax.IntegralType.Long_())),
          hydra.ext.java.coder.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.Integer_(new hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.Uint32ToBigint.apply((v).value)))));
      }
      
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Uint64 v) {
        return hydra.ext.java.utils.Utils.javaConstructorCall(
          hydra.ext.java.utils.Utils.javaConstructorName(
            new hydra.ext.java.syntax.Identifier("java.math.BigInteger"),
            (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
          java.util.List.of(hydra.ext.java.coder.Coder.encodeLiteral(new hydra.core.Literal.String_(hydra.lib.literals.ShowBigint.apply(hydra.lib.literals.Uint64ToBigint.apply((v).value))))),
          (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing()));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.FormalParameter> fieldTypeToFormalParam(hydra.ext.java.helpers.Aliases aliases, hydra.core.FieldType ft) {
    return hydra.lib.flows.Bind.apply(
      hydra.ext.java.coder.Coder.encodeType(
        aliases,
        (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
        (ft).type),
      (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.FormalParameter>>) (jt -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaTypeToJavaFormalParameter(
        jt,
        (ft).name))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> applyCastIfSafe(hydra.ext.java.helpers.Aliases aliases, hydra.core.Type castType, hydra.ext.java.syntax.Expression expr) {
    java.util.Set<hydra.core.Name> castVars = hydra.ext.java.coder.Coder.collectTypeVars(castType);
    java.util.Set<hydra.core.Name> inScope = (aliases).inScopeTypeParams;
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> javaTypeVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.logic.Or.apply(
        hydra.lib.sets.Member.apply(
          v,
          inScope),
        hydra.ext.java.coder.Coder.isLambdaBoundVariable(v))),
      hydra.lib.sets.ToList.apply(castVars))));
    java.util.Set<hydra.core.Name> trusted = (aliases).trustedTypeVars;
    hydra.util.Lazy<Boolean> isSafe = new hydra.util.Lazy<>(() -> hydra.lib.logic.Or.apply(
      hydra.lib.sets.Null.apply(trusted),
      hydra.lib.logic.Or.apply(
        hydra.lib.sets.Null.apply(javaTypeVars.get()),
        hydra.lib.sets.Null.apply(hydra.lib.sets.Difference.apply(
          javaTypeVars.get(),
          trusted)))));
    return hydra.lib.logic.IfElse.lazy(
      isSafe.get(),
      () -> hydra.lib.flows.Bind.apply(
        hydra.ext.java.coder.Coder.encodeType(
          aliases,
          (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
          castType),
        (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jtype -> hydra.lib.flows.Bind.apply(
          hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jtype),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (rt -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.Utils.javaCastExpression(
            rt,
            hydra.ext.java.utils.Utils.javaExpressionToJavaUnaryExpression(expr)))))))),
      () -> hydra.lib.flows.Pure.apply(expr));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> encodeVariable(hydra.ext.java.helpers.JavaEnvironment env, hydra.core.Name name) {
    hydra.ext.java.helpers.Aliases aliases = (env).aliases;
    hydra.ext.java.syntax.Identifier jid = hydra.ext.java.utils.Utils.javaIdentifier((name).value);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        name,
        (aliases).branchVars),
      () -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaFieldAccessToJavaExpression(new hydra.ext.java.syntax.FieldAccess(new hydra.ext.java.syntax.FieldAccess_Qualifier.Primary(hydra.ext.java.utils.Utils.javaExpressionToJavaPrimary(hydra.ext.java.utils.Utils.javaIdentifierToJavaExpression(jid))), hydra.ext.java.utils.Utils.javaIdentifier(hydra.ext.java.names.Names.valueFieldName())))),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.logic.And.apply(
          hydra.lib.equality.Equal.apply(
            name,
            new hydra.core.Name(hydra.lib.strings.Cat.apply(java.util.List.of(
              hydra.ext.java.names.Names.instanceName(),
              "_",
              hydra.ext.java.names.Names.valueFieldName())))),
          hydra.ext.java.coder.Coder.isRecursiveVariable(
            aliases,
            name)),
        () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
          hydra.ext.java.syntax.Expression instanceExpr = hydra.ext.java.utils.Utils.javaIdentifierToJavaExpression(hydra.ext.java.utils.Utils.javaIdentifier(hydra.ext.java.names.Names.instanceName()));
          return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaFieldAccessToJavaExpression(new hydra.ext.java.syntax.FieldAccess(new hydra.ext.java.syntax.FieldAccess_Qualifier.Primary(hydra.ext.java.utils.Utils.javaExpressionToJavaPrimary(instanceExpr)), hydra.ext.java.utils.Utils.javaIdentifier(hydra.ext.java.names.Names.valueFieldName()))));
        })).get(),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.And.apply(
            hydra.ext.java.coder.Coder.isRecursiveVariable(
              aliases,
              name),
            hydra.lib.logic.Not.apply(hydra.ext.java.coder.Coder.isLambdaBoundIn(
              name,
              (aliases).lambdaVars))),
          () -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocation(
            hydra.util.Maybe.just((hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>) ((hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>) (hydra.util.Either.<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>left(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), jid))))),
            new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.getMethodName()),
            (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of())))),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.logic.And.apply(
              hydra.lib.sets.Member.apply(
                name,
                (aliases).thunkedVars),
              hydra.lib.logic.Not.apply(hydra.ext.java.coder.Coder.isLambdaBoundIn(
                name,
                (aliases).lambdaVars))),
            () -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocation(
              hydra.util.Maybe.just((hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>) ((hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>) (hydra.util.Either.<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>left(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), jid))))),
              new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.getMethodName()),
              (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of())))),
            () -> hydra.lib.logic.IfElse.lazy(
              hydra.ext.java.coder.Coder.isLambdaBoundIn(
                name,
                (aliases).lambdaVars),
              () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
                hydra.core.Name actualName = hydra.ext.java.coder.Coder.findMatchingLambdaVar(
                  name,
                  (aliases).lambdaVars);
                return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaIdentifierToJavaExpression(hydra.ext.java.utils.Utils.variableToJavaIdentifier(actualName)));
              })).get(),
              () -> hydra.lib.flows.Bind.apply(
                hydra.ext.java.coder.Coder.classifyDataReference(name),
                (java.util.function.Function<hydra.ext.java.helpers.JavaSymbolClass, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (cls -> (cls).accept(new hydra.ext.java.helpers.JavaSymbolClass.PartialVisitor<>() {
                  @Override
                  public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.ext.java.helpers.JavaSymbolClass.HoistedLambda arity) {
                    return hydra.ext.java.coder.Coder.encodeVariable_hoistedLambdaCase(
                      aliases,
                      name,
                      (arity).value);
                  }
                  
                  @Override
                  public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.ext.java.helpers.JavaSymbolClass.LocalVariable ignored) {
                    return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaIdentifierToJavaExpression(hydra.ext.java.coder.Coder.elementJavaIdentifier(
                      false,
                      false,
                      aliases,
                      name)));
                  }
                  
                  @Override
                  public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.ext.java.helpers.JavaSymbolClass.Constant ignored) {
                    return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaIdentifierToJavaExpression(hydra.ext.java.coder.Coder.elementJavaIdentifier(
                      false,
                      false,
                      aliases,
                      name)));
                  }
                  
                  @Override
                  public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.ext.java.helpers.JavaSymbolClass.NullaryFunction ignored) {
                    return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocation(
                      (hydra.util.Maybe<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>>) (hydra.util.Maybe.<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>>nothing()),
                      hydra.ext.java.coder.Coder.elementJavaIdentifier(
                        false,
                        false,
                        aliases,
                        name),
                      (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of()))));
                  }
                  
                  @Override
                  public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.ext.java.helpers.JavaSymbolClass.UnaryFunction ignored) {
                    return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaIdentifierToJavaExpression(hydra.ext.java.coder.Coder.elementJavaIdentifier(
                      false,
                      true,
                      aliases,
                      name)));
                  }
                }))))))));
  }
  
  static hydra.ext.java.syntax.Expression encodeVariable_buildCurried(java.util.List<hydra.core.Name> params, hydra.ext.java.syntax.Expression inner) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(params),
      () -> inner,
      () -> hydra.ext.java.utils.Utils.javaLambda(
        hydra.lib.lists.Head.apply(params),
        hydra.ext.java.coder.Coder.encodeVariable_buildCurried(
          hydra.lib.lists.Tail.apply(params),
          inner)));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> encodeVariable_hoistedLambdaCase(hydra.ext.java.helpers.Aliases aliases, hydra.core.Name name, Integer arity) {
    hydra.util.Lazy<java.util.List<hydra.core.Name>> paramNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<Integer, hydra.core.Name>) (i -> new hydra.core.Name(hydra.lib.strings.Cat2.apply(
        "p",
        hydra.lib.literals.ShowInt32.apply(i)))),
      hydra.lib.math.Range.apply(
        0,
        hydra.lib.math.Sub.apply(
          arity,
          1))));
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.Expression>> paramExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.Expression>) (pn -> hydra.ext.java.utils.Utils.javaIdentifierToJavaExpression(hydra.ext.java.utils.Utils.variableToJavaIdentifier(pn))),
      paramNames.get()));
    hydra.util.Lazy<hydra.ext.java.syntax.Expression> call = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocation(
      (hydra.util.Maybe<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>>) (hydra.util.Maybe.<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>>nothing()),
      hydra.ext.java.coder.Coder.elementJavaIdentifier(
        false,
        false,
        aliases,
        name),
      paramExprs.get())));
    hydra.ext.java.syntax.Expression lam = hydra.ext.java.coder.Coder.encodeVariable_buildCurried(
      paramNames.get(),
      call.get());
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.dereferenceElement(name),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (mel -> hydra.lib.maybes.Cases.apply(
        mel,
        hydra.lib.flows.Pure.apply(lam),
        (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (el -> hydra.lib.maybes.Cases.apply(
          (el).type,
          hydra.lib.flows.Pure.apply(lam),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (ts -> {
            hydra.core.Type typ = (ts).type;
            return hydra.lib.flows.Bind.apply(
              hydra.ext.java.coder.Coder.encodeType(
                aliases,
                (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                typ),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jtype -> hydra.lib.flows.Bind.apply(
                hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jtype),
                (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (rt -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.Utils.javaCastExpression(
                  rt,
                  hydra.ext.java.utils.Utils.javaExpressionToJavaUnaryExpression(lam))))))));
          }))))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> encodeNullaryConstant(hydra.ext.java.helpers.JavaEnvironment env, hydra.core.Type typ, hydra.core.Function fun) {
    hydra.ext.java.helpers.Aliases aliases = (env).aliases;
    return (fun).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Function instance) {
        return hydra.monads.Monads.unexpected(
          "nullary function",
          hydra.show.core.Core.function(fun));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Function.Primitive name) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.java.coder.Coder.encodeNullaryConstant_typeArgsFromReturnType(
            aliases,
            typ),
          (java.util.function.Function<java.util.List<hydra.ext.java.syntax.TypeArgument>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (targs -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply(targs),
            () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
              hydra.ext.java.syntax.MethodInvocation_Header header = new hydra.ext.java.syntax.MethodInvocation_Header.Simple(new hydra.ext.java.syntax.MethodName(hydra.ext.java.coder.Coder.elementJavaIdentifier(
                true,
                false,
                aliases,
                (name).value)));
              return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(header, (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of()))));
            })).get(),
            () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
              String fullName = (hydra.ext.java.coder.Coder.elementJavaIdentifier(
                true,
                false,
                aliases,
                (name).value)).value;
              return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
                java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
                  ".",
                  fullName);
                return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
                  hydra.util.Lazy<hydra.ext.java.syntax.Identifier> className = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Intercalate.apply(
                    ".",
                    hydra.lib.lists.Init.apply(parts))));
                  return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
                    hydra.util.Lazy<hydra.ext.java.syntax.Identifier> methodName = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.Identifier(hydra.lib.lists.Last.apply(parts)));
                    return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocationStaticWithTypeArgs(
                      className.get(),
                      methodName.get(),
                      targs,
                      (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of()))));
                  })).get();
                })).get();
              })).get();
            })).get())));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>> encodeNullaryConstant_typeArgsFromReturnType(hydra.ext.java.helpers.Aliases aliases, hydra.core.Type t) {
    return (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>> otherwise(hydra.core.Type instance) {
        return hydra.lib.flows.Pure.apply((java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>> visit(hydra.core.Type.Set st) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.java.coder.Coder.encodeType(
            aliases,
            (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
            (st).value),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>>>) (jst -> hydra.lib.flows.Bind.apply(
            hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jst),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>>>) (rt -> hydra.lib.flows.Pure.apply(java.util.List.of(new hydra.ext.java.syntax.TypeArgument.Reference(rt)))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>> visit(hydra.core.Type.List lt_) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.java.coder.Coder.encodeType(
            aliases,
            (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
            (lt_).value),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>>>) (jlt -> hydra.lib.flows.Bind.apply(
            hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jlt),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>>>) (rt -> hydra.lib.flows.Pure.apply(java.util.List.of(new hydra.ext.java.syntax.TypeArgument.Reference(rt)))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>> visit(hydra.core.Type.Maybe mt) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.java.coder.Coder.encodeType(
            aliases,
            (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
            (mt).value),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>>>) (jmt -> hydra.lib.flows.Bind.apply(
            hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jmt),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>>>) (rt -> hydra.lib.flows.Pure.apply(java.util.List.of(new hydra.ext.java.syntax.TypeArgument.Reference(rt)))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>> visit(hydra.core.Type.Map mp) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.java.coder.Coder.encodeType(
            aliases,
            (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
            ((mp).value).keys),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>>>) (jkt -> hydra.lib.flows.Bind.apply(
            hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jkt),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>>>) (rk -> hydra.lib.flows.Bind.apply(
              hydra.ext.java.coder.Coder.encodeType(
                aliases,
                (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                ((mp).value).values),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>>>) (jvt -> hydra.lib.flows.Bind.apply(
                hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jvt),
                (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>>>) (rv -> hydra.lib.flows.Pure.apply(java.util.List.of(
                  new hydra.ext.java.syntax.TypeArgument.Reference(rk),
                  new hydra.ext.java.syntax.TypeArgument.Reference(rv)))))))))));
      }
    });
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.Name> buildTypeVarSubst(java.util.Set<hydra.core.Name> schemeVarSet, hydra.core.Type freshTyp, hydra.core.Type canonTyp) {
    return hydra.ext.java.coder.Coder.buildTypeVarSubst_go(
      schemeVarSet,
      hydra.rewriting.Rewriting.deannotateType(freshTyp),
      hydra.rewriting.Rewriting.deannotateType(canonTyp));
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.Name> buildTypeVarSubst_go(java.util.Set<hydra.core.Name> svs, hydra.core.Type ft, hydra.core.Type ct) {
    java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, java.util.Map<hydra.core.Name, hydra.core.Name>>> goSub = (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, java.util.Map<hydra.core.Name, hydra.core.Name>>>) (a -> (java.util.function.Function<hydra.core.Type, java.util.Map<hydra.core.Name, hydra.core.Name>>) (b -> hydra.ext.java.coder.Coder.buildTypeVarSubst_go(
      svs,
      hydra.rewriting.Rewriting.deannotateType(a),
      hydra.rewriting.Rewriting.deannotateType(b))));
    return (ft).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Forall cfa) {
            return hydra.ext.java.coder.Coder.buildTypeVarSubst_go(
              svs,
              ft,
              hydra.rewriting.Rewriting.deannotateType(((cfa).value).body));
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Variable fn) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Variable cn) {
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.And.apply(
                hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
                  (fn).value,
                  (cn).value)),
                hydra.lib.sets.Member.apply(
                  (cn).value,
                  svs)),
              () -> hydra.lib.maps.Singleton.apply(
                (fn).value,
                (cn).value),
              () -> (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())));
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Function fft) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Function cft) {
            return hydra.lib.maps.Union.apply(
              ((goSub).apply(((fft).value).domain)).apply(((cft).value).domain),
              ((goSub).apply(((fft).value).codomain)).apply(((cft).value).codomain));
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Application fat) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Application cat) {
            return hydra.lib.maps.Union.apply(
              ((goSub).apply(((fat).value).function)).apply(((cat).value).function),
              ((goSub).apply(((fat).value).argument)).apply(((cat).value).argument));
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.List fl) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.List cl) {
            return ((goSub).apply((fl).value)).apply((cl).value);
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Set fs) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Set cs) {
            return ((goSub).apply((fs).value)).apply((cs).value);
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Maybe fm) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Maybe cm) {
            return ((goSub).apply((fm).value)).apply((cm).value);
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Map fmt) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Map cmt) {
            return hydra.lib.maps.Union.apply(
              ((goSub).apply(((fmt).value).keys)).apply(((cmt).value).keys),
              ((goSub).apply(((fmt).value).values)).apply(((cmt).value).values));
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Pair fpt) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Pair cpt) {
            return hydra.lib.maps.Union.apply(
              ((goSub).apply(((fpt).value).first)).apply(((cpt).value).first),
              ((goSub).apply(((fpt).value).second)).apply(((cpt).value).second));
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Either fet) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Either cet) {
            return hydra.lib.maps.Union.apply(
              ((goSub).apply(((fet).value).left)).apply(((cet).value).left),
              ((goSub).apply(((fet).value).right)).apply(((cet).value).right));
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Forall ffa) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return hydra.ext.java.coder.Coder.buildTypeVarSubst_go(
              svs,
              hydra.rewriting.Rewriting.deannotateType(((ffa).value).body),
              ct);
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Forall cfa) {
            return ((goSub).apply(((ffa).value).body)).apply(((cfa).value).body);
          }
        });
      }
    });
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.Type> buildTypeSubst(java.util.Set<hydra.core.Name> schemeVarSet, hydra.core.Type schemeType, hydra.core.Type actualType) {
    return hydra.ext.java.coder.Coder.buildTypeSubst_go(
      schemeVarSet,
      hydra.rewriting.Rewriting.deannotateType(schemeType),
      hydra.rewriting.Rewriting.deannotateType(actualType));
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.Type> buildTypeSubst_go(java.util.Set<hydra.core.Name> svs, hydra.core.Type st, hydra.core.Type at) {
    java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, java.util.Map<hydra.core.Name, hydra.core.Type>>> goSub = (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, java.util.Map<hydra.core.Name, hydra.core.Type>>>) (a -> (java.util.function.Function<hydra.core.Type, java.util.Map<hydra.core.Name, hydra.core.Type>>) (b -> hydra.ext.java.coder.Coder.buildTypeSubst_go(
      svs,
      hydra.rewriting.Rewriting.deannotateType(a),
      hydra.rewriting.Rewriting.deannotateType(b))));
    return (st).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Variable v) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.sets.Member.apply(
            (v).value,
            svs),
          () -> hydra.lib.maps.Singleton.apply(
            (v).value,
            at),
          () -> (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())));
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Function sft) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Function aft) {
            return hydra.lib.maps.Union.apply(
              ((goSub).apply(((sft).value).domain)).apply(((aft).value).domain),
              ((goSub).apply(((sft).value).codomain)).apply(((aft).value).codomain));
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Application sat) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Application aat) {
            return hydra.lib.maps.Union.apply(
              ((goSub).apply(((sat).value).function)).apply(((aat).value).function),
              ((goSub).apply(((sat).value).argument)).apply(((aat).value).argument));
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.List sl) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.List al) {
            return ((goSub).apply((sl).value)).apply((al).value);
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Set ss) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Set as_) {
            return ((goSub).apply((ss).value)).apply((as_).value);
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Maybe sm) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Maybe am) {
            return ((goSub).apply((sm).value)).apply((am).value);
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Map smt) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Map amt) {
            return hydra.lib.maps.Union.apply(
              ((goSub).apply(((smt).value).keys)).apply(((amt).value).keys),
              ((goSub).apply(((smt).value).values)).apply(((amt).value).values));
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Pair spt) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Pair apt) {
            return hydra.lib.maps.Union.apply(
              ((goSub).apply(((spt).value).first)).apply(((apt).value).first),
              ((goSub).apply(((spt).value).second)).apply(((apt).value).second));
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Either set_) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Either aet) {
            return hydra.lib.maps.Union.apply(
              ((goSub).apply(((set_).value).left)).apply(((aet).value).left),
              ((goSub).apply(((set_).value).right)).apply(((aet).value).right));
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Forall sfa) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return ((goSub).apply(((sfa).value).body)).apply(at);
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Forall afa) {
            return ((goSub).apply(((sfa).value).body)).apply(((afa).value).body);
          }
        });
      }
    });
  }
  
  static hydra.typing.TypeContext javaEnvGetTC(hydra.ext.java.helpers.JavaEnvironment env) {
    return (env).typeContext;
  }
  
  static hydra.ext.java.helpers.JavaEnvironment javaEnvSetTC(hydra.typing.TypeContext tc, hydra.ext.java.helpers.JavaEnvironment env) {
    return new hydra.ext.java.helpers.JavaEnvironment((env).aliases, tc);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>> analyzeJavaFunction(hydra.ext.java.helpers.JavaEnvironment v1, hydra.core.Term v2) {
    return hydra.coderUtils.CoderUtils.analyzeFunctionTerm(
      hydra.ext.java.coder.Coder::javaEnvGetTC,
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.ext.java.helpers.JavaEnvironment, hydra.ext.java.helpers.JavaEnvironment>>) (p0 -> p1 -> hydra.ext.java.coder.Coder.javaEnvSetTC(
        p0,
        p1)),
      v1,
      v2);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>> analyzeJavaFunctionNoInfer(hydra.ext.java.helpers.JavaEnvironment v1, hydra.core.Term v2) {
    return hydra.coderUtils.CoderUtils.analyzeFunctionTermNoInfer(
      hydra.ext.java.coder.Coder::javaEnvGetTC,
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.ext.java.helpers.JavaEnvironment, hydra.ext.java.helpers.JavaEnvironment>>) (p0 -> p1 -> hydra.ext.java.coder.Coder.javaEnvSetTC(
        p0,
        p1)),
      v1,
      v2);
  }
  
  static <T0> T0 withLambda(hydra.ext.java.helpers.JavaEnvironment env, hydra.core.Lambda lam, java.util.function.Function<hydra.ext.java.helpers.JavaEnvironment, T0> k) {
    return hydra.schemas.Schemas.withLambdaContext(
      hydra.ext.java.coder.Coder::javaEnvGetTC,
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.ext.java.helpers.JavaEnvironment, hydra.ext.java.helpers.JavaEnvironment>>) (p0 -> p1 -> hydra.ext.java.coder.Coder.javaEnvSetTC(
        p0,
        p1)),
      env,
      lam,
      (java.util.function.Function<hydra.ext.java.helpers.JavaEnvironment, T0>) (env1 -> {
        hydra.ext.java.helpers.Aliases aliases = (env1).aliases;
        hydra.util.Lazy<hydra.ext.java.helpers.Aliases> aliases2 = new hydra.util.Lazy<>(() -> new hydra.ext.java.helpers.Aliases((aliases).currentNamespace, (aliases).packages, (aliases).branchVars, (aliases).recursiveVars, (aliases).inScopeTypeParams, (aliases).polymorphicLocals, (aliases).inScopeJavaVars, (aliases).varRenames, hydra.lib.sets.Insert.apply(
          (lam).parameter,
          (aliases).lambdaVars), (aliases).typeVarSubst, (aliases).trustedTypeVars, (aliases).methodCodomain, (aliases).thunkedVars));
        hydra.ext.java.helpers.JavaEnvironment env2 = new hydra.ext.java.helpers.JavaEnvironment(aliases2.get(), (env1).typeContext);
        return (k).apply(env2);
      }));
  }
  
  static <T0> T0 withTypeLambda(hydra.ext.java.helpers.JavaEnvironment v1, hydra.core.TypeLambda v2, java.util.function.Function<hydra.ext.java.helpers.JavaEnvironment, T0> v3) {
    return hydra.schemas.Schemas.withTypeLambdaContext(
      hydra.ext.java.coder.Coder::javaEnvGetTC,
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.ext.java.helpers.JavaEnvironment, hydra.ext.java.helpers.JavaEnvironment>>) (p0 -> p1 -> hydra.ext.java.coder.Coder.javaEnvSetTC(
        p0,
        p1)),
      v1,
      v2,
      v3);
  }
  
  static hydra.core.Term propagateType(hydra.core.Type typ, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Term, hydra.core.Term> setTypeAnn = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> hydra.annotations.Annotations.setTermAnnotation(
      hydra.constants.Constants.key_type(),
      hydra.util.Maybe.just(hydra.encode.core.Core.type(typ)),
      t));
    return (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (setTypeAnn).apply(term);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return (setTypeAnn).apply(term);
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda lam) {
            hydra.core.Term annotated = (setTypeAnn).apply(term);
            return (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
              @Override
              public hydra.core.Term otherwise(hydra.core.Type instance) {
                return annotated;
              }
              
              @Override
              public hydra.core.Term visit(hydra.core.Type.Function ft) {
                return hydra.ext.java.coder.Coder.propagateType_propagateIntoLambda(
                  ((ft).value).codomain,
                  annotated);
              }
            });
          }
        });
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        return (setTypeAnn).apply(hydra.ext.java.coder.Coder.propagateType_rebuildLet(
          term,
          ((lt).value).bindings,
          hydra.ext.java.coder.Coder.propagateType(
            typ,
            ((lt).value).body)));
      }
    });
  }
  
  static hydra.core.Term propagateType_propagateIntoLambda(hydra.core.Type cod, hydra.core.Term t) {
    return (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.ext.java.coder.Coder.propagateType_propagateIntoLambda(
          cod,
          ((at).value).body), ((at).value).annotation));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return t;
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda lam) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(((lam).value).parameter, ((lam).value).domain, hydra.ext.java.coder.Coder.propagateType(
              cod,
              ((lam).value).body))));
          }
        });
      }
    });
  }
  
  static hydra.core.Term propagateType_rebuildLet(hydra.core.Term t, java.util.List<hydra.core.Binding> bindings, hydra.core.Term newBody) {
    return (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.ext.java.coder.Coder.propagateType_rebuildLet(
          ((at).value).body,
          bindings,
          newBody), ((at).value).annotation));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Let _lt) {
        return new hydra.core.Term.Let(new hydra.core.Let(bindings, newBody));
      }
    });
  }
  
  static java.util.List<hydra.core.Binding> flattenBindings(java.util.List<hydra.core.Binding> bindings) {
    return hydra.lib.lists.Bind.apply(
      bindings,
      (java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>>) (b -> (hydra.rewriting.Rewriting.deannotateTerm((b).term)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public java.util.List<hydra.core.Binding> otherwise(hydra.core.Term instance) {
          return java.util.List.of(b);
        }
        
        @Override
        public java.util.List<hydra.core.Binding> visit(hydra.core.Term.Let lt) {
          return hydra.lib.lists.Concat2.apply(
            hydra.ext.java.coder.Coder.flattenBindings(((lt).value).bindings),
            java.util.List.of(new hydra.core.Binding((b).name, ((lt).value).body, (b).type)));
        }
      })));
  }
  
  static java.util.List<hydra.core.Binding> dedupBindings(java.util.Set<hydra.core.Name> inScope, java.util.List<hydra.core.Binding> bs) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(bs),
      () -> (java.util.List<hydra.core.Binding>) (java.util.List.<hydra.core.Binding>of()),
      () -> ((java.util.function.Supplier<java.util.List<hydra.core.Binding>>) (() -> {
        hydra.util.Lazy<hydra.core.Binding> b = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(bs));
        return ((java.util.function.Supplier<java.util.List<hydra.core.Binding>>) (() -> {
          hydra.util.Lazy<java.util.List<hydra.core.Binding>> rest = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(bs));
          return ((java.util.function.Supplier<java.util.List<hydra.core.Binding>>) (() -> {
            hydra.core.Name name = (b.get()).name;
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.sets.Member.apply(
                name,
                inScope),
              () -> ((java.util.function.Supplier<java.util.List<hydra.core.Binding>>) (() -> {
                hydra.core.Name newName = hydra.ext.java.coder.Coder.freshJavaName(
                  name,
                  inScope);
                return ((java.util.function.Supplier<java.util.List<hydra.core.Binding>>) (() -> {
                  hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Name>> subst = new hydra.util.Lazy<>(() -> hydra.lib.maps.Singleton.apply(
                    name,
                    newName));
                  return ((java.util.function.Supplier<java.util.List<hydra.core.Binding>>) (() -> {
                    hydra.util.Lazy<java.util.List<hydra.core.Binding>> rest2 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                      (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b2 -> new hydra.core.Binding((b2).name, hydra.rewriting.Rewriting.substituteVariables(
                        subst.get(),
                        (b2).term), (b2).type)),
                      rest.get()));
                    return hydra.lib.lists.Cons.apply(
                      new hydra.core.Binding(newName, (b.get()).term, (b.get()).type),
                      hydra.ext.java.coder.Coder.dedupBindings(
                        hydra.lib.sets.Insert.apply(
                          newName,
                          inScope),
                        rest2.get()));
                  })).get();
                })).get();
              })).get(),
              () -> hydra.lib.lists.Cons.apply(
                b.get(),
                hydra.ext.java.coder.Coder.dedupBindings(
                  hydra.lib.sets.Insert.apply(
                    name,
                    inScope),
                  rest.get())));
          })).get();
        })).get();
      })).get());
  }
  
  static hydra.core.Name freshJavaName(hydra.core.Name base, java.util.Set<hydra.core.Name> avoid) {
    return hydra.ext.java.coder.Coder.freshJavaName_go(
      base,
      avoid,
      2);
  }
  
  static hydra.core.Name freshJavaName_go(hydra.core.Name base, java.util.Set<hydra.core.Name> avoid, Integer i) {
    hydra.core.Name candidate = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
      (base).value,
      hydra.lib.literals.ShowInt32.apply(i)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        candidate,
        avoid),
      () -> hydra.ext.java.coder.Coder.freshJavaName_go(
        base,
        avoid,
        hydra.lib.math.Add.apply(
          i,
          1)),
      () -> candidate);
  }
  
  static Boolean needsThunking(hydra.core.Term t) {
    return (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (b -> (java.util.function.Function<hydra.core.Term, Boolean>) (st -> hydra.lib.logic.Or.apply(
            b,
            hydra.ext.java.coder.Coder.needsThunking(st)))),
          false,
          hydra.rewriting.Rewriting.subterms(t));
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Let _lt) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.TypeApplication _ta) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.TypeLambda _tl) {
        return true;
      }
    });
  }
  
  static Boolean bindingIsFunctionType(hydra.core.Binding b) {
    return hydra.lib.maybes.Maybe.apply(
      (hydra.rewriting.Rewriting.deannotateTerm((b).term)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public Boolean otherwise(hydra.core.Term instance) {
          return false;
        }
        
        @Override
        public Boolean visit(hydra.core.Term.Function _f) {
          return true;
        }
      }),
      (java.util.function.Function<hydra.core.TypeScheme, Boolean>) (ts -> (hydra.rewriting.Rewriting.deannotateType((ts).type)).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public Boolean otherwise(hydra.core.Type instance) {
          return false;
        }
        
        @Override
        public Boolean visit(hydra.core.Type.Function _ft) {
          return true;
        }
        
        @Override
        public Boolean visit(hydra.core.Type.Forall fa) {
          return (hydra.rewriting.Rewriting.deannotateType(((fa).value).body)).accept(new hydra.core.Type.PartialVisitor<>() {
            @Override
            public Boolean otherwise(hydra.core.Type instance) {
              return false;
            }
            
            @Override
            public Boolean visit(hydra.core.Type.Function _ft2) {
              return true;
            }
          });
        }
      })),
      (b).type);
  }
  
  static hydra.util.Maybe<hydra.core.Type> decodeTypeFromTerm(hydra.core.Term term) {
    return (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
        return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
      }
      
      @Override
      public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Union inj) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            ((inj).value).typeName,
            new hydra.core.Name("hydra.core.Type")),
          () -> ((java.util.function.Supplier<hydra.util.Maybe<hydra.core.Type>>) (() -> {
            String fname = ((((inj).value).field).name).value;
            return ((java.util.function.Supplier<hydra.util.Maybe<hydra.core.Type>>) (() -> {
              hydra.core.Term fterm = (((inj).value).field).term;
              return hydra.lib.logic.IfElse.lazy(
                hydra.lib.equality.Equal.apply(
                  fname,
                  "variable"),
                () -> (fterm).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
                    return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
                  }
                  
                  @Override
                  public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Wrap wt) {
                    return (((wt).value).body).accept(new hydra.core.Term.PartialVisitor<>() {
                      @Override
                      public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
                        return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
                      }
                      
                      @Override
                      public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Literal lit) {
                        return ((lit).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                          @Override
                          public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Literal instance) {
                            return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
                          }
                          
                          @Override
                          public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Literal.String_ s) {
                            return hydra.util.Maybe.just(new hydra.core.Type.Variable(new hydra.core.Name((s).value)));
                          }
                        });
                      }
                    });
                  }
                }),
                () -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.equality.Equal.apply(
                    fname,
                    "annotated"),
                  () -> (fterm).accept(new hydra.core.Term.PartialVisitor<>() {
                    @Override
                    public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
                      return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
                    }
                    
                    @Override
                    public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Record rec) {
                      return hydra.lib.maybes.Bind.apply(
                        hydra.lib.lists.SafeHead.apply(hydra.lib.lists.Filter.apply(
                          (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
                            (f).name,
                            new hydra.core.Name("body"))),
                          ((rec).value).fields)),
                        (java.util.function.Function<hydra.core.Field, hydra.util.Maybe<hydra.core.Type>>) (bodyField -> hydra.ext.java.coder.Coder.decodeTypeFromTerm((bodyField).term)));
                    }
                  }),
                  () -> hydra.lib.logic.IfElse.lazy(
                    hydra.lib.equality.Equal.apply(
                      fname,
                      "application"),
                    () -> (fterm).accept(new hydra.core.Term.PartialVisitor<>() {
                      @Override
                      public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
                        return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
                      }
                      
                      @Override
                      public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Record rec) {
                        return hydra.lib.maybes.Bind.apply(
                          hydra.lib.lists.SafeHead.apply(hydra.lib.lists.Filter.apply(
                            (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
                              (f).name,
                              new hydra.core.Name("function"))),
                            ((rec).value).fields)),
                          (java.util.function.Function<hydra.core.Field, hydra.util.Maybe<hydra.core.Type>>) (funcField -> hydra.lib.maybes.Bind.apply(
                            hydra.ext.java.coder.Coder.decodeTypeFromTerm((funcField).term),
                            (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (func -> hydra.lib.maybes.Bind.apply(
                              hydra.lib.lists.SafeHead.apply(hydra.lib.lists.Filter.apply(
                                (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
                                  (f).name,
                                  new hydra.core.Name("argument"))),
                                ((rec).value).fields)),
                              (java.util.function.Function<hydra.core.Field, hydra.util.Maybe<hydra.core.Type>>) (argField -> hydra.lib.maybes.Map.apply(
                                (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (arg -> new hydra.core.Type.Application(new hydra.core.ApplicationType(func, arg))),
                                hydra.ext.java.coder.Coder.decodeTypeFromTerm((argField).term))))))));
                      }
                    }),
                    () -> hydra.lib.logic.IfElse.lazy(
                      hydra.lib.equality.Equal.apply(
                        fname,
                        "function"),
                      () -> (fterm).accept(new hydra.core.Term.PartialVisitor<>() {
                        @Override
                        public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
                          return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
                        }
                        
                        @Override
                        public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Record rec) {
                          return hydra.lib.maybes.Bind.apply(
                            hydra.lib.lists.SafeHead.apply(hydra.lib.lists.Filter.apply(
                              (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
                                (f).name,
                                new hydra.core.Name("domain"))),
                              ((rec).value).fields)),
                            (java.util.function.Function<hydra.core.Field, hydra.util.Maybe<hydra.core.Type>>) (domField -> hydra.lib.maybes.Bind.apply(
                              hydra.ext.java.coder.Coder.decodeTypeFromTerm((domField).term),
                              (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (dom -> hydra.lib.maybes.Bind.apply(
                                hydra.lib.lists.SafeHead.apply(hydra.lib.lists.Filter.apply(
                                  (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
                                    (f).name,
                                    new hydra.core.Name("codomain"))),
                                  ((rec).value).fields)),
                                (java.util.function.Function<hydra.core.Field, hydra.util.Maybe<hydra.core.Type>>) (codField -> hydra.lib.maybes.Map.apply(
                                  (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (cod -> new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod))),
                                  hydra.ext.java.coder.Coder.decodeTypeFromTerm((codField).term))))))));
                        }
                      }),
                      () -> hydra.lib.logic.IfElse.lazy(
                        hydra.lib.equality.Equal.apply(
                          fname,
                          "literal"),
                        () -> (fterm).accept(new hydra.core.Term.PartialVisitor<>() {
                          @Override
                          public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
                            return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
                          }
                          
                          @Override
                          public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Union litInj) {
                            return hydra.lib.logic.IfElse.lazy(
                              hydra.lib.equality.Equal.apply(
                                ((((litInj).value).field).name).value,
                                "string"),
                              () -> hydra.util.Maybe.just(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
                              () -> (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()));
                          }
                        }),
                        () -> (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()))))));
            })).get();
          })).get(),
          () -> (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()));
      }
    });
  }
  
  static hydra.util.Maybe<hydra.core.Type> tryInferFunctionType(hydra.core.Function fun) {
    return (fun).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Function instance) {
        return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
      }
      
      @Override
      public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Function.Lambda lam) {
        return hydra.lib.maybes.Bind.apply(
          ((lam).value).domain,
          (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (dom -> {
            hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> mCod = new hydra.util.Lazy<>(() -> (((lam).value).body).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
                return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
              }
              
              @Override
              public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Annotated at) {
                return hydra.lib.maybes.Bind.apply(
                  hydra.lib.maps.Lookup.apply(
                    hydra.constants.Constants.key_type(),
                    ((at).value).annotation),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Type>>) (typeTerm -> hydra.ext.java.coder.Coder.decodeTypeFromTerm(typeTerm)));
              }
            }));
            return hydra.lib.maybes.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (cod -> new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod))),
              mCod.get());
          }));
      }
    });
  }
  
  static hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Type>> collectTypeApps(hydra.core.Term t, java.util.List<hydra.core.Type> acc) {
    return (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Type>> otherwise(hydra.core.Term instance) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Type>>(hydra.rewriting.Rewriting.deannotateTerm(t), acc)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Type>> visit(hydra.core.Term.TypeApplication ta) {
        return hydra.ext.java.coder.Coder.collectTypeApps(
          ((ta).value).body,
          hydra.lib.lists.Cons.apply(
            ((ta).value).type,
            acc));
      }
    });
  }
  
  static hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Type>> collectTypeApps0(hydra.core.Term t, java.util.List<hydra.core.Type> acc) {
    return (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Type>> otherwise(hydra.core.Term instance) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Type>>(t, acc)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Type>> visit(hydra.core.Term.TypeApplication ta) {
        return hydra.ext.java.coder.Coder.collectTypeApps0(
          ((ta).value).body,
          hydra.lib.lists.Cons.apply(
            ((ta).value).type,
            acc));
      }
    });
  }
  
  static Integer countFunctionParams(hydra.core.Type t) {
    return (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Type instance) {
        return 0;
      }
      
      @Override
      public Integer visit(hydra.core.Type.Function ft) {
        return hydra.lib.math.Add.apply(
          1,
          hydra.ext.java.coder.Coder.countFunctionParams(((ft).value).codomain));
      }
    });
  }
  
  static hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> peelDomainTypes(Integer n, hydra.core.Type t) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Lte.apply(
        n,
        0),
      () -> (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()), t))),
      () -> (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> otherwise(hydra.core.Type instance) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()), t)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Function ft) {
          hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> rest = hydra.ext.java.coder.Coder.peelDomainTypes(
            hydra.lib.math.Sub.apply(
              n,
              1),
            ((ft).value).codomain);
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Cons.apply(
            ((ft).value).domain,
            hydra.lib.pairs.First.apply(rest)), hydra.lib.pairs.Second.apply(rest))));
        }
      }));
  }
  
  static hydra.core.Type unwrapReturnType(hydra.core.Type t) {
    return (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return t;
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Function ft) {
        return hydra.ext.java.coder.Coder.unwrapReturnType(((ft).value).codomain);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Application at) {
        return hydra.ext.java.coder.Coder.unwrapReturnType(((at).value).argument);
      }
    });
  }
  
  static hydra.util.Maybe<hydra.core.Name> findPairFirst(hydra.core.Type t) {
    return (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing());
      }
      
      @Override
      public hydra.util.Maybe<hydra.core.Name> visit(hydra.core.Type.Pair pt) {
        return (hydra.rewriting.Rewriting.deannotateType(((pt).value).first)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing());
          }
          
          @Override
          public hydra.util.Maybe<hydra.core.Name> visit(hydra.core.Type.Variable v) {
            return hydra.util.Maybe.just((v).value);
          }
        });
      }
    });
  }
  
  static java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> extractInOutPair(hydra.core.Type t) {
    return (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Function ft) {
        return (hydra.rewriting.Rewriting.deannotateType(((ft).value).domain)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
            return (java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>of());
          }
          
          @Override
          public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Variable inVar) {
            hydra.core.Type retType = hydra.ext.java.coder.Coder.unwrapReturnType(((ft).value).codomain);
            return (hydra.rewriting.Rewriting.deannotateType(retType)).accept(new hydra.core.Type.PartialVisitor<>() {
              @Override
              public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
                return (java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>of());
              }
              
              @Override
              public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Pair pt) {
                return (hydra.rewriting.Rewriting.deannotateType(((pt).value).first)).accept(new hydra.core.Type.PartialVisitor<>() {
                  @Override
                  public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
                    return (java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>of());
                  }
                  
                  @Override
                  public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Variable outVar) {
                    return java.util.List.of((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>((inVar).value, (outVar).value))));
                  }
                });
              }
            });
          }
        });
      }
    });
  }
  
  static java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> extractDirectReturn(java.util.Set<hydra.core.Name> tparamSet, hydra.core.Type t) {
    return hydra.ext.java.coder.Coder.extractDirectReturn_go(
      tparamSet,
      t);
  }
  
  static java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> extractDirectReturn_go(java.util.Set<hydra.core.Name> tparamSet, hydra.core.Type t) {
    return (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Function ft) {
        hydra.core.Type cod = ((ft).value).codomain;
        hydra.core.Type dom = hydra.rewriting.Rewriting.deannotateType(((ft).value).domain);
        return (dom).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
            return hydra.ext.java.coder.Coder.extractDirectReturn_go(
              tparamSet,
              cod);
          }
          
          @Override
          public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Variable inVar) {
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.sets.Member.apply(
                (inVar).value,
                tparamSet),
              () -> (hydra.rewriting.Rewriting.deannotateType(cod)).accept(new hydra.core.Type.PartialVisitor<>() {
                @Override
                public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
                  return (java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>of());
                }
                
                @Override
                public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Function ft2) {
                  hydra.core.Type midArg = hydra.rewriting.Rewriting.deannotateType(((ft2).value).domain);
                  hydra.core.Type retPart = hydra.rewriting.Rewriting.deannotateType(((ft2).value).codomain);
                  return (midArg).accept(new hydra.core.Type.PartialVisitor<>() {
                    @Override
                    public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
                      return (retPart).accept(new hydra.core.Type.PartialVisitor<>() {
                        @Override
                        public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
                          return (java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>of());
                        }
                        
                        @Override
                        public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Variable outVar) {
                          return hydra.lib.logic.IfElse.lazy(
                            hydra.lib.sets.Member.apply(
                              (outVar).value,
                              tparamSet),
                            () -> java.util.List.of((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>((inVar).value, (outVar).value)))),
                            () -> (java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>of()));
                        }
                      });
                    }
                    
                    @Override
                    public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Variable midVar) {
                      return hydra.lib.logic.IfElse.lazy(
                        hydra.lib.sets.Member.apply(
                          (midVar).value,
                          tparamSet),
                        () -> (java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>of()),
                        () -> (retPart).accept(new hydra.core.Type.PartialVisitor<>() {
                          @Override
                          public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
                            return (java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>of());
                          }
                          
                          @Override
                          public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Variable outVar) {
                            return hydra.lib.logic.IfElse.lazy(
                              hydra.lib.sets.Member.apply(
                                (outVar).value,
                                tparamSet),
                              () -> java.util.List.of((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>((inVar).value, (outVar).value)))),
                              () -> (java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>of()));
                          }
                        }));
                    }
                  });
                }
              }),
              () -> hydra.ext.java.coder.Coder.extractDirectReturn_go(
                tparamSet,
                cod));
          }
        });
      }
    });
  }
  
  static <T0> java.util.Map<T0, hydra.core.Type> nameMapToTypeMap(java.util.Map<T0, hydra.core.Name> m) {
    return hydra.lib.maps.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (v -> new hydra.core.Type.Variable(v)),
      m);
  }
  
  static <T0, T1> java.util.Map<T0, java.util.List<T1>> groupPairsByFirst(java.util.List<hydra.util.Tuple.Tuple2<T0, T1>> pairs) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Map<T0, java.util.List<T1>>, java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, java.util.Map<T0, java.util.List<T1>>>>) (m -> (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, java.util.Map<T0, java.util.List<T1>>>) (p -> {
        hydra.util.Lazy<T1> v = new hydra.util.Lazy<>(() -> hydra.ext.java.coder.Coder.<T0, T1>groupPairsByFirst_v(p));
        return hydra.lib.maps.Alter.apply(
          (java.util.function.Function<hydra.util.Maybe<java.util.List<T1>>, hydra.util.Maybe<java.util.List<T1>>>) (mv -> hydra.lib.maybes.Maybe.apply(
            hydra.util.Maybe.just(java.util.List.of(v.get())),
            (java.util.function.Function<java.util.List<T1>, hydra.util.Maybe<java.util.List<T1>>>) (vs -> hydra.util.Maybe.just(hydra.lib.lists.Concat2.apply(
              vs,
              java.util.List.of(v.get())))),
            mv)),
          hydra.ext.java.coder.Coder.<T0, T1>groupPairsByFirst_k(p),
          m);
      })),
      (java.util.Map<T0, java.util.List<T1>>) ((java.util.Map<T0, java.util.List<T1>>) (hydra.lib.maps.Empty.<T0, java.util.List<T1>>apply())),
      pairs);
  }
  
  static <T0, T1> T0 groupPairsByFirst_k(hydra.util.Tuple.Tuple2<T0, T1> p) {
    return hydra.lib.pairs.First.apply(p);
  }
  
  static <T0, T1> T1 groupPairsByFirst_v(hydra.util.Tuple.Tuple2<T0, T1> p) {
    return hydra.lib.pairs.Second.apply(p);
  }
  
  static <T0> java.util.Map<T0, T0> selfRefSubstitution(java.util.Map<T0, java.util.List<T0>> grouped) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Map<T0, T0>, java.util.function.Function<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>, java.util.Map<T0, T0>>>) (subst -> (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>, java.util.Map<T0, T0>>) (entry -> hydra.ext.java.coder.Coder.<T0>selfRefSubstitution_processGroup(
        subst,
        hydra.lib.pairs.First.apply(entry),
        hydra.lib.pairs.Second.apply(entry)))),
      (java.util.Map<T0, T0>) ((java.util.Map<T0, T0>) (hydra.lib.maps.Empty.<T0, T0>apply())),
      hydra.lib.maps.ToList.apply(grouped));
  }
  
  static <T0> java.util.Map<T0, T0> selfRefSubstitution_processGroup(java.util.Map<T0, T0> subst, T0 inVar, java.util.List<T0> outVars) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Elem.apply(
        inVar,
        outVars),
      () -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<java.util.Map<T0, T0>, java.util.function.Function<T0, java.util.Map<T0, T0>>>) (s -> (java.util.function.Function<T0, java.util.Map<T0, T0>>) (v -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            v,
            inVar),
          () -> s,
          () -> hydra.lib.maps.Insert.apply(
            v,
            inVar,
            s)))),
        subst,
        outVars),
      () -> subst);
  }
  
  static <T0> java.util.Map<T0, T0> directRefSubstitution(java.util.Set<T0> directInputVars, hydra.util.Maybe<T0> codVar, java.util.Map<T0, java.util.List<T0>> grouped) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Map<T0, T0>, java.util.function.Function<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>, java.util.Map<T0, T0>>>) (subst -> (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>, java.util.Map<T0, T0>>) (entry -> hydra.ext.java.coder.Coder.<T0>directRefSubstitution_processGroup(
        directInputVars,
        codVar,
        subst,
        hydra.lib.pairs.First.apply(entry),
        hydra.lib.pairs.Second.apply(entry)))),
      (java.util.Map<T0, T0>) ((java.util.Map<T0, T0>) (hydra.lib.maps.Empty.<T0, T0>apply())),
      hydra.lib.maps.ToList.apply(grouped));
  }
  
  static <T0> java.util.Map<T0, T0> directRefSubstitution_processGroup(java.util.Set<T0> directInputVars, hydra.util.Maybe<T0> codVar, java.util.Map<T0, T0> subst, T0 inVar, java.util.List<T0> outVars) {
    hydra.util.Lazy<java.util.List<T0>> safeNonSelfVars = new hydra.util.Lazy<>(() -> hydra.ext.java.coder.Coder.<T0>directRefSubstitution_processGroup_safeNonSelfVars(
      codVar,
      directInputVars,
      hydra.ext.java.coder.Coder.<T0>directRefSubstitution_processGroup_nonSelfVars(
        inVar,
        outVars)));
    hydra.util.Lazy<Integer> selfRefCount = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(hydra.lib.lists.Filter.apply(
      (java.util.function.Function<T0, Boolean>) (v -> hydra.lib.equality.Equal.apply(
        v,
        inVar)),
      outVars)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        hydra.lib.equality.Gte.apply(
          selfRefCount.get(),
          2),
        hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(safeNonSelfVars.get()))),
      () -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<java.util.Map<T0, T0>, java.util.function.Function<T0, java.util.Map<T0, T0>>>) (s -> (java.util.function.Function<T0, java.util.Map<T0, T0>>) (v -> hydra.lib.maps.Insert.apply(
          v,
          inVar,
          s))),
        subst,
        safeNonSelfVars.get()),
      () -> subst);
  }
  
  static <T0> java.util.List<T0> directRefSubstitution_processGroup_nonSelfVars(T0 inVar, java.util.List<T0> outVars) {
    return hydra.lib.lists.Filter.apply(
      (java.util.function.Function<T0, Boolean>) (v -> hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
        v,
        inVar))),
      outVars);
  }
  
  static <T0> java.util.List<T0> directRefSubstitution_processGroup_safeNonSelfVars(hydra.util.Maybe<T0> codVar, java.util.Set<T0> directInputVars, java.util.List<T0> nonSelfVars) {
    return hydra.lib.lists.Filter.apply(
      (java.util.function.Function<T0, Boolean>) (v -> hydra.lib.logic.And.apply(
        hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
          v,
          directInputVars)),
        hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
          hydra.util.Maybe.just(v),
          codVar)))),
      nonSelfVars);
  }
  
  static <T0> hydra.util.Maybe<T0> findSelfRefVar(java.util.Map<T0, java.util.List<T0>> grouped) {
    hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>>> selfRefs = new hydra.util.Lazy<>(() -> hydra.ext.java.coder.Coder.<T0>findSelfRefVar_selfRefs(grouped));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(selfRefs.get()),
      () -> (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),
      () -> hydra.util.Maybe.just(hydra.lib.pairs.First.apply(hydra.lib.lists.Head.apply(selfRefs.get()))));
  }
  
  static <T0> java.util.List<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>> findSelfRefVar_selfRefs(java.util.Map<T0, java.util.List<T0>> grouped) {
    return hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>, Boolean>) (entry -> hydra.lib.lists.Elem.apply(
        hydra.lib.pairs.First.apply(entry),
        hydra.lib.pairs.Second.apply(entry))),
      hydra.lib.maps.ToList.apply(grouped));
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.Type> detectAccumulatorUnification(java.util.List<hydra.core.Type> doms, hydra.core.Type cod, java.util.List<hydra.core.Name> tparams) {
    hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>> allPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Bind.apply(
      doms,
      (java.util.function.Function<hydra.core.Type, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>>) (d -> hydra.ext.java.coder.Coder.extractInOutPair(d))));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.List<hydra.core.Name>>> groupedByInput = new hydra.util.Lazy<>(() -> hydra.ext.java.coder.Coder.groupPairsByFirst(allPairs.get()));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Name>> selfRefSubst = new hydra.util.Lazy<>(() -> hydra.ext.java.coder.Coder.selfRefSubstitution(groupedByInput.get()));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Name>> codSubst = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.apply(
      (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
      (java.util.function.Function<hydra.core.Name, java.util.Map<hydra.core.Name, hydra.core.Name>>) (cv -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.maps.Member.apply(
          cv,
          selfRefSubst.get()),
        () -> (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
        () -> hydra.lib.maybes.Maybe.apply(
          (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
          (java.util.function.Function<hydra.core.Name, java.util.Map<hydra.core.Name, hydra.core.Name>>) (refVar -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              cv,
              refVar),
            () -> (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
            () -> hydra.lib.maps.Singleton.apply(
              cv,
              refVar))),
          hydra.ext.java.coder.Coder.findSelfRefVar(groupedByInput.get())))),
      hydra.ext.java.coder.Coder.findPairFirst(cod)));
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.Name>> codVar = new hydra.util.Lazy<>(() -> (hydra.rewriting.Rewriting.deannotateType(cod)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing());
      }
      
      @Override
      public hydra.util.Maybe<hydra.core.Name> visit(hydra.core.Type.Variable v) {
        return hydra.util.Maybe.just((v).value);
      }
    }));
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> domVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Bind.apply(
      doms,
      (java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Name>>) (d -> hydra.lib.sets.ToList.apply(hydra.ext.java.coder.Coder.collectTypeVars(d))))));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Type>> danglingSubst = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.apply(
      (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
      (java.util.function.Function<hydra.core.Name, java.util.Map<hydra.core.Name, hydra.core.Type>>) (cv -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.sets.Member.apply(
          cv,
          domVars.get()),
        () -> (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
        () -> hydra.lib.maybes.Maybe.apply(
          (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
          (java.util.function.Function<hydra.core.Name, java.util.Map<hydra.core.Name, hydra.core.Type>>) (refVar -> hydra.lib.maps.Singleton.apply(
            cv,
            new hydra.core.Type.Variable(refVar))),
          hydra.ext.java.coder.Coder.findSelfRefVar(groupedByInput.get())))),
      hydra.ext.java.coder.Coder.findPairFirst(cod)));
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> tparamSet = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(tparams));
    hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>> directPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Bind.apply(
      doms,
      (java.util.function.Function<hydra.core.Type, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>>) (d -> hydra.ext.java.coder.Coder.extractDirectReturn(
        tparamSet.get(),
        d))));
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> directInputVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>, hydra.core.Name>) (p -> hydra.lib.pairs.First.apply(p)),
      directPairs.get())));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.List<hydra.core.Name>>> groupedDirect = new hydra.util.Lazy<>(() -> hydra.ext.java.coder.Coder.groupPairsByFirst(directPairs.get()));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Name>> directRefSubst = new hydra.util.Lazy<>(() -> hydra.ext.java.coder.Coder.directRefSubstitution(
      directInputVars.get(),
      codVar.get(),
      groupedDirect.get()));
    return hydra.lib.maps.Union.apply(
      hydra.lib.maps.Union.apply(
        hydra.lib.maps.Union.apply(
          hydra.ext.java.coder.Coder.nameMapToTypeMap(selfRefSubst.get()),
          hydra.ext.java.coder.Coder.nameMapToTypeMap(codSubst.get())),
        danglingSubst.get()),
      hydra.ext.java.coder.Coder.nameMapToTypeMap(directRefSubst.get()));
  }
  
  static Boolean typesMatch(hydra.core.Type a, hydra.core.Type b) {
    return (a).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Variable va) {
        return (b).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Type instance) {
            return true;
          }
          
          @Override
          public Boolean visit(hydra.core.Type.Variable vb) {
            return hydra.lib.equality.Equal.apply(
              (va).value,
              (vb).value);
          }
        });
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Wrap wa) {
        return (b).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Type instance) {
            return true;
          }
          
          @Override
          public Boolean visit(hydra.core.Type.Wrap wb) {
            return hydra.lib.equality.Equal.apply(
              ((wa).value).typeName,
              ((wb).value).typeName);
          }
        });
      }
    });
  }
  
  static Boolean isSimpleName(hydra.core.Name name) {
    return hydra.lib.equality.Equal.apply(
      hydra.lib.lists.Length.apply(hydra.lib.strings.SplitOn.apply(
        ".",
        (name).value)),
      1);
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Type>> filterPhantomTypeArgs(hydra.core.Name calleeName, java.util.List<hydra.core.Type> allTypeArgs) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.dereferenceElement(calleeName),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Type>>>) (mel -> hydra.lib.maybes.Cases.apply(
        mel,
        hydra.lib.flows.Pure.apply(allTypeArgs),
        (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Type>>>) (el -> hydra.lib.maybes.Cases.apply(
          (el).type,
          hydra.lib.flows.Pure.apply(allTypeArgs),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Type>>>) (ts -> {
            hydra.core.Type schemeType = (ts).type;
            Integer nParams = hydra.ext.java.coder.Coder.countFunctionParams(schemeType);
            hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> peeled = hydra.ext.java.coder.Coder.peelDomainTypes(
              nParams,
              schemeType);
            hydra.util.Lazy<hydra.core.Type> calleeCod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(peeled));
            hydra.util.Lazy<java.util.List<hydra.core.Type>> calleeDoms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(peeled));
            hydra.util.Lazy<java.util.List<hydra.core.Name>> schemeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
              (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.ext.java.coder.Coder.isSimpleName(v)),
              (ts).variables));
            java.util.Map<hydra.core.Name, hydra.core.Type> overgenSubst = hydra.ext.java.coder.Coder.detectAccumulatorUnification(
              calleeDoms.get(),
              calleeCod.get(),
              schemeVars.get());
            java.util.Set<hydra.core.Name> schemeTypeVars = hydra.ext.java.coder.Coder.collectTypeVars((ts).type);
            hydra.util.Lazy<java.util.List<Boolean>> keepFlags = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.logic.And.apply(
                hydra.lib.sets.Member.apply(
                  v,
                  schemeTypeVars),
                hydra.lib.logic.Not.apply(hydra.lib.maps.Member.apply(
                  v,
                  overgenSubst)))),
              schemeVars.get()));
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
                hydra.lib.lists.Length.apply(schemeVars.get()),
                hydra.lib.lists.Length.apply(allTypeArgs))),
              () -> hydra.lib.flows.Pure.apply(allTypeArgs),
              () -> hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.filterPhantomTypeArgs_filterAndApply(
                allTypeArgs,
                keepFlags.get(),
                overgenSubst)));
          }))))));
  }
  
  static java.util.List<hydra.core.Type> filterPhantomTypeArgs_filterAndApply(java.util.List<hydra.core.Type> allTypeArgs, java.util.List<Boolean> keepFlags, java.util.Map<hydra.core.Name, hydra.core.Type> overgenSubst) {
    hydra.util.Lazy<java.util.List<hydra.core.Type>> filtered = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Type, Boolean>, hydra.core.Type>) (p -> hydra.lib.pairs.First.apply(p)),
      hydra.lib.lists.Filter.apply(
        (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Type, Boolean>, Boolean>) (p -> hydra.lib.pairs.Second.apply(p)),
        hydra.lib.lists.Zip.apply(
          allTypeArgs,
          keepFlags))));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Not.apply(hydra.lib.maps.Null.apply(overgenSubst)),
      () -> hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes(
          overgenSubst,
          t)),
        filtered.get()),
      () -> filtered.get());
  }
  
  static <T0> java.util.List<T0> filterByFlags(java.util.List<T0> xs, java.util.List<Boolean> flags) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, Boolean>, T0>) (p -> hydra.lib.pairs.First.apply(p)),
      hydra.lib.lists.Filter.apply(
        (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, Boolean>, Boolean>) (p -> hydra.lib.pairs.Second.apply(p)),
        hydra.lib.lists.Zip.apply(
          xs,
          flags)));
  }
  
  static hydra.core.Type applySubstSimple(java.util.Map<hydra.core.Name, hydra.core.Type> subst, hydra.core.Type t) {
    return (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return t;
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable v) {
        return hydra.lib.maps.FindWithDefault.apply(
          t,
          (v).value,
          subst);
      }
    });
  }
  
  static <T0> java.util.Map<hydra.core.Name, T0> buildArgSubst(java.util.Set<hydra.core.Name> schemeVarSet, java.util.List<hydra.core.Type> schemeDoms, java.util.List<T0> argTypes) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Bind.apply(
      hydra.lib.lists.Zip.apply(
        schemeDoms,
        argTypes),
      (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Type, T0>, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, T0>>>) (p -> {
        hydra.util.Lazy<hydra.core.Type> sdom = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
        return (hydra.rewriting.Rewriting.deannotateType(sdom.get())).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, T0>> otherwise(hydra.core.Type instance) {
            return (java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, T0>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.core.Name, T0>>of());
          }
          
          @Override
          public java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, T0>> visit(hydra.core.Type.Variable v) {
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.sets.Member.apply(
                (v).value,
                schemeVarSet),
              () -> java.util.List.of((hydra.util.Tuple.Tuple2<hydra.core.Name, T0>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, T0>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, T0>((v).value, hydra.ext.java.coder.Coder.buildArgSubst_argType(p))))),
              () -> (java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, T0>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.core.Name, T0>>of()));
          }
        });
      })));
  }
  
  static <T0, T1> T1 buildArgSubst_argType(hydra.util.Tuple.Tuple2<T0, T1> p) {
    return hydra.lib.pairs.Second.apply(p);
  }
  
  static java.util.List<hydra.core.Type> resolveTypeApps(java.util.List<hydra.core.Name> schemeVars, java.util.List<hydra.core.Type> fallbackTypeApps, java.util.Map<hydra.core.Name, hydra.core.Type> argSubst) {
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> resolvedVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply(argSubst)));
    hydra.util.Lazy<java.util.List<hydra.core.Name>> unresolvedVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
        v,
        resolvedVars.get()))),
      schemeVars));
    hydra.util.Lazy<java.util.Set<hydra.core.Type>> usedTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maps.Elems.apply(argSubst)));
    hydra.util.Lazy<java.util.List<hydra.core.Type>> unusedIrTypes = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Type, Boolean>) (t -> hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
        t,
        usedTypes.get()))),
      fallbackTypeApps));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Type>> remainingSubst = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      unresolvedVars.get(),
      unusedIrTypes.get())));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Type>> fullSubst = new hydra.util.Lazy<>(() -> hydra.lib.maps.Union.apply(
      argSubst,
      remainingSubst.get()));
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (v -> hydra.lib.maps.FindWithDefault.apply(
        new hydra.core.Type.Variable(v),
        v,
        fullSubst.get())),
      schemeVars);
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Type>> correctTypeAppsWithArgs(java.util.List<hydra.core.Name> schemeVars, java.util.List<hydra.core.Type> fallbackTypeApps, hydra.core.Type schemeType, java.util.List<hydra.core.Term> args) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Type>> irSubst = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      schemeVars,
      fallbackTypeApps)));
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>> peeled = new hydra.util.Lazy<>(() -> hydra.ext.java.coder.Coder.peelDomainTypes(
      hydra.lib.lists.Length.apply(args),
      schemeType));
    hydra.util.Lazy<java.util.List<hydra.core.Type>> schemeDoms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(peeled.get()));
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> schemeVarSet = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(schemeVars));
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapList.apply(
        (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>>>) (arg -> hydra.annotations.Annotations.getType(hydra.annotations.Annotations.termAnnotationInternal(arg))),
        args),
      (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.core.Type>>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Type>>>) (mArgTypes -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, Boolean>) (m -> hydra.lib.maybes.IsNothing.apply(m)),
          mArgTypes))),
        () -> hydra.lib.flows.Pure.apply(fallbackTypeApps),
        () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Type>>>) (() -> {
          hydra.util.Lazy<java.util.List<hydra.core.Type>> argTypes = new hydra.util.Lazy<>(() -> hydra.lib.lists.Bind.apply(
            mArgTypes,
            (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, java.util.List<hydra.core.Type>>) (m -> hydra.lib.maybes.Cases.apply(
              m,
              (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
              (java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>) (x -> hydra.lib.lists.Pure.apply(x))))));
          return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Type>>>) (() -> {
            hydra.util.Lazy<java.util.List<hydra.core.Type>> irDoms = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (d -> hydra.ext.java.coder.Coder.applySubstSimple(
                irSubst.get(),
                d)),
              schemeDoms.get()));
            return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Type>>>) (() -> {
              hydra.util.Lazy<Boolean> domsMatch = new hydra.util.Lazy<>(() -> hydra.lib.lists.Null.apply(hydra.lib.lists.Filter.apply(
                (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Type, hydra.core.Type>, Boolean>) (p -> hydra.lib.logic.Not.apply(hydra.ext.java.coder.Coder.typesMatch(
                  hydra.rewriting.Rewriting.deannotateType(hydra.lib.pairs.First.apply(p)),
                  hydra.rewriting.Rewriting.deannotateType(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.lists.Zip.apply(
                  irDoms.get(),
                  argTypes.get()))));
              return hydra.lib.logic.IfElse.lazy(
                domsMatch.get(),
                () -> hydra.lib.flows.Pure.apply(fallbackTypeApps),
                () -> hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.resolveTypeApps(
                  schemeVars,
                  fallbackTypeApps,
                  hydra.ext.java.coder.Coder.buildArgSubst(
                    schemeVarSet.get(),
                    schemeDoms.get(),
                    argTypes.get()))));
            })).get();
          })).get();
        })).get())));
  }
  
  static <T0> hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Type>> correctTypeApps(T0 tc, hydra.core.Name name, java.util.List<hydra.core.Term> args, java.util.List<hydra.core.Type> fallbackTypeApps) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.dereferenceElement(name),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Type>>>) (mel -> hydra.lib.maybes.Cases.apply(
        mel,
        hydra.lib.flows.Pure.apply(fallbackTypeApps),
        (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Type>>>) (el -> hydra.lib.maybes.Cases.apply(
          (el).type,
          hydra.lib.flows.Pure.apply(fallbackTypeApps),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Type>>>) (ts -> {
            hydra.util.Lazy<java.util.List<hydra.core.Name>> allSchemeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
              (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.ext.java.coder.Coder.isSimpleName(v)),
              (ts).variables));
            hydra.core.Type schemeType = (ts).type;
            Integer nParams = hydra.ext.java.coder.Coder.countFunctionParams(schemeType);
            hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> peeled = hydra.ext.java.coder.Coder.peelDomainTypes(
              nParams,
              schemeType);
            hydra.util.Lazy<hydra.core.Type> calleeCod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(peeled));
            hydra.util.Lazy<java.util.List<hydra.core.Type>> calleeDoms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(peeled));
            java.util.Set<hydra.core.Name> schemeTypeVars = hydra.ext.java.coder.Coder.collectTypeVars(schemeType);
            hydra.util.Lazy<java.util.List<Boolean>> usedFlags = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.sets.Member.apply(
                v,
                schemeTypeVars)),
              allSchemeVars.get()));
            hydra.util.Lazy<java.util.List<hydra.core.Name>> usedSchemeVars = new hydra.util.Lazy<>(() -> hydra.ext.java.coder.Coder.filterByFlags(
              allSchemeVars.get(),
              usedFlags.get()));
            java.util.Map<hydra.core.Name, hydra.core.Type> overgenSubst = hydra.ext.java.coder.Coder.detectAccumulatorUnification(
              calleeDoms.get(),
              calleeCod.get(),
              usedSchemeVars.get());
            hydra.util.Lazy<java.util.List<Boolean>> keepFlags = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.logic.And.apply(
                hydra.lib.sets.Member.apply(
                  v,
                  schemeTypeVars),
                hydra.lib.logic.Not.apply(hydra.lib.maps.Member.apply(
                  v,
                  overgenSubst)))),
              allSchemeVars.get()));
            hydra.util.Lazy<java.util.List<hydra.core.Type>> filteredFallback0 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                hydra.lib.lists.Length.apply(allSchemeVars.get()),
                hydra.lib.lists.Length.apply(fallbackTypeApps)),
              () -> hydra.ext.java.coder.Coder.filterByFlags(
                fallbackTypeApps,
                keepFlags.get()),
              () -> fallbackTypeApps));
            hydra.util.Lazy<java.util.List<hydra.core.Type>> filteredFallback = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.maps.Null.apply(overgenSubst),
              () -> filteredFallback0.get(),
              () -> hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes(
                  overgenSubst,
                  t)),
                filteredFallback0.get())));
            hydra.util.Lazy<java.util.List<hydra.core.Name>> schemeVars = new hydra.util.Lazy<>(() -> hydra.ext.java.coder.Coder.filterByFlags(
              allSchemeVars.get(),
              keepFlags.get()));
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.Or.apply(
                hydra.lib.lists.Null.apply(schemeVars.get()),
                hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
                  hydra.lib.lists.Length.apply(schemeVars.get()),
                  hydra.lib.lists.Length.apply(filteredFallback.get())))),
              () -> hydra.lib.flows.Pure.apply(filteredFallback.get()),
              () -> hydra.ext.java.coder.Coder.correctTypeAppsWithArgs(
                schemeVars.get(),
                filteredFallback.get(),
                schemeType,
                args));
          }))))));
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.Name> buildSubstFromAnnotations_go(java.util.Set<hydra.core.Name> schemeVarSet, hydra.graph.Graph g, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Term instance) {
        return (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Annotated at) {
        java.util.Map<hydra.core.Name, hydra.core.Term> anns = ((at).value).annotation;
        hydra.core.Term body = ((at).value).body;
        hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Name>> annSubst = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.apply(
          hydra.lib.maps.Lookup.apply(
            hydra.constants.Constants.key_type(),
            anns),
          (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
          (java.util.function.Function<hydra.core.Term, java.util.Map<hydra.core.Name, hydra.core.Name>>) (typeTerm -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.Name>>) (ignored -> (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()))),
            (java.util.function.Function<hydra.core.Type, java.util.Map<hydra.core.Name, hydra.core.Name>>) (annType -> (hydra.rewriting.Rewriting.deannotateTerm(body)).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Term instance) {
                return (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
              }
              
              @Override
              public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Function f) {
                return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
                  @Override
                  public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Function instance) {
                    return (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
                  }
                  
                  @Override
                  public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Function.Lambda lam) {
                    return hydra.lib.maybes.Cases.apply(
                      ((lam).value).domain,
                      (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
                      (java.util.function.Function<hydra.core.Type, java.util.Map<hydra.core.Name, hydra.core.Name>>) (dom -> (hydra.rewriting.Rewriting.deannotateType(annType)).accept(new hydra.core.Type.PartialVisitor<>() {
                        @Override
                        public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
                          return (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
                        }
                        
                        @Override
                        public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Function ft) {
                          return hydra.ext.java.coder.Coder.buildTypeVarSubst(
                            schemeVarSet,
                            ((ft).value).domain,
                            dom);
                        }
                      })));
                  }
                });
              }
            })),
            hydra.decode.core.Core.type(
              g,
              typeTerm)))));
        java.util.Map<hydra.core.Name, hydra.core.Name> bodySubst = hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
          schemeVarSet,
          g,
          body);
        return hydra.lib.maps.Union.apply(
          annSubst.get(),
          bodySubst);
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Application app) {
        return hydra.lib.maps.Union.apply(
          hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
            schemeVarSet,
            g,
            ((app).value).function),
          hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
            schemeVarSet,
            g,
            ((app).value).argument));
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Function instance) {
            return (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Function.Lambda lam) {
            return hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
              schemeVarSet,
              g,
              ((lam).value).body);
          }
          
          @Override
          public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Function.Elimination elim) {
            return ((elim).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Elimination instance) {
                return (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
              }
              
              @Override
              public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Elimination.Union cs) {
                hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Name>> caseSubsts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                  (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Field, java.util.Map<hydra.core.Name, hydra.core.Name>>>) (acc -> (java.util.function.Function<hydra.core.Field, java.util.Map<hydra.core.Name, hydra.core.Name>>) (fld -> hydra.lib.maps.Union.apply(
                    acc,
                    hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
                      schemeVarSet,
                      g,
                      (fld).term)))),
                  (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
                  ((cs).value).cases));
                hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Name>> defSubst = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.apply(
                  ((cs).value).default_,
                  (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
                  (java.util.function.Function<hydra.core.Term, java.util.Map<hydra.core.Name, hydra.core.Name>>) (d -> hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
                    schemeVarSet,
                    g,
                    d))));
                return hydra.lib.maps.Union.apply(
                  defSubst.get(),
                  caseSubsts.get());
              }
            });
          }
        });
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Let lt) {
        hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Name>> bindingSubst = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Binding, java.util.Map<hydra.core.Name, hydra.core.Name>>>) (acc -> (java.util.function.Function<hydra.core.Binding, java.util.Map<hydra.core.Name, hydra.core.Name>>) (b -> hydra.lib.maps.Union.apply(
            acc,
            hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
              schemeVarSet,
              g,
              (b).term)))),
          (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
          ((lt).value).bindings));
        return hydra.lib.maps.Union.apply(
          bindingSubst.get(),
          hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
            schemeVarSet,
            g,
            ((lt).value).body));
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.List terms) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Term, java.util.Map<hydra.core.Name, hydra.core.Name>>>) (acc -> (java.util.function.Function<hydra.core.Term, java.util.Map<hydra.core.Name, hydra.core.Name>>) (t -> hydra.lib.maps.Union.apply(
            acc,
            hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
              schemeVarSet,
              g,
              t)))),
          (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
          (terms).value);
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Cases.apply(
          (mt).value,
          (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
          (java.util.function.Function<hydra.core.Term, java.util.Map<hydra.core.Name, hydra.core.Name>>) (t -> hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
            schemeVarSet,
            g,
            t)));
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Pair p) {
        return hydra.lib.maps.Union.apply(
          hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
            schemeVarSet,
            g,
            hydra.lib.pairs.First.apply((p).value)),
          hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
            schemeVarSet,
            g,
            hydra.lib.pairs.Second.apply((p).value)));
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Record r) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Field, java.util.Map<hydra.core.Name, hydra.core.Name>>>) (acc -> (java.util.function.Function<hydra.core.Field, java.util.Map<hydra.core.Name, hydra.core.Name>>) (fld -> hydra.lib.maps.Union.apply(
            acc,
            hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
              schemeVarSet,
              g,
              (fld).term)))),
          (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
          ((r).value).fields);
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Set terms) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Term, java.util.Map<hydra.core.Name, hydra.core.Name>>>) (acc -> (java.util.function.Function<hydra.core.Term, java.util.Map<hydra.core.Name, hydra.core.Name>>) (t -> hydra.lib.maps.Union.apply(
            acc,
            hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
              schemeVarSet,
              g,
              t)))),
          (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
          hydra.lib.sets.ToList.apply((terms).value));
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.TypeApplication ta) {
        return hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
          schemeVarSet,
          g,
          ((ta).value).body);
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.TypeLambda tl) {
        return hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
          schemeVarSet,
          g,
          ((tl).value).body);
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, java.util.Map<hydra.core.Name, hydra.core.Name>>) (t -> hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
            schemeVarSet,
            g,
            t)),
          (java.util.function.Function<hydra.core.Term, java.util.Map<hydra.core.Name, hydra.core.Name>>) (t -> hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
            schemeVarSet,
            g,
            t)),
          (e).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Name>> buildSubstFromAnnotations(java.util.Set<hydra.core.Name> schemeVarSet, hydra.core.Term term) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Name>>>) (g -> hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.buildSubstFromAnnotations_go(
        schemeVarSet,
        g,
        term))));
  }
  
  static hydra.core.Term applyOvergenSubstToTermAnnotations_go(java.util.Map<hydra.core.Name, hydra.core.Type> subst, hydra.graph.Graph cx, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return term;
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        java.util.Map<hydra.core.Name, hydra.core.Term> ann = ((at).value).annotation;
        hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Term>> ann_ = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.apply(
          hydra.lib.maps.Lookup.apply(
            hydra.constants.Constants.key_type(),
            ann),
          ann,
          (java.util.function.Function<hydra.core.Term, java.util.Map<hydra.core.Name, hydra.core.Term>>) (typeTerm -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.Term>>) (ignored -> ann),
            (java.util.function.Function<hydra.core.Type, java.util.Map<hydra.core.Name, hydra.core.Term>>) (t -> {
              hydra.core.Type t_ = hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes(
                subst,
                t);
              return hydra.lib.maps.Insert.apply(
                hydra.constants.Constants.key_type(),
                hydra.encode.core.Core.type(t_),
                ann);
            }),
            hydra.decode.core.Core.type(
              cx,
              typeTerm)))));
        hydra.core.Term inner = ((at).value).body;
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.ext.java.coder.Coder.applyOvergenSubstToTermAnnotations_go(
          subst,
          cx,
          inner), ann_.get()));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Application app) {
        return new hydra.core.Term.Application(new hydra.core.Application(hydra.ext.java.coder.Coder.applyOvergenSubstToTermAnnotations_go(
          subst,
          cx,
          ((app).value).function), hydra.ext.java.coder.Coder.applyOvergenSubstToTermAnnotations_go(
          subst,
          cx,
          ((app).value).argument)));
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
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(((lam).value).parameter, hydra.lib.maybes.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (d -> hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes(
                subst,
                d)),
              ((lam).value).domain), hydra.ext.java.coder.Coder.applyOvergenSubstToTermAnnotations_go(
              subst,
              cx,
              ((lam).value).body))));
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.Function.Elimination elim) {
            return ((elim).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public hydra.core.Term otherwise(hydra.core.Elimination instance) {
                return term;
              }
              
              @Override
              public hydra.core.Term visit(hydra.core.Elimination.Union cs) {
                return new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(((cs).value).typeName, hydra.lib.maybes.Map.apply(
                  (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (d -> hydra.ext.java.coder.Coder.applyOvergenSubstToTermAnnotations_go(
                    subst,
                    cx,
                    d)),
                  ((cs).value).default_), hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (fld -> new hydra.core.Field((fld).name, hydra.ext.java.coder.Coder.applyOvergenSubstToTermAnnotations_go(
                    subst,
                    cx,
                    (fld).term))),
                  ((cs).value).cases)))));
              }
            });
          }
        });
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        return new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, hydra.ext.java.coder.Coder.applyOvergenSubstToTermAnnotations_go(
            subst,
            cx,
            (b).term), (b).type)),
          ((lt).value).bindings), hydra.ext.java.coder.Coder.applyOvergenSubstToTermAnnotations_go(
          subst,
          cx,
          ((lt).value).body)));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication ta) {
        return new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(hydra.ext.java.coder.Coder.applyOvergenSubstToTermAnnotations_go(
          subst,
          cx,
          ((ta).value).body), hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes(
          subst,
          ((ta).value).type)));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
        return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(((tl).value).parameter, hydra.ext.java.coder.Coder.applyOvergenSubstToTermAnnotations_go(
          subst,
          cx,
          ((tl).value).body)));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> applyOvergenSubstToTermAnnotations(java.util.Map<hydra.core.Name, hydra.core.Type> subst, hydra.core.Term term0) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (cx -> hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.applyOvergenSubstToTermAnnotations_go(
        subst,
        cx,
        term0))));
  }
  
  static hydra.ext.java.syntax.ReferenceType javaComparableRefType() {
    return new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Class_(new hydra.ext.java.syntax.ClassType((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()), new hydra.ext.java.syntax.ClassTypeQualifier.None(), hydra.ext.java.utils.Utils.javaTypeIdentifier("Comparable"), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()))));
  }
  
  static hydra.ext.java.syntax.Expression comparableCompareExpr(String otherVar, String fname) {
    hydra.ext.java.syntax.Expression arg = hydra.ext.java.utils.Utils.javaExpressionNameToJavaExpression(hydra.ext.java.utils.Utils.fieldExpression(
      hydra.ext.java.utils.Utils.javaIdentifier(otherVar),
      hydra.ext.java.utils.Utils.javaIdentifier(fname)));
    hydra.ext.java.syntax.MethodInvocation_Variant castVar = new hydra.ext.java.syntax.MethodInvocation_Variant.Primary(hydra.ext.java.utils.Utils.javaExpressionToJavaPrimary(hydra.ext.java.utils.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.Utils.javaCastExpression(
      hydra.ext.java.coder.Coder.javaComparableRefType(),
      hydra.ext.java.utils.Utils.javaIdentifierToJavaUnaryExpression(new hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.Utils.sanitizeJavaName(fname)))))));
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation_Header> header = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(castVar, (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.compareToMethodName()))));
    return hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(header.get(), java.util.List.of(arg)));
  }
  
  static hydra.ext.java.syntax.Expression arraysCompareExpr(String otherVar, String fname) {
    hydra.util.Lazy<hydra.ext.java.syntax.Expression> arg1 = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaExpressionNameToJavaExpression(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.Utils.sanitizeJavaName(fname)))));
    hydra.ext.java.syntax.Expression arg2 = hydra.ext.java.utils.Utils.javaExpressionNameToJavaExpression(hydra.ext.java.utils.Utils.fieldExpression(
      hydra.ext.java.utils.Utils.javaIdentifier(otherVar),
      hydra.ext.java.utils.Utils.javaIdentifier(fname)));
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation_Header> header = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Type(hydra.ext.java.utils.Utils.javaTypeName(new hydra.ext.java.syntax.Identifier("java.util.Arrays"))), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()), new hydra.ext.java.syntax.Identifier("compare"))));
    return hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(header.get(), java.util.List.of(
      arg1.get(),
      arg2)));
  }
  
  static hydra.ext.java.syntax.Expression hashCodeCompareExpr(String otherVar, String fname) {
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation_Header> header = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Type(hydra.ext.java.utils.Utils.javaTypeName(new hydra.ext.java.syntax.Identifier("Integer"))), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()), new hydra.ext.java.syntax.Identifier("compare"))));
    hydra.util.Lazy<hydra.ext.java.syntax.Expression> otherHashCode = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Expression(hydra.ext.java.utils.Utils.fieldExpression(
      hydra.ext.java.utils.Utils.javaIdentifier(otherVar),
      hydra.ext.java.utils.Utils.javaIdentifier(fname))), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.hashCodeMethodName()))), (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of()))));
    hydra.util.Lazy<hydra.ext.java.syntax.Expression> thisHashCode = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Expression(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.Utils.sanitizeJavaName(fname)))), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.hashCodeMethodName()))), (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of()))));
    return hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(header.get(), java.util.List.of(
      thisHashCode.get(),
      otherHashCode.get())));
  }
  
  static hydra.ext.java.syntax.Expression compareFieldExpr(String otherVar, hydra.core.FieldType ft) {
    String fname = ((ft).name).value;
    hydra.core.Type ftype = (ft).type;
    return hydra.lib.logic.IfElse.lazy(
      hydra.ext.java.coder.Coder.isBinaryType(ftype),
      () -> hydra.ext.java.coder.Coder.arraysCompareExpr(
        otherVar,
        fname),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.ext.java.coder.Coder.isNonComparableType(ftype),
        () -> hydra.ext.java.coder.Coder.hashCodeCompareExpr(
          otherVar,
          fname),
        () -> hydra.ext.java.coder.Coder.comparableCompareExpr(
          otherVar,
          fname)));
  }
  
  static hydra.ext.java.syntax.Expression cmpNotZeroExpr() {
    hydra.util.Lazy<hydra.ext.java.syntax.EqualityExpression> lhs = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaRelationalExpressionToJavaEqualityExpression(hydra.ext.java.utils.Utils.javaPostfixExpressionToJavaRelationalExpression(new hydra.ext.java.syntax.PostfixExpression.Name(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), hydra.ext.java.utils.Utils.javaIdentifier("cmp"))))));
    hydra.ext.java.syntax.RelationalExpression rhs = hydra.ext.java.utils.Utils.javaPostfixExpressionToJavaRelationalExpression(new hydra.ext.java.syntax.PostfixExpression.Primary(hydra.ext.java.utils.Utils.javaLiteralToJavaPrimary(hydra.ext.java.utils.Utils.javaInt(new java.math.BigInteger("0")))));
    return hydra.ext.java.utils.Utils.javaEqualityExpressionToJavaExpression(new hydra.ext.java.syntax.EqualityExpression.NotEqual(new hydra.ext.java.syntax.EqualityExpression_Binary(lhs.get(), rhs)));
  }
  
  static <T0> hydra.ext.java.syntax.BlockStatement cmpDeclStatement(T0 aliases) {
    return hydra.ext.java.utils.Utils.<T0>variableDeclarationStatement(
      aliases,
      hydra.ext.java.utils.Utils.javaIntType(),
      hydra.ext.java.utils.Utils.javaIdentifier("cmp"),
      hydra.ext.java.utils.Utils.javaIntExpression(new java.math.BigInteger("0")));
  }
  
  static java.util.List<hydra.ext.java.syntax.BlockStatement> compareAndReturnStmts(String otherVar, hydra.core.FieldType f) {
    return java.util.List.of(
      new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaAssignmentStatement(
        new hydra.ext.java.syntax.LeftHandSide.ExpressionName(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), hydra.ext.java.utils.Utils.javaIdentifier("cmp"))),
        hydra.ext.java.coder.Coder.compareFieldExpr(
          otherVar,
          f))),
      new hydra.ext.java.syntax.BlockStatement.Statement(new hydra.ext.java.syntax.Statement.IfThen(new hydra.ext.java.syntax.IfThenStatement(hydra.ext.java.coder.Coder.cmpNotZeroExpr(), hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.utils.Utils.javaExpressionNameToJavaExpression(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), hydra.ext.java.utils.Utils.javaIdentifier("cmp")))))))));
  }
  
  static <T0> java.util.List<hydra.ext.java.syntax.BlockStatement> compareToBody(T0 aliases, String otherVar, java.util.List<hydra.core.FieldType> fields) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(fields),
      () -> java.util.List.of(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.utils.Utils.javaIntExpression(new java.math.BigInteger("0")))))),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(fields),
          1),
        () -> java.util.List.of(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.coder.Coder.compareFieldExpr(
          otherVar,
          hydra.lib.lists.Head.apply(fields)))))),
        () -> hydra.lib.lists.Concat2.apply(
          java.util.List.of(hydra.ext.java.coder.Coder.<T0>cmpDeclStatement(aliases)),
          hydra.lib.lists.Concat2.apply(
            hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.FieldType, java.util.List<hydra.ext.java.syntax.BlockStatement>>) (f -> hydra.ext.java.coder.Coder.compareAndReturnStmts(
                otherVar,
                f)),
              hydra.lib.lists.Init.apply(fields))),
            java.util.List.of(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.coder.Coder.compareFieldExpr(
              otherVar,
              hydra.lib.lists.Last.apply(fields))))))))));
  }
  
  static hydra.ext.java.syntax.Expression tagCompareExpr() {
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation> otherGetClass = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation(new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Expression(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.otherInstanceName()))), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()), new hydra.ext.java.syntax.Identifier("getClass"))), (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of())));
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation> otherGetName = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation(new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Primary(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaPrimary(otherGetClass.get())), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()), new hydra.ext.java.syntax.Identifier("getName"))), (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of())));
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation> thisGetClass = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation(new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Primary(hydra.ext.java.utils.Utils.javaExpressionToJavaPrimary(hydra.ext.java.utils.Utils.javaThis())), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()), new hydra.ext.java.syntax.Identifier("getClass"))), (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of())));
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation> thisGetName = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation(new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Primary(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaPrimary(thisGetClass.get())), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()), new hydra.ext.java.syntax.Identifier("getName"))), (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of())));
    return hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Primary(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaPrimary(thisGetName.get())), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.compareToMethodName()))), java.util.List.of(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(otherGetName.get()))));
  }
  
  static hydra.ext.java.syntax.Expression tagCmpNotZeroExpr() {
    hydra.util.Lazy<hydra.ext.java.syntax.EqualityExpression> lhs = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaRelationalExpressionToJavaEqualityExpression(hydra.ext.java.utils.Utils.javaPostfixExpressionToJavaRelationalExpression(new hydra.ext.java.syntax.PostfixExpression.Name(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), hydra.ext.java.utils.Utils.javaIdentifier("tagCmp"))))));
    hydra.ext.java.syntax.RelationalExpression rhs = hydra.ext.java.utils.Utils.javaPostfixExpressionToJavaRelationalExpression(new hydra.ext.java.syntax.PostfixExpression.Primary(hydra.ext.java.utils.Utils.javaLiteralToJavaPrimary(hydra.ext.java.utils.Utils.javaInt(new java.math.BigInteger("0")))));
    return hydra.ext.java.utils.Utils.javaEqualityExpressionToJavaExpression(new hydra.ext.java.syntax.EqualityExpression.NotEqual(new hydra.ext.java.syntax.EqualityExpression_Binary(lhs.get(), rhs)));
  }
  
  static <T0> hydra.ext.java.syntax.ClassBodyDeclaration recordCompareToMethod(hydra.ext.java.helpers.Aliases aliases, T0 tparams, hydra.core.Name elName, java.util.List<hydra.core.FieldType> fields) {
    java.util.List<hydra.ext.java.syntax.Annotation> anns = java.util.List.of(
      hydra.ext.java.utils.Utils.overrideAnnotation(),
      hydra.ext.java.utils.Utils.suppressWarningsUncheckedAnnotation());
    java.util.List<hydra.ext.java.syntax.MethodModifier> mods = java.util.List.of(new hydra.ext.java.syntax.MethodModifier.Public());
    hydra.ext.java.syntax.FormalParameter param = hydra.ext.java.utils.Utils.javaTypeToJavaFormalParameter(
      hydra.ext.java.utils.Utils.javaTypeFromTypeName(
        aliases,
        elName),
      new hydra.core.Name(hydra.ext.java.names.Names.otherInstanceName()));
    hydra.ext.java.syntax.Result result = hydra.ext.java.utils.Utils.javaTypeToJavaResult(hydra.ext.java.utils.Utils.javaIntType());
    return hydra.ext.java.utils.Utils.methodDeclaration(
      mods,
      (java.util.List<hydra.ext.java.syntax.TypeParameter>) (java.util.List.<hydra.ext.java.syntax.TypeParameter>of()),
      anns,
      hydra.ext.java.names.Names.compareToMethodName(),
      java.util.List.of(param),
      result,
      hydra.util.Maybe.just(hydra.ext.java.coder.Coder.compareToBody(
        aliases,
        hydra.ext.java.names.Names.otherInstanceName(),
        fields)));
  }
  
  static <T0> hydra.ext.java.syntax.ClassBodyDeclaration variantCompareToMethod(hydra.ext.java.helpers.Aliases aliases, T0 tparams, hydra.core.Name parentName, hydra.core.Name variantName, java.util.List<hydra.core.FieldType> fields) {
    java.util.List<hydra.ext.java.syntax.Annotation> anns = java.util.List.of(
      hydra.ext.java.utils.Utils.overrideAnnotation(),
      hydra.ext.java.utils.Utils.suppressWarningsUncheckedAnnotation());
    hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> tagDeclStmt = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.variableDeclarationStatement(
      aliases,
      hydra.ext.java.utils.Utils.javaIntType(),
      hydra.ext.java.utils.Utils.javaIdentifier("tagCmp"),
      hydra.ext.java.coder.Coder.tagCompareExpr()));
    hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> tagReturnStmt = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.BlockStatement.Statement(new hydra.ext.java.syntax.Statement.IfThen(new hydra.ext.java.syntax.IfThenStatement(hydra.ext.java.coder.Coder.tagCmpNotZeroExpr(), hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.utils.Utils.javaExpressionNameToJavaExpression(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), hydra.ext.java.utils.Utils.javaIdentifier("tagCmp")))))))));
    hydra.util.Lazy<hydra.ext.java.syntax.Expression> castOtherExpr = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.Utils.javaCastExpression(
      hydra.ext.java.utils.Utils.nameToJavaReferenceType(
        aliases,
        false,
        (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()),
        variantName,
        (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
      hydra.ext.java.utils.Utils.javaIdentifierToJavaUnaryExpression(new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.otherInstanceName())))));
    String varTmpName = "o";
    hydra.ext.java.syntax.Type variantJavaType = hydra.ext.java.utils.Utils.javaTypeFromTypeName(
      aliases,
      variantName);
    hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> castDeclStmt = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.variableDeclarationStatement(
      aliases,
      variantJavaType,
      hydra.ext.java.utils.Utils.javaIdentifier(varTmpName),
      castOtherExpr.get()));
    java.util.List<hydra.ext.java.syntax.BlockStatement> emptyReturn = java.util.List.of(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.utils.Utils.javaIntExpression(new java.math.BigInteger("0"))))));
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.BlockStatement>> valueCompareStmt = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(fields),
      () -> emptyReturn,
      () -> hydra.lib.lists.Concat2.apply(
        java.util.List.of(castDeclStmt.get()),
        hydra.ext.java.coder.Coder.compareToBody(
          aliases,
          varTmpName,
          fields))));
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.BlockStatement>> body = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
      java.util.List.of(
        tagDeclStmt.get(),
        tagReturnStmt.get()),
      valueCompareStmt.get()));
    java.util.List<hydra.ext.java.syntax.MethodModifier> mods = java.util.List.of(new hydra.ext.java.syntax.MethodModifier.Public());
    hydra.ext.java.syntax.FormalParameter param = hydra.ext.java.utils.Utils.javaTypeToJavaFormalParameter(
      hydra.ext.java.utils.Utils.javaTypeFromTypeName(
        aliases,
        parentName),
      new hydra.core.Name(hydra.ext.java.names.Names.otherInstanceName()));
    hydra.ext.java.syntax.Result result = hydra.ext.java.utils.Utils.javaTypeToJavaResult(hydra.ext.java.utils.Utils.javaIntType());
    return hydra.ext.java.utils.Utils.methodDeclaration(
      mods,
      (java.util.List<hydra.ext.java.syntax.TypeParameter>) (java.util.List.<hydra.ext.java.syntax.TypeParameter>of()),
      anns,
      hydra.ext.java.names.Names.compareToMethodName(),
      java.util.List.of(param),
      result,
      hydra.util.Maybe.just(body.get()));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclaration> recordMemberVar(hydra.ext.java.helpers.Aliases aliases, hydra.core.FieldType ft) {
    hydra.core.Name fname = (ft).name;
    hydra.core.Type ftype = (ft).type;
    java.util.List<hydra.ext.java.syntax.FieldModifier> mods = java.util.List.of(
      new hydra.ext.java.syntax.FieldModifier.Public(),
      new hydra.ext.java.syntax.FieldModifier.Final());
    return hydra.lib.flows.Bind.apply(
      hydra.ext.java.coder.Coder.encodeType(
        aliases,
        (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
        ftype),
      (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclaration>>) (jt -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMemberField(
        mods,
        jt,
        hydra.ext.java.utils.Utils.fieldNameToJavaVariableDeclarator(fname)))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclaration> recordWithMethod(hydra.ext.java.helpers.Aliases aliases, hydra.core.Name elName, java.util.List<hydra.core.FieldType> fields, hydra.core.FieldType field) {
    hydra.ext.java.syntax.Identifier consId = new hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.Utils.sanitizeJavaName(hydra.names.Names.localNameOf(elName)));
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.Expression>> fieldArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.FieldType, hydra.ext.java.syntax.Expression>) (f -> hydra.ext.java.utils.Utils.fieldNameToJavaExpression((f).name)),
      fields));
    String methodName = hydra.lib.strings.Cat2.apply(
      "with",
      hydra.formatting.Formatting.nonAlnumToUnderscores(hydra.formatting.Formatting.capitalize(((field).name).value)));
    java.util.List<hydra.ext.java.syntax.MethodModifier> mods = java.util.List.of(new hydra.ext.java.syntax.MethodModifier.Public());
    hydra.util.Lazy<hydra.ext.java.syntax.Result> result = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.referenceTypeToResult(hydra.ext.java.utils.Utils.nameToJavaReferenceType(
      aliases,
      false,
      (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()),
      elName,
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))));
    hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> returnStmt = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.utils.Utils.javaConstructorCall(
      hydra.ext.java.utils.Utils.javaConstructorName(
        consId,
        (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
      fieldArgs.get(),
      (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing()))))));
    return hydra.lib.flows.Bind.apply(
      hydra.ext.java.coder.Coder.fieldTypeToFormalParam(
        aliases,
        field),
      (java.util.function.Function<hydra.ext.java.syntax.FormalParameter, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclaration>>) (param -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.methodDeclaration(
        mods,
        (java.util.List<hydra.ext.java.syntax.TypeParameter>) (java.util.List.<hydra.ext.java.syntax.TypeParameter>of()),
        hydra.ext.java.coder.Coder.<hydra.ext.java.syntax.Annotation>recordWithMethod_anns(),
        methodName,
        java.util.List.of(param),
        result.get(),
        hydra.util.Maybe.just(java.util.List.of(returnStmt.get()))))));
  }
  
  static <T0> java.util.List<T0> recordWithMethod_anns() {
    return (java.util.List<T0>) (java.util.List.<T0>of());
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclaration> recordConstructor(hydra.ext.java.helpers.Aliases aliases, hydra.core.Name elName, java.util.List<hydra.core.FieldType> fields) {
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.BlockStatement>> assignStmts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.FieldType, hydra.ext.java.syntax.BlockStatement>) (f -> new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.toAssignStmt((f).name))),
      fields));
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapList.apply(
        (java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.FormalParameter>>) (f -> hydra.ext.java.coder.Coder.fieldTypeToFormalParam(
          aliases,
          f)),
        fields),
      (java.util.function.Function<java.util.List<hydra.ext.java.syntax.FormalParameter>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclaration>>) (params -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.makeConstructor(
        aliases,
        elName,
        false,
        params,
        assignStmts.get()))));
  }
  
  static hydra.ext.java.syntax.InclusiveOrExpression eqClause(String tmpName, hydra.core.FieldType ft) {
    String fname = ((ft).name).value;
    hydra.core.Type ftype = (ft).type;
    return hydra.lib.logic.IfElse.lazy(
      hydra.ext.java.coder.Coder.isBinaryType(ftype),
      () -> hydra.ext.java.coder.Coder.arraysEqualsClause(
        tmpName,
        fname),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.ext.java.coder.Coder.isBigNumericType(ftype),
        () -> hydra.ext.java.coder.Coder.compareToZeroClause(
          tmpName,
          fname),
        () -> hydra.ext.java.coder.Coder.equalsClause(
          tmpName,
          fname)));
  }
  
  static hydra.ext.java.syntax.InclusiveOrExpression equalsClause(String tmpName, String fname) {
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation_Header> header = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Type(hydra.ext.java.utils.Utils.javaTypeName(new hydra.ext.java.syntax.Identifier("java.util.Objects"))), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.equalsMethodName()))));
    hydra.ext.java.syntax.Expression otherArg = hydra.ext.java.utils.Utils.javaExpressionNameToJavaExpression(hydra.ext.java.utils.Utils.fieldExpression(
      hydra.ext.java.utils.Utils.javaIdentifier(tmpName),
      hydra.ext.java.utils.Utils.javaIdentifier(fname)));
    hydra.ext.java.syntax.Expression thisArg = hydra.ext.java.utils.Utils.javaExpressionNameToJavaExpression(hydra.ext.java.utils.Utils.fieldExpression(
      new hydra.ext.java.syntax.Identifier("this"),
      hydra.ext.java.utils.Utils.javaIdentifier(fname)));
    return hydra.ext.java.utils.Utils.javaPostfixExpressionToJavaInclusiveOrExpression(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaPostfixExpression(new hydra.ext.java.syntax.MethodInvocation(header.get(), java.util.List.of(
      thisArg,
      otherArg))));
  }
  
  static hydra.ext.java.syntax.InclusiveOrExpression arraysEqualsClause(String tmpName, String fname) {
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation_Header> header = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Type(hydra.ext.java.utils.Utils.javaTypeName(new hydra.ext.java.syntax.Identifier("java.util.Arrays"))), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.equalsMethodName()))));
    hydra.ext.java.syntax.Expression otherArg = hydra.ext.java.utils.Utils.javaExpressionNameToJavaExpression(hydra.ext.java.utils.Utils.fieldExpression(
      hydra.ext.java.utils.Utils.javaIdentifier(tmpName),
      hydra.ext.java.utils.Utils.javaIdentifier(fname)));
    hydra.ext.java.syntax.Expression thisArg = hydra.ext.java.utils.Utils.javaExpressionNameToJavaExpression(hydra.ext.java.utils.Utils.fieldExpression(
      new hydra.ext.java.syntax.Identifier("this"),
      hydra.ext.java.utils.Utils.javaIdentifier(fname)));
    return hydra.ext.java.utils.Utils.javaPostfixExpressionToJavaInclusiveOrExpression(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaPostfixExpression(new hydra.ext.java.syntax.MethodInvocation(header.get(), java.util.List.of(
      thisArg,
      otherArg))));
  }
  
  static hydra.ext.java.syntax.InclusiveOrExpression compareToZeroClause(String tmpName, String fname) {
    hydra.ext.java.syntax.Expression compareToArg = hydra.ext.java.utils.Utils.javaExpressionNameToJavaExpression(hydra.ext.java.utils.Utils.fieldExpression(
      hydra.ext.java.utils.Utils.javaIdentifier(tmpName),
      hydra.ext.java.utils.Utils.javaIdentifier(fname)));
    hydra.ext.java.syntax.MethodInvocation_Variant compareToVar = new hydra.ext.java.syntax.MethodInvocation_Variant.Expression(hydra.ext.java.utils.Utils.fieldExpression(
      new hydra.ext.java.syntax.Identifier("this"),
      hydra.ext.java.utils.Utils.javaIdentifier(fname)));
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation_Header> compareToHeader = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(compareToVar, (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.compareToMethodName()))));
    hydra.ext.java.syntax.EqualityExpression lhs = hydra.ext.java.utils.Utils.javaRelationalExpressionToJavaEqualityExpression(hydra.ext.java.utils.Utils.javaPostfixExpressionToJavaRelationalExpression(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaPostfixExpression(new hydra.ext.java.syntax.MethodInvocation(compareToHeader.get(), java.util.List.of(compareToArg)))));
    hydra.ext.java.syntax.RelationalExpression rhs = hydra.ext.java.utils.Utils.javaPostfixExpressionToJavaRelationalExpression(new hydra.ext.java.syntax.PostfixExpression.Primary(hydra.ext.java.utils.Utils.javaLiteralToJavaPrimary(hydra.ext.java.utils.Utils.javaInt(new java.math.BigInteger("0")))));
    return hydra.ext.java.utils.Utils.javaEqualityExpressionToJavaInclusiveOrExpression(new hydra.ext.java.syntax.EqualityExpression.Equal(new hydra.ext.java.syntax.EqualityExpression_Binary(lhs, rhs)));
  }
  
  static hydra.ext.java.syntax.ClassBodyDeclaration recordEqualsMethod(hydra.ext.java.helpers.Aliases aliases, hydra.core.Name elName, java.util.List<hydra.core.FieldType> fields) {
    java.util.List<hydra.ext.java.syntax.Annotation> anns = java.util.List.of(hydra.ext.java.utils.Utils.overrideAnnotation());
    String tmpName = "o";
    hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> castStmt = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.variableDeclarationStatement(
      aliases,
      hydra.ext.java.utils.Utils.javaTypeFromTypeName(
        aliases,
        elName),
      hydra.ext.java.utils.Utils.javaIdentifier(tmpName),
      hydra.ext.java.utils.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.Utils.javaCastExpression(
        hydra.ext.java.utils.Utils.nameToJavaReferenceType(
          aliases,
          false,
          (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()),
          elName,
          (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
        hydra.ext.java.utils.Utils.javaIdentifierToJavaUnaryExpression(new hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.Utils.sanitizeJavaName(hydra.ext.java.names.Names.otherInstanceName())))))));
    hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> instanceOfStmt = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.BlockStatement.Statement(new hydra.ext.java.syntax.Statement.IfThen(new hydra.ext.java.syntax.IfThenStatement(hydra.ext.java.utils.Utils.javaUnaryExpressionToJavaExpression(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Not(hydra.ext.java.utils.Utils.javaRelationalExpressionToJavaUnaryExpression(hydra.ext.java.utils.Utils.javaInstanceOf(
      hydra.ext.java.utils.Utils.javaIdentifierToJavaRelationalExpression(hydra.ext.java.utils.Utils.javaIdentifier(hydra.ext.java.names.Names.otherInstanceName())),
      hydra.ext.java.utils.Utils.nameToJavaReferenceType(
        aliases,
        false,
        (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()),
        elName,
        (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))))))), hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.utils.Utils.javaBooleanExpression(false)))))));
    java.util.List<hydra.ext.java.syntax.MethodModifier> mods = java.util.List.of(new hydra.ext.java.syntax.MethodModifier.Public());
    hydra.util.Lazy<hydra.ext.java.syntax.FormalParameter> param = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaTypeToJavaFormalParameter(
      hydra.ext.java.utils.Utils.javaRefType(
        (java.util.List<hydra.ext.java.syntax.ReferenceType>) (java.util.List.<hydra.ext.java.syntax.ReferenceType>of()),
        (hydra.util.Maybe<hydra.ext.java.syntax.PackageName>) (hydra.util.Maybe.<hydra.ext.java.syntax.PackageName>nothing()),
        "Object"),
      new hydra.core.Name(hydra.ext.java.names.Names.otherInstanceName())));
    hydra.ext.java.syntax.Result result = hydra.ext.java.utils.Utils.javaTypeToJavaResult(hydra.ext.java.utils.Utils.javaBooleanType());
    hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> returnAllFieldsEqual = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(fields),
      () -> hydra.ext.java.utils.Utils.javaBooleanExpression(true),
      () -> hydra.ext.java.utils.Utils.javaConditionalAndExpressionToJavaExpression(new hydra.ext.java.syntax.ConditionalAndExpression(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.FieldType, hydra.ext.java.syntax.InclusiveOrExpression>) (f -> hydra.ext.java.coder.Coder.eqClause(
          tmpName,
          f)),
        fields))))))));
    return hydra.ext.java.utils.Utils.methodDeclaration(
      mods,
      (java.util.List<hydra.ext.java.syntax.TypeParameter>) (java.util.List.<hydra.ext.java.syntax.TypeParameter>of()),
      anns,
      hydra.ext.java.names.Names.equalsMethodName(),
      java.util.List.of(param.get()),
      result,
      hydra.util.Maybe.just(java.util.List.of(
        instanceOfStmt.get(),
        castStmt.get(),
        returnAllFieldsEqual.get())));
  }
  
  static hydra.ext.java.syntax.MultiplicativeExpression hashCodeMultPair(java.math.BigInteger i, hydra.core.Name fname) {
    String fnameStr = (fname).value;
    hydra.ext.java.syntax.MultiplicativeExpression lhs = new hydra.ext.java.syntax.MultiplicativeExpression.Unary(hydra.ext.java.utils.Utils.javaPrimaryToJavaUnaryExpression(hydra.ext.java.utils.Utils.javaLiteralToJavaPrimary(hydra.ext.java.utils.Utils.javaInt(i))));
    hydra.util.Lazy<hydra.ext.java.syntax.UnaryExpression> rhs = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaPostfixExpressionToJavaUnaryExpression(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaPostfixExpression(new hydra.ext.java.syntax.MethodInvocation(new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Type(hydra.ext.java.utils.Utils.javaTypeName(new hydra.ext.java.syntax.Identifier("java.util.Objects"))), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.hashCodeMethodName()))), java.util.List.of(hydra.ext.java.utils.Utils.javaExpressionNameToJavaExpression(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.Utils.sanitizeJavaName(fnameStr)))))))));
    return new hydra.ext.java.syntax.MultiplicativeExpression.Times(new hydra.ext.java.syntax.MultiplicativeExpression_Binary(lhs, rhs.get()));
  }
  
  static java.util.List<java.math.BigInteger> first20Primes() {
    return java.util.List.of(
      new java.math.BigInteger("2"),
      new java.math.BigInteger("3"),
      new java.math.BigInteger("5"),
      new java.math.BigInteger("7"),
      new java.math.BigInteger("11"),
      new java.math.BigInteger("13"),
      new java.math.BigInteger("17"),
      new java.math.BigInteger("19"),
      new java.math.BigInteger("23"),
      new java.math.BigInteger("29"),
      new java.math.BigInteger("31"),
      new java.math.BigInteger("37"),
      new java.math.BigInteger("41"),
      new java.math.BigInteger("43"),
      new java.math.BigInteger("47"),
      new java.math.BigInteger("53"),
      new java.math.BigInteger("59"),
      new java.math.BigInteger("61"),
      new java.math.BigInteger("67"),
      new java.math.BigInteger("71"));
  }
  
  static hydra.ext.java.syntax.ClassBodyDeclaration recordHashCodeMethod(java.util.List<hydra.core.FieldType> fields) {
    java.util.List<hydra.ext.java.syntax.Annotation> anns = java.util.List.of(hydra.ext.java.utils.Utils.overrideAnnotation());
    java.util.List<hydra.ext.java.syntax.MethodModifier> mods = java.util.List.of(new hydra.ext.java.syntax.MethodModifier.Public());
    hydra.ext.java.syntax.Result result = hydra.ext.java.utils.Utils.javaTypeToJavaResult(hydra.ext.java.utils.Utils.javaIntType());
    hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> returnSum = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.BlockStatement.Statement(hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(fields),
      () -> hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.utils.Utils.javaIntExpression(new java.math.BigInteger("0")))),
      () -> hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.utils.Utils.javaAdditiveExpressionToJavaExpression(hydra.ext.java.utils.Utils.addExpressions(hydra.lib.lists.ZipWith.apply(
        (java.util.function.Function<java.math.BigInteger, java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.MultiplicativeExpression>>) (p0 -> p1 -> hydra.ext.java.coder.Coder.hashCodeMultPair(
          p0,
          p1)),
        hydra.ext.java.coder.Coder.first20Primes(),
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.FieldType, hydra.core.Name>) (f -> (f).name),
          fields)))))))));
    return hydra.ext.java.utils.Utils.methodDeclaration(
      mods,
      (java.util.List<hydra.ext.java.syntax.TypeParameter>) (java.util.List.<hydra.ext.java.syntax.TypeParameter>of()),
      anns,
      hydra.ext.java.names.Names.hashCodeMethodName(),
      (java.util.List<hydra.ext.java.syntax.FormalParameter>) (java.util.List.<hydra.ext.java.syntax.FormalParameter>of()),
      result,
      hydra.util.Maybe.just(java.util.List.of(returnSum.get())));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> constantDecl(String javaName, hydra.ext.java.helpers.Aliases aliases, hydra.core.Name name) {
    java.util.List<hydra.ext.java.syntax.FieldModifier> mods = java.util.List.of(
      new hydra.ext.java.syntax.FieldModifier.Public(),
      new hydra.ext.java.syntax.FieldModifier.Static(),
      new hydra.ext.java.syntax.FieldModifier.Final());
    hydra.ext.java.syntax.Identifier nameName = hydra.ext.java.utils.Utils.nameToJavaName(
      aliases,
      new hydra.core.Name("hydra.core.Name"));
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (g -> hydra.lib.flows.Bind.apply(
        hydra.schemas.Schemas.graphToTypeContext(g),
        (java.util.function.Function<hydra.typing.TypeContext, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (tc -> {
          hydra.ext.java.helpers.JavaEnvironment env = new hydra.ext.java.helpers.JavaEnvironment(aliases, tc);
          return hydra.lib.flows.Bind.apply(
            hydra.ext.java.coder.Coder.encodeType(
              aliases,
              (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
              new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name"))),
            (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (jt -> hydra.lib.flows.Bind.apply(
              hydra.ext.java.coder.Coder.encodeTerm(
                env,
                new hydra.core.Term.Literal(new hydra.core.Literal.String_((name).value))),
              (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (arg -> {
                hydra.util.Lazy<hydra.ext.java.syntax.VariableInitializer> init = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.VariableInitializer.Expression(hydra.ext.java.utils.Utils.javaConstructorCall(
                  hydra.ext.java.utils.Utils.javaConstructorName(
                    nameName,
                    (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
                  java.util.List.of(arg),
                  (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing()))));
                hydra.ext.java.syntax.VariableDeclarator var = hydra.ext.java.utils.Utils.javaVariableDeclarator(
                  new hydra.ext.java.syntax.Identifier(javaName),
                  hydra.util.Maybe.just(init.get()));
                return hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.noComment(hydra.ext.java.utils.Utils.javaMemberField(
                  mods,
                  jt,
                  var)));
              }))));
        }))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> constantDeclForFieldType(hydra.ext.java.helpers.Aliases aliases, hydra.core.FieldType ftyp) {
    hydra.core.Name name = (ftyp).name;
    String javaName = hydra.lib.strings.Cat2.apply(
      "FIELD_NAME_",
      hydra.formatting.Formatting.nonAlnumToUnderscores(hydra.formatting.Formatting.convertCase(
        new hydra.util.CaseConvention.Camel(),
        new hydra.util.CaseConvention.UpperSnake(),
        (name).value)));
    return hydra.ext.java.coder.Coder.constantDecl(
      javaName,
      aliases,
      name);
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> constantDeclForTypeName(hydra.ext.java.helpers.Aliases aliases, hydra.core.Name name) {
    return hydra.ext.java.coder.Coder.constantDecl(
      "TYPE_NAME",
      aliases,
      name);
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration> declarationForRecordType(Boolean isInner, Boolean isSer, hydra.ext.java.helpers.Aliases aliases, java.util.List<hydra.ext.java.syntax.TypeParameter> tparams, hydra.core.Name elName, java.util.List<hydra.core.FieldType> fields) {
    return hydra.ext.java.coder.Coder.declarationForRecordType_(
      isInner,
      isSer,
      aliases,
      tparams,
      elName,
      (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing()),
      fields);
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration> declarationForRecordType_(Boolean isInner, Boolean isSer, hydra.ext.java.helpers.Aliases aliases, java.util.List<hydra.ext.java.syntax.TypeParameter> tparams, hydra.core.Name elName, hydra.util.Maybe<hydra.core.Name> parentName, java.util.List<hydra.core.FieldType> fields) {
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapList.apply(
        (java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclaration>>) (f -> hydra.ext.java.coder.Coder.recordMemberVar(
          aliases,
          f)),
        fields),
      (java.util.function.Function<java.util.List<hydra.ext.java.syntax.ClassBodyDeclaration>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration>>) (memberVars -> hydra.lib.flows.Bind.apply(
        hydra.lib.flows.MapList.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.ext.java.syntax.ClassBodyDeclaration, hydra.core.FieldType>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (p -> hydra.ext.java.coder.Coder.addComment(
            hydra.lib.pairs.First.apply(p),
            hydra.lib.pairs.Second.apply(p))),
          hydra.lib.lists.Zip.apply(
            memberVars,
            fields)),
        (java.util.function.Function<java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration>>) (memberVars_ -> hydra.lib.flows.Bind.apply(
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Gt.apply(
              hydra.lib.lists.Length.apply(fields),
              1),
            () -> hydra.lib.flows.MapList.apply(
              (java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclaration>>) (f -> hydra.ext.java.coder.Coder.recordWithMethod(
                aliases,
                elName,
                fields,
                f)),
              fields),
            () -> hydra.lib.flows.Pure.apply((java.util.List<hydra.ext.java.syntax.ClassBodyDeclaration>) (java.util.List.<hydra.ext.java.syntax.ClassBodyDeclaration>of()))),
          (java.util.function.Function<java.util.List<hydra.ext.java.syntax.ClassBodyDeclaration>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration>>) (withMethods -> hydra.lib.flows.Bind.apply(
            hydra.ext.java.coder.Coder.recordConstructor(
              aliases,
              elName,
              fields),
            (java.util.function.Function<hydra.ext.java.syntax.ClassBodyDeclaration, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration>>) (cons -> hydra.lib.flows.Bind.apply(
              hydra.lib.logic.IfElse.lazy(
                isInner,
                () -> hydra.lib.flows.Pure.apply((java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>) (java.util.List.<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>of())),
                () -> hydra.lib.flows.Bind.apply(
                  hydra.ext.java.coder.Coder.constantDeclForTypeName(
                    aliases,
                    elName),
                  (java.util.function.Function<hydra.ext.java.syntax.ClassBodyDeclarationWithComments, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>>) (d -> hydra.lib.flows.Bind.apply(
                    hydra.lib.flows.MapList.apply(
                      (java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (f -> hydra.ext.java.coder.Coder.constantDeclForFieldType(
                        aliases,
                        f)),
                      fields),
                    (java.util.function.Function<java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>>) (dfields -> hydra.lib.flows.Pure.apply(hydra.lib.lists.Cons.apply(
                      d,
                      dfields))))))),
              (java.util.function.Function<java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration>>) (tn -> {
                hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.ClassBodyDeclaration>> comparableMethods = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.apply(
                  parentName,
                  hydra.lib.logic.IfElse.lazy(
                    hydra.lib.logic.And.apply(
                      hydra.lib.logic.Not.apply(isInner),
                      isSer),
                    () -> java.util.List.of(hydra.ext.java.coder.Coder.recordCompareToMethod(
                      aliases,
                      tparams,
                      elName,
                      fields)),
                    () -> (java.util.List<hydra.ext.java.syntax.ClassBodyDeclaration>) (java.util.List.<hydra.ext.java.syntax.ClassBodyDeclaration>of())),
                  (java.util.function.Function<hydra.core.Name, java.util.List<hydra.ext.java.syntax.ClassBodyDeclaration>>) (pn -> hydra.lib.logic.IfElse.lazy(
                    isSer,
                    () -> java.util.List.of(hydra.ext.java.coder.Coder.variantCompareToMethod(
                      aliases,
                      tparams,
                      pn,
                      elName,
                      fields)),
                    () -> (java.util.List<hydra.ext.java.syntax.ClassBodyDeclaration>) (java.util.List.<hydra.ext.java.syntax.ClassBodyDeclaration>of())))));
                hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>> bodyDecls = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                  tn,
                  hydra.lib.lists.Concat2.apply(
                    memberVars_,
                    hydra.lib.lists.Map.apply(
                      (java.util.function.Function<hydra.ext.java.syntax.ClassBodyDeclaration, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>) (x -> hydra.ext.java.coder.Coder.noComment(x)),
                      hydra.lib.lists.Concat2.apply(
                        java.util.List.of(
                          cons,
                          hydra.ext.java.coder.Coder.recordEqualsMethod(
                            aliases,
                            elName,
                            fields),
                          hydra.ext.java.coder.Coder.recordHashCodeMethod(fields)),
                        hydra.lib.lists.Concat2.apply(
                          comparableMethods.get(),
                          withMethods))))));
                hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.InterfaceType>> ifaces = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                  isInner,
                  () -> hydra.ext.java.coder.Coder.serializableTypes(isSer),
                  () -> hydra.ext.java.coder.Coder.interfaceTypes(
                    isSer,
                    aliases,
                    tparams,
                    elName)));
                return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaClassDeclaration(
                  aliases,
                  tparams,
                  elName,
                  hydra.ext.java.coder.Coder.classModsPublic(),
                  (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing()),
                  ifaces.get(),
                  bodyDecls.get()));
              }))))))))));
  }
  
  static <T0> hydra.compute.Flow<T0, java.util.List<hydra.ext.java.syntax.TypeArgument>> takeTypeArgs(String label, Integer n, java.util.List<hydra.ext.java.syntax.Type> tyapps) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Lt.apply(
        hydra.lib.lists.Length.apply(tyapps),
        n),
      () -> hydra.monads.Monads.unexpected(
        hydra.lib.strings.Cat.apply(java.util.List.of(
          "needed type arguments for ",
          label,
          ", found too few")),
        "takeTypeArgs"),
      () -> hydra.lib.flows.MapList.apply(
        (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<T0, hydra.ext.java.syntax.TypeArgument>>) (jt -> hydra.lib.flows.Bind.apply(
          hydra.ext.java.utils.Utils.<T0>javaTypeToJavaReferenceType(jt),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<T0, hydra.ext.java.syntax.TypeArgument>>) (rt -> hydra.lib.flows.Pure.apply(new hydra.ext.java.syntax.TypeArgument.Reference(rt))))),
        hydra.lib.lists.Take.apply(
          n,
          tyapps)));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, Boolean> isFieldUnitType(hydra.core.Name typeName, hydra.core.Name fieldName) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, Boolean>>) (g -> hydra.lib.flows.Bind.apply(
        hydra.schemas.Schemas.graphToInferenceContext(g),
        (java.util.function.Function<hydra.typing.InferenceContext, hydra.compute.Flow<hydra.graph.Graph, Boolean>>) (ix -> {
          java.util.Map<hydra.core.Name, hydra.core.TypeScheme> schemaTypes = (ix).schemaTypes;
          return hydra.lib.maybes.Cases.apply(
            hydra.lib.maps.Lookup.apply(
              typeName,
              schemaTypes),
            hydra.lib.flows.Pure.apply(false),
            (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<hydra.graph.Graph, Boolean>>) (ts -> (hydra.rewriting.Rewriting.deannotateType((ts).type)).accept(new hydra.core.Type.PartialVisitor<>() {
              @Override
              public hydra.compute.Flow<hydra.graph.Graph, Boolean> otherwise(hydra.core.Type instance) {
                return hydra.lib.flows.Pure.apply(false);
              }
              
              @Override
              public hydra.compute.Flow<hydra.graph.Graph, Boolean> visit(hydra.core.Type.Union rt) {
                return hydra.lib.flows.Pure.apply(hydra.lib.maybes.Cases.apply(
                  hydra.lib.lists.Find.apply(
                    (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.lib.equality.Equal.apply(
                      (ft).name,
                      fieldName)),
                    ((rt).value).fields),
                  false,
                  (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.schemas.Schemas.isUnitType(hydra.rewriting.Rewriting.deannotateType((ft).type)))));
              }
            })));
        }))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> encodeTerm(hydra.ext.java.helpers.JavaEnvironment env, hydra.core.Term term) {
    return hydra.ext.java.coder.Coder.encodeTermInternal(
      env,
      (java.util.List<java.util.Map<hydra.core.Name, hydra.core.Term>>) (java.util.List.<java.util.Map<hydra.core.Name, hydra.core.Term>>of()),
      (java.util.List<hydra.ext.java.syntax.Type>) (java.util.List.<hydra.ext.java.syntax.Type>of()),
      term);
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> encodeTermInternal(hydra.ext.java.helpers.JavaEnvironment env, java.util.List<java.util.Map<hydra.core.Name, hydra.core.Term>> anns, java.util.List<hydra.ext.java.syntax.Type> tyapps, hydra.core.Term term) {
    hydra.ext.java.helpers.Aliases aliases = (env).aliases;
    java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>> encode = (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (t -> hydra.ext.java.coder.Coder.encodeTerm(
      env,
      t));
    hydra.typing.TypeContext tc = (env).typeContext;
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Term instance) {
        return hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.encodeLiteral(new hydra.core.Literal.String_("Unimplemented term variant")));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Annotated at) {
        return hydra.ext.java.coder.Coder.encodeTermInternal(
          env,
          hydra.lib.lists.Cons.apply(
            ((at).value).annotation,
            anns),
          tyapps,
          ((at).value).body);
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Application app) {
        return hydra.monads.Monads.withTrace(
          "encode application",
          hydra.ext.java.coder.Coder.encodeApplication(
            env,
            (app).value));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Either et) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.java.coder.Coder.takeTypeArgs(
            "either",
            2,
            tyapps),
          (java.util.function.Function<java.util.List<hydra.ext.java.syntax.TypeArgument>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (targs -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (term1 -> hydra.lib.flows.Bind.apply(
              (encode).apply(term1),
              (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (expr -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocationStaticWithTypeArgs(
                new hydra.ext.java.syntax.Identifier("hydra.util.Either"),
                new hydra.ext.java.syntax.Identifier("left"),
                targs,
                java.util.List.of(expr))))))),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (term1 -> hydra.lib.flows.Bind.apply(
              (encode).apply(term1),
              (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (expr -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocationStaticWithTypeArgs(
                new hydra.ext.java.syntax.Identifier("hydra.util.Either"),
                new hydra.ext.java.syntax.Identifier("right"),
                targs,
                java.util.List.of(expr))))))),
            (et).value)));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Function f) {
        return hydra.monads.Monads.withTrace(
          hydra.lib.strings.Cat2.apply(
            "encode function (",
            hydra.lib.strings.Cat2.apply(
              hydra.show.core.Core.function((f).value),
              ")")),
          ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
            hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Term>> combinedAnns = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
              (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, java.util.Map<hydra.core.Name, hydra.core.Term>>>) (acc -> (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, java.util.Map<hydra.core.Name, hydra.core.Term>>) (m -> hydra.lib.maps.Union.apply(
                acc,
                m))),
              (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())),
              anns));
            return hydra.lib.flows.Bind.apply(
              hydra.annotations.Annotations.getType(combinedAnns.get()),
              (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (mt -> hydra.lib.flows.Bind.apply(
                hydra.lib.maybes.Cases.apply(
                  mt,
                  hydra.lib.maybes.Cases.apply(
                    hydra.ext.java.coder.Coder.tryInferFunctionType((f).value),
                    hydra.coderUtils.CoderUtils.tryTypeOf(
                      "4",
                      tc,
                      term),
                    (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (inferredType -> hydra.lib.flows.Pure.apply(inferredType))),
                  (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (t -> hydra.lib.flows.Pure.apply(t))),
                (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (typ -> (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
                  @Override
                  public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Type instance) {
                    return hydra.ext.java.coder.Coder.encodeNullaryConstant(
                      env,
                      typ,
                      (f).value);
                  }
                  
                  @Override
                  public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Type.Function ft) {
                    return hydra.ext.java.coder.Coder.encodeFunction(
                      env,
                      ((ft).value).domain,
                      ((ft).value).codomain,
                      (f).value);
                  }
                })))));
          })).get());
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Let lt) {
        return hydra.monads.Monads.withTrace(
          "encode let as block",
          ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
            java.util.List<hydra.core.Binding> bindings = ((lt).value).bindings;
            return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
              hydra.core.Term body = ((lt).value).body;
              return hydra.lib.logic.IfElse.lazy(
                hydra.lib.lists.Null.apply(bindings),
                () -> (encode).apply(body),
                () -> hydra.lib.flows.Bind.apply(
                  hydra.ext.java.coder.Coder.bindingsToStatements(
                    env,
                    bindings),
                  (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.helpers.JavaEnvironment>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (bindResult -> {
                    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.BlockStatement>> bindingStmts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindResult));
                    hydra.util.Lazy<hydra.ext.java.helpers.JavaEnvironment> env2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindResult));
                    return hydra.lib.flows.Bind.apply(
                      hydra.ext.java.coder.Coder.encodeTerm(
                        env2.get(),
                        body),
                      (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jbody -> {
                        hydra.ext.java.helpers.Aliases aliases2 = (env2.get()).aliases;
                        hydra.ext.java.syntax.BlockStatement returnSt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(jbody)));
                        hydra.util.Lazy<hydra.ext.java.syntax.Block> block = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.Block(hydra.lib.lists.Concat2.apply(
                          bindingStmts.get(),
                          java.util.List.of(returnSt))));
                        hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Term>> combinedAnns = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                          (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, java.util.Map<hydra.core.Name, hydra.core.Term>>>) (acc -> (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, java.util.Map<hydra.core.Name, hydra.core.Term>>) (m -> hydra.lib.maps.Union.apply(
                            acc,
                            m))),
                          (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())),
                          anns));
                        hydra.util.Lazy<hydra.ext.java.syntax.Expression> nullaryLambda = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.Expression.Lambda(new hydra.ext.java.syntax.LambdaExpression(new hydra.ext.java.syntax.LambdaParameters.Tuple((java.util.List<hydra.ext.java.syntax.LambdaParameters>) (java.util.List.<hydra.ext.java.syntax.LambdaParameters>of())), new hydra.ext.java.syntax.LambdaBody.Block(block.get()))));
                        hydra.typing.TypeContext tc2 = (env2.get()).typeContext;
                        return hydra.lib.flows.Bind.apply(
                          hydra.annotations.Annotations.getType(combinedAnns.get()),
                          (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (mt -> hydra.lib.flows.Bind.apply(
                            hydra.lib.maybes.Cases.apply(
                              mt,
                              hydra.coderUtils.CoderUtils.tryTypeOf(
                                "let-body",
                                tc2,
                                body),
                              (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (t -> hydra.lib.flows.Pure.apply(t))),
                            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (letType -> hydra.lib.flows.Bind.apply(
                              hydra.ext.java.coder.Coder.encodeType(
                                aliases2,
                                (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                                letType),
                              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jLetType -> hydra.lib.flows.Bind.apply(
                                hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jLetType),
                                (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (rt -> {
                                  hydra.ext.java.syntax.ReferenceType supplierRt = new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Class_(hydra.ext.java.utils.Utils.javaClassType(
                                    java.util.List.of(rt),
                                    hydra.ext.java.names.Names.javaUtilFunctionPackageName(),
                                    "Supplier")));
                                  hydra.ext.java.syntax.Expression castExpr = hydra.ext.java.utils.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.Utils.javaCastExpression(
                                    supplierRt,
                                    hydra.ext.java.utils.Utils.javaExpressionToJavaUnaryExpression(nullaryLambda.get())));
                                  return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocation(
                                    hydra.util.Maybe.just((hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>) ((hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>) (hydra.util.Either.<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>right(hydra.ext.java.utils.Utils.javaExpressionToJavaPrimary(castExpr))))),
                                    new hydra.ext.java.syntax.Identifier("get"),
                                    (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of()))));
                                }))))))));
                      }));
                  })));
            })).get();
          })).get());
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.List els) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            encode,
            (els).value),
          (java.util.function.Function<java.util.List<hydra.ext.java.syntax.Expression>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jels -> hydra.lib.flows.Bind.apply(
            hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply(jels),
              () -> hydra.ext.java.coder.Coder.takeTypeArgs(
                "list",
                1,
                tyapps),
              () -> hydra.lib.flows.Pure.apply((java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()))),
            (java.util.function.Function<java.util.List<hydra.ext.java.syntax.TypeArgument>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (targs -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocationStaticWithTypeArgs(
              new hydra.ext.java.syntax.Identifier("java.util.List"),
              new hydra.ext.java.syntax.Identifier("of"),
              targs,
              jels)))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Literal l) {
        return hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.encodeLiteral((l).value));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Map m) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            encode,
            hydra.lib.maps.Keys.apply((m).value)),
          (java.util.function.Function<java.util.List<hydra.ext.java.syntax.Expression>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jkeys -> hydra.lib.flows.Bind.apply(
            hydra.lib.flows.MapList.apply(
              encode,
              hydra.lib.maps.Elems.apply((m).value)),
            (java.util.function.Function<java.util.List<hydra.ext.java.syntax.Expression>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jvals -> {
              hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.Expression>> pairExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression>, hydra.ext.java.syntax.Expression>) (kv -> hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocationStatic(
                  new hydra.ext.java.syntax.Identifier("java.util.Map"),
                  new hydra.ext.java.syntax.Identifier("entry"),
                  java.util.List.of(
                    hydra.lib.pairs.First.apply(kv),
                    hydra.lib.pairs.Second.apply(kv))))),
                hydra.lib.lists.Zip.apply(
                  jkeys,
                  jvals)));
              return hydra.lib.flows.Bind.apply(
                hydra.lib.logic.IfElse.lazy(
                  hydra.lib.maps.Null.apply((m).value),
                  () -> hydra.ext.java.coder.Coder.takeTypeArgs(
                    "map",
                    2,
                    tyapps),
                  () -> hydra.lib.flows.Pure.apply((java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()))),
                (java.util.function.Function<java.util.List<hydra.ext.java.syntax.TypeArgument>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (targs -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocationStaticWithTypeArgs(
                  new hydra.ext.java.syntax.Identifier("java.util.Map"),
                  new hydra.ext.java.syntax.Identifier("ofEntries"),
                  targs,
                  pairExprs.get())))));
            }))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Cases.apply(
          (mt).value,
          hydra.lib.flows.Bind.apply(
            hydra.ext.java.coder.Coder.takeTypeArgs(
              "maybe",
              1,
              tyapps),
            (java.util.function.Function<java.util.List<hydra.ext.java.syntax.TypeArgument>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (targs -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocationStaticWithTypeArgs(
              new hydra.ext.java.syntax.Identifier("hydra.util.Maybe"),
              new hydra.ext.java.syntax.Identifier("nothing"),
              targs,
              (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of())))))),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (term1 -> hydra.lib.flows.Bind.apply(
            (encode).apply(term1),
            (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (expr -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocationStatic(
              new hydra.ext.java.syntax.Identifier("hydra.util.Maybe"),
              new hydra.ext.java.syntax.Identifier("just"),
              java.util.List.of(expr))))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Pair p) {
        return hydra.lib.flows.Bind.apply(
          (encode).apply(hydra.lib.pairs.First.apply((p).value)),
          (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jterm1 -> hydra.lib.flows.Bind.apply(
            (encode).apply(hydra.lib.pairs.Second.apply((p).value)),
            (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jterm2 -> hydra.lib.flows.Bind.apply(
              hydra.lib.logic.IfElse.lazy(
                hydra.lib.lists.Null.apply(tyapps),
                () -> hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
                () -> hydra.lib.flows.Bind.apply(
                  hydra.lib.flows.MapList.apply(
                    (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ReferenceType>>) (jt -> hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt)),
                    tyapps),
                  (java.util.function.Function<java.util.List<hydra.ext.java.syntax.ReferenceType>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>>>) (rts -> hydra.lib.flows.Pure.apply(hydra.util.Maybe.just(new hydra.ext.java.syntax.TypeArgumentsOrDiamond.Arguments(hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.TypeArgument>) (rt -> new hydra.ext.java.syntax.TypeArgument.Reference(rt)),
                    rts))))))),
              (java.util.function.Function<hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (mtargs -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaConstructorCall(
                hydra.ext.java.utils.Utils.javaConstructorName(
                  new hydra.ext.java.syntax.Identifier("hydra.util.Tuple.Tuple2"),
                  mtargs),
                java.util.List.of(
                  jterm1,
                  jterm2),
                (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing())))))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Record rec) {
        hydra.core.Name recName = ((rec).value).typeName;
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (fld -> (encode).apply((fld).term)),
            ((rec).value).fields),
          (java.util.function.Function<java.util.List<hydra.ext.java.syntax.Expression>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (fieldExprs -> {
            hydra.ext.java.syntax.Identifier consId = hydra.ext.java.utils.Utils.nameToJavaName(
              aliases,
              recName);
            return hydra.lib.flows.Bind.apply(
              hydra.lib.logic.IfElse.lazy(
                hydra.lib.lists.Null.apply(tyapps),
                () -> hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
                () -> hydra.lib.flows.Bind.apply(
                  hydra.lib.flows.MapList.apply(
                    (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ReferenceType>>) (jt -> hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt)),
                    tyapps),
                  (java.util.function.Function<java.util.List<hydra.ext.java.syntax.ReferenceType>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>>>) (rts -> hydra.lib.flows.Pure.apply(hydra.util.Maybe.just(new hydra.ext.java.syntax.TypeArgumentsOrDiamond.Arguments(hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.TypeArgument>) (rt -> new hydra.ext.java.syntax.TypeArgument.Reference(rt)),
                    rts))))))),
              (java.util.function.Function<hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (mtargs -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaConstructorCall(
                hydra.ext.java.utils.Utils.javaConstructorName(
                  consId,
                  mtargs),
                fieldExprs,
                (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing())))));
          }));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Set s) {
        hydra.util.Lazy<java.util.List<hydra.core.Term>> slist = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply((s).value));
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            encode,
            slist.get()),
          (java.util.function.Function<java.util.List<hydra.ext.java.syntax.Expression>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jels -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.sets.Null.apply((s).value),
            () -> hydra.lib.flows.Bind.apply(
              hydra.ext.java.coder.Coder.takeTypeArgs(
                "set",
                1,
                tyapps),
              (java.util.function.Function<java.util.List<hydra.ext.java.syntax.TypeArgument>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (targs -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocationStaticWithTypeArgs(
                new hydra.ext.java.syntax.Identifier("java.util.Set"),
                new hydra.ext.java.syntax.Identifier("of"),
                targs,
                (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of())))))),
            () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
              hydra.ext.java.syntax.Primary prim = hydra.ext.java.utils.Utils.javaMethodInvocationToJavaPrimary(hydra.ext.java.utils.Utils.methodInvocationStatic(
                new hydra.ext.java.syntax.Identifier("java.util.stream.Stream"),
                new hydra.ext.java.syntax.Identifier("of"),
                jels));
              return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
                hydra.util.Lazy<hydra.ext.java.syntax.Expression> coll = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocationStatic(
                  new hydra.ext.java.syntax.Identifier("java.util.stream.Collectors"),
                  new hydra.ext.java.syntax.Identifier("toSet"),
                  (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of()))));
                return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocation(
                  hydra.util.Maybe.just((hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>) ((hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>) (hydra.util.Either.<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>right(prim)))),
                  new hydra.ext.java.syntax.Identifier("collect"),
                  java.util.List.of(coll.get()))));
              })).get();
            })).get())));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.TypeLambda tl) {
        return hydra.ext.java.coder.Coder.withTypeLambda(
          env,
          (tl).value,
          (java.util.function.Function<hydra.ext.java.helpers.JavaEnvironment, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (env2 -> {
            hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Term>> combinedAnns = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
              (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, java.util.Map<hydra.core.Name, hydra.core.Term>>>) (acc -> (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, java.util.Map<hydra.core.Name, hydra.core.Term>>) (m -> hydra.lib.maps.Union.apply(
                acc,
                m))),
              (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())),
              anns));
            return hydra.lib.flows.Bind.apply(
              hydra.annotations.Annotations.getType(combinedAnns.get()),
              (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (mtyp -> {
                hydra.util.Lazy<hydra.core.Term> annotatedBody = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.apply(
                  mtyp,
                  ((tl).value).body,
                  (java.util.function.Function<hydra.core.Type, hydra.core.Term>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
                    @Override
                    public hydra.core.Term otherwise(hydra.core.Type instance) {
                      return ((tl).value).body;
                    }
                    
                    @Override
                    public hydra.core.Term visit(hydra.core.Type.Forall fa) {
                      return hydra.annotations.Annotations.setTermAnnotation(
                        hydra.constants.Constants.key_type(),
                        hydra.util.Maybe.just(hydra.encode.core.Core.type(((fa).value).body)),
                        ((tl).value).body);
                    }
                  }))));
                return hydra.ext.java.coder.Coder.encodeTerm(
                  env2,
                  annotatedBody.get());
              }));
          }));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Union inj) {
        hydra.core.Field injField = ((inj).value).field;
        hydra.core.Name injFieldName = (injField).name;
        hydra.core.Name injTypeName = ((inj).value).typeName;
        String typeId = (hydra.ext.java.utils.Utils.nameToJavaName(
          aliases,
          injTypeName)).value;
        hydra.ext.java.syntax.Identifier consId = new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Cat.apply(java.util.List.of(
          typeId,
          ".",
          hydra.ext.java.utils.Utils.sanitizeJavaName(hydra.formatting.Formatting.capitalize((injFieldName).value)))));
        hydra.core.Term injFieldTerm = (injField).term;
        return hydra.lib.flows.Bind.apply(
          hydra.ext.java.coder.Coder.isFieldUnitType(
            injTypeName,
            injFieldName),
          (java.util.function.Function<Boolean, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (fieldIsUnit -> hydra.lib.flows.Bind.apply(
            hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.Or.apply(
                hydra.schemas.Schemas.isUnitTerm(hydra.rewriting.Rewriting.deannotateTerm(injFieldTerm)),
                fieldIsUnit),
              () -> hydra.lib.flows.Pure.apply((java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of())),
              () -> hydra.lib.flows.Bind.apply(
                (encode).apply(injFieldTerm),
                (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.Expression>>>) (ex -> hydra.lib.flows.Pure.apply(java.util.List.of(ex))))),
            (java.util.function.Function<java.util.List<hydra.ext.java.syntax.Expression>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (args -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaConstructorCall(
              hydra.ext.java.utils.Utils.javaConstructorName(
                consId,
                (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
              args,
              (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing())))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Variable name) {
        return hydra.ext.java.coder.Coder.encodeVariable(
          env,
          (name).value);
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Unit ignored) {
        return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaLiteralToJavaExpression(new hydra.ext.java.syntax.Literal.Null()));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Wrap wt) {
        return hydra.lib.flows.Bind.apply(
          (encode).apply(((wt).value).body),
          (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jarg -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaConstructorCall(
            hydra.ext.java.utils.Utils.javaConstructorName(
              hydra.ext.java.utils.Utils.nameToJavaName(
                aliases,
                ((wt).value).typeName),
              (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
            java.util.List.of(jarg),
            (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing())))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.TypeApplication ta) {
        hydra.core.Type atyp = ((ta).value).type;
        hydra.core.Term body = ((ta).value).body;
        return hydra.lib.flows.Bind.apply(
          hydra.ext.java.coder.Coder.encodeType(
            aliases,
            (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
            atyp),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jatyp -> {
            hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Term>> combinedAnns = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
              (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, java.util.Map<hydra.core.Name, hydra.core.Term>>>) (acc -> (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, java.util.Map<hydra.core.Name, hydra.core.Term>>) (m -> hydra.lib.maps.Union.apply(
                acc,
                m))),
              (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())),
              anns));
            return hydra.lib.flows.Bind.apply(
              hydra.annotations.Annotations.getType(combinedAnns.get()),
              (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (mtyp -> hydra.lib.flows.Bind.apply(
                hydra.lib.maybes.Cases.apply(
                  mtyp,
                  hydra.coderUtils.CoderUtils.tryTypeOf(
                    "5",
                    tc,
                    term),
                  (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (t -> hydra.lib.flows.Pure.apply(t))),
                (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (typ -> {
                  hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Type>> collected0 = hydra.ext.java.coder.Coder.collectTypeApps0(
                    body,
                    java.util.List.of(atyp));
                  hydra.util.Lazy<java.util.List<hydra.core.Type>> allTypeArgs0 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(collected0));
                  hydra.util.Lazy<hydra.core.Term> innermostBody0 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(collected0));
                  return hydra.lib.flows.Bind.apply(
                    hydra.ext.java.coder.Coder.correctCastType(
                      innermostBody0.get(),
                      allTypeArgs0.get(),
                      typ),
                    (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (correctedTyp -> {
                      hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Type>> collected = hydra.ext.java.coder.Coder.collectTypeApps(
                        body,
                        java.util.List.of(atyp));
                      hydra.util.Lazy<java.util.List<hydra.core.Type>> allTypeArgs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(collected));
                      hydra.util.Lazy<hydra.core.Term> innermostBody = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(collected));
                      return (innermostBody.get()).accept(new hydra.core.Term.PartialVisitor<>() {
                        @Override
                        public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Term instance) {
                          return hydra.ext.java.coder.Coder.typeAppFallbackCast(
                            env,
                            aliases,
                            anns,
                            tyapps,
                            jatyp,
                            body,
                            correctedTyp);
                        }
                        
                        @Override
                        public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Variable varName) {
                          return hydra.lib.flows.Bind.apply(
                            hydra.ext.java.coder.Coder.classifyDataReference((varName).value),
                            (java.util.function.Function<hydra.ext.java.helpers.JavaSymbolClass, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (cls -> hydra.ext.java.coder.Coder.typeAppNullaryOrHoisted(
                              env,
                              aliases,
                              anns,
                              tyapps,
                              jatyp,
                              body,
                              correctedTyp,
                              (varName).value,
                              cls,
                              allTypeArgs.get())));
                        }
                      });
                    }));
                }))));
          }));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>> annotateLambdaArgs(hydra.core.Name cname, java.util.List<hydra.core.Type> tApps, java.util.List<hydra.core.Term> argTerms) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(tApps),
      () -> hydra.lib.flows.Pure.apply(argTerms),
      () -> hydra.lib.flows.Bind.apply(
        hydra.lib.flows.Bind.apply(
          hydra.lexical.Lexical.dereferenceElement(cname),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.TypeScheme>>>) (mel -> hydra.lib.maybes.Cases.apply(
            mel,
            hydra.lib.flows.Bind.apply(
              hydra.monads.Monads.<hydra.graph.Graph>getState(),
              (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.TypeScheme>>>) (g -> hydra.lib.flows.Pure.apply(hydra.lib.maybes.Map.apply(
                (java.util.function.Function<hydra.graph.Primitive, hydra.core.TypeScheme>) (prim -> (prim).type),
                hydra.lib.maps.Lookup.apply(
                  cname,
                  (g).primitives))))),
            (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.TypeScheme>>>) (el -> hydra.lib.flows.Pure.apply((el).type))))),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.TypeScheme>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>>>) (mts -> hydra.lib.maybes.Cases.apply(
          mts,
          hydra.lib.flows.Pure.apply(argTerms),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>>>) (ts -> {
            hydra.core.Type schemeType = (ts).type;
            java.util.Set<hydra.core.Name> schemeTypeVars = hydra.ext.java.coder.Coder.collectTypeVars(schemeType);
            hydra.util.Lazy<java.util.List<hydra.core.Name>> schemeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
              (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.sets.Member.apply(
                v,
                schemeTypeVars)),
              (ts).variables));
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.Or.apply(
                hydra.lib.lists.Null.apply(schemeVars.get()),
                hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
                  hydra.lib.lists.Length.apply(schemeVars.get()),
                  hydra.lib.lists.Length.apply(tApps)))),
              () -> hydra.lib.flows.Pure.apply(argTerms),
              () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>>>) (() -> {
                hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Type>> subst = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
                  schemeVars.get(),
                  tApps)));
                return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>>>) (() -> {
                  hydra.util.Lazy<java.util.List<hydra.core.Type>> expectedTypes = new hydra.util.Lazy<>(() -> hydra.ext.java.coder.Coder.peelExpectedTypes(
                    subst.get(),
                    hydra.lib.lists.Length.apply(argTerms),
                    schemeType));
                  return hydra.lib.flows.Pure.apply(hydra.lib.lists.ZipWith.apply(
                    (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Type, hydra.core.Term>>) (arg -> (java.util.function.Function<hydra.core.Type, hydra.core.Term>) (mExpected -> hydra.ext.java.coder.Coder.propagateType(
                      mExpected,
                      arg))),
                    argTerms,
                    hydra.lib.lists.Concat2.apply(
                      expectedTypes.get(),
                      hydra.lib.lists.Replicate.apply(
                        hydra.lib.lists.Length.apply(argTerms),
                        new hydra.core.Type.Variable(new hydra.core.Name("unused"))))));
                })).get();
              })).get());
          })))));
  }
  
  static hydra.ext.java.syntax.Expression applyJavaArg(hydra.ext.java.syntax.Expression expr, hydra.ext.java.syntax.Expression jarg) {
    return hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocation(
      hydra.util.Maybe.just((hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>) ((hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>) (hydra.util.Either.<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>right(hydra.ext.java.utils.Utils.javaExpressionToJavaPrimary(expr))))),
      new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.applyMethodName()),
      java.util.List.of(jarg)));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> encodeApplication(hydra.ext.java.helpers.JavaEnvironment env, hydra.core.Application app) {
    hydra.ext.java.helpers.Aliases aliases = (env).aliases;
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, java.util.List<hydra.core.Type>>>> gathered = new hydra.util.Lazy<>(() -> hydra.coderUtils.CoderUtils.gatherArgsWithTypeApps(
      new hydra.core.Term.Application(app),
      (java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()),
      (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of())));
    hydra.util.Lazy<java.util.List<hydra.core.Term>> args = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(gathered.get())));
    hydra.util.Lazy<hydra.core.Term> fun = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(gathered.get()));
    hydra.typing.TypeContext tc = (env).typeContext;
    hydra.util.Lazy<java.util.List<hydra.core.Type>> typeApps = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(gathered.get())));
    return hydra.lib.flows.Bind.apply(
      hydra.annotations.Annotations.getType(hydra.annotations.Annotations.termAnnotationInternal(fun.get())),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (mfunTyp -> hydra.lib.flows.Bind.apply(
        hydra.lib.maybes.Cases.apply(
          mfunTyp,
          hydra.coderUtils.CoderUtils.tryTypeOf(
            "1",
            tc,
            fun.get()),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (t -> hydra.lib.flows.Pure.apply(t))),
        (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (funTyp -> {
          Integer arity = hydra.arity.Arity.typeArity(funTyp);
          hydra.core.Term deannotatedFun = hydra.rewriting.Rewriting.deannotateTerm(fun.get());
          hydra.util.Lazy<hydra.util.Maybe<hydra.core.Name>> calleeName = new hydra.util.Lazy<>(() -> (deannotatedFun).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public hydra.util.Maybe<hydra.core.Name> otherwise(hydra.core.Term instance) {
              return (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing());
            }
            
            @Override
            public hydra.util.Maybe<hydra.core.Name> visit(hydra.core.Term.Function f) {
              return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
                @Override
                public hydra.util.Maybe<hydra.core.Name> otherwise(hydra.core.Function instance) {
                  return (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing());
                }
                
                @Override
                public hydra.util.Maybe<hydra.core.Name> visit(hydra.core.Function.Primitive n) {
                  return hydra.util.Maybe.just((n).value);
                }
              });
            }
            
            @Override
            public hydra.util.Maybe<hydra.core.Name> visit(hydra.core.Term.Variable n) {
              return hydra.util.Maybe.just((n).value);
            }
          }));
          return hydra.lib.flows.Bind.apply(
            hydra.lib.maybes.Cases.apply(
              calleeName.get(),
              hydra.lib.flows.Pure.apply(args.get()),
              (java.util.function.Function<hydra.core.Name, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>>>) (cname -> hydra.ext.java.coder.Coder.annotateLambdaArgs(
                cname,
                typeApps.get(),
                args.get()))),
            (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (annotatedArgs -> (deannotatedFun).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Term instance) {
                return hydra.ext.java.coder.Coder.encodeApplication_fallback(
                  env,
                  aliases,
                  tc,
                  typeApps.get(),
                  (app).function,
                  (app).argument);
              }
              
              @Override
              public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Function f) {
                return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
                  @Override
                  public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Function instance) {
                    return hydra.ext.java.coder.Coder.encodeApplication_fallback(
                      env,
                      aliases,
                      tc,
                      typeApps.get(),
                      (app).function,
                      (app).argument);
                  }
                  
                  @Override
                  public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Function.Primitive name) {
                    hydra.util.Lazy<java.util.List<hydra.core.Term>> hargs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Take.apply(
                      arity,
                      annotatedArgs));
                    hydra.util.Lazy<java.util.List<hydra.core.Term>> rargs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Drop.apply(
                      arity,
                      annotatedArgs));
                    return hydra.lib.flows.Bind.apply(
                      hydra.ext.java.coder.Coder.functionCall(
                        env,
                        true,
                        (name).value,
                        hargs.get(),
                        (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of())),
                      (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (initialCall -> hydra.lib.flows.Foldl.apply(
                        (java.util.function.Function<hydra.ext.java.syntax.Expression, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (h -> hydra.lib.flows.Bind.apply(
                          hydra.ext.java.coder.Coder.encodeTerm(
                            env,
                            h),
                          (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jarg -> hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.applyJavaArg(
                            acc,
                            jarg)))))),
                        initialCall,
                        rargs.get())));
                  }
                });
              }
              
              @Override
              public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Variable name) {
                return hydra.lib.logic.IfElse.lazy(
                  hydra.lib.logic.And.apply(
                    hydra.ext.java.coder.Coder.isRecursiveVariable(
                      aliases,
                      (name).value),
                    hydra.lib.logic.Not.apply(hydra.ext.java.coder.Coder.isLambdaBoundIn(
                      (name).value,
                      (aliases).lambdaVars))),
                  () -> hydra.ext.java.coder.Coder.encodeApplication_fallback(
                    env,
                    aliases,
                    tc,
                    typeApps.get(),
                    (app).function,
                    (app).argument),
                  () -> hydra.lib.flows.Bind.apply(
                    hydra.ext.java.coder.Coder.classifyDataReference((name).value),
                    (java.util.function.Function<hydra.ext.java.helpers.JavaSymbolClass, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (symClass -> {
                      java.util.Set<hydra.core.Name> inScope = (aliases).inScopeTypeParams;
                      java.util.Set<hydra.core.Name> trusted = (aliases).trustedTypeVars;
                      hydra.util.Lazy<java.util.List<hydra.core.Type>> filteredTypeApps = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                        hydra.lib.logic.Or.apply(
                          hydra.lib.sets.Null.apply(trusted),
                          hydra.lib.sets.Null.apply(inScope)),
                        () -> (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
                        () -> ((java.util.function.Supplier<java.util.List<hydra.core.Type>>) (() -> {
                          hydra.util.Lazy<java.util.Set<hydra.core.Name>> allVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.Unions.apply(hydra.lib.lists.Map.apply(
                            (java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>) (t -> hydra.ext.java.coder.Coder.collectTypeVars(t)),
                            typeApps.get())));
                          return hydra.lib.logic.IfElse.lazy(
                            hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.lib.sets.Difference.apply(
                              allVars.get(),
                              inScope))),
                            () -> (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
                            () -> hydra.lib.logic.IfElse.lazy(
                              hydra.lib.sets.Null.apply(hydra.lib.sets.Difference.apply(
                                allVars.get(),
                                trusted)),
                              () -> typeApps.get(),
                              () -> (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of())));
                        })).get()));
                      Integer methodArity = (symClass).accept(new hydra.ext.java.helpers.JavaSymbolClass.PartialVisitor<>() {
                        @Override
                        public Integer otherwise(hydra.ext.java.helpers.JavaSymbolClass instance) {
                          return arity;
                        }
                        
                        @Override
                        public Integer visit(hydra.ext.java.helpers.JavaSymbolClass.HoistedLambda n) {
                          return (n).value;
                        }
                      });
                      hydra.util.Lazy<java.util.List<hydra.core.Term>> hargs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Take.apply(
                        methodArity,
                        annotatedArgs));
                      hydra.util.Lazy<java.util.List<hydra.core.Term>> rargs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Drop.apply(
                        methodArity,
                        annotatedArgs));
                      return hydra.lib.flows.Bind.apply(
                        hydra.lib.logic.IfElse.lazy(
                          hydra.lib.lists.Null.apply(filteredTypeApps.get()),
                          () -> hydra.lib.flows.Pure.apply((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of())),
                          () -> hydra.ext.java.coder.Coder.correctTypeApps(
                            tc,
                            (name).value,
                            hargs.get(),
                            filteredTypeApps.get())),
                        (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (safeTypeApps -> hydra.lib.flows.Bind.apply(
                          hydra.ext.java.coder.Coder.filterPhantomTypeArgs(
                            (name).value,
                            safeTypeApps),
                          (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (finalTypeApps -> hydra.lib.flows.Bind.apply(
                            hydra.ext.java.coder.Coder.functionCall(
                              env,
                              false,
                              (name).value,
                              hargs.get(),
                              finalTypeApps),
                            (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (initialCall -> hydra.lib.flows.Foldl.apply(
                              (java.util.function.Function<hydra.ext.java.syntax.Expression, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (h -> hydra.lib.flows.Bind.apply(
                                hydra.ext.java.coder.Coder.encodeTerm(
                                  env,
                                  h),
                                (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jarg -> hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.applyJavaArg(
                                  acc,
                                  jarg)))))),
                              initialCall,
                              rargs.get())))))));
                    })));
              }
            })));
        }))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> encodeApplication_fallback(hydra.ext.java.helpers.JavaEnvironment env, hydra.ext.java.helpers.Aliases aliases, hydra.typing.TypeContext tc, java.util.List<hydra.core.Type> typeApps, hydra.core.Term lhs2, hydra.core.Term rhs2) {
    return hydra.monads.Monads.withTrace(
      "fallback",
      hydra.lib.flows.Bind.apply(
        hydra.annotations.Annotations.getType(hydra.annotations.Annotations.termAnnotationInternal(lhs2)),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (mt -> hydra.lib.flows.Bind.apply(
          hydra.lib.maybes.Cases.apply(
            mt,
            hydra.coderUtils.CoderUtils.tryTypeOf(
              "2",
              tc,
              lhs2),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (typ -> hydra.lib.flows.Pure.apply(typ))),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (t -> (hydra.rewriting.Rewriting.deannotateTypeParameters(hydra.rewriting.Rewriting.deannotateType(t))).accept(new hydra.core.Type.PartialVisitor<>() {
            @Override
            public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Type instance) {
              return hydra.monads.Monads.fail(hydra.lib.strings.Cat.apply(java.util.List.of(
                "Unexpected type: ",
                hydra.show.core.Core.type(t))));
            }
            
            @Override
            public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Type.Function ft) {
              hydra.core.Type cod = ((ft).value).codomain;
              hydra.core.Type dom = ((ft).value).domain;
              return (hydra.rewriting.Rewriting.deannotateTerm(lhs2)).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Term instance) {
                  return hydra.lib.flows.Bind.apply(
                    hydra.ext.java.coder.Coder.encodeTerm(
                      env,
                      lhs2),
                    (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jfun -> hydra.lib.flows.Bind.apply(
                      hydra.ext.java.coder.Coder.encodeTerm(
                        env,
                        rhs2),
                      (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jarg -> hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.applyJavaArg(
                        jfun,
                        jarg))))));
                }
                
                @Override
                public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Function f) {
                  return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
                    @Override
                    public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Function instance) {
                      return hydra.lib.flows.Bind.apply(
                        hydra.ext.java.coder.Coder.encodeTerm(
                          env,
                          lhs2),
                        (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jfun -> hydra.lib.flows.Bind.apply(
                          hydra.ext.java.coder.Coder.encodeTerm(
                            env,
                            rhs2),
                          (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jarg -> hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.applyJavaArg(
                            jfun,
                            jarg))))));
                    }
                    
                    @Override
                    public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Function.Elimination e) {
                      return hydra.lib.flows.Bind.apply(
                        hydra.ext.java.coder.Coder.encodeTerm(
                          env,
                          rhs2),
                        (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jarg -> hydra.lib.flows.Bind.apply(
                          hydra.lib.logic.IfElse.lazy(
                            hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.ext.java.coder.Coder.javaTypeArgumentsForType(dom))),
                            () -> hydra.lib.flows.Pure.apply(dom),
                            () -> hydra.lib.flows.Bind.apply(
                              hydra.annotations.Annotations.getType(hydra.annotations.Annotations.termAnnotationInternal(rhs2)),
                              (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (mrt -> hydra.lib.maybes.Cases.apply(
                                mrt,
                                hydra.lib.flows.Bind.apply(
                                  hydra.coderUtils.CoderUtils.tryTypeOf(
                                    "dom-enrich",
                                    tc,
                                    rhs2),
                                  (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (rt -> hydra.lib.flows.Pure.apply(hydra.lib.logic.IfElse.lazy(
                                    hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.ext.java.coder.Coder.javaTypeArgumentsForType(rt))),
                                    () -> rt,
                                    () -> dom)))),
                                (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (rt -> hydra.lib.flows.Pure.apply(hydra.lib.logic.IfElse.lazy(
                                  hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.ext.java.coder.Coder.javaTypeArgumentsForType(rt))),
                                  () -> rt,
                                  () -> dom))))))),
                          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (enrichedDom -> hydra.ext.java.coder.Coder.encodeElimination(
                            env,
                            hydra.util.Maybe.just(jarg),
                            enrichedDom,
                            cod,
                            (e).value)))));
                    }
                  });
                }
              });
            }
          }))))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> functionCall(hydra.ext.java.helpers.JavaEnvironment env, Boolean isPrim, hydra.core.Name name, java.util.List<hydra.core.Term> args, java.util.List<hydra.core.Type> typeApps) {
    hydra.ext.java.helpers.Aliases aliases = (env).aliases;
    Boolean isLambdaBound = hydra.ext.java.coder.Coder.isLambdaBoundIn(
      name,
      (aliases).lambdaVars);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        isPrim,
        hydra.lib.logic.And.apply(
          hydra.lib.lists.Null.apply(args),
          hydra.lib.logic.Not.apply(isLambdaBound))),
      () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
        String classWithApply = (hydra.ext.java.coder.Coder.elementJavaIdentifier(
          true,
          false,
          aliases,
          name)).value;
        return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
          String suffix = hydra.lib.strings.Cat2.apply(
            ".",
            hydra.ext.java.names.Names.applyMethodName());
          return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
            hydra.util.Lazy<String> className = new hydra.util.Lazy<>(() -> hydra.lib.strings.FromList.apply(hydra.lib.lists.Take.apply(
              hydra.lib.math.Sub.apply(
                hydra.lib.strings.Length.apply(classWithApply),
                hydra.lib.strings.Length.apply(suffix)),
              hydra.lib.strings.ToList.apply(classWithApply))));
            return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaIdentifierToJavaExpression(new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Cat.apply(java.util.List.of(
              className.get(),
              "::",
              hydra.ext.java.names.Names.applyMethodName())))));
          })).get();
        })).get();
      })).get(),
      () -> hydra.lib.flows.Bind.apply(
        hydra.lib.flows.MapList.apply(
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (arg -> hydra.ext.java.coder.Coder.encodeTerm(
            env,
            arg)),
          args),
        (java.util.function.Function<java.util.List<hydra.ext.java.syntax.Expression>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jargs0 -> {
          hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>> wrapResult = hydra.ext.java.coder.Coder.wrapLazyArguments(
            name,
            jargs0);
          hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.Expression>> jargs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(wrapResult));
          hydra.util.Lazy<hydra.util.Maybe<String>> mMethodOverride = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(wrapResult));
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.logic.Or.apply(
              hydra.ext.java.coder.Coder.isLocalVariable(name),
              isLambdaBound),
            () -> hydra.lib.flows.Bind.apply(
              hydra.ext.java.coder.Coder.encodeVariable(
                env,
                name),
              (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (baseExpr -> hydra.lib.flows.Pure.apply(hydra.lib.lists.Foldl.apply(
                (java.util.function.Function<hydra.ext.java.syntax.Expression, java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression>>) (acc -> (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression>) (jarg -> hydra.ext.java.coder.Coder.applyJavaArg(
                  acc,
                  jarg))),
                baseExpr,
                jargs.get())))),
            () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
              java.util.function.Function<hydra.ext.java.syntax.Identifier, hydra.ext.java.syntax.Identifier> overrideMethodName = (java.util.function.Function<hydra.ext.java.syntax.Identifier, hydra.ext.java.syntax.Identifier>) (jid -> hydra.lib.maybes.Cases.apply(
                mMethodOverride.get(),
                jid,
                (java.util.function.Function<String, hydra.ext.java.syntax.Identifier>) (m -> {
                  String s = (jid).value;
                  return new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Cat2.apply(
                    hydra.lib.strings.FromList.apply(hydra.lib.lists.Take.apply(
                      hydra.lib.math.Sub.apply(
                        hydra.lib.strings.Length.apply(s),
                        hydra.lib.strings.Length.apply(hydra.ext.java.names.Names.applyMethodName())),
                      hydra.lib.strings.ToList.apply(s))),
                    m));
                })));
              return hydra.lib.logic.IfElse.lazy(
                hydra.lib.lists.Null.apply(typeApps),
                () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
                  hydra.ext.java.syntax.MethodInvocation_Header header = new hydra.ext.java.syntax.MethodInvocation_Header.Simple(new hydra.ext.java.syntax.MethodName((overrideMethodName).apply(hydra.ext.java.coder.Coder.elementJavaIdentifier(
                    isPrim,
                    false,
                    aliases,
                    name))));
                  return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(header, jargs.get())));
                })).get(),
                () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
                  hydra.module.QualifiedName qn = hydra.names.Names.qualifyName(name);
                  return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
                    hydra.util.Maybe<hydra.module.Namespace> mns = (qn).namespace;
                    return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
                      String localName = (qn).local;
                      return hydra.lib.maybes.Cases.apply(
                        mns,
                        ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
                          hydra.ext.java.syntax.MethodInvocation_Header header = new hydra.ext.java.syntax.MethodInvocation_Header.Simple(new hydra.ext.java.syntax.MethodName((overrideMethodName).apply(hydra.ext.java.coder.Coder.elementJavaIdentifier(
                            isPrim,
                            false,
                            aliases,
                            name))));
                          return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(header, jargs.get())));
                        })).get(),
                        (java.util.function.Function<hydra.module.Namespace, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (ns_ -> {
                          hydra.ext.java.syntax.Identifier classId = hydra.ext.java.utils.Utils.nameToJavaName(
                            aliases,
                            hydra.names.Names.unqualifyName(new hydra.module.QualifiedName(hydra.util.Maybe.just(ns_), hydra.ext.java.coder.Coder.elementsClassName(ns_))));
                          hydra.util.Lazy<hydra.ext.java.syntax.Identifier> methodId = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                            isPrim,
                            () -> (overrideMethodName).apply(new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Cat2.apply(
                              (hydra.ext.java.utils.Utils.nameToJavaName(
                                aliases,
                                hydra.names.Names.unqualifyName(new hydra.module.QualifiedName(hydra.util.Maybe.just(ns_), hydra.formatting.Formatting.capitalize(localName))))).value,
                              hydra.lib.strings.Cat2.apply(
                                ".",
                                hydra.ext.java.names.Names.applyMethodName())))),
                            () -> new hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.Utils.sanitizeJavaName(localName))));
                          return hydra.lib.flows.Bind.apply(
                            hydra.lib.flows.MapList.apply(
                              (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.TypeArgument>>) (t -> hydra.lib.flows.Bind.apply(
                                hydra.ext.java.coder.Coder.encodeType(
                                  aliases,
                                  (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                                  t),
                                (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.TypeArgument>>) (jt -> hydra.lib.flows.Bind.apply(
                                  hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt),
                                  (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.TypeArgument>>) (rt -> hydra.lib.flows.Pure.apply(new hydra.ext.java.syntax.TypeArgument.Reference(rt))))))),
                              typeApps),
                            (java.util.function.Function<java.util.List<hydra.ext.java.syntax.TypeArgument>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jTypeArgs -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocationStaticWithTypeArgs(
                              classId,
                              methodId.get(),
                              jTypeArgs,
                              jargs.get())))));
                        }));
                    })).get();
                  })).get();
                })).get());
            })).get());
        })));
  }
  
  static hydra.ext.java.syntax.Expression buildCurriedLambda(java.util.List<hydra.core.Name> params, hydra.ext.java.syntax.Expression inner) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.ext.java.syntax.Expression, java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.Expression>>) (acc -> (java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.Expression>) (p -> hydra.ext.java.utils.Utils.javaLambda(
        p,
        acc))),
      inner,
      hydra.lib.lists.Reverse.apply(params));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> encodeFunction(hydra.ext.java.helpers.JavaEnvironment env, hydra.core.Type dom, hydra.core.Type cod, hydra.core.Function fun) {
    hydra.ext.java.helpers.Aliases aliases = (env).aliases;
    return (fun).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Function instance) {
        return hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.encodeLiteral(new hydra.core.Literal.String_(hydra.lib.strings.Cat2.apply(
          "Unimplemented function variant: ",
          hydra.show.core.Core.function(fun)))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Function.Elimination elm) {
        return hydra.monads.Monads.withTrace(
          hydra.lib.strings.Cat.apply(java.util.List.of(
            "elimination (",
            hydra.show.core.Core.elimination((elm).value),
            ")")),
          hydra.ext.java.coder.Coder.encodeElimination(
            env,
            (hydra.util.Maybe<hydra.ext.java.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.java.syntax.Expression>nothing()),
            dom,
            cod,
            (elm).value));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Function.Lambda lam) {
        return hydra.monads.Monads.withTrace(
          hydra.lib.strings.Cat2.apply(
            "lambda ",
            (((lam).value).parameter).value),
          hydra.ext.java.coder.Coder.withLambda(
            env,
            (lam).value,
            (java.util.function.Function<hydra.ext.java.helpers.JavaEnvironment, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (env2 -> {
              hydra.core.Term body = ((lam).value).body;
              hydra.core.Name lambdaVar = ((lam).value).parameter;
              return (hydra.rewriting.Rewriting.deannotateTerm(body)).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Term instance) {
                  return hydra.lib.flows.Bind.apply(
                    hydra.monads.Monads.withTrace(
                      "analyze function body",
                      hydra.ext.java.coder.Coder.analyzeJavaFunction(
                        env2,
                        body)),
                    (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (fs -> {
                      hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, java.util.List<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
                      hydra.util.Lazy<hydra.ext.java.helpers.JavaEnvironment> env3 = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, hydra.ext.java.helpers.JavaEnvironment>) (projected -> projected.environment)).apply(fs));
                      hydra.util.Lazy<hydra.core.Term> innerBody = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
                      return hydra.lib.flows.Bind.apply(
                        hydra.ext.java.coder.Coder.bindingsToStatements(
                          env3.get(),
                          bindings.get()),
                        (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.helpers.JavaEnvironment>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (bindResult -> {
                          hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.BlockStatement>> bindingStmts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindResult));
                          hydra.util.Lazy<hydra.ext.java.helpers.JavaEnvironment> env4 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindResult));
                          return hydra.lib.flows.Bind.apply(
                            hydra.ext.java.coder.Coder.encodeTerm(
                              env4.get(),
                              innerBody.get()),
                            (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jbody -> {
                              hydra.util.Lazy<hydra.ext.java.syntax.Expression> lam1 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                                hydra.lib.lists.Null.apply(bindings.get()),
                                () -> hydra.ext.java.utils.Utils.javaLambda(
                                  lambdaVar,
                                  jbody),
                                () -> ((java.util.function.Supplier<hydra.ext.java.syntax.Expression>) (() -> {
                                  hydra.ext.java.syntax.BlockStatement returnSt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(jbody)));
                                  return hydra.ext.java.utils.Utils.javaLambdaFromBlock(
                                    lambdaVar,
                                    new hydra.ext.java.syntax.Block(hydra.lib.lists.Concat2.apply(
                                      bindingStmts.get(),
                                      java.util.List.of(returnSt))));
                                })).get()));
                              return hydra.ext.java.coder.Coder.applyCastIfSafe(
                                aliases,
                                new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod)),
                                lam1.get());
                            }));
                        }));
                    }));
                }
                
                @Override
                public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Function f2) {
                  return ((f2).value).accept(new hydra.core.Function.PartialVisitor<>() {
                    @Override
                    public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Function instance) {
                      return hydra.lib.flows.Bind.apply(
                        hydra.monads.Monads.withTrace(
                          "analyze function body",
                          hydra.ext.java.coder.Coder.analyzeJavaFunction(
                            env2,
                            body)),
                        (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (fs -> {
                          hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, java.util.List<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
                          hydra.util.Lazy<hydra.ext.java.helpers.JavaEnvironment> env3 = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, hydra.ext.java.helpers.JavaEnvironment>) (projected -> projected.environment)).apply(fs));
                          hydra.util.Lazy<hydra.core.Term> innerBody = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
                          return hydra.lib.flows.Bind.apply(
                            hydra.ext.java.coder.Coder.bindingsToStatements(
                              env3.get(),
                              bindings.get()),
                            (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.helpers.JavaEnvironment>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (bindResult -> {
                              hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.BlockStatement>> bindingStmts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindResult));
                              hydra.util.Lazy<hydra.ext.java.helpers.JavaEnvironment> env4 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindResult));
                              return hydra.lib.flows.Bind.apply(
                                hydra.ext.java.coder.Coder.encodeTerm(
                                  env4.get(),
                                  innerBody.get()),
                                (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jbody -> {
                                  hydra.util.Lazy<hydra.ext.java.syntax.Expression> lam1 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                                    hydra.lib.lists.Null.apply(bindings.get()),
                                    () -> hydra.ext.java.utils.Utils.javaLambda(
                                      lambdaVar,
                                      jbody),
                                    () -> ((java.util.function.Supplier<hydra.ext.java.syntax.Expression>) (() -> {
                                      hydra.ext.java.syntax.BlockStatement returnSt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(jbody)));
                                      return hydra.ext.java.utils.Utils.javaLambdaFromBlock(
                                        lambdaVar,
                                        new hydra.ext.java.syntax.Block(hydra.lib.lists.Concat2.apply(
                                          bindingStmts.get(),
                                          java.util.List.of(returnSt))));
                                    })).get()));
                                  return hydra.ext.java.coder.Coder.applyCastIfSafe(
                                    aliases,
                                    new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod)),
                                    lam1.get());
                                }));
                            }));
                        }));
                    }
                    
                    @Override
                    public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Function.Lambda innerLam) {
                      return (hydra.rewriting.Rewriting.deannotateType(cod)).accept(new hydra.core.Type.PartialVisitor<>() {
                        @Override
                        public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Type instance) {
                          return hydra.monads.Monads.fail(hydra.lib.strings.Cat2.apply(
                            "expected function type for lambda body, but got: ",
                            hydra.show.core.Core.type(cod)));
                        }
                        
                        @Override
                        public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Type.Function ft) {
                          hydra.core.Type cod2 = ((ft).value).codomain;
                          hydra.core.Type dom2 = ((ft).value).domain;
                          return hydra.lib.flows.Bind.apply(
                            hydra.ext.java.coder.Coder.encodeFunction(
                              env2,
                              dom2,
                              cod2,
                              new hydra.core.Function.Lambda((innerLam).value)),
                            (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (innerJavaLambda -> {
                              hydra.ext.java.syntax.Expression lam1 = hydra.ext.java.utils.Utils.javaLambda(
                                lambdaVar,
                                innerJavaLambda);
                              return hydra.ext.java.coder.Coder.applyCastIfSafe(
                                aliases,
                                new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod)),
                                lam1);
                            }));
                        }
                      });
                    }
                  });
                }
              });
            })));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Function.Primitive name) {
        Integer arity = hydra.arity.Arity.typeArity(new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod)));
        String classWithApply = (hydra.ext.java.coder.Coder.elementJavaIdentifier(
          true,
          false,
          aliases,
          (name).value)).value;
        String suffix = hydra.lib.strings.Cat2.apply(
          ".",
          hydra.ext.java.names.Names.applyMethodName());
        hydra.util.Lazy<String> className = new hydra.util.Lazy<>(() -> hydra.lib.strings.FromList.apply(hydra.lib.lists.Take.apply(
          hydra.lib.math.Sub.apply(
            hydra.lib.strings.Length.apply(classWithApply),
            hydra.lib.strings.Length.apply(suffix)),
          hydra.lib.strings.ToList.apply(classWithApply))));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Lte.apply(
            arity,
            1),
          () -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaIdentifierToJavaExpression(new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Cat.apply(java.util.List.of(
            className.get(),
            "::",
            hydra.ext.java.names.Names.applyMethodName()))))),
          () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
            hydra.util.Lazy<java.util.List<hydra.core.Name>> paramNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<Integer, hydra.core.Name>) (i -> new hydra.core.Name(hydra.lib.strings.Cat2.apply(
                "p",
                hydra.lib.literals.ShowInt32.apply(i)))),
              hydra.lib.math.Range.apply(
                0,
                hydra.lib.math.Sub.apply(
                  arity,
                  1))));
            return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
              hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.Expression>> paramExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.Expression>) (p -> hydra.ext.java.utils.Utils.javaIdentifierToJavaExpression(hydra.ext.java.utils.Utils.variableToJavaIdentifier(p))),
                paramNames.get()));
              return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
                hydra.ext.java.syntax.Identifier classId = new hydra.ext.java.syntax.Identifier(className.get());
                return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
                  hydra.ext.java.syntax.Expression call = hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocationStatic(
                    classId,
                    new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.applyMethodName()),
                    paramExprs.get()));
                  return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
                    hydra.ext.java.syntax.Expression curried = hydra.ext.java.coder.Coder.buildCurriedLambda(
                      paramNames.get(),
                      call);
                    return hydra.lib.flows.Bind.apply(
                      hydra.ext.java.coder.Coder.encodeType(
                        aliases,
                        (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                        new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod))),
                      (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jtype -> hydra.lib.flows.Bind.apply(
                        hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jtype),
                        (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (rt -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.Utils.javaCastExpression(
                          rt,
                          hydra.ext.java.utils.Utils.javaExpressionToJavaUnaryExpression(curried))))))));
                  })).get();
                })).get();
              })).get();
            })).get();
          })).get());
      }
    });
  }
  
  static <T0> hydra.core.Type extractArgType(T0 _lhs, hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return typ;
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Application at1) {
        return (((at1).value).function).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.core.Type otherwise(hydra.core.Type instance) {
            return typ;
          }
          
          @Override
          public hydra.core.Type visit(hydra.core.Type.Application _at2) {
            return ((at1).value).argument;
          }
        });
      }
    });
  }
  
  static hydra.core.Term annotateBodyWithCod(hydra.core.Type typ, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Term, hydra.core.Term> setAnn = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> hydra.annotations.Annotations.setTermAnnotation(
      hydra.constants.Constants.key_type(),
      hydra.util.Maybe.just(hydra.encode.core.Core.type(typ)),
      t));
    return (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (setAnn).apply(term);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication _ta) {
        return (setAnn).apply(term);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Application app) {
        hydra.core.Term lhs = ((app).value).function;
        hydra.core.Term rhs = ((app).value).argument;
        hydra.util.Lazy<hydra.core.Term> annotatedRhs = new hydra.util.Lazy<>(() -> (hydra.rewriting.Rewriting.deannotateTerm(rhs)).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Term instance) {
            return rhs;
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.Term.TypeApplication _ta2) {
            return hydra.ext.java.coder.Coder.annotateBodyWithCod(
              hydra.ext.java.coder.Coder.extractArgType(
                lhs,
                typ),
              rhs);
          }
        }));
        return (setAnn).apply(new hydra.core.Term.Application(new hydra.core.Application(lhs, annotatedRhs.get())));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.TypeArgument>> domTypeArgs(hydra.ext.java.helpers.Aliases aliases, hydra.core.Type d) {
    java.util.List<hydra.core.Type> args = hydra.ext.java.coder.Coder.extractTypeApplicationArgs(hydra.rewriting.Rewriting.deannotateType(d));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(args)),
      () -> hydra.lib.flows.MapList.apply(
        (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.TypeArgument>>) (t -> hydra.lib.flows.Bind.apply(
          hydra.ext.java.coder.Coder.encodeType(
            aliases,
            (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
            t),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.TypeArgument>>) (jt -> hydra.lib.flows.Bind.apply(
            hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.TypeArgument>>) (rt -> hydra.lib.flows.Pure.apply(new hydra.ext.java.syntax.TypeArgument.Reference(rt))))))),
        args),
      () -> hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.javaTypeArgumentsForType(d)));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> otherwiseBranch(hydra.ext.java.helpers.JavaEnvironment env, hydra.ext.java.helpers.Aliases aliases, hydra.core.Type dom, hydra.core.Type cod, hydra.core.Name tname, hydra.ext.java.syntax.Type jcod, java.util.List<hydra.ext.java.syntax.TypeArgument> targs, hydra.core.Term d) {
    java.util.List<hydra.ext.java.syntax.Annotation> anns = java.util.List.of(hydra.ext.java.utils.Utils.overrideAnnotation());
    hydra.util.Lazy<hydra.ext.java.syntax.Type> jdom = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.utils.Utils.nameToJavaReferenceType(
      aliases,
      true,
      targs,
      tname,
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))));
    java.util.List<hydra.ext.java.syntax.MethodModifier> mods = java.util.List.of(new hydra.ext.java.syntax.MethodModifier.Public());
    hydra.ext.java.syntax.FormalParameter param = hydra.ext.java.utils.Utils.javaTypeToJavaFormalParameter(
      jdom.get(),
      new hydra.core.Name("instance"));
    hydra.ext.java.syntax.Result result = new hydra.ext.java.syntax.Result.Type(new hydra.ext.java.syntax.UnannType(jcod));
    return hydra.lib.flows.Bind.apply(
      hydra.ext.java.coder.Coder.analyzeJavaFunction(
        env,
        d),
      (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (fs -> {
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, java.util.List<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
        hydra.util.Lazy<hydra.ext.java.helpers.JavaEnvironment> env2 = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, hydra.ext.java.helpers.JavaEnvironment>) (projected -> projected.environment)).apply(fs));
        hydra.util.Lazy<hydra.core.Term> rawBody = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
        hydra.core.Term innerBody = hydra.ext.java.coder.Coder.annotateBodyWithCod(
          cod,
          rawBody.get());
        return hydra.lib.flows.Bind.apply(
          hydra.ext.java.coder.Coder.bindingsToStatements(
            env2.get(),
            bindings.get()),
          (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.helpers.JavaEnvironment>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (bindResult -> {
            hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.BlockStatement>> bindingStmts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindResult));
            hydra.util.Lazy<hydra.ext.java.helpers.JavaEnvironment> env3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindResult));
            return hydra.lib.flows.Bind.apply(
              hydra.ext.java.coder.Coder.encodeTerm(
                env3.get(),
                innerBody),
              (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (jret -> {
                hydra.ext.java.syntax.BlockStatement returnStmt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(jret)));
                hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.BlockStatement>> allStmts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                  bindingStmts.get(),
                  java.util.List.of(returnStmt)));
                return hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.noComment(hydra.ext.java.utils.Utils.methodDeclaration(
                  mods,
                  (java.util.List<hydra.ext.java.syntax.TypeParameter>) (java.util.List.<hydra.ext.java.syntax.TypeParameter>of()),
                  anns,
                  hydra.ext.java.names.Names.otherwiseMethodName(),
                  java.util.List.of(param),
                  result,
                  hydra.util.Maybe.just(allStmts.get()))));
              }));
          }));
      }));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> visitBranch(hydra.ext.java.helpers.JavaEnvironment env, hydra.ext.java.helpers.Aliases aliases, hydra.core.Type dom, hydra.core.Name tname, hydra.ext.java.syntax.Type jcod, java.util.List<hydra.ext.java.syntax.TypeArgument> targs, hydra.core.Field field) {
    java.util.List<hydra.ext.java.syntax.Annotation> anns = java.util.List.of(hydra.ext.java.utils.Utils.overrideAnnotation());
    hydra.ext.java.syntax.Type jdom = new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.utils.Utils.nameToJavaReferenceType(
      aliases,
      true,
      targs,
      tname,
      hydra.util.Maybe.just(hydra.formatting.Formatting.capitalize(((field).name).value))));
    java.util.List<hydra.ext.java.syntax.MethodModifier> mods = java.util.List.of(new hydra.ext.java.syntax.MethodModifier.Public());
    hydra.ext.java.syntax.Result result = new hydra.ext.java.syntax.Result.Type(new hydra.ext.java.syntax.UnannType(jcod));
    return (hydra.rewriting.Rewriting.deannotateTerm((field).term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.fail(hydra.lib.strings.Cat2.apply(
          "visitBranch: field term is not a lambda: ",
          hydra.show.core.Core.term((field).term)));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> otherwise(hydra.core.Function instance) {
            return hydra.monads.Monads.fail(hydra.lib.strings.Cat2.apply(
              "visitBranch: field term is not a lambda: ",
              hydra.show.core.Core.term((field).term)));
          }
          
          @Override
          public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> visit(hydra.core.Function.Lambda lam) {
            return hydra.ext.java.coder.Coder.withLambda(
              env,
              (lam).value,
              (java.util.function.Function<hydra.ext.java.helpers.JavaEnvironment, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (env2 -> {
                hydra.core.Term body = ((lam).value).body;
                hydra.core.Name lambdaParam = ((lam).value).parameter;
                hydra.ext.java.helpers.JavaEnvironment env3 = hydra.ext.java.coder.Coder.insertBranchVar(
                  lambdaParam,
                  env2);
                return hydra.lib.flows.Bind.apply(
                  hydra.ext.java.coder.Coder.analyzeJavaFunction(
                    env3,
                    body),
                  (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (fs -> {
                    hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, java.util.List<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
                    hydra.util.Lazy<hydra.ext.java.helpers.JavaEnvironment> env4 = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, hydra.ext.java.helpers.JavaEnvironment>) (projected -> projected.environment)).apply(fs));
                    hydra.util.Lazy<hydra.core.Term> innerBody = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
                    return hydra.lib.flows.Bind.apply(
                      hydra.ext.java.coder.Coder.bindingsToStatements(
                        env4.get(),
                        bindings.get()),
                      (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.helpers.JavaEnvironment>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (bindResult -> {
                        hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.BlockStatement>> bindingStmts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindResult));
                        hydra.util.Lazy<hydra.ext.java.helpers.JavaEnvironment> env5 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindResult));
                        return hydra.lib.flows.Bind.apply(
                          hydra.ext.java.coder.Coder.encodeTerm(
                            env5.get(),
                            innerBody.get()),
                          (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (jret -> {
                            hydra.ext.java.syntax.BlockStatement returnStmt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(jret)));
                            hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.BlockStatement>> allStmts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                              bindingStmts.get(),
                              java.util.List.of(returnStmt)));
                            hydra.ext.java.syntax.FormalParameter param = hydra.ext.java.utils.Utils.javaTypeToJavaFormalParameter(
                              jdom,
                              lambdaParam);
                            return hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.noComment(hydra.ext.java.utils.Utils.methodDeclaration(
                              mods,
                              (java.util.List<hydra.ext.java.syntax.TypeParameter>) (java.util.List.<hydra.ext.java.syntax.TypeParameter>of()),
                              anns,
                              hydra.ext.java.names.Names.visitMethodName(),
                              java.util.List.of(param),
                              result,
                              hydra.util.Maybe.just(allStmts.get()))));
                          }));
                      }));
                  }));
              }));
          }
        });
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> encodeElimination(hydra.ext.java.helpers.JavaEnvironment env, hydra.util.Maybe<hydra.ext.java.syntax.Expression> marg, hydra.core.Type dom, hydra.core.Type cod, hydra.core.Elimination elm) {
    hydra.ext.java.helpers.Aliases aliases = (env).aliases;
    return (elm).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Elimination instance) {
        return hydra.monads.Monads.unexpected(
          "elimination case",
          "encodeElimination");
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Elimination.Record proj) {
        hydra.core.Name fname = ((proj).value).field;
        return hydra.lib.flows.Bind.apply(
          hydra.ext.java.coder.Coder.encodeType(
            aliases,
            (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
            dom),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jdom0 -> hydra.lib.flows.Bind.apply(
            hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jdom0),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jdomr -> hydra.lib.maybes.Cases.apply(
              marg,
              ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
                hydra.core.Name projVar = new hydra.core.Name("projected");
                return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
                  hydra.ext.java.syntax.Expression jbody = hydra.ext.java.utils.Utils.javaExpressionNameToJavaExpression(hydra.ext.java.utils.Utils.fieldExpression(
                    hydra.ext.java.utils.Utils.variableToJavaIdentifier(projVar),
                    hydra.ext.java.utils.Utils.javaIdentifier((fname).value)));
                  return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaLambda(
                    projVar,
                    jbody));
                })).get();
              })).get(),
              (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jarg -> {
                hydra.ext.java.syntax.FieldAccess_Qualifier qual = new hydra.ext.java.syntax.FieldAccess_Qualifier.Primary(hydra.ext.java.utils.Utils.javaExpressionToJavaPrimary(jarg));
                return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaFieldAccessToJavaExpression(new hydra.ext.java.syntax.FieldAccess(qual, hydra.ext.java.utils.Utils.javaIdentifier((fname).value))));
              }))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Elimination.Union cs) {
        hydra.util.Maybe<hydra.core.Term> def_ = ((cs).value).default_;
        java.util.List<hydra.core.Field> fields = ((cs).value).cases;
        hydra.core.Name tname = ((cs).value).typeName;
        return hydra.lib.maybes.Cases.apply(
          marg,
          ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
            hydra.core.Name uVar = new hydra.core.Name("u");
            return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (() -> {
              hydra.core.Term typedLambda = new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(uVar, hydra.util.Maybe.just(dom), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(elm)), new hydra.core.Term.Variable(uVar))))));
              return hydra.ext.java.coder.Coder.encodeTerm(
                env,
                typedLambda);
            })).get();
          })).get(),
          (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jarg -> {
            hydra.ext.java.syntax.Identifier consId = hydra.ext.java.coder.Coder.innerClassRef(
              aliases,
              tname,
              hydra.ext.java.names.Names.partialVisitorName());
            hydra.ext.java.syntax.Primary prim = hydra.ext.java.utils.Utils.javaExpressionToJavaPrimary(jarg);
            return hydra.lib.flows.Bind.apply(
              hydra.ext.java.coder.Coder.encodeType(
                aliases,
                (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                cod),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jcod -> hydra.lib.flows.Bind.apply(
                hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jcod),
                (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (rt -> hydra.lib.flows.Bind.apply(
                  hydra.ext.java.coder.Coder.domTypeArgs(
                    aliases,
                    dom),
                  (java.util.function.Function<java.util.List<hydra.ext.java.syntax.TypeArgument>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (domArgs -> {
                    hydra.util.Lazy<hydra.ext.java.syntax.TypeArgumentsOrDiamond> targs = new hydra.util.Lazy<>(() -> hydra.ext.java.coder.Coder.typeArgsOrDiamond(hydra.lib.lists.Concat2.apply(
                      domArgs,
                      java.util.List.of(new hydra.ext.java.syntax.TypeArgument.Reference(rt)))));
                    return hydra.lib.flows.Bind.apply(
                      hydra.lib.maybes.Cases.apply(
                        def_,
                        hydra.lib.flows.Pure.apply((java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>) (java.util.List.<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>of())),
                        (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>>) (d -> hydra.lib.flows.Bind.apply(
                          hydra.ext.java.coder.Coder.otherwiseBranch(
                            env,
                            aliases,
                            dom,
                            cod,
                            tname,
                            jcod,
                            domArgs,
                            d),
                          (java.util.function.Function<hydra.ext.java.syntax.ClassBodyDeclarationWithComments, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>>) (b -> hydra.lib.flows.Pure.apply(java.util.List.of(b)))))),
                      (java.util.function.Function<java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (otherwiseBranches -> hydra.lib.flows.Bind.apply(
                        hydra.lib.flows.MapList.apply(
                          (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (f -> hydra.ext.java.coder.Coder.visitBranch(
                            env,
                            aliases,
                            dom,
                            tname,
                            jcod,
                            domArgs,
                            f)),
                          fields),
                        (java.util.function.Function<java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (visitBranches -> {
                          hydra.util.Lazy<hydra.ext.java.syntax.ClassBody> body = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.ClassBody(hydra.lib.lists.Concat2.apply(
                            otherwiseBranches,
                            visitBranches)));
                          hydra.util.Lazy<hydra.ext.java.syntax.Expression> visitor = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaConstructorCall(
                            hydra.ext.java.utils.Utils.javaConstructorName(
                              consId,
                              hydra.util.Maybe.just(targs.get())),
                            (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of()),
                            hydra.util.Maybe.just(body.get())));
                          return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocation(
                            hydra.util.Maybe.just((hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>) ((hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>) (hydra.util.Either.<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>right(prim)))),
                            new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.acceptMethodName()),
                            java.util.List.of(visitor.get()))));
                        }))));
                  }))))));
          }));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.core.Elimination.Wrap wrapName) {
        java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression> withArg = (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression>) (ja -> hydra.ext.java.utils.Utils.javaFieldAccessToJavaExpression(new hydra.ext.java.syntax.FieldAccess(new hydra.ext.java.syntax.FieldAccess_Qualifier.Primary(hydra.ext.java.utils.Utils.javaExpressionToJavaPrimary(ja)), hydra.ext.java.utils.Utils.javaIdentifier(hydra.ext.java.names.Names.valueFieldName()))));
        return hydra.lib.flows.Pure.apply(hydra.lib.maybes.Cases.apply(
          marg,
          ((java.util.function.Supplier<hydra.ext.java.syntax.Expression>) (() -> {
            hydra.core.Name wVar = new hydra.core.Name("wrapped");
            return ((java.util.function.Supplier<hydra.ext.java.syntax.Expression>) (() -> {
              hydra.ext.java.syntax.Expression wArg = hydra.ext.java.utils.Utils.javaIdentifierToJavaExpression(hydra.ext.java.utils.Utils.variableToJavaIdentifier(wVar));
              return hydra.ext.java.utils.Utils.javaLambda(
                wVar,
                (withArg).apply(wArg));
            })).get();
          })).get(),
          (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression>) (jarg -> (withArg).apply(jarg))));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>> toDeclInit(hydra.ext.java.helpers.Aliases aliasesExt, hydra.typing.TypeContext tcExt, java.util.Set<hydra.core.Name> recursiveVars, java.util.List<hydra.core.Binding> flatBindings, hydra.core.Name name) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        name,
        recursiveVars),
      () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>>>) (() -> {
        hydra.util.Lazy<hydra.core.Binding> binding = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.equality.Equal.apply(
            (b).name,
            name)),
          flatBindings)));
        return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>>>) (() -> {
          hydra.core.Term value = (binding.get()).term;
          return hydra.lib.flows.Bind.apply(
            hydra.lib.maybes.Cases.apply(
              (binding.get()).type,
              hydra.coderUtils.CoderUtils.tryTypeOf(
                "6",
                tcExt,
                value),
              (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (ts -> hydra.lib.flows.Pure.apply((ts).type))),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>>>) (typ -> hydra.lib.flows.Bind.apply(
              hydra.ext.java.coder.Coder.encodeType(
                aliasesExt,
                (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                typ),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>>>) (jtype -> {
                hydra.ext.java.syntax.Identifier arid = new hydra.ext.java.syntax.Identifier("java.util.concurrent.atomic.AtomicReference");
                hydra.util.Lazy<hydra.ext.java.syntax.AnnotatedIdentifier> aid = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.AnnotatedIdentifier((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()), arid));
                hydra.ext.java.syntax.Identifier id = hydra.ext.java.utils.Utils.variableToJavaIdentifier(name);
                return hydra.lib.flows.Bind.apply(
                  hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jtype),
                  (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>>>) (rt -> {
                    hydra.ext.java.syntax.PackageName pkg = hydra.ext.java.names.Names.javaPackageName(java.util.List.of(
                      "java",
                      "util",
                      "concurrent",
                      "atomic"));
                    hydra.ext.java.syntax.Type artype = hydra.ext.java.utils.Utils.javaRefType(
                      java.util.List.of(rt),
                      hydra.util.Maybe.just(pkg),
                      "AtomicReference");
                    hydra.ext.java.syntax.TypeArgumentsOrDiamond targs = hydra.ext.java.coder.Coder.typeArgsOrDiamond(java.util.List.of(new hydra.ext.java.syntax.TypeArgument.Reference(rt)));
                    hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate ci = new hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate(java.util.List.of(aid.get()), hydra.util.Maybe.just(targs));
                    hydra.util.Lazy<hydra.ext.java.syntax.Expression> body = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaConstructorCall(
                      ci,
                      (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of()),
                      (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing())));
                    return hydra.lib.flows.Pure.apply(hydra.util.Maybe.just(hydra.ext.java.utils.Utils.variableDeclarationStatement(
                      aliasesExt,
                      artype,
                      id,
                      body.get())));
                  }));
              }))));
        })).get();
      })).get(),
      () -> hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>) (hydra.util.Maybe.<hydra.ext.java.syntax.BlockStatement>nothing())));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.BlockStatement> toDeclStatement(hydra.ext.java.helpers.JavaEnvironment envExt, hydra.ext.java.helpers.Aliases aliasesExt, hydra.typing.TypeContext tcExt, java.util.Set<hydra.core.Name> recursiveVars, java.util.Set<hydra.core.Name> thunkedVars, java.util.List<hydra.core.Binding> flatBindings, hydra.core.Name name) {
    hydra.util.Lazy<hydra.core.Binding> binding = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.equality.Equal.apply(
        (b).name,
        name)),
      flatBindings)));
    hydra.core.Term value = (binding.get()).term;
    return hydra.lib.flows.Bind.apply(
      hydra.lib.maybes.Cases.apply(
        (binding.get()).type,
        hydra.coderUtils.CoderUtils.tryTypeOf(
          "7",
          tcExt,
          value),
        (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (ts -> hydra.lib.flows.Pure.apply((ts).type))),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.BlockStatement>>) (typ -> hydra.lib.flows.Bind.apply(
        hydra.ext.java.coder.Coder.encodeType(
          aliasesExt,
          (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
          typ),
        (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.BlockStatement>>) (jtype -> {
          hydra.core.Term annotatedValue = hydra.annotations.Annotations.setTermAnnotation(
            hydra.constants.Constants.key_type(),
            hydra.util.Maybe.just(hydra.encode.core.Core.type(typ)),
            value);
          hydra.ext.java.syntax.Identifier id = hydra.ext.java.utils.Utils.variableToJavaIdentifier(name);
          return hydra.lib.flows.Bind.apply(
            hydra.ext.java.coder.Coder.encodeTerm(
              envExt,
              annotatedValue),
            (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.BlockStatement>>) (rhs2 -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.sets.Member.apply(
                name,
                recursiveVars),
              () -> hydra.lib.flows.Pure.apply(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaStatement(hydra.ext.java.utils.Utils.methodInvocation(
                hydra.util.Maybe.just((hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>) ((hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>) (hydra.util.Either.<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>left(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), id))))),
                new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.setMethodName()),
                java.util.List.of(rhs2))))),
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.sets.Member.apply(
                  name,
                  thunkedVars),
                () -> hydra.lib.flows.Bind.apply(
                  hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jtype),
                  (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.BlockStatement>>) (rt -> {
                    hydra.ext.java.syntax.LambdaBody lambdaBody = new hydra.ext.java.syntax.LambdaBody.Expression(rhs2);
                    hydra.util.Lazy<hydra.ext.java.syntax.Expression> supplierLambda = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.Expression.Lambda(new hydra.ext.java.syntax.LambdaExpression(new hydra.ext.java.syntax.LambdaParameters.Tuple((java.util.List<hydra.ext.java.syntax.LambdaParameters>) (java.util.List.<hydra.ext.java.syntax.LambdaParameters>of())), lambdaBody)));
                    hydra.ext.java.syntax.TypeArgumentsOrDiamond targs = hydra.ext.java.coder.Coder.typeArgsOrDiamond(java.util.List.of(new hydra.ext.java.syntax.TypeArgument.Reference(rt)));
                    hydra.util.Lazy<hydra.ext.java.syntax.Expression> lazyExpr = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaConstructorCall(
                      hydra.ext.java.utils.Utils.javaConstructorName(
                        new hydra.ext.java.syntax.Identifier("hydra.util.Lazy"),
                        hydra.util.Maybe.just(targs)),
                      java.util.List.of(supplierLambda.get()),
                      (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing())));
                    hydra.ext.java.syntax.Type lazyType = hydra.ext.java.utils.Utils.javaRefType(
                      java.util.List.of(rt),
                      hydra.ext.java.names.Names.hydraUtilPackageName(),
                      "Lazy");
                    return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.variableDeclarationStatement(
                      aliasesExt,
                      lazyType,
                      id,
                      lazyExpr.get()));
                  })),
                () -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.variableDeclarationStatement(
                  aliasesExt,
                  jtype,
                  id,
                  rhs2))))));
        }))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.helpers.JavaEnvironment>> bindingsToStatements(hydra.ext.java.helpers.JavaEnvironment env, java.util.List<hydra.core.Binding> bindings) {
    hydra.ext.java.helpers.Aliases aliases = (env).aliases;
    java.util.List<hydra.core.Binding> flatBindings = hydra.ext.java.coder.Coder.dedupBindings(
      (aliases).inScopeJavaVars,
      hydra.ext.java.coder.Coder.flattenBindings(bindings));
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> bindingVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.core.Name>) (b -> (b).name),
      flatBindings)));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.Set<hydra.core.Name>>> allDeps = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.Set<hydra.core.Name>>>) (b -> {
        hydra.util.Lazy<java.util.Set<hydra.core.Name>> deps = new hydra.util.Lazy<>(() -> hydra.lib.sets.Intersection.apply(
          bindingVars.get(),
          hydra.rewriting.Rewriting.freeVariablesInTerm((b).term)));
        hydra.core.Name key = (b).name;
        return (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.Set<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.Set<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.Set<hydra.core.Name>>(key, deps.get())));
      }),
      flatBindings)));
    hydra.util.Lazy<java.util.List<java.util.List<hydra.core.Name>>> sorted = new hydra.util.Lazy<>(() -> hydra.sorting.Sorting.topologicalSortComponents(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.Set<hydra.core.Name>>, hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>>) (entry -> {
        hydra.util.Lazy<java.util.Set<hydra.core.Name>> deps = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(entry));
        hydra.util.Lazy<hydra.core.Name> key = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(entry));
        return (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>(key.get(), hydra.lib.sets.ToList.apply(deps.get()))));
      }),
      hydra.lib.maps.ToList.apply(allDeps.get()))));
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> recursiveVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>) (names -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(names),
          1),
        () -> ((java.util.function.Supplier<java.util.List<hydra.core.Name>>) (() -> {
          hydra.util.Lazy<hydra.core.Name> singleName = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(names));
          return hydra.lib.maybes.Cases.apply(
            hydra.lib.maps.Lookup.apply(
              singleName.get(),
              allDeps.get()),
            (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()),
            (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.List<hydra.core.Name>>) (deps -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.sets.Member.apply(
                singleName.get(),
                deps),
              () -> java.util.List.of(singleName.get()),
              () -> (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()))));
        })).get(),
        () -> names)),
      sorted.get()))));
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> thunkedVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Name>>) (b -> {
        hydra.core.Name bname = (b).name;
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.And.apply(
            hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
              bname,
              recursiveVars.get())),
            hydra.lib.logic.And.apply(
              hydra.ext.java.coder.Coder.needsThunking((b).term),
              hydra.lib.logic.Not.apply(hydra.ext.java.coder.Coder.bindingIsFunctionType(b)))),
          () -> java.util.List.of(bname),
          () -> (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()));
      }),
      flatBindings))));
    hydra.util.Lazy<hydra.ext.java.helpers.Aliases> aliasesExtended = new hydra.util.Lazy<>(() -> new hydra.ext.java.helpers.Aliases((aliases).currentNamespace, (aliases).packages, (aliases).branchVars, hydra.lib.sets.Union.apply(
      (aliases).recursiveVars,
      recursiveVars.get()), (aliases).inScopeTypeParams, (aliases).polymorphicLocals, hydra.lib.sets.Union.apply(
      (aliases).inScopeJavaVars,
      bindingVars.get()), (aliases).varRenames, (aliases).lambdaVars, (aliases).typeVarSubst, (aliases).trustedTypeVars, (aliases).methodCodomain, hydra.lib.sets.Union.apply(
      (aliases).thunkedVars,
      thunkedVars.get())));
    hydra.typing.TypeContext tc = (env).typeContext;
    hydra.typing.TypeContext tcExtended = hydra.schemas.Schemas.extendTypeContextForLet(
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (p0 -> p1 -> hydra.coderUtils.CoderUtils.bindingMetadata(
        p0,
        p1)),
      tc,
      new hydra.core.Let(flatBindings, new hydra.core.Term.Variable(new hydra.core.Name("dummy"))));
    hydra.ext.java.helpers.JavaEnvironment envExtended = new hydra.ext.java.helpers.JavaEnvironment(aliasesExtended.get(), tcExtended);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(bindings),
      () -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.helpers.JavaEnvironment>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.helpers.JavaEnvironment>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.helpers.JavaEnvironment>((java.util.List<hydra.ext.java.syntax.BlockStatement>) (java.util.List.<hydra.ext.java.syntax.BlockStatement>of()), envExtended)))),
      () -> hydra.lib.flows.Bind.apply(
        hydra.lib.flows.MapList.apply(
          (java.util.function.Function<java.util.List<hydra.core.Name>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.BlockStatement>>>) (names -> hydra.lib.flows.Bind.apply(
            hydra.lib.flows.MapList.apply(
              (java.util.function.Function<hydra.core.Name, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>>>) (n -> hydra.ext.java.coder.Coder.toDeclInit(
                aliasesExtended.get(),
                tcExtended,
                recursiveVars.get(),
                flatBindings,
                n)),
              names),
            (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.BlockStatement>>>) (inits -> hydra.lib.flows.Bind.apply(
              hydra.lib.flows.MapList.apply(
                (java.util.function.Function<hydra.core.Name, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.BlockStatement>>) (n -> hydra.ext.java.coder.Coder.toDeclStatement(
                  envExtended,
                  aliasesExtended.get(),
                  tcExtended,
                  recursiveVars.get(),
                  thunkedVars.get(),
                  flatBindings,
                  n)),
                names),
              (java.util.function.Function<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.java.syntax.BlockStatement>>>) (decls -> hydra.lib.flows.Pure.apply(hydra.lib.lists.Concat2.apply(
                hydra.lib.maybes.Cat.apply(inits),
                decls))))))),
          sorted.get()),
        (java.util.function.Function<java.util.List<java.util.List<hydra.ext.java.syntax.BlockStatement>>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.helpers.JavaEnvironment>>>) (groups -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.helpers.JavaEnvironment>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.helpers.JavaEnvironment>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.helpers.JavaEnvironment>(hydra.lib.lists.Concat.apply(groups), envExtended)))))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration> toClassDecl(Boolean isInner, Boolean isSer, hydra.ext.java.helpers.Aliases aliases, java.util.List<hydra.ext.java.syntax.TypeParameter> tparams, hydra.core.Name elName, hydra.core.Type t) {
    java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration>> wrap = (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration>>) (t_ -> hydra.ext.java.coder.Coder.declarationForRecordType(
      isInner,
      isSer,
      aliases,
      tparams,
      elName,
      java.util.List.of(new hydra.core.FieldType(new hydra.core.Name("value"), hydra.rewriting.Rewriting.deannotateType(t_)))));
    return (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration> otherwise(hydra.core.Type instance) {
        return (wrap).apply(t);
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration> visit(hydra.core.Type.Record rt) {
        return hydra.ext.java.coder.Coder.declarationForRecordType(
          isInner,
          isSer,
          aliases,
          tparams,
          elName,
          ((rt).value).fields);
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration> visit(hydra.core.Type.Union rt) {
        return hydra.ext.java.coder.Coder.declarationForUnionType(
          isSer,
          aliases,
          tparams,
          elName,
          ((rt).value).fields);
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration> visit(hydra.core.Type.Forall fa) {
        hydra.core.Type body = ((fa).value).body;
        hydra.core.Name v = ((fa).value).parameter;
        hydra.ext.java.syntax.TypeParameter param = hydra.ext.java.utils.Utils.javaTypeParameter(hydra.formatting.Formatting.capitalize((v).value));
        return hydra.ext.java.coder.Coder.toClassDecl(
          false,
          isSer,
          aliases,
          hydra.lib.lists.Concat2.apply(
            tparams,
            java.util.List.of(param)),
          elName,
          body);
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration> visit(hydra.core.Type.Wrap wt) {
        hydra.core.Type wtype = ((wt).value).body;
        return hydra.ext.java.coder.Coder.declarationForRecordType(
          isInner,
          isSer,
          aliases,
          tparams,
          elName,
          java.util.List.of(new hydra.core.FieldType(new hydra.core.Name("value"), wtype)));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration> declarationForUnionType(Boolean isSer, hydra.ext.java.helpers.Aliases aliases, java.util.List<hydra.ext.java.syntax.TypeParameter> tparams, hydra.core.Name elName, java.util.List<hydra.core.FieldType> fields) {
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapList.apply(
        (java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration>>) (ft -> {
          hydra.core.Name fname = (ft).name;
          hydra.core.Type ftype = (ft).type;
          hydra.util.Lazy<java.util.List<hydra.core.FieldType>> rfields = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
            hydra.schemas.Schemas.isUnitType(hydra.rewriting.Rewriting.deannotateType(ftype)),
            () -> (java.util.List<hydra.core.FieldType>) (java.util.List.<hydra.core.FieldType>of()),
            () -> java.util.List.of(new hydra.core.FieldType(new hydra.core.Name("value"), hydra.rewriting.Rewriting.deannotateType(ftype)))));
          hydra.core.Name varName = hydra.ext.java.utils.Utils.variantClassName(
            false,
            elName,
            fname);
          return hydra.lib.flows.Bind.apply(
            hydra.ext.java.coder.Coder.declarationForRecordType_(
              true,
              isSer,
              aliases,
              (java.util.List<hydra.ext.java.syntax.TypeParameter>) (java.util.List.<hydra.ext.java.syntax.TypeParameter>of()),
              varName,
              hydra.lib.logic.IfElse.lazy(
                isSer,
                () -> hydra.util.Maybe.just(elName),
                () -> (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing())),
              rfields.get()),
            (java.util.function.Function<hydra.ext.java.syntax.ClassDeclaration, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration>>) (innerDecl -> hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.augmentVariantClass(
              aliases,
              tparams,
              elName,
              innerDecl))));
        }),
        fields),
      (java.util.function.Function<java.util.List<hydra.ext.java.syntax.ClassDeclaration>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration>>) (variantClasses -> {
        hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.ClassBodyDeclaration>> variantDecls = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.ext.java.syntax.ClassDeclaration, hydra.ext.java.syntax.ClassBodyDeclaration>) (vc -> new hydra.ext.java.syntax.ClassBodyDeclaration.ClassMember(new hydra.ext.java.syntax.ClassMemberDeclaration.Class_(vc))),
          variantClasses));
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.ext.java.syntax.ClassBodyDeclaration, hydra.core.FieldType>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (pair -> hydra.ext.java.coder.Coder.addComment(
              hydra.lib.pairs.First.apply(pair),
              hydra.lib.pairs.Second.apply(pair))),
            hydra.lib.lists.Zip.apply(
              variantDecls.get(),
              fields)),
          (java.util.function.Function<java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration>>) (variantDecls_ -> {
            hydra.ext.java.syntax.ClassBodyDeclaration acceptDecl = hydra.ext.java.utils.Utils.toAcceptMethod(
              true,
              tparams);
            java.util.List<hydra.ext.java.syntax.InterfaceMethodModifier> defaultMod = java.util.List.of(new hydra.ext.java.syntax.InterfaceMethodModifier.Default());
            hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.TypeArgument>> typeArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.ext.java.syntax.TypeParameter, hydra.ext.java.syntax.TypeArgument>) (tp -> hydra.ext.java.utils.Utils.typeParameterToTypeArgument(tp)),
              tparams));
            hydra.util.Lazy<hydra.ext.java.syntax.FormalParameter> mainInstanceParam = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaTypeToJavaFormalParameter(
              hydra.ext.java.utils.Utils.javaClassTypeToJavaType(hydra.ext.java.utils.Utils.nameToJavaClassType(
                aliases,
                false,
                typeArgs.get(),
                elName,
                (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))),
              new hydra.core.Name("instance")));
            hydra.ext.java.syntax.Result resultR = hydra.ext.java.utils.Utils.javaTypeToJavaResult(new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.utils.Utils.visitorTypeVariable()));
            hydra.ext.java.syntax.BlockStatement throwStmt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaThrowIllegalStateException(java.util.List.of(hydra.ext.java.utils.Utils.javaAdditiveExpressionToJavaExpression(hydra.ext.java.utils.Utils.addExpressions(java.util.List.of(
              hydra.ext.java.utils.Utils.javaStringMultiplicativeExpression("Non-exhaustive patterns when matching: "),
              new hydra.ext.java.syntax.MultiplicativeExpression.Unary(hydra.ext.java.utils.Utils.javaIdentifierToJavaUnaryExpression(new hydra.ext.java.syntax.Identifier("instance")))))))));
            hydra.util.Lazy<hydra.ext.java.syntax.InterfaceMemberDeclaration> otherwiseDecl = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.interfaceMethodDeclaration(
              defaultMod,
              (java.util.List<hydra.ext.java.syntax.TypeParameter>) (java.util.List.<hydra.ext.java.syntax.TypeParameter>of()),
              hydra.ext.java.names.Names.otherwiseMethodName(),
              java.util.List.of(mainInstanceParam.get()),
              resultR,
              hydra.util.Maybe.just(java.util.List.of(throwStmt))));
            hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.InterfaceMemberDeclaration>> pvVisitMethods = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.FieldType, hydra.ext.java.syntax.InterfaceMemberDeclaration>) (ft -> {
                hydra.core.Name fname = (ft).name;
                hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation> mi = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.methodInvocation(
                  (hydra.util.Maybe<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>>) (hydra.util.Maybe.<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>>nothing()),
                  new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.otherwiseMethodName()),
                  java.util.List.of(hydra.ext.java.utils.Utils.javaIdentifierToJavaExpression(new hydra.ext.java.syntax.Identifier("instance")))));
                hydra.util.Lazy<hydra.ext.java.syntax.Type> varRef = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaClassTypeToJavaType(hydra.ext.java.utils.Utils.nameToJavaClassType(
                  aliases,
                  false,
                  typeArgs.get(),
                  hydra.ext.java.utils.Utils.variantClassName(
                    false,
                    elName,
                    fname),
                  (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))));
                hydra.ext.java.syntax.FormalParameter param = hydra.ext.java.utils.Utils.javaTypeToJavaFormalParameter(
                  varRef.get(),
                  new hydra.core.Name("instance"));
                hydra.ext.java.syntax.BlockStatement returnOtherwise = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.utils.Utils.javaPrimaryToJavaExpression(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaPrimary(mi.get())))));
                return hydra.ext.java.utils.Utils.interfaceMethodDeclaration(
                  defaultMod,
                  (java.util.List<hydra.ext.java.syntax.TypeParameter>) (java.util.List.<hydra.ext.java.syntax.TypeParameter>of()),
                  hydra.ext.java.names.Names.visitMethodName(),
                  java.util.List.of(param),
                  resultR,
                  hydra.util.Maybe.just(java.util.List.of(returnOtherwise)));
              }),
              fields));
            hydra.util.Lazy<hydra.ext.java.syntax.InterfaceBody> pvBody = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.InterfaceBody(hydra.lib.lists.Concat2.apply(
              java.util.List.of(otherwiseDecl.get()),
              pvVisitMethods.get())));
            hydra.util.Lazy<hydra.ext.java.syntax.ClassType> visitorClassType = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaClassType(
              hydra.lib.lists.Concat2.apply(
                hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.ext.java.syntax.TypeParameter, hydra.ext.java.syntax.ReferenceType>) (tp -> hydra.ext.java.utils.Utils.typeParameterToReferenceType(tp)),
                  tparams),
                java.util.List.of(hydra.ext.java.utils.Utils.visitorTypeVariable())),
              (hydra.util.Maybe<hydra.ext.java.syntax.PackageName>) (hydra.util.Maybe.<hydra.ext.java.syntax.PackageName>nothing()),
              hydra.ext.java.names.Names.visitorName()));
            hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.TypeParameter>> vtparams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
              tparams,
              java.util.List.of(hydra.ext.java.utils.Utils.javaTypeParameter(hydra.ext.java.names.Names.visitorReturnParameter()))));
            hydra.ext.java.syntax.ClassBodyDeclaration partialVisitor = hydra.ext.java.utils.Utils.javaInterfaceDeclarationToJavaClassBodyDeclaration(new hydra.ext.java.syntax.NormalInterfaceDeclaration(java.util.List.of(new hydra.ext.java.syntax.InterfaceModifier.Public()), new hydra.ext.java.syntax.TypeIdentifier(new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.partialVisitorName())), vtparams.get(), java.util.List.of(new hydra.ext.java.syntax.InterfaceType(visitorClassType.get())), pvBody.get()));
            hydra.util.Lazy<hydra.ext.java.syntax.ClassBodyDeclaration> privateConst = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.makeConstructor(
              aliases,
              elName,
              true,
              (java.util.List<hydra.ext.java.syntax.FormalParameter>) (java.util.List.<hydra.ext.java.syntax.FormalParameter>of()),
              (java.util.List<hydra.ext.java.syntax.BlockStatement>) (java.util.List.<hydra.ext.java.syntax.BlockStatement>of())));
            hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.InterfaceMemberDeclaration>> visitorMethods = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.FieldType, hydra.ext.java.syntax.InterfaceMemberDeclaration>) (ft -> {
                hydra.core.Name fname = (ft).name;
                hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.TypeArgument>> typeArgs2 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.ext.java.syntax.TypeParameter, hydra.ext.java.syntax.TypeArgument>) (tp -> hydra.ext.java.utils.Utils.typeParameterToTypeArgument(tp)),
                  tparams));
                hydra.util.Lazy<hydra.ext.java.syntax.Type> varRef = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaClassTypeToJavaType(hydra.ext.java.utils.Utils.nameToJavaClassType(
                  aliases,
                  false,
                  typeArgs2.get(),
                  hydra.ext.java.utils.Utils.variantClassName(
                    false,
                    elName,
                    fname),
                  (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))));
                hydra.ext.java.syntax.FormalParameter param = hydra.ext.java.utils.Utils.javaTypeToJavaFormalParameter(
                  varRef.get(),
                  new hydra.core.Name("instance"));
                hydra.ext.java.syntax.Result resultR2 = hydra.ext.java.utils.Utils.javaTypeToJavaResult(new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.utils.Utils.visitorTypeVariable()));
                return hydra.ext.java.utils.Utils.interfaceMethodDeclaration(
                  (java.util.List<hydra.ext.java.syntax.InterfaceMethodModifier>) (java.util.List.<hydra.ext.java.syntax.InterfaceMethodModifier>of()),
                  (java.util.List<hydra.ext.java.syntax.TypeParameter>) (java.util.List.<hydra.ext.java.syntax.TypeParameter>of()),
                  hydra.ext.java.names.Names.visitMethodName(),
                  java.util.List.of(param),
                  resultR,
                  (hydra.util.Maybe<java.util.List<hydra.ext.java.syntax.BlockStatement>>) (hydra.util.Maybe.<java.util.List<hydra.ext.java.syntax.BlockStatement>>nothing()));
              }),
              fields));
            hydra.ext.java.syntax.InterfaceBody visitorBody = new hydra.ext.java.syntax.InterfaceBody(visitorMethods.get());
            hydra.util.Lazy<hydra.ext.java.syntax.ClassBodyDeclaration> visitor = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaInterfaceDeclarationToJavaClassBodyDeclaration(new hydra.ext.java.syntax.NormalInterfaceDeclaration(java.util.List.of(new hydra.ext.java.syntax.InterfaceModifier.Public()), new hydra.ext.java.syntax.TypeIdentifier(new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.visitorName())), vtparams.get(), (java.util.List<hydra.ext.java.syntax.InterfaceType>) (java.util.List.<hydra.ext.java.syntax.InterfaceType>of()), visitorBody)));
            return hydra.lib.flows.Bind.apply(
              hydra.ext.java.coder.Coder.constantDeclForTypeName(
                aliases,
                elName),
              (java.util.function.Function<hydra.ext.java.syntax.ClassBodyDeclarationWithComments, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration>>) (tn0 -> hydra.lib.flows.Bind.apply(
                hydra.lib.flows.MapList.apply(
                  (java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (ft -> hydra.ext.java.coder.Coder.constantDeclForFieldType(
                    aliases,
                    ft)),
                  fields),
                (java.util.function.Function<java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration>>) (tn1 -> {
                  hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>> otherDecls = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.ext.java.syntax.ClassBodyDeclaration, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>) (d -> hydra.ext.java.coder.Coder.noComment(d)),
                    java.util.List.of(
                      privateConst.get(),
                      acceptDecl,
                      visitor.get(),
                      partialVisitor)));
                  hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>> tn = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                    java.util.List.of(tn0),
                    tn1));
                  hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>> bodyDecls = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(java.util.List.of(
                    tn.get(),
                    otherDecls.get(),
                    variantDecls_)));
                  hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.ClassModifier>> mods = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                    hydra.ext.java.coder.Coder.classModsPublic(),
                    java.util.List.of(new hydra.ext.java.syntax.ClassModifier.Abstract())));
                  return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaClassDeclaration(
                    aliases,
                    tparams,
                    elName,
                    mods.get(),
                    (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing()),
                    hydra.ext.java.coder.Coder.interfaceTypes(
                      isSer,
                      aliases,
                      tparams,
                      elName),
                    bodyDecls.get()));
                }))));
          }));
      }));
  }
  
  static hydra.ext.java.syntax.ClassDeclaration augmentVariantClass(hydra.ext.java.helpers.Aliases aliases, java.util.List<hydra.ext.java.syntax.TypeParameter> tparams, hydra.core.Name elName, hydra.ext.java.syntax.ClassDeclaration cd) {
    return (cd).accept(new hydra.ext.java.syntax.ClassDeclaration.PartialVisitor<>() {
      @Override
      public hydra.ext.java.syntax.ClassDeclaration otherwise(hydra.ext.java.syntax.ClassDeclaration instance) {
        return cd;
      }
      
      @Override
      public hydra.ext.java.syntax.ClassDeclaration visit(hydra.ext.java.syntax.ClassDeclaration.Normal ncd) {
        hydra.ext.java.syntax.ClassBodyDeclarationWithComments acceptDecl = hydra.ext.java.coder.Coder.noComment(hydra.ext.java.utils.Utils.toAcceptMethod(
          false,
          tparams));
        hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.TypeArgument>> args = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.ext.java.syntax.TypeParameter, hydra.ext.java.syntax.TypeArgument>) (tp -> hydra.ext.java.utils.Utils.typeParameterToTypeArgument(tp)),
          tparams));
        hydra.util.Lazy<hydra.ext.java.syntax.ClassType> extendsPart = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.nameToJavaClassType(
          aliases,
          true,
          args.get(),
          elName,
          (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())));
        hydra.ext.java.syntax.ClassBody oldBody = ((ncd).value).body;
        java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments> oldDecls = (oldBody).value;
        hydra.util.Lazy<hydra.ext.java.syntax.ClassBody> newBody = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.ClassBody(hydra.lib.lists.Concat2.apply(
          oldDecls,
          java.util.List.of(acceptDecl))));
        java.util.List<hydra.ext.java.syntax.ClassModifier> newMods = java.util.List.of(
          new hydra.ext.java.syntax.ClassModifier.Public(),
          new hydra.ext.java.syntax.ClassModifier.Static(),
          new hydra.ext.java.syntax.ClassModifier.Final());
        return new hydra.ext.java.syntax.ClassDeclaration.Normal(new hydra.ext.java.syntax.NormalClassDeclaration(newMods, ((ncd).value).identifier, tparams, hydra.util.Maybe.just(extendsPart.get()), ((ncd).value).implements_, newBody.get()));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>> encodeTypeDefinition(hydra.ext.java.syntax.PackageDeclaration pkg, hydra.ext.java.helpers.Aliases aliases, hydra.module.TypeDefinition tdef) {
    hydra.core.Type typ = (tdef).type;
    Boolean serializable = hydra.ext.java.coder.Coder.isSerializableJavaType(typ);
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.ImportDeclaration>> imports = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      serializable,
      () -> java.util.List.of(new hydra.ext.java.syntax.ImportDeclaration.SingleType(new hydra.ext.java.syntax.SingleTypeImportDeclaration(hydra.ext.java.utils.Utils.javaTypeName(new hydra.ext.java.syntax.Identifier("java.io.Serializable"))))),
      () -> (java.util.List<hydra.ext.java.syntax.ImportDeclaration>) (java.util.List.<hydra.ext.java.syntax.ImportDeclaration>of())));
    hydra.core.Name name = (tdef).name;
    return hydra.lib.flows.Bind.apply(
      hydra.ext.java.coder.Coder.toClassDecl(
        false,
        serializable,
        aliases,
        (java.util.List<hydra.ext.java.syntax.TypeParameter>) (java.util.List.<hydra.ext.java.syntax.TypeParameter>of()),
        name,
        typ),
      (java.util.function.Function<hydra.ext.java.syntax.ClassDeclaration, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>>) (decl -> hydra.lib.flows.Bind.apply(
        hydra.annotations.Annotations.getTypeDescription(typ),
        (java.util.function.Function<hydra.util.Maybe<String>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>>) (comment -> {
          hydra.ext.java.syntax.TypeDeclarationWithComments tdecl = new hydra.ext.java.syntax.TypeDeclarationWithComments(new hydra.ext.java.syntax.TypeDeclaration.Class_(decl), comment);
          return hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>(name, new hydra.ext.java.syntax.CompilationUnit.Ordinary(new hydra.ext.java.syntax.OrdinaryCompilationUnit(hydra.util.Maybe.just(pkg), imports.get(), java.util.List.of(tdecl)))))));
        }))));
  }
  
  static hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> peelDomainsAndCod(Integer n, hydra.core.Type t) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Lte.apply(
        n,
        0),
      () -> (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()), t))),
      () -> (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> otherwise(hydra.core.Type instance) {
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()), t)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Function ft) {
          hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> rest = hydra.ext.java.coder.Coder.peelDomainsAndCod(
            hydra.lib.math.Sub.apply(
              n,
              1),
            ((ft).value).codomain);
          return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Cons.apply(
            ((ft).value).domain,
            hydra.lib.pairs.First.apply(rest)), hydra.lib.pairs.Second.apply(rest))));
        }
      }));
  }
  
  static Boolean isSerializableJavaType(hydra.core.Type typ) {
    return (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Record rt) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Union rt) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Wrap wt) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Forall fa) {
        return hydra.ext.java.coder.Coder.isSerializableJavaType(((fa).value).body);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> correctCastType(hydra.core.Term innerBody, java.util.List<hydra.core.Type> typeArgs, hydra.core.Type fallback) {
    return (hydra.rewriting.Rewriting.deannotateTerm(innerBody)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> otherwise(hydra.core.Term instance) {
        return hydra.lib.flows.Pure.apply(fallback);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.Pair _p) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply(typeArgs),
            2),
          () -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Pair(new hydra.core.PairType(hydra.lib.lists.Head.apply(typeArgs), hydra.lib.lists.Head.apply(hydra.lib.lists.Tail.apply(typeArgs))))),
          () -> hydra.lib.flows.Pure.apply(fallback));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> typeAppFallbackCast(hydra.ext.java.helpers.JavaEnvironment env, hydra.ext.java.helpers.Aliases aliases, java.util.List<java.util.Map<hydra.core.Name, hydra.core.Term>> anns, java.util.List<hydra.ext.java.syntax.Type> tyapps, hydra.ext.java.syntax.Type jatyp, hydra.core.Term body, hydra.core.Type typ) {
    hydra.core.Term annotatedBody = hydra.annotations.Annotations.setTermAnnotation(
      hydra.constants.Constants.key_type(),
      hydra.util.Maybe.just(hydra.encode.core.Core.type(typ)),
      body);
    return hydra.lib.flows.Bind.apply(
      hydra.ext.java.coder.Coder.encodeTermInternal(
        env,
        anns,
        hydra.lib.lists.Cons.apply(
          jatyp,
          tyapps),
        annotatedBody),
      (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jbody -> hydra.lib.flows.Bind.apply(
        hydra.ext.java.coder.Coder.encodeType(
          aliases,
          (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
          typ),
        (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jtype -> hydra.lib.flows.Bind.apply(
          hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jtype),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (rt -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.Utils.javaCastExpression(
            rt,
            hydra.ext.java.utils.Utils.javaExpressionToJavaUnaryExpression(jbody))))))))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> typeAppNullaryOrHoisted(hydra.ext.java.helpers.JavaEnvironment env, hydra.ext.java.helpers.Aliases aliases, java.util.List<java.util.Map<hydra.core.Name, hydra.core.Term>> anns, java.util.List<hydra.ext.java.syntax.Type> tyapps, hydra.ext.java.syntax.Type jatyp, hydra.core.Term body, hydra.core.Type correctedTyp, hydra.core.Name varName, hydra.ext.java.helpers.JavaSymbolClass cls, java.util.List<hydra.core.Type> allTypeArgs) {
    hydra.module.QualifiedName qn = hydra.names.Names.qualifyName(varName);
    String localName = (qn).local;
    hydra.util.Maybe<hydra.module.Namespace> mns = (qn).namespace;
    return (cls).accept(new hydra.ext.java.helpers.JavaSymbolClass.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> otherwise(hydra.ext.java.helpers.JavaSymbolClass instance) {
        return hydra.ext.java.coder.Coder.typeAppFallbackCast(
          env,
          aliases,
          anns,
          tyapps,
          jatyp,
          body,
          correctedTyp);
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.ext.java.helpers.JavaSymbolClass.NullaryFunction _u) {
        return hydra.lib.maybes.Cases.apply(
          mns,
          hydra.ext.java.coder.Coder.typeAppFallbackCast(
            env,
            aliases,
            anns,
            tyapps,
            jatyp,
            body,
            correctedTyp),
          (java.util.function.Function<hydra.module.Namespace, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (ns_ -> {
            hydra.ext.java.syntax.Identifier classId = hydra.ext.java.utils.Utils.nameToJavaName(
              aliases,
              hydra.names.Names.unqualifyName(new hydra.module.QualifiedName(hydra.util.Maybe.just(ns_), hydra.ext.java.coder.Coder.elementsClassName(ns_))));
            hydra.ext.java.syntax.Identifier methodId = new hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.Utils.sanitizeJavaName(localName));
            return hydra.lib.flows.Bind.apply(
              hydra.ext.java.coder.Coder.filterPhantomTypeArgs(
                varName,
                allTypeArgs),
              (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (filteredTypeArgs -> hydra.lib.flows.Bind.apply(
                hydra.lib.flows.MapList.apply(
                  (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.TypeArgument>>) (t -> hydra.lib.flows.Bind.apply(
                    hydra.ext.java.coder.Coder.encodeType(
                      aliases,
                      (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                      t),
                    (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.TypeArgument>>) (jt -> hydra.lib.flows.Bind.apply(
                      hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt),
                      (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.TypeArgument>>) (rt -> hydra.lib.flows.Pure.apply(new hydra.ext.java.syntax.TypeArgument.Reference(rt))))))),
                  filteredTypeArgs),
                (java.util.function.Function<java.util.List<hydra.ext.java.syntax.TypeArgument>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jTypeArgs -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocationStaticWithTypeArgs(
                  classId,
                  methodId,
                  jTypeArgs,
                  (java.util.List<hydra.ext.java.syntax.Expression>) (java.util.List.<hydra.ext.java.syntax.Expression>of()))))))));
          }));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression> visit(hydra.ext.java.helpers.JavaSymbolClass.HoistedLambda arity) {
        return hydra.lib.maybes.Cases.apply(
          mns,
          hydra.ext.java.coder.Coder.typeAppFallbackCast(
            env,
            aliases,
            anns,
            tyapps,
            jatyp,
            body,
            correctedTyp),
          (java.util.function.Function<hydra.module.Namespace, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (ns_ -> {
            hydra.ext.java.syntax.Identifier classId = hydra.ext.java.utils.Utils.nameToJavaName(
              aliases,
              hydra.names.Names.unqualifyName(new hydra.module.QualifiedName(hydra.util.Maybe.just(ns_), hydra.ext.java.coder.Coder.elementsClassName(ns_))));
            hydra.ext.java.syntax.Identifier methodId = new hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.Utils.sanitizeJavaName(localName));
            return hydra.lib.flows.Bind.apply(
              hydra.ext.java.coder.Coder.filterPhantomTypeArgs(
                varName,
                allTypeArgs),
              (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (filteredTypeArgs -> hydra.lib.flows.Bind.apply(
                hydra.lib.flows.MapList.apply(
                  (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.TypeArgument>>) (t -> hydra.lib.flows.Bind.apply(
                    hydra.ext.java.coder.Coder.encodeType(
                      aliases,
                      (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                      t),
                    (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.TypeArgument>>) (jt -> hydra.lib.flows.Bind.apply(
                      hydra.ext.java.utils.Utils.javaTypeToJavaReferenceType(jt),
                      (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.TypeArgument>>) (rt -> hydra.lib.flows.Pure.apply(new hydra.ext.java.syntax.TypeArgument.Reference(rt))))))),
                  filteredTypeArgs),
                (java.util.function.Function<java.util.List<hydra.ext.java.syntax.TypeArgument>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.Expression>>) (jTypeArgs -> {
                  hydra.util.Lazy<java.util.List<hydra.core.Name>> paramNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                    (java.util.function.Function<Integer, hydra.core.Name>) (i -> new hydra.core.Name(hydra.lib.strings.Cat2.apply(
                      "p",
                      hydra.lib.literals.ShowInt32.apply(i)))),
                    hydra.lib.math.Range.apply(
                      0,
                      hydra.lib.math.Sub.apply(
                        (arity).value,
                        1))));
                  hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.Expression>> paramExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.Expression>) (p -> hydra.ext.java.utils.Utils.javaIdentifierToJavaExpression(hydra.ext.java.utils.Utils.variableToJavaIdentifier(p))),
                    paramNames.get()));
                  hydra.ext.java.syntax.Expression call = hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocationStaticWithTypeArgs(
                    classId,
                    methodId,
                    jTypeArgs,
                    paramExprs.get()));
                  return hydra.lib.flows.Pure.apply(hydra.ext.java.coder.Coder.buildCurriedLambda(
                    paramNames.get(),
                    call));
                }))));
          }));
      }
    });
  }
  
  static hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term> flattenApps(hydra.core.Term t, java.util.List<hydra.core.Term> acc) {
    return (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term>(acc, t)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term> visit(hydra.core.Term.Application app) {
        return hydra.ext.java.coder.Coder.flattenApps(
          ((app).value).function,
          hydra.lib.lists.Cons.apply(
            ((app).value).argument,
            acc));
      }
    });
  }
  
  static hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term> collectLambdaDomains(hydra.core.Term t) {
    return (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term>((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()), t)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term> visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term> otherwise(hydra.core.Function instance) {
            return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term>((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()), t)));
          }
          
          @Override
          public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term> visit(hydra.core.Function.Lambda lam) {
            return hydra.lib.maybes.Cases.apply(
              ((lam).value).domain,
              (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term>((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()), t))),
              (java.util.function.Function<hydra.core.Type, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term>>) (dom -> {
                hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term> rest = hydra.ext.java.coder.Coder.collectLambdaDomains(((lam).value).body);
                return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term>(hydra.lib.lists.Cons.apply(
                  dom,
                  hydra.lib.pairs.First.apply(rest)), hydra.lib.pairs.Second.apply(rest))));
              }));
          }
        });
      }
    });
  }
  
  static hydra.core.Term rebuildApps(hydra.core.Term f, java.util.List<hydra.core.Term> args, hydra.core.Type fType) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(args),
      () -> f,
      () -> (hydra.rewriting.Rewriting.deannotateType(fType)).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Type instance) {
          return hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (a -> new hydra.core.Term.Application(new hydra.core.Application(acc, a)))),
            f,
            args);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Type.Function ft) {
          hydra.util.Lazy<hydra.core.Term> arg = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(args));
          hydra.core.Term app = new hydra.core.Term.Application(new hydra.core.Application(f, arg.get()));
          hydra.core.Type remainingType = ((ft).value).codomain;
          hydra.core.Term annotatedApp = hydra.annotations.Annotations.setTermAnnotation(
            hydra.constants.Constants.key_type(),
            hydra.util.Maybe.just(hydra.encode.core.Core.type(remainingType)),
            app);
          hydra.util.Lazy<java.util.List<hydra.core.Term>> rest = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(args));
          return hydra.ext.java.coder.Coder.rebuildApps(
            annotatedApp,
            rest.get(),
            remainingType);
        }
      }));
  }
  
  static hydra.core.Term propagateTypesInAppChain(hydra.core.Type fixedCod, hydra.core.Type resultType, hydra.core.Term t) {
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term>> flattened = new hydra.util.Lazy<>(() -> hydra.ext.java.coder.Coder.flattenApps(
      t,
      (java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of())));
    hydra.util.Lazy<java.util.List<hydra.core.Term>> args = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(flattened.get()));
    hydra.util.Lazy<hydra.core.Term> fun = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(flattened.get()));
    hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Term> lambdaDomsResult = hydra.ext.java.coder.Coder.collectLambdaDomains(fun.get());
    hydra.util.Lazy<java.util.List<hydra.core.Type>> lambdaDoms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(lambdaDomsResult));
    hydra.util.Lazy<Integer> nArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(args.get()));
    hydra.util.Lazy<Integer> nLambdaDoms = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(lambdaDoms.get()));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        hydra.lib.equality.Gt.apply(
          nLambdaDoms.get(),
          0),
        hydra.lib.equality.Gt.apply(
          nArgs.get(),
          0)),
      () -> ((java.util.function.Supplier<hydra.core.Term>) (() -> {
        hydra.util.Lazy<hydra.core.Type> bodyRetType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.ext.java.coder.Coder.peelDomainsAndCod(
          hydra.lib.math.Sub.apply(
            nLambdaDoms.get(),
            nArgs.get()),
          resultType)));
        return ((java.util.function.Supplier<hydra.core.Term>) (() -> {
          hydra.util.Lazy<hydra.core.Type> funType = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (c -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (d -> new hydra.core.Type.Function(new hydra.core.FunctionType(d, c)))),
            bodyRetType.get(),
            hydra.lib.lists.Reverse.apply(lambdaDoms.get())));
          return ((java.util.function.Supplier<hydra.core.Term>) (() -> {
            hydra.core.Term annotatedFun = hydra.annotations.Annotations.setTermAnnotation(
              hydra.constants.Constants.key_type(),
              hydra.util.Maybe.just(hydra.encode.core.Core.type(funType.get())),
              fun.get());
            return hydra.ext.java.coder.Coder.rebuildApps(
              annotatedFun,
              args.get(),
              funType.get());
          })).get();
        })).get();
      })).get(),
      () -> (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return hydra.annotations.Annotations.setTermAnnotation(
            hydra.constants.Constants.key_type(),
            hydra.util.Maybe.just(hydra.encode.core.Core.type(resultType)),
            t);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Application app) {
          hydra.core.Term lhs = ((app).value).function;
          hydra.util.Lazy<hydra.core.Term> annotatedLhs = new hydra.util.Lazy<>(() -> (hydra.rewriting.Rewriting.deannotateTerm(lhs)).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public hydra.core.Term otherwise(hydra.core.Term instance) {
              return lhs;
            }
            
            @Override
            public hydra.core.Term visit(hydra.core.Term.Function fn) {
              return ((fn).value).accept(new hydra.core.Function.PartialVisitor<>() {
                @Override
                public hydra.core.Term otherwise(hydra.core.Function instance) {
                  return lhs;
                }
                
                @Override
                public hydra.core.Term visit(hydra.core.Function.Elimination elim) {
                  return ((elim).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
                    @Override
                    public hydra.core.Term otherwise(hydra.core.Elimination instance) {
                      return lhs;
                    }
                    
                    @Override
                    public hydra.core.Term visit(hydra.core.Elimination.Union cs) {
                      hydra.util.Lazy<hydra.core.Type> dom = new hydra.util.Lazy<>(() -> hydra.schemas.Schemas.nominalApplication(
                        ((cs).value).typeName,
                        (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of())));
                      hydra.core.Type ft = new hydra.core.Type.Function(new hydra.core.FunctionType(dom.get(), fixedCod));
                      return hydra.annotations.Annotations.setTermAnnotation(
                        hydra.constants.Constants.key_type(),
                        hydra.util.Maybe.just(hydra.encode.core.Core.type(ft)),
                        lhs);
                    }
                  });
                }
              });
            }
          }));
          hydra.core.Term rhs = ((app).value).argument;
          return hydra.annotations.Annotations.setTermAnnotation(
            hydra.constants.Constants.key_type(),
            hydra.util.Maybe.just(hydra.encode.core.Core.type(resultType)),
            new hydra.core.Term.Application(new hydra.core.Application(annotatedLhs.get(), rhs)));
        }
      }));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.InterfaceMemberDeclaration> encodeTermDefinition(hydra.ext.java.helpers.JavaEnvironment env, hydra.module.TermDefinition tdef) {
    hydra.core.Name name = (tdef).name;
    hydra.core.Term term0 = (tdef).term;
    hydra.core.TypeScheme ts = (tdef).type;
    return hydra.monads.Monads.withTrace(
      hydra.lib.strings.Cat2.apply(
        "encode term definition \"",
        hydra.lib.strings.Cat2.apply(
          (name).value,
          "\"")),
      ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (() -> {
        hydra.core.Term term = hydra.rewriting.Rewriting.unshadowVariables(term0);
        return hydra.lib.flows.Bind.apply(
          hydra.monads.Monads.withTrace(
            "analyze function term for term assignment",
            hydra.ext.java.coder.Coder.analyzeJavaFunction(
              env,
              term)),
          (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (fs -> {
            hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, java.util.List<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
            hydra.util.Lazy<hydra.core.Term> body = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
            hydra.util.Lazy<java.util.List<hydra.core.Name>> params = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, java.util.List<hydra.core.Name>>) (projected -> projected.params)).apply(fs));
            hydra.util.Lazy<Integer> numParams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(params.get()));
            hydra.core.Type schemeType = (ts).type;
            hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.core.Type> peelResult = hydra.ext.java.coder.Coder.peelDomainsAndCod(
              numParams.get(),
              schemeType);
            hydra.util.Lazy<hydra.core.Type> cod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(peelResult));
            hydra.util.Lazy<java.util.List<hydra.core.Type>> doms = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, java.util.List<hydra.core.Type>>) (projected -> projected.domains)).apply(fs));
            hydra.util.Lazy<hydra.ext.java.helpers.JavaEnvironment> env2 = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, hydra.ext.java.helpers.JavaEnvironment>) (projected -> projected.environment)).apply(fs));
            hydra.util.Lazy<java.util.List<hydra.core.Type>> schemeDoms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(peelResult));
            java.util.Set<hydra.core.Name> schemeTypeVars = hydra.ext.java.coder.Coder.collectTypeVars((ts).type);
            hydra.util.Lazy<java.util.List<hydra.core.Name>> termVars = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.helpers.JavaEnvironment>, java.util.List<hydra.core.Name>>) (projected -> projected.typeParams)).apply(fs));
            hydra.util.Lazy<java.util.List<hydra.core.Name>> schemeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
              (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.ext.java.coder.Coder.isSimpleName(v)),
              (ts).variables));
            hydra.util.Lazy<java.util.List<hydra.core.Name>> usedSchemeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
              (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.sets.Member.apply(
                v,
                schemeTypeVars)),
              schemeVars.get()));
            hydra.util.Lazy<java.util.List<hydra.core.Name>> tparams = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply(usedSchemeVars.get()),
              () -> termVars.get(),
              () -> usedSchemeVars.get()));
            hydra.util.Lazy<java.util.Set<hydra.core.Name>> schemeVarSet = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(tparams.get()));
            return hydra.lib.flows.Bind.apply(
              hydra.lib.logic.IfElse.lazy(
                hydra.lib.lists.Null.apply(tparams.get()),
                () -> hydra.lib.flows.Pure.apply((java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()))),
                () -> hydra.ext.java.coder.Coder.buildSubstFromAnnotations(
                  schemeVarSet.get(),
                  term)),
              (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (typeVarSubst -> {
                hydra.ext.java.helpers.Aliases aliases2base = (env2.get()).aliases;
                java.util.Map<hydra.core.Name, hydra.core.Type> overgenSubst = hydra.ext.java.coder.Coder.detectAccumulatorUnification(
                  schemeDoms.get(),
                  cod.get(),
                  tparams.get());
                hydra.util.Lazy<hydra.core.Type> fixedCod = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.maps.Null.apply(overgenSubst),
                  () -> cod.get(),
                  () -> hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes(
                    overgenSubst,
                    cod.get())));
                hydra.util.Lazy<java.util.List<hydra.core.Name>> fixedTparams = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.maps.Null.apply(overgenSubst),
                  () -> tparams.get(),
                  () -> hydra.lib.lists.Filter.apply(
                    (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.logic.Not.apply(hydra.lib.maps.Member.apply(
                      v,
                      overgenSubst))),
                    tparams.get())));
                hydra.util.Lazy<java.util.Set<hydra.core.Name>> fixedSchemeVarSet = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(fixedTparams.get()));
                hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Name>> overgenVarSubst = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>>) (entry -> {
                    hydra.util.Lazy<hydra.core.Name> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(entry));
                    hydra.util.Lazy<hydra.core.Type> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(entry));
                    return (v.get()).accept(new hydra.core.Type.PartialVisitor<>() {
                      @Override
                      public hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
                        return (hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>) (hydra.util.Maybe.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>nothing());
                      }
                      
                      @Override
                      public hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Variable n) {
                        return hydra.util.Maybe.just((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>(k.get(), (n).value))));
                      }
                    });
                  }),
                  hydra.lib.maps.ToList.apply(overgenSubst)))));
                hydra.util.Lazy<java.util.List<hydra.core.Type>> fixedDoms = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.maps.Null.apply(overgenSubst),
                  () -> schemeDoms.get(),
                  () -> hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (d -> hydra.ext.java.coder.Coder.substituteTypeVarsWithTypes(
                      overgenSubst,
                      d)),
                    schemeDoms.get())));
                hydra.util.Lazy<java.util.Set<hydra.core.Name>> trustedVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.Unions.apply(hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>) (d -> hydra.ext.java.coder.Coder.collectTypeVars(d)),
                  hydra.lib.lists.Concat2.apply(
                    fixedDoms.get(),
                    java.util.List.of(fixedCod.get())))));
                hydra.util.Lazy<hydra.ext.java.helpers.Aliases> aliases2 = new hydra.util.Lazy<>(() -> new hydra.ext.java.helpers.Aliases((aliases2base).currentNamespace, (aliases2base).packages, (aliases2base).branchVars, (aliases2base).recursiveVars, fixedSchemeVarSet.get(), (aliases2base).polymorphicLocals, (aliases2base).inScopeJavaVars, (aliases2base).varRenames, hydra.lib.sets.Union.apply(
                  (aliases2base).lambdaVars,
                  hydra.lib.sets.FromList.apply(params.get())), hydra.lib.maps.Union.apply(
                  overgenVarSubst.get(),
                  typeVarSubst), hydra.lib.sets.Intersection.apply(
                  trustedVars.get(),
                  fixedSchemeVarSet.get()), hydra.util.Maybe.just(fixedCod.get()), (aliases2base).thunkedVars));
                hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.apply(
                  (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
                  (ts).constraints));
                hydra.ext.java.helpers.JavaEnvironment env2WithTypeParams = new hydra.ext.java.helpers.JavaEnvironment(aliases2.get(), (env2.get()).typeContext);
                hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.TypeParameter>> jparams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.TypeParameter>) (v -> hydra.ext.java.utils.Utils.javaTypeParameter(hydra.formatting.Formatting.capitalize((v).value))),
                  fixedTparams.get()));
                return hydra.lib.flows.Bind.apply(
                  hydra.ext.java.coder.Coder.bindingsToStatements(
                    env2WithTypeParams,
                    bindings.get()),
                  (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.helpers.JavaEnvironment>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (bindResult -> {
                    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.BlockStatement>> bindingStmts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindResult));
                    hydra.util.Lazy<hydra.ext.java.helpers.JavaEnvironment> env3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindResult));
                    return hydra.lib.flows.Bind.apply(
                      hydra.lib.logic.IfElse.lazy(
                        hydra.lib.maps.Null.apply(overgenSubst),
                        () -> hydra.lib.flows.Pure.apply(body.get()),
                        () -> hydra.ext.java.coder.Coder.applyOvergenSubstToTermAnnotations(
                          overgenSubst,
                          body.get())),
                      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (body_ -> {
                        hydra.core.Term annotatedBody = hydra.ext.java.coder.Coder.propagateTypesInAppChain(
                          fixedCod.get(),
                          fixedCod.get(),
                          body_);
                        return hydra.lib.flows.Bind.apply(
                          hydra.lib.flows.MapList.apply(
                            (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Type, hydra.core.Name>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.FormalParameter>>) (pair -> hydra.lib.flows.Bind.apply(
                              hydra.ext.java.coder.Coder.encodeType(
                                aliases2.get(),
                                (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                                hydra.lib.pairs.First.apply(pair)),
                              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.FormalParameter>>) (jdom -> hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaTypeToJavaFormalParameter(
                                jdom,
                                hydra.lib.pairs.Second.apply(pair)))))),
                            hydra.lib.lists.Zip.apply(
                              fixedDoms.get(),
                              params.get())),
                          (java.util.function.Function<java.util.List<hydra.ext.java.syntax.FormalParameter>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (jformalParams -> hydra.lib.flows.Bind.apply(
                            hydra.ext.java.coder.Coder.encodeType(
                              aliases2.get(),
                              (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                              fixedCod.get()),
                            (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (jcod -> {
                              hydra.ext.java.syntax.Result result = hydra.ext.java.utils.Utils.javaTypeToJavaResult(jcod);
                              return hydra.lib.flows.Bind.apply(
                                hydra.ext.java.coder.Coder.encodeTerm(
                                  env3.get(),
                                  annotatedBody),
                                (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (jbody -> {
                                  String jname = hydra.ext.java.utils.Utils.sanitizeJavaName(hydra.formatting.Formatting.decapitalize(hydra.names.Names.localNameOf(name)));
                                  java.util.List<hydra.ext.java.syntax.InterfaceMethodModifier> mods = java.util.List.of(new hydra.ext.java.syntax.InterfaceMethodModifier.Static());
                                  hydra.ext.java.syntax.BlockStatement returnSt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(jbody)));
                                  return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.interfaceMethodDeclaration(
                                    mods,
                                    jparams.get(),
                                    jname,
                                    jformalParams,
                                    result,
                                    hydra.util.Maybe.just(hydra.lib.lists.Concat2.apply(
                                      bindingStmts.get(),
                                      java.util.List.of(returnSt)))));
                                }));
                            }))));
                      }));
                  }));
              }));
          }));
      })).get());
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>> encodeDefinitions(hydra.module.Module mod, java.util.List<hydra.module.Definition> defs) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>>) (g -> hydra.lib.flows.Bind.apply(
        hydra.inference.Inference.initialTypeContext(g),
        (java.util.function.Function<hydra.typing.TypeContext, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>>) (tc -> {
          hydra.ext.java.helpers.Aliases aliases = hydra.ext.java.utils.Utils.importAliasesForModule(mod);
          hydra.ext.java.helpers.JavaEnvironment env = new hydra.ext.java.helpers.JavaEnvironment(aliases, tc);
          hydra.util.Tuple.Tuple2<java.util.List<hydra.module.TypeDefinition>, java.util.List<hydra.module.TermDefinition>> partitioned = hydra.schemas.Schemas.partitionDefinitions(defs);
          hydra.util.Lazy<java.util.List<hydra.module.TypeDefinition>> typeDefs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(partitioned));
          hydra.util.Lazy<java.util.List<hydra.module.TypeDefinition>> nonTypedefDefs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
            (java.util.function.Function<hydra.module.TypeDefinition, Boolean>) (td -> {
              hydra.core.Type typ = (td).type;
              return hydra.ext.java.coder.Coder.isSerializableJavaType(typ);
            }),
            typeDefs.get()));
          hydra.ext.java.syntax.PackageDeclaration pkg = hydra.ext.java.utils.Utils.javaPackageDeclaration((mod).namespace);
          hydra.util.Lazy<java.util.List<hydra.module.TermDefinition>> termDefs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(partitioned));
          return hydra.lib.flows.Bind.apply(
            hydra.lib.flows.MapList.apply(
              (java.util.function.Function<hydra.module.TypeDefinition, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>>) (td -> hydra.ext.java.coder.Coder.encodeTypeDefinition(
                pkg,
                aliases,
                td)),
              nonTypedefDefs.get()),
            (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>>) (typeUnits -> hydra.lib.flows.Bind.apply(
              hydra.lib.logic.IfElse.lazy(
                hydra.lib.lists.Null.apply(termDefs.get()),
                () -> hydra.lib.flows.Pure.apply((java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>of())),
                () -> hydra.lib.flows.Bind.apply(
                  hydra.lib.flows.MapList.apply(
                    (java.util.function.Function<hydra.module.TermDefinition, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (td -> hydra.ext.java.coder.Coder.encodeTermDefinition(
                      env,
                      td)),
                    termDefs.get()),
                  (java.util.function.Function<java.util.List<hydra.ext.java.syntax.InterfaceMemberDeclaration>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>>>) (dataMembers -> hydra.lib.flows.Pure.apply(java.util.List.of(hydra.ext.java.coder.Coder.constructElementsInterface(
                    mod,
                    dataMembers)))))),
              (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>>) (termUnits -> hydra.lib.flows.Pure.apply(hydra.lib.maps.FromList.apply(hydra.lib.lists.Concat2.apply(
                typeUnits,
                termUnits)))))));
        }))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.Map<String, String>> moduleToJava(hydra.module.Module mod, java.util.List<hydra.module.Definition> defs) {
    return hydra.monads.Monads.withTrace(
      hydra.lib.strings.Cat2.apply(
        "encode module: ",
        ((mod).namespace).value),
      hydra.lib.flows.Bind.apply(
        hydra.ext.java.coder.Coder.encodeDefinitions(
          mod,
          defs),
        (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<String, String>>>) (units -> hydra.lib.flows.Pure.apply(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>, hydra.util.Tuple.Tuple2<String, String>>) (entry -> {
            hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(entry));
            hydra.util.Lazy<hydra.ext.java.syntax.CompilationUnit> unit = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(entry));
            return (hydra.util.Tuple.Tuple2<String, String>) ((hydra.util.Tuple.Tuple2<String, String>) (new hydra.util.Tuple.Tuple2<String, String>(hydra.ext.java.coder.Coder.bindingNameToFilePath(name.get()), hydra.serialization.Serialization.printExpr(hydra.serialization.Serialization.parenthesize(hydra.ext.java.serde.Serde.writeCompilationUnit(unit.get()))))));
          }),
          hydra.lib.maps.ToList.apply(units)))))));
  }
}
