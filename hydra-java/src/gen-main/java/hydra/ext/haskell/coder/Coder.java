// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.coder;

/**
 * Functions for encoding Hydra modules as Haskell modules
 */
public interface Coder {
  static Boolean includeTypeDefinitions() {
    return false;
  }
  
  static Boolean useCoreImport() {
    return true;
  }
  
  static hydra.core.Name keyHaskellVar() {
    return new hydra.core.Name("haskellVar");
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Type> adaptTypeToHaskellAndEncode(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Type v1) {
    return hydra.adapt.modules.Modules.adaptTypeToLanguageAndEncode(
      hydra.ext.haskell.language.Language.haskellLanguage(),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Type>>) (v12 -> hydra.ext.haskell.coder.Coder.encodeType(
        namespaces,
        v12)),
      v1);
  }
  
  static String constantForFieldName(hydra.core.Name tname, hydra.core.Name fname) {
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      "_",
      hydra.names.Names.localNameOf(tname),
      "_",
      (fname).value));
  }
  
  static String constantForTypeName(hydra.core.Name tname) {
    return hydra.lib.strings.Cat2.apply(
      "_",
      hydra.names.Names.localNameOf(tname));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Module> constructModule(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.module.Module mod, java.util.List<hydra.module.Definition> defs) {
    java.util.function.Function<hydra.module.Namespace, String> h = (java.util.function.Function<hydra.module.Namespace, String>) (namespace -> (namespace).value);
    java.util.function.Function<String, hydra.ext.haskell.ast.ModuleName> importName = (java.util.function.Function<String, hydra.ext.haskell.ast.ModuleName>) (name -> new hydra.ext.haskell.ast.ModuleName(hydra.lib.strings.Intercalate.apply(
      ".",
      hydra.lib.lists.Map.apply(
        hydra.formatting.Formatting::capitalize,
        hydra.lib.strings.SplitOn.apply(
          ".",
          name)))));
    java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>, hydra.ext.haskell.ast.Import> toImport = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>, hydra.ext.haskell.ast.Import>) (pair -> {
      hydra.util.Lazy<hydra.ext.haskell.ast.ModuleName> alias = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
      hydra.util.Lazy<hydra.module.Namespace> namespace = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
      String name = (h).apply(namespace.get());
      return new hydra.ext.haskell.ast.Import(true, (importName).apply(name), hydra.util.Maybe.just(alias.get()), (hydra.util.Maybe<hydra.ext.haskell.ast.SpecImport>) (hydra.util.Maybe.<hydra.ext.haskell.ast.SpecImport>nothing()));
    });
    hydra.util.Lazy<java.util.List<hydra.ext.haskell.ast.Import>> domainImports = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      toImport,
      hydra.lib.maps.ToList.apply(((java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, java.util.Map<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>>) (projected -> projected.mapping)).apply(namespaces))));
    java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>, hydra.ext.haskell.ast.Import> toImport2 = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>, hydra.ext.haskell.ast.Import>) (triple -> {
      hydra.util.Lazy<java.util.List<String>> hidden = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(triple));
      hydra.util.Lazy<hydra.util.Maybe<String>> malias = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(triple)));
      hydra.util.Lazy<String> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(triple)));
      hydra.util.Lazy<hydra.util.Maybe<hydra.ext.haskell.ast.SpecImport>> spec = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(hidden.get()),
        () -> (hydra.util.Maybe<hydra.ext.haskell.ast.SpecImport>) (hydra.util.Maybe.<hydra.ext.haskell.ast.SpecImport>nothing()),
        () -> hydra.util.Maybe.just(new hydra.ext.haskell.ast.SpecImport.Hiding(hydra.lib.lists.Map.apply(
          (java.util.function.Function<String, hydra.ext.haskell.ast.ImportExportSpec>) (n -> new hydra.ext.haskell.ast.ImportExportSpec((hydra.util.Maybe<hydra.ext.haskell.ast.ImportModifier>) (hydra.util.Maybe.<hydra.ext.haskell.ast.ImportModifier>nothing()), hydra.ext.haskell.utils.Utils.simpleName(n), (hydra.util.Maybe<hydra.ext.haskell.ast.SubspecImportExportSpec>) (hydra.util.Maybe.<hydra.ext.haskell.ast.SubspecImportExportSpec>nothing()))),
          hidden.get())))));
      return new hydra.ext.haskell.ast.Import(hydra.lib.maybes.IsJust.apply(malias.get()), new hydra.ext.haskell.ast.ModuleName(name.get()), hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, hydra.ext.haskell.ast.ModuleName>) (x -> new hydra.ext.haskell.ast.ModuleName(x)),
        malias.get()), spec.get());
    });
    hydra.util.Lazy<java.util.List<hydra.ext.haskell.ast.Import>> standardImports = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      toImport2,
      hydra.lib.lists.Concat2.apply(
        java.util.List.of(
          (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>((hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>) ((hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>) (new hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>("Prelude", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())))), java.util.List.of(
            "Enum",
            "Ordering",
            "decodeFloat",
            "encodeFloat",
            "fail",
            "map",
            "pure",
            "sum")))),
          (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>((hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>) ((hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>) (new hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>("Data.ByteString", hydra.util.Maybe.just("B")))), (java.util.List<String>) (java.util.List.<String>of())))),
          (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>((hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>) ((hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>) (new hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>("Data.Int", hydra.util.Maybe.just("I")))), (java.util.List<String>) (java.util.List.<String>of())))),
          (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>((hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>) ((hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>) (new hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>("Data.List", hydra.util.Maybe.just("L")))), (java.util.List<String>) (java.util.List.<String>of())))),
          (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>((hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>) ((hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>) (new hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>("Data.Map", hydra.util.Maybe.just("M")))), (java.util.List<String>) (java.util.List.<String>of())))),
          (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>((hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>) ((hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>) (new hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>("Data.Set", hydra.util.Maybe.just("S")))), (java.util.List<String>) (java.util.List.<String>of()))))),
        hydra.lib.logic.IfElse.lazy(
          hydra.schemas.Schemas.moduleContainsBinaryLiterals(mod),
          () -> java.util.List.of((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>((hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>) ((hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>) (new hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>("Hydra.Lib.Literals", hydra.util.Maybe.just("Literals")))), (java.util.List<String>) (java.util.List.<String>of()))))),
          () -> (java.util.List<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<String, hydra.util.Maybe<String>>, java.util.List<String>>>of())))));
    hydra.util.Lazy<java.util.List<hydra.ext.haskell.ast.Import>> imports = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
      domainImports.get(),
      standardImports.get()));
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Module>>) (g -> hydra.lib.flows.Bind.apply(
        hydra.lib.flows.MapList.apply(
          ((java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.module.Definition, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.haskell.ast.DeclarationWithComments>>>>) (v1 -> (java.util.function.Function<hydra.module.Definition, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.haskell.ast.DeclarationWithComments>>>) (v2 -> hydra.ext.haskell.coder.Coder.constructModule_createDeclarations(
            (java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, java.util.function.Function<hydra.module.TermDefinition, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments>>>) (p0 -> p1 -> hydra.ext.haskell.coder.Coder.toDataDeclaration(
              p0,
              p1)),
            (java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.haskell.ast.DeclarationWithComments>>>>>) (p0 -> p1 -> p2 -> hydra.ext.haskell.coder.Coder.toTypeDeclarationsFrom(
              p0,
              p1,
              p2)),
            namespaces,
            v1,
            v2)))).apply(g),
          defs),
        (java.util.function.Function<java.util.List<java.util.List<hydra.ext.haskell.ast.DeclarationWithComments>>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Module>>) (declLists -> {
          hydra.util.Lazy<java.util.List<hydra.ext.haskell.ast.DeclarationWithComments>> decls = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(declLists));
          hydra.util.Maybe<String> mc = (mod).description;
          return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Module(hydra.util.Maybe.just(new hydra.ext.haskell.ast.ModuleHead(mc, (importName).apply((h).apply((mod).namespace)), (java.util.List<hydra.ext.haskell.ast.Export>) (java.util.List.<hydra.ext.haskell.ast.Export>of()))), imports.get(), decls.get()));
        }))));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T1, java.util.List<T2>> constructModule_createDeclarations(java.util.function.Function<T0, java.util.function.Function<hydra.module.TermDefinition, hydra.compute.Flow<T1, T2>>> hydra_ext_haskell_coder_toDataDeclaration2, java.util.function.Function<T0, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T1, java.util.List<T2>>>>> hydra_ext_haskell_coder_toTypeDeclarationsFrom2, T0 namespaces, T3 g, hydra.module.Definition def) {
    return (def).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T1, java.util.List<T2>> visit(hydra.module.Definition.Type type) {
        hydra.core.Name name = ((type).value).name;
        hydra.core.Type typ = ((type).value).type;
        return (((hydra_ext_haskell_coder_toTypeDeclarationsFrom2).apply(namespaces)).apply(name)).apply(typ);
      }
      
      @Override
      public hydra.compute.Flow<T1, java.util.List<T2>> visit(hydra.module.Definition.Term term) {
        return hydra.lib.flows.Bind.apply(
          ((hydra_ext_haskell_coder_toDataDeclaration2).apply(namespaces)).apply((term).value),
          (java.util.function.Function<T2, hydra.compute.Flow<T1, java.util.List<T2>>>) (d -> hydra.lib.flows.Pure.apply(java.util.List.of(d))));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> encodeFunction(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Function fun) {
    return (fun).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Function.Elimination e) {
        return ((e).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Elimination.Wrap name) {
            return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Expression.Variable(hydra.ext.haskell.utils.Utils.elementReference(
              namespaces,
              hydra.names.Names.qname(
                hydra.lib.maybes.FromJust.apply(hydra.names.Names.namespaceOf((name).value)),
                hydra.ext.haskell.utils.Utils.newtypeAccessorName((name).value)))));
          }
          
          @Override
          public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Elimination.Record proj) {
            hydra.core.Name dn = ((proj).value).typeName;
            hydra.core.Name fname = ((proj).value).field;
            return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Expression.Variable(hydra.ext.haskell.utils.Utils.recordFieldReference(
              namespaces,
              dn,
              fname)));
          }
          
          @Override
          public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Elimination.Union stmt) {
            hydra.util.Maybe<hydra.core.Term> def = ((stmt).value).default_;
            hydra.core.Name dn = ((stmt).value).typeName;
            java.util.List<hydra.core.Field> fields = ((stmt).value).cases;
            java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.FieldType>, java.util.function.Function<hydra.core.Field, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Alternative>>> toAlt = (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.FieldType>, java.util.function.Function<hydra.core.Field, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Alternative>>>) (fieldMap -> (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Alternative>>) (field -> {
              hydra.core.Name fn = (field).name;
              hydra.core.Term fun_ = (field).term;
              return hydra.annotations.Annotations.withDepth(
                hydra.ext.haskell.coder.Coder.keyHaskellVar(),
                (java.util.function.Function<Integer, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Alternative>>) (depth -> {
                  String v0 = hydra.lib.strings.Cat2.apply(
                    "v",
                    hydra.lib.literals.ShowInt32.apply(depth));
                  hydra.core.Term raw = new hydra.core.Term.Application(new hydra.core.Application(fun_, new hydra.core.Term.Variable(new hydra.core.Name(v0))));
                  hydra.core.Term rhsTerm = hydra.rewriting.Rewriting.simplifyTerm(raw);
                  hydra.util.Lazy<String> v1 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                    hydra.rewriting.Rewriting.isFreeVariableInTerm(
                      new hydra.core.Name(v0),
                      rhsTerm),
                    () -> hydra.constants.Constants.ignoredVariable(),
                    () -> v0));
                  return hydra.lib.flows.Bind.apply(
                    hydra.monads.Monads.<hydra.graph.Graph>getState(),
                    (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Alternative>>) (g_ufr -> {
                      hydra.ext.haskell.ast.Name hname = hydra.ext.haskell.utils.Utils.unionFieldReference(
                        g_ufr,
                        namespaces,
                        dn,
                        fn);
                      return hydra.lib.flows.Bind.apply(
                        hydra.lib.maybes.Cases.apply(
                          hydra.lib.maps.Lookup.apply(
                            fn,
                            fieldMap),
                          hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
                            "field ",
                            hydra.lib.literals.ShowString.apply((fn).value),
                            " not found in ",
                            hydra.lib.literals.ShowString.apply((dn).value)))),
                          (java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.haskell.ast.Pattern>>>) (fieldType -> {
                            hydra.core.Type ft = (fieldType).type;
                            return (hydra.rewriting.Rewriting.deannotateType(ft)).accept(new hydra.core.Type.PartialVisitor<>() {
                              @Override
                              public hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.haskell.ast.Pattern>> otherwise(hydra.core.Type instance) {
                                return hydra.ext.haskell.coder.Coder.encodeFunction_singleArg(
                                  hydra.ext.haskell.utils.Utils::rawName,
                                  v1.get());
                              }
                              
                              @Override
                              public hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.haskell.ast.Pattern>> visit(hydra.core.Type.Unit ignored) {
                                return hydra.ext.haskell.coder.Coder.<hydra.graph.Graph, hydra.ext.haskell.ast.Pattern>encodeFunction_noArgs();
                              }
                            });
                          })),
                        (java.util.function.Function<java.util.List<hydra.ext.haskell.ast.Pattern>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Alternative>>) (args -> {
                          hydra.ext.haskell.ast.Pattern lhs = hydra.ext.haskell.utils.Utils.applicationPattern(
                            hname,
                            args);
                          return hydra.lib.flows.Bind.apply(
                            hydra.lib.flows.Map.apply(
                              (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.CaseRhs>) (x -> new hydra.ext.haskell.ast.CaseRhs(x)),
                              hydra.ext.haskell.coder.Coder.encodeTerm(
                                namespaces,
                                rhsTerm)),
                            (java.util.function.Function<hydra.ext.haskell.ast.CaseRhs, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Alternative>>) (rhs -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Alternative(lhs, rhs, (hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>) (hydra.util.Maybe.<hydra.ext.haskell.ast.LocalBindings>nothing())))));
                        }));
                    }));
                }));
            }));
            hydra.util.Lazy<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>> caseExpr = new hydra.util.Lazy<>(() -> hydra.lib.flows.Bind.apply(
              hydra.lexical.Lexical.withSchemaContext(hydra.schemas.Schemas.requireUnionType(dn)),
              (java.util.function.Function<hydra.core.RowType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (rt -> {
                java.util.function.Function<hydra.core.FieldType, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.FieldType>> toFieldMapEntry = (java.util.function.Function<hydra.core.FieldType, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.FieldType>>) (f -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.FieldType>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.FieldType>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.FieldType>((f).name, f))));
                hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.FieldType>> fieldMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
                  toFieldMapEntry,
                  (rt).fields)));
                return hydra.lib.flows.Bind.apply(
                  hydra.lib.flows.MapList.apply(
                    (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Alternative>>) (v1 -> ((toAlt).apply(fieldMap.get())).apply(v1)),
                    fields),
                  (java.util.function.Function<java.util.List<hydra.ext.haskell.ast.Alternative>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (ecases -> hydra.lib.flows.Bind.apply(
                    hydra.lib.maybes.Cases.apply(
                      def,
                      hydra.lib.flows.Pure.apply((java.util.List<hydra.ext.haskell.ast.Alternative>) (java.util.List.<hydra.ext.haskell.ast.Alternative>of())),
                      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.haskell.ast.Alternative>>>) (d -> hydra.lib.flows.Bind.apply(
                        hydra.lib.flows.Map.apply(
                          (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.CaseRhs>) (x -> new hydra.ext.haskell.ast.CaseRhs(x)),
                          hydra.ext.haskell.coder.Coder.encodeTerm(
                            namespaces,
                            d)),
                        (java.util.function.Function<hydra.ext.haskell.ast.CaseRhs, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.haskell.ast.Alternative>>>) (cs -> {
                          hydra.ext.haskell.ast.Pattern lhs = new hydra.ext.haskell.ast.Pattern.Name(hydra.ext.haskell.utils.Utils.rawName(hydra.constants.Constants.ignoredVariable()));
                          hydra.util.Lazy<hydra.ext.haskell.ast.Alternative> alt = new hydra.util.Lazy<>(() -> new hydra.ext.haskell.ast.Alternative(lhs, cs, (hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>) (hydra.util.Maybe.<hydra.ext.haskell.ast.LocalBindings>nothing())));
                          return hydra.lib.flows.Pure.apply(java.util.List.of(alt.get()));
                        })))),
                    (java.util.function.Function<java.util.List<hydra.ext.haskell.ast.Alternative>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (dcases -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Expression.Case(new hydra.ext.haskell.ast.CaseExpression(hydra.ext.haskell.utils.Utils.hsvar("x"), hydra.lib.lists.Concat2.apply(
                      ecases,
                      dcases))))))));
              })));
            return hydra.lib.flows.Map.apply(
              (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression>) (v1 -> hydra.ext.haskell.utils.Utils.hslambda(
                hydra.ext.haskell.utils.Utils.rawName("x"),
                v1)),
              caseExpr.get());
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Function.Lambda lam) {
        hydra.core.Term body = ((lam).value).body;
        hydra.core.Name v = ((lam).value).parameter;
        return hydra.lib.flows.Bind.apply(
          hydra.ext.haskell.coder.Coder.encodeTerm(
            namespaces,
            body),
          (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (hbody -> hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hslambda(
            hydra.ext.haskell.utils.Utils.elementReference(
              namespaces,
              v),
            hbody))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Function.Primitive name) {
        return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Expression.Variable(hydra.ext.haskell.utils.Utils.elementReference(
          namespaces,
          (name).value)));
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T0, java.util.List<T1>> encodeFunction_noArgs() {
    return hydra.lib.flows.Pure.apply((java.util.List<T1>) (java.util.List.<T1>of()));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, java.util.List<hydra.ext.haskell.ast.Pattern>> encodeFunction_singleArg(java.util.function.Function<T0, hydra.ext.haskell.ast.Name> hydra_ext_haskell_utils_rawName2, T0 v1) {
    return hydra.lib.flows.Pure.apply(java.util.List.of(new hydra.ext.haskell.ast.Pattern.Name((hydra_ext_haskell_utils_rawName2).apply(v1))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> encodeLiteral(hydra.core.Literal l) {
    return (l).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> otherwise(hydra.core.Literal instance) {
        return hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
          "literal value ",
          hydra.show.core.Core.literal(l)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.Literal.Binary bs) {
        return hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hsapp(
          hydra.ext.haskell.utils.Utils.hsvar("Literals.stringToBinary"),
          hydra.ext.haskell.utils.Utils.hslit(new hydra.ext.haskell.ast.Literal.String_(hydra.lib.literals.BinaryToString.apply((bs).value)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.Literal.Boolean_ b) {
        return hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hsvar(hydra.lib.logic.IfElse.lazy(
          (b).value,
          () -> "True",
          () -> "False")));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.Literal.Float_ fv) {
        return ((fv).value).accept(new hydra.core.FloatValue.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.FloatValue.Float32 f) {
            return hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hslit(new hydra.ext.haskell.ast.Literal.Float_((f).value)));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.FloatValue.Float64 f) {
            return hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hslit(new hydra.ext.haskell.ast.Literal.Double_((f).value)));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.FloatValue.Bigfloat f) {
            return hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hslit(new hydra.ext.haskell.ast.Literal.Double_(hydra.lib.literals.BigfloatToFloat64.apply((f).value))));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.Literal.Integer_ iv) {
        return ((iv).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Bigint i) {
            return hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hslit(new hydra.ext.haskell.ast.Literal.Integer_((i).value)));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Int8 i) {
            return hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hslit(new hydra.ext.haskell.ast.Literal.Integer_(hydra.lib.literals.Int8ToBigint.apply((i).value))));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Int16 i) {
            return hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hslit(new hydra.ext.haskell.ast.Literal.Integer_(hydra.lib.literals.Int16ToBigint.apply((i).value))));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Int32 i) {
            return hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hslit(new hydra.ext.haskell.ast.Literal.Int((i).value)));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Int64 i) {
            return hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hslit(new hydra.ext.haskell.ast.Literal.Integer_(hydra.lib.literals.Int64ToBigint.apply((i).value))));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Uint8 i) {
            return hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hslit(new hydra.ext.haskell.ast.Literal.Integer_(hydra.lib.literals.Uint8ToBigint.apply((i).value))));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Uint16 i) {
            return hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hslit(new hydra.ext.haskell.ast.Literal.Integer_(hydra.lib.literals.Uint16ToBigint.apply((i).value))));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Uint32 i) {
            return hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hslit(new hydra.ext.haskell.ast.Literal.Integer_(hydra.lib.literals.Uint32ToBigint.apply((i).value))));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Uint64 i) {
            return hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hslit(new hydra.ext.haskell.ast.Literal.Integer_(hydra.lib.literals.Uint64ToBigint.apply((i).value))));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Expression> visit(hydra.core.Literal.String_ s) {
        return hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hslit(new hydra.ext.haskell.ast.Literal.String_((s).value)));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> encodeTerm(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>> encode = (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (v1 -> hydra.ext.haskell.coder.Coder.encodeTerm(
      namespaces,
      v1));
    java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>> nonemptyMap = (java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (m -> {
      java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>> encodePair = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (pair -> {
        hydra.util.Lazy<hydra.core.Term> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
        hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> hk = (encode).apply(k.get());
        hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
        hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> hv = (encode).apply(v.get());
        return hydra.lib.flows.Map.apply(
          (java.util.function.Function<java.util.List<hydra.ext.haskell.ast.Expression>, hydra.ext.haskell.ast.Expression>) (x -> new hydra.ext.haskell.ast.Expression.Tuple(x)),
          hydra.lib.flows.Sequence.apply(java.util.List.of(
            hk,
            hv)));
      });
      hydra.ext.haskell.ast.Expression lhs = hydra.ext.haskell.utils.Utils.hsvar("M.fromList");
      return hydra.lib.flows.Bind.apply(
        hydra.lib.flows.Map.apply(
          (java.util.function.Function<java.util.List<hydra.ext.haskell.ast.Expression>, hydra.ext.haskell.ast.Expression>) (x -> new hydra.ext.haskell.ast.Expression.List(x)),
          hydra.lib.flows.MapList.apply(
            encodePair,
            hydra.lib.maps.ToList.apply(m))),
        (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (rhs -> hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hsapp(
          lhs,
          rhs))));
    });
    java.util.function.Function<java.util.Set<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>> nonemptySet = (java.util.function.Function<java.util.Set<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (s -> {
      hydra.ext.haskell.ast.Expression lhs = hydra.ext.haskell.utils.Utils.hsvar("S.fromList");
      return hydra.lib.flows.Bind.apply(
        hydra.ext.haskell.coder.Coder.encodeTerm(
          namespaces,
          new hydra.core.Term.List(hydra.lib.sets.ToList.apply(s))),
        (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (rhs -> hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hsapp(
          lhs,
          rhs))));
    });
    return (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> otherwise(hydra.core.Term instance) {
        return hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
          "unexpected term: ",
          hydra.show.core.Core.term(term)));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Application app) {
        hydra.core.Term arg = ((app).value).argument;
        hydra.core.Term fun = ((app).value).function;
        return hydra.lib.flows.Bind.apply(
          (encode).apply(fun),
          (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (hfun -> hydra.lib.flows.Bind.apply(
            (encode).apply(arg),
            (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (harg -> hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hsapp(
              hfun,
              harg))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (l -> hydra.lib.flows.Bind.apply(
            (encode).apply(l),
            (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (hl -> hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hsapp(
              hydra.ext.haskell.utils.Utils.hsvar("Left"),
              hl))))),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (r -> hydra.lib.flows.Bind.apply(
            (encode).apply(r),
            (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (hr -> hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hsapp(
              hydra.ext.haskell.utils.Utils.hsvar("Right"),
              hr))))),
          (e).value);
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Function f) {
        return hydra.ext.haskell.coder.Coder.encodeFunction(
          namespaces,
          (f).value);
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Let letTerm) {
        java.util.List<hydra.core.Binding> bindings = ((letTerm).value).bindings;
        java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.LocalBinding>> encodeBinding = (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.LocalBinding>>) (binding -> {
          hydra.core.Name name = (binding).name;
          hydra.ext.haskell.ast.Name hname = hydra.ext.haskell.utils.Utils.simpleName((name).value);
          hydra.core.Term term_ = (binding).term;
          return hydra.lib.flows.Bind.apply(
            (encode).apply(term_),
            (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.LocalBinding>>) (hexpr -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.LocalBinding.Value(hydra.ext.haskell.utils.Utils.simpleValueBinding(
              hname,
              hexpr,
              (hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>) (hydra.util.Maybe.<hydra.ext.haskell.ast.LocalBindings>nothing()))))));
        });
        hydra.core.Term env = ((letTerm).value).body;
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            encodeBinding,
            bindings),
          (java.util.function.Function<java.util.List<hydra.ext.haskell.ast.LocalBinding>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (hbindings -> hydra.lib.flows.Bind.apply(
            (encode).apply(env),
            (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (hinner -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Expression.Let(new hydra.ext.haskell.ast.LetExpression(hbindings, hinner)))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.List els) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            encode,
            (els).value),
          (java.util.function.Function<java.util.List<hydra.ext.haskell.ast.Expression>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (helems -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Expression.List(helems))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Literal v) {
        return hydra.ext.haskell.coder.Coder.encodeLiteral((v).value);
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Map m) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.maps.Null.apply((m).value),
          () -> hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hsvar("M.empty")),
          () -> (nonemptyMap).apply((m).value));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.maybes.Cases.apply(
          (m).value,
          hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hsvar("Nothing")),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (t -> hydra.lib.flows.Bind.apply(
            (encode).apply(t),
            (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (ht -> hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hsapp(
              hydra.ext.haskell.utils.Utils.hsvar("Just"),
              ht))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Pair p) {
        return hydra.lib.flows.Bind.apply(
          (encode).apply(hydra.lib.pairs.First.apply((p).value)),
          (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (f -> hydra.lib.flows.Bind.apply(
            (encode).apply(hydra.lib.pairs.Second.apply((p).value)),
            (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (s -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Expression.Tuple(java.util.List.of(
              f,
              s)))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Record record) {
        java.util.List<hydra.core.Field> fields = ((record).value).fields;
        hydra.core.Name sname = ((record).value).typeName;
        java.util.function.Function<hydra.core.Field, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.FieldUpdate>> toFieldUpdate = (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.FieldUpdate>>) (field -> {
          hydra.core.Name fn = (field).name;
          hydra.ext.haskell.ast.Name fieldRef = hydra.ext.haskell.utils.Utils.recordFieldReference(
            namespaces,
            sname,
            fn);
          hydra.core.Term ft = (field).term;
          return hydra.lib.flows.Bind.apply(
            (encode).apply(ft),
            (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.FieldUpdate>>) (hft -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.FieldUpdate(fieldRef, hft))));
        });
        hydra.ext.haskell.ast.Name typeName = hydra.ext.haskell.utils.Utils.elementReference(
          namespaces,
          sname);
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            toFieldUpdate,
            fields),
          (java.util.function.Function<java.util.List<hydra.ext.haskell.ast.FieldUpdate>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (updates -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Expression.ConstructRecord(new hydra.ext.haskell.ast.ConstructRecordExpression(typeName, updates)))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Set s) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.sets.Null.apply((s).value),
          () -> hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hsvar("S.empty")),
          () -> (nonemptySet).apply((s).value));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.TypeLambda abs) {
        hydra.core.Term term1 = ((abs).value).body;
        return (encode).apply(term1);
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.TypeApplication typed) {
        hydra.core.Term term1 = ((typed).value).body;
        return (encode).apply(term1);
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Union injection) {
        hydra.core.Field field = ((injection).value).field;
        hydra.core.Name fn = (field).name;
        hydra.core.Term ft = (field).term;
        hydra.core.Name sname = ((injection).value).typeName;
        return hydra.lib.flows.Bind.apply(
          hydra.monads.Monads.<hydra.graph.Graph>getState(),
          (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (g_ufr2 -> {
            hydra.ext.haskell.ast.Expression lhs = new hydra.ext.haskell.ast.Expression.Variable(hydra.ext.haskell.utils.Utils.unionFieldReference(
              g_ufr2,
              namespaces,
              sname,
              fn));
            hydra.util.Lazy<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>> dflt = new hydra.util.Lazy<>(() -> hydra.lib.flows.Map.apply(
              (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression>) (v1 -> hydra.ext.haskell.utils.Utils.hsapp(
                lhs,
                v1)),
              (encode).apply(ft)));
            return hydra.lib.flows.Bind.apply(
              hydra.schemas.Schemas.requireUnionField(
                sname,
                fn),
              (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (ftyp -> (hydra.rewriting.Rewriting.deannotateType(ftyp)).accept(new hydra.core.Type.PartialVisitor<>() {
                @Override
                public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> otherwise(hydra.core.Type instance) {
                  return dflt.get();
                }
                
                @Override
                public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Type.Unit ignored) {
                  return hydra.lib.flows.Pure.apply(lhs);
                }
              })));
          }));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Unit ignored) {
        return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Expression.Tuple((java.util.List<hydra.ext.haskell.ast.Expression>) (java.util.List.<hydra.ext.haskell.ast.Expression>of())));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Variable name) {
        return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Expression.Variable(hydra.ext.haskell.utils.Utils.elementReference(
          namespaces,
          (name).value)));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Wrap wrapped) {
        hydra.core.Name tname = ((wrapped).value).typeName;
        hydra.ext.haskell.ast.Expression lhs = new hydra.ext.haskell.ast.Expression.Variable(hydra.ext.haskell.utils.Utils.elementReference(
          namespaces,
          tname));
        hydra.core.Term term_ = ((wrapped).value).body;
        return hydra.lib.flows.Bind.apply(
          (encode).apply(term_),
          (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (rhs -> hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.hsapp(
            lhs,
            rhs))));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> encodeType(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Type typ) {
    java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type>> encode = (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type>>) (v1 -> hydra.ext.haskell.coder.Coder.<T0>encodeType_encode(
      namespaces,
      v1));
    hydra.util.Lazy<hydra.ext.haskell.ast.Type> unitTuple = new hydra.util.Lazy<>(() -> new hydra.ext.haskell.ast.Type.Tuple((java.util.List<hydra.ext.haskell.ast.Type>) (java.util.List.<hydra.ext.haskell.ast.Type>of())));
    return hydra.monads.Monads.withTrace(
      "encode type",
      (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> otherwise(hydra.core.Type instance) {
          return hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
            "unexpected type: ",
            hydra.show.core.Core.type(typ)));
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Application app) {
          hydra.core.Type lhs = ((app).value).function;
          hydra.core.Type rhs = ((app).value).argument;
          return hydra.lib.flows.Bind.apply(
            (encode).apply(lhs),
            (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type>>) (hlhs -> hydra.lib.flows.Bind.apply(
              (encode).apply(rhs),
              (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type>>) (hrhs -> hydra.lib.flows.Pure.apply(hydra.ext.haskell.utils.Utils.toTypeApplication(java.util.List.of(
                hlhs,
                hrhs)))))));
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Either eitherType) {
          hydra.core.Type left = ((eitherType).value).left;
          hydra.core.Type right = ((eitherType).value).right;
          return hydra.lib.flows.Map.apply(
            hydra.ext.haskell.utils.Utils::toTypeApplication,
            hydra.lib.flows.Sequence.apply(java.util.List.of(
              hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.utils.Utils.rawName("Either"))),
              (encode).apply(left),
              (encode).apply(right))));
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Function funType) {
          hydra.core.Type cod = ((funType).value).codomain;
          hydra.core.Type dom = ((funType).value).domain;
          return hydra.lib.flows.Bind.apply(
            (encode).apply(dom),
            (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type>>) (hdom -> hydra.lib.flows.Bind.apply(
              (encode).apply(cod),
              (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type>>) (hcod -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Function(new hydra.ext.haskell.ast.FunctionType(hdom, hcod)))))));
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Forall forallType) {
          hydra.core.Type body = ((forallType).value).body;
          hydra.core.Name v = ((forallType).value).parameter;
          return (encode).apply(body);
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.List lt) {
          return hydra.lib.flows.Bind.apply(
            (encode).apply((lt).value),
            (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type>>) (hlt -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.List(hlt))));
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Literal lt) {
          return ((lt).value).accept(new hydra.core.LiteralType.PartialVisitor<>() {
            @Override
            public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> otherwise(hydra.core.LiteralType instance) {
              return hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
                "unexpected literal type: ",
                hydra.show.core.Core.literalType((lt).value)));
            }
            
            @Override
            public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.LiteralType.Binary ignored) {
              return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.utils.Utils.rawName("B.ByteString")));
            }
            
            @Override
            public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.LiteralType.Boolean_ ignored) {
              return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.utils.Utils.rawName("Bool")));
            }
            
            @Override
            public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.LiteralType.Float_ ft) {
              return ((ft).value).accept(new hydra.core.FloatType.PartialVisitor<>() {
                @Override
                public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.FloatType.Float32 ignored) {
                  return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.utils.Utils.rawName("Float")));
                }
                
                @Override
                public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.FloatType.Float64 ignored) {
                  return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.utils.Utils.rawName("Double")));
                }
                
                @Override
                public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.FloatType.Bigfloat ignored) {
                  return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.utils.Utils.rawName("Double")));
                }
              });
            }
            
            @Override
            public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.LiteralType.Integer_ it) {
              return ((it).value).accept(new hydra.core.IntegerType.PartialVisitor<>() {
                @Override
                public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> otherwise(hydra.core.IntegerType instance) {
                  return hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
                    "unexpected integer type: ",
                    hydra.show.core.Core.integerType((it).value)));
                }
                
                @Override
                public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.IntegerType.Bigint ignored) {
                  return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.utils.Utils.rawName("Integer")));
                }
                
                @Override
                public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.IntegerType.Int8 ignored) {
                  return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.utils.Utils.rawName("I.Int8")));
                }
                
                @Override
                public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.IntegerType.Int16 ignored) {
                  return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.utils.Utils.rawName("I.Int16")));
                }
                
                @Override
                public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.IntegerType.Int32 ignored) {
                  return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.utils.Utils.rawName("Int")));
                }
                
                @Override
                public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.IntegerType.Int64 ignored) {
                  return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.utils.Utils.rawName("I.Int64")));
                }
              });
            }
            
            @Override
            public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.LiteralType.String_ ignored) {
              return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.utils.Utils.rawName("String")));
            }
          });
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Map mapType) {
          hydra.core.Type kt = ((mapType).value).keys;
          hydra.core.Type vt = ((mapType).value).values;
          return hydra.lib.flows.Map.apply(
            hydra.ext.haskell.utils.Utils::toTypeApplication,
            hydra.lib.flows.Sequence.apply(java.util.List.of(
              hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.utils.Utils.rawName("M.Map"))),
              (encode).apply(kt),
              (encode).apply(vt))));
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Maybe ot) {
          return hydra.lib.flows.Map.apply(
            hydra.ext.haskell.utils.Utils::toTypeApplication,
            hydra.lib.flows.Sequence.apply(java.util.List.of(
              hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.utils.Utils.rawName("Maybe"))),
              (encode).apply((ot).value))));
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Pair pt) {
          return hydra.lib.flows.Bind.apply(
            (encode).apply(((pt).value).first),
            (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type>>) (f -> hydra.lib.flows.Bind.apply(
              (encode).apply(((pt).value).second),
              (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type>>) (s -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Tuple(java.util.List.of(
                f,
                s)))))));
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Record rt) {
          return ((java.util.function.Function<hydra.core.Name, hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type>>) (v1 -> hydra.ext.haskell.coder.Coder.encodeType_ref(
            (java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, java.util.function.Function<hydra.core.Name, hydra.ext.haskell.ast.Name>>) (p0 -> p1 -> hydra.ext.haskell.utils.Utils.elementReference(
              p0,
              p1)),
            namespaces,
            v1))).apply(((rt).value).typeName);
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Set st) {
          return hydra.lib.flows.Map.apply(
            hydra.ext.haskell.utils.Utils::toTypeApplication,
            hydra.lib.flows.Sequence.apply(java.util.List.of(
              hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.utils.Utils.rawName("S.Set"))),
              (encode).apply((st).value))));
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Union rt) {
          hydra.core.Name typeName = ((rt).value).typeName;
          return ((java.util.function.Function<hydra.core.Name, hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type>>) (v1 -> hydra.ext.haskell.coder.Coder.encodeType_ref(
            (java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, java.util.function.Function<hydra.core.Name, hydra.ext.haskell.ast.Name>>) (p0 -> p1 -> hydra.ext.haskell.utils.Utils.elementReference(
              p0,
              p1)),
            namespaces,
            v1))).apply(typeName);
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Unit ignored) {
          return hydra.lib.flows.Pure.apply(unitTuple.get());
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Variable v1) {
          return ((java.util.function.Function<hydra.core.Name, hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type>>) (v12 -> hydra.ext.haskell.coder.Coder.encodeType_ref(
            (java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, java.util.function.Function<hydra.core.Name, hydra.ext.haskell.ast.Name>>) (p0 -> p1 -> hydra.ext.haskell.utils.Utils.elementReference(
              p0,
              p1)),
            namespaces,
            v12))).apply((v1).value);
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Wrap wrapped) {
          hydra.core.Name name = ((wrapped).value).typeName;
          return ((java.util.function.Function<hydra.core.Name, hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type>>) (v1 -> hydra.ext.haskell.coder.Coder.encodeType_ref(
            (java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, java.util.function.Function<hydra.core.Name, hydra.ext.haskell.ast.Name>>) (p0 -> p1 -> hydra.ext.haskell.utils.Utils.elementReference(
              p0,
              p1)),
            namespaces,
            v1))).apply(name);
        }
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.haskell.ast.Type> encodeType_encode(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Type v1) {
    return hydra.ext.haskell.coder.Coder.<T0>encodeType(
      namespaces,
      v1);
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T2, hydra.ext.haskell.ast.Type> encodeType_ref(java.util.function.Function<T0, java.util.function.Function<T1, hydra.ext.haskell.ast.Name>> hydra_ext_haskell_utils_elementReference2, T0 namespaces, T1 name) {
    return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Variable(((hydra_ext_haskell_utils_elementReference2).apply(namespaces)).apply(name)));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Type> encodeTypeWithClassAssertions(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>> explicitClasses, hydra.core.Type typ) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>> classes = new hydra.util.Lazy<>(() -> hydra.lib.maps.Union.apply(
      explicitClasses,
      hydra.ext.haskell.coder.Coder.getImplicitTypeClasses(typ)));
    hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.classes.TypeClass>>> assertPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      p0 -> hydra.ext.haskell.coder.Coder.<hydra.core.Name, hydra.classes.TypeClass>encodeTypeWithClassAssertions_toPairs(p0),
      hydra.lib.maps.ToList.apply(classes.get()))));
    java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.classes.TypeClass>, hydra.ext.haskell.ast.Assertion> encodeAssertion = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.classes.TypeClass>, hydra.ext.haskell.ast.Assertion>) (pair -> {
      hydra.util.Lazy<hydra.classes.TypeClass> cls = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
      hydra.ext.haskell.ast.Name hname = hydra.ext.haskell.utils.Utils.rawName((cls.get()).accept(new hydra.classes.TypeClass.PartialVisitor<>() {
        @Override
        public String visit(hydra.classes.TypeClass.Equality ignored) {
          return "Eq";
        }
        
        @Override
        public String visit(hydra.classes.TypeClass.Ordering ignored) {
          return "Ord";
        }
      }));
      hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
      hydra.ext.haskell.ast.Type htype = new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.utils.Utils.rawName((name.get()).value));
      return new hydra.ext.haskell.ast.Assertion.Class_(new hydra.ext.haskell.ast.ClassAssertion(hname, java.util.List.of(htype)));
    });
    java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>> implicitClasses = hydra.ext.haskell.coder.Coder.getImplicitTypeClasses(typ);
    return hydra.monads.Monads.withTrace(
      "encode with assertions",
      hydra.lib.flows.Bind.apply(
        hydra.ext.haskell.coder.Coder.adaptTypeToHaskellAndEncode(
          namespaces,
          typ),
        (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Type>>) (htyp -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(assertPairs.get()),
          () -> hydra.lib.flows.Pure.apply(htyp),
          () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Type>>) (() -> {
            hydra.util.Lazy<java.util.List<hydra.ext.haskell.ast.Assertion>> encoded = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              encodeAssertion,
              assertPairs.get()));
            hydra.util.Lazy<hydra.ext.haskell.ast.Assertion> hassert = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                hydra.lib.lists.Length.apply(encoded.get()),
                1),
              () -> hydra.lib.lists.Head.apply(encoded.get()),
              () -> new hydra.ext.haskell.ast.Assertion.Tuple(encoded.get())));
            return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Type.Ctx(new hydra.ext.haskell.ast.ContextType(hassert.get(), htyp)));
          })).get()))));
  }
  
  static <T0, T1> java.util.List<hydra.util.Tuple.Tuple2<T0, T1>> encodeTypeWithClassAssertions_toPairs(hydra.util.Tuple.Tuple2<T0, java.util.Set<T1>> mapEntry) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T0, T1>>) (v1 -> hydra.ext.haskell.coder.Coder.<T0, java.util.Set<T1>, T1>encodeTypeWithClassAssertions_toPair(
        mapEntry,
        v1)),
      hydra.lib.sets.ToList.apply(hydra.ext.haskell.coder.Coder.<T0, java.util.Set<T1>>encodeTypeWithClassAssertions_clsSet(mapEntry)));
  }
  
  static <T0, T1> T0 encodeTypeWithClassAssertions_name(hydra.util.Tuple.Tuple2<T0, T1> mapEntry) {
    return hydra.lib.pairs.First.apply(mapEntry);
  }
  
  static <T0, T1> T1 encodeTypeWithClassAssertions_clsSet(hydra.util.Tuple.Tuple2<T0, T1> mapEntry) {
    return hydra.lib.pairs.Second.apply(mapEntry);
  }
  
  static <T0, T1, T2> hydra.util.Tuple.Tuple2<T0, T2> encodeTypeWithClassAssertions_toPair(hydra.util.Tuple.Tuple2<T0, T1> mapEntry, T2 c) {
    return (hydra.util.Tuple.Tuple2<T0, T2>) ((hydra.util.Tuple.Tuple2<T0, T2>) (new hydra.util.Tuple.Tuple2<T0, T2>(hydra.ext.haskell.coder.Coder.<T0, T1>encodeTypeWithClassAssertions_name(mapEntry), c)));
  }
  
  static java.util.Set<hydra.core.Name> findOrdVariables(hydra.core.Type typ) {
    java.util.function.Function<hydra.core.Name, Boolean> isTypeVariable = (java.util.function.Function<hydra.core.Name, Boolean>) (v -> {
      hydra.util.Lazy<Boolean> hasNoNamespace = new hydra.util.Lazy<>(() -> hydra.lib.maybes.IsNothing.apply(hydra.names.Names.namespaceOf(v)));
      String nameStr = (v).value;
      hydra.util.Lazy<Boolean> startsWithT = new hydra.util.Lazy<>(() -> hydra.lib.equality.Equal.apply(
        hydra.lib.strings.CharAt.apply(
          0,
          nameStr),
        116));
      return hydra.lib.logic.And.apply(
        hasNoNamespace.get(),
        startsWithT.get());
    });
    java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>> tryType = (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>>) (names -> (java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>) (t -> (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.Set<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return names;
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Variable v) {
        return hydra.lib.logic.IfElse.lazy(
          (isTypeVariable).apply((v).value),
          () -> hydra.lib.sets.Insert.apply(
            (v).value,
            names),
          () -> names);
      }
    })));
    java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>> fold = (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>>) (names -> (java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>) (typ_ -> (typ_).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.Set<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return names;
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Map mapType) {
        hydra.core.Type kt = ((mapType).value).keys;
        return ((tryType).apply(names)).apply(kt);
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Set et) {
        return ((tryType).apply(names)).apply((et).value);
      }
    })));
    return hydra.rewriting.Rewriting.foldOverType(
      new hydra.coders.TraversalOrder.Pre(),
      fold,
      (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      typ);
  }
  
  static java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>> getImplicitTypeClasses(hydra.core.Type typ) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      p0 -> hydra.ext.haskell.coder.Coder.<hydra.core.Name>getImplicitTypeClasses_toPair(p0),
      hydra.lib.sets.ToList.apply(hydra.ext.haskell.coder.Coder.findOrdVariables(typ))));
  }
  
  static <T0> hydra.util.Tuple.Tuple2<T0, java.util.Set<hydra.classes.TypeClass>> getImplicitTypeClasses_toPair(T0 name) {
    return (hydra.util.Tuple.Tuple2<T0, java.util.Set<hydra.classes.TypeClass>>) ((hydra.util.Tuple.Tuple2<T0, java.util.Set<hydra.classes.TypeClass>>) (new hydra.util.Tuple.Tuple2<T0, java.util.Set<hydra.classes.TypeClass>>(name, hydra.lib.sets.FromList.apply(java.util.List.of(new hydra.classes.TypeClass.Ordering())))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Module> moduleToHaskellModule(hydra.module.Module mod, java.util.List<hydra.module.Definition> defs) {
    return hydra.lib.flows.Bind.apply(
      hydra.ext.haskell.utils.Utils.namespacesForModule(mod),
      (java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Module>>) (namespaces -> hydra.ext.haskell.coder.Coder.constructModule(
        namespaces,
        mod,
        defs)));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.Map<String, String>> moduleToHaskell(hydra.module.Module mod, java.util.List<hydra.module.Definition> defs) {
    return hydra.lib.flows.Bind.apply(
      hydra.ext.haskell.coder.Coder.moduleToHaskellModule(
        mod,
        defs),
      (java.util.function.Function<hydra.ext.haskell.ast.Module, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<String, String>>>) (hsmod -> {
        String filepath = hydra.names.Names.namespaceToFilePath(
          new hydra.util.CaseConvention.Pascal(),
          new hydra.module.FileExtension("hs"),
          (mod).namespace);
        String s = hydra.serialization.Serialization.printExpr(hydra.serialization.Serialization.parenthesize(hydra.ext.haskell.serde.Serde.moduleToExpr(hsmod)));
        return hydra.lib.flows.Pure.apply(hydra.lib.maps.Singleton.apply(
          filepath,
          s));
      }));
  }
  
  static <T0> java.util.List<hydra.ext.haskell.ast.DeclarationWithComments> nameDecls(T0 g, hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Name name, hydra.core.Type typ) {
    java.util.function.Function<hydra.core.FieldType, hydra.util.Tuple.Tuple2<String, String>> toConstant = (java.util.function.Function<hydra.core.FieldType, hydra.util.Tuple.Tuple2<String, String>>) (fieldType -> {
      hydra.core.Name fname = (fieldType).name;
      return (hydra.util.Tuple.Tuple2<String, String>) ((hydra.util.Tuple.Tuple2<String, String>) (new hydra.util.Tuple.Tuple2<String, String>(hydra.ext.haskell.coder.Coder.constantForFieldName(
        name,
        fname), (fname).value)));
    });
    hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<String, String>>> fieldDecls = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      toConstant,
      hydra.lexical.Lexical.fieldsOf(typ)));
    String nm = (name).value;
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<String, String>> nameDecl = new hydra.util.Lazy<>(() -> (hydra.util.Tuple.Tuple2<String, String>) ((hydra.util.Tuple.Tuple2<String, String>) (new hydra.util.Tuple.Tuple2<String, String>(hydra.ext.haskell.coder.Coder.constantForTypeName(name), nm))));
    java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.util.Tuple.Tuple2<String, String>, hydra.ext.haskell.ast.DeclarationWithComments>> toDecl = (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.util.Tuple.Tuple2<String, String>, hydra.ext.haskell.ast.DeclarationWithComments>>) (n -> (java.util.function.Function<hydra.util.Tuple.Tuple2<String, String>, hydra.ext.haskell.ast.DeclarationWithComments>) (pair -> {
      hydra.util.Lazy<String> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
      hydra.util.Lazy<String> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
      hydra.util.Lazy<hydra.ext.haskell.ast.Declaration> decl = new hydra.util.Lazy<>(() -> new hydra.ext.haskell.ast.Declaration.ValueBinding(new hydra.ext.haskell.ast.ValueBinding.Simple(new hydra.ext.haskell.ast.SimpleValueBinding(hydra.ext.haskell.utils.Utils.applicationPattern(
        hydra.ext.haskell.utils.Utils.simpleName(k.get()),
        (java.util.List<hydra.ext.haskell.ast.Pattern>) (java.util.List.<hydra.ext.haskell.ast.Pattern>of())), new hydra.ext.haskell.ast.RightHandSide(new hydra.ext.haskell.ast.Expression.Application(new hydra.ext.haskell.ast.ApplicationExpression(new hydra.ext.haskell.ast.Expression.Variable(hydra.ext.haskell.utils.Utils.elementReference(
        namespaces,
        n)), new hydra.ext.haskell.ast.Expression.Literal(new hydra.ext.haskell.ast.Literal.String_(v.get()))))), (hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>) (hydra.util.Maybe.<hydra.ext.haskell.ast.LocalBindings>nothing())))));
      return new hydra.ext.haskell.ast.DeclarationWithComments(decl.get(), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()));
    }));
    return hydra.lib.logic.IfElse.lazy(
      hydra.ext.haskell.coder.Coder.useCoreImport(),
      () -> hydra.lib.lists.Cons.apply(
        ((toDecl).apply(new hydra.core.Name("hydra.core.Name"))).apply(nameDecl.get()),
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<String, String>, hydra.ext.haskell.ast.DeclarationWithComments>) (v1 -> ((toDecl).apply(new hydra.core.Name("hydra.core.Name"))).apply(v1)),
          fieldDecls.get())),
      () -> (java.util.List<hydra.ext.haskell.ast.DeclarationWithComments>) (java.util.List.<hydra.ext.haskell.ast.DeclarationWithComments>of()));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments> toDataDeclaration(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.module.TermDefinition def) {
    hydra.core.Name name = (def).name;
    hydra.ext.haskell.ast.Name hname = hydra.ext.haskell.utils.Utils.simpleName(hydra.names.Names.localNameOf(name));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.ext.haskell.ast.ValueBinding, hydra.ext.haskell.ast.ValueBinding>> rewriteValueBinding = new java.util.concurrent.atomic.AtomicReference<>();
    rewriteValueBinding.set((java.util.function.Function<hydra.ext.haskell.ast.ValueBinding, hydra.ext.haskell.ast.ValueBinding>) (vb -> (vb).accept(new hydra.ext.haskell.ast.ValueBinding.PartialVisitor<>() {
      @Override
      public hydra.ext.haskell.ast.ValueBinding visit(hydra.ext.haskell.ast.ValueBinding.Simple simple) {
        hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings> bindings = ((simple).value).localBindings;
        hydra.ext.haskell.ast.Pattern pattern_ = ((simple).value).pattern;
        hydra.ext.haskell.ast.RightHandSide rhs = ((simple).value).rhs;
        return (pattern_).accept(new hydra.ext.haskell.ast.Pattern.PartialVisitor<>() {
          @Override
          public hydra.ext.haskell.ast.ValueBinding otherwise(hydra.ext.haskell.ast.Pattern instance) {
            return vb;
          }
          
          @Override
          public hydra.ext.haskell.ast.ValueBinding visit(hydra.ext.haskell.ast.Pattern.Application appPat) {
            java.util.List<hydra.ext.haskell.ast.Pattern> args = ((appPat).value).args;
            hydra.ext.haskell.ast.Name name_ = ((appPat).value).name;
            hydra.ext.haskell.ast.Expression rhsExpr = (rhs).value;
            return (rhsExpr).accept(new hydra.ext.haskell.ast.Expression.PartialVisitor<>() {
              @Override
              public hydra.ext.haskell.ast.ValueBinding otherwise(hydra.ext.haskell.ast.Expression instance) {
                return vb;
              }
              
              @Override
              public hydra.ext.haskell.ast.ValueBinding visit(hydra.ext.haskell.ast.Expression.Lambda lambda_) {
                hydra.ext.haskell.ast.Expression body = ((lambda_).value).inner;
                java.util.List<hydra.ext.haskell.ast.Pattern> vars = ((lambda_).value).bindings;
                hydra.util.Lazy<hydra.ext.haskell.ast.Pattern> newPattern = new hydra.util.Lazy<>(() -> hydra.ext.haskell.utils.Utils.applicationPattern(
                  name_,
                  hydra.lib.lists.Concat2.apply(
                    args,
                    vars)));
                hydra.ext.haskell.ast.RightHandSide newRhs = new hydra.ext.haskell.ast.RightHandSide(body);
                return (rewriteValueBinding.get()).apply(new hydra.ext.haskell.ast.ValueBinding.Simple(new hydra.ext.haskell.ast.SimpleValueBinding(newPattern.get(), newRhs, bindings)));
              }
            });
          }
        });
      }
    })));
    hydra.core.Term term = (def).term;
    hydra.core.TypeScheme typ = (def).type;
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.Maybe<String>, java.util.function.Function<hydra.ext.haskell.ast.Name, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments>>>>>> toDecl = new java.util.concurrent.atomic.AtomicReference<>();
    toDecl.set((java.util.function.Function<hydra.util.Maybe<String>, java.util.function.Function<hydra.ext.haskell.ast.Name, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments>>>>>) (comments -> (java.util.function.Function<hydra.ext.haskell.ast.Name, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments>>>>) (hname_ -> (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments>>>) (term_ -> (java.util.function.Function<hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments>>) (bindings -> (hydra.rewriting.Rewriting.deannotateTerm(term_)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments> otherwise(hydra.core.Term instance) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.haskell.coder.Coder.encodeTerm(
            namespaces,
            term_),
          (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments>>) (hterm -> {
            hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> schemeConstraints = (typ).constraints;
            hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>> schemeClasses = new hydra.util.Lazy<>(() -> hydra.ext.haskell.coder.Coder.typeSchemeConstraintsToClassMap(schemeConstraints));
            hydra.ext.haskell.ast.ValueBinding vb = hydra.ext.haskell.utils.Utils.simpleValueBinding(
              hname_,
              hterm,
              bindings);
            return hydra.lib.flows.Bind.apply(
              hydra.annotations.Annotations.getTypeClasses(hydra.rewriting.Rewriting.removeTypesFromTerm(term)),
              (java.util.function.Function<java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments>>) (explicitClasses -> {
                hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>> combinedClasses = new hydra.util.Lazy<>(() -> hydra.lib.maps.Union.apply(
                  schemeClasses.get(),
                  explicitClasses));
                return hydra.lib.flows.Bind.apply(
                  hydra.ext.haskell.coder.Coder.encodeTypeWithClassAssertions(
                    namespaces,
                    combinedClasses.get(),
                    (typ).type),
                  (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments>>) (htype -> {
                    hydra.ext.haskell.ast.Declaration decl = new hydra.ext.haskell.ast.Declaration.TypedBinding(new hydra.ext.haskell.ast.TypedBinding(new hydra.ext.haskell.ast.TypeSignature(hname_, htype), (rewriteValueBinding.get()).apply(vb)));
                    return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.DeclarationWithComments(decl, comments));
                  }));
              }));
          }));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments> visit(hydra.core.Term.Let letTerm) {
        hydra.core.Term env = ((letTerm).value).body;
        java.util.List<hydra.core.Binding> lbindings = ((letTerm).value).bindings;
        hydra.util.Lazy<java.util.List<hydra.ext.haskell.ast.Name>> hnames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.ext.haskell.ast.Name>) (binding -> hydra.ext.haskell.utils.Utils.simpleName(((binding).name).value)),
          lbindings));
        hydra.util.Lazy<java.util.List<hydra.core.Term>> terms = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          projected -> projected.term,
          lbindings));
        java.util.function.Function<hydra.ext.haskell.ast.Name, java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.LocalBinding>> toBinding = (java.util.function.Function<hydra.ext.haskell.ast.Name, java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.LocalBinding>>) (hname_ -> (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.LocalBinding>) (hterm_ -> new hydra.ext.haskell.ast.LocalBinding.Value(hydra.ext.haskell.utils.Utils.simpleValueBinding(
          hname_,
          hterm_,
          (hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>) (hydra.util.Maybe.<hydra.ext.haskell.ast.LocalBindings>nothing())))));
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (v1 -> hydra.ext.haskell.coder.Coder.encodeTerm(
              namespaces,
              v1)),
            terms.get()),
          (java.util.function.Function<java.util.List<hydra.ext.haskell.ast.Expression>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments>>) (hterms -> {
            hydra.util.Lazy<java.util.List<hydra.ext.haskell.ast.LocalBinding>> hbindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.ZipWith.apply(
              toBinding,
              hnames.get(),
              hterms));
            return ((((toDecl.get()).apply(comments)).apply(hname_)).apply(env)).apply(hydra.util.Maybe.just(new hydra.ext.haskell.ast.LocalBindings(hbindings.get())));
          }));
      }
    }))))));
    return hydra.lib.flows.Bind.apply(
      hydra.annotations.Annotations.getTermDescription(term),
      (java.util.function.Function<hydra.util.Maybe<String>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments>>) (comments -> ((((toDecl.get()).apply(comments)).apply(hname)).apply(term)).apply((hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>) (hydra.util.Maybe.<hydra.ext.haskell.ast.LocalBindings>nothing()))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.haskell.ast.DeclarationWithComments>> toTypeDeclarationsFrom(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Name elementName, hydra.core.Type typ) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.ext.haskell.ast.Name, java.util.function.Function<java.util.List<hydra.core.Name>, hydra.ext.haskell.ast.DeclarationHead>>> declHead = new java.util.concurrent.atomic.AtomicReference<>();
    declHead.set((java.util.function.Function<hydra.ext.haskell.ast.Name, java.util.function.Function<java.util.List<hydra.core.Name>, hydra.ext.haskell.ast.DeclarationHead>>) (name -> (java.util.function.Function<java.util.List<hydra.core.Name>, hydra.ext.haskell.ast.DeclarationHead>) (vars_ -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(vars_),
      () -> new hydra.ext.haskell.ast.DeclarationHead.Simple(name),
      () -> ((java.util.function.Supplier<hydra.ext.haskell.ast.DeclarationHead>) (() -> {
        hydra.util.Lazy<hydra.core.Name> h = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(vars_));
        hydra.ext.haskell.ast.Variable hvar = new hydra.ext.haskell.ast.Variable(hydra.ext.haskell.utils.Utils.simpleName((h.get()).value));
        hydra.util.Lazy<java.util.List<hydra.core.Name>> rest = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(vars_));
        return new hydra.ext.haskell.ast.DeclarationHead.Application(new hydra.ext.haskell.ast.ApplicationDeclarationHead(((declHead.get()).apply(name)).apply(rest.get()), hvar));
      })).get()))));
    String lname = hydra.names.Names.localNameOf(elementName);
    hydra.ext.haskell.ast.Name hname = hydra.ext.haskell.utils.Utils.simpleName(lname);
    java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments>>> newtypeCons = (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments>>>) (tname -> (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments>>) (typ_ -> {
      hydra.ext.haskell.ast.Name hname2 = hydra.ext.haskell.utils.Utils.simpleName(hydra.ext.haskell.utils.Utils.newtypeAccessorName(tname));
      return hydra.lib.flows.Bind.apply(
        hydra.ext.haskell.coder.Coder.adaptTypeToHaskellAndEncode(
          namespaces,
          typ_),
        (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments>>) (htype -> {
          hydra.ext.haskell.ast.Name constructorName = hydra.ext.haskell.utils.Utils.simpleName(hydra.names.Names.localNameOf(tname));
          hydra.util.Lazy<hydra.ext.haskell.ast.FieldWithComments> hfield = new hydra.util.Lazy<>(() -> new hydra.ext.haskell.ast.FieldWithComments(new hydra.ext.haskell.ast.Field(hname, htype), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())));
          return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.ConstructorWithComments(new hydra.ext.haskell.ast.Constructor.Record(new hydra.ext.haskell.ast.RecordConstructor(constructorName, java.util.List.of(hfield.get()))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())));
        }));
    }));
    java.util.function.Function<String, java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments>>> recordCons = (java.util.function.Function<String, java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments>>>) (lname_ -> (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments>>) (fields -> {
      java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.FieldWithComments>> toField = (java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.FieldWithComments>>) (fieldType -> {
        hydra.core.Name fname = (fieldType).name;
        hydra.core.Type ftype = (fieldType).type;
        hydra.ext.haskell.ast.Name hname_ = hydra.ext.haskell.utils.Utils.simpleName(hydra.lib.strings.Cat2.apply(
          hydra.formatting.Formatting.decapitalize(lname_),
          hydra.formatting.Formatting.capitalize((fname).value)));
        return hydra.lib.flows.Bind.apply(
          hydra.ext.haskell.coder.Coder.adaptTypeToHaskellAndEncode(
            namespaces,
            ftype),
          (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.FieldWithComments>>) (htype -> hydra.lib.flows.Bind.apply(
            hydra.annotations.Annotations.getTypeDescription(ftype),
            (java.util.function.Function<hydra.util.Maybe<String>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.FieldWithComments>>) (comments -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.FieldWithComments(new hydra.ext.haskell.ast.Field(hname_, htype), comments))))));
      });
      return hydra.lib.flows.Bind.apply(
        hydra.lib.flows.MapList.apply(
          toField,
          fields),
        (java.util.function.Function<java.util.List<hydra.ext.haskell.ast.FieldWithComments>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments>>) (hFields -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.ConstructorWithComments(new hydra.ext.haskell.ast.Constructor.Record(new hydra.ext.haskell.ast.RecordConstructor(hydra.ext.haskell.utils.Utils.simpleName(lname_), hFields)), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())))));
    }));
    java.util.function.Function<hydra.graph.Graph, java.util.function.Function<String, java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments>>>> unionCons = (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<String, java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments>>>>) (g_ -> (java.util.function.Function<String, java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments>>>) (lname_ -> (java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments>>) (fieldType -> {
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<String, String>> deconflict = new java.util.concurrent.atomic.AtomicReference<>();
      deconflict.set((java.util.function.Function<String, String>) (name -> {
        hydra.util.Lazy<hydra.core.Name> tname = new hydra.util.Lazy<>(() -> hydra.names.Names.unqualifyName(new hydra.module.QualifiedName(hydra.util.Maybe.just(hydra.lib.pairs.First.apply(((java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>>) (projected -> projected.focus)).apply(namespaces))), name)));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.maybes.IsJust.apply(hydra.lib.lists.Find.apply(
            (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.equality.Equal.apply(
              (b).name,
              tname.get())),
            (g_).elements)),
          () -> (deconflict.get()).apply(hydra.lib.strings.Cat2.apply(
            name,
            "_")),
          () -> name);
      }));
      hydra.core.Name fname = (fieldType).name;
      hydra.core.Type ftype = (fieldType).type;
      return hydra.lib.flows.Bind.apply(
        hydra.annotations.Annotations.getTypeDescription(ftype),
        (java.util.function.Function<hydra.util.Maybe<String>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments>>) (comments -> {
          String nm = (deconflict.get()).apply(hydra.lib.strings.Cat2.apply(
            hydra.formatting.Formatting.capitalize(lname_),
            hydra.formatting.Formatting.capitalize((fname).value)));
          return hydra.lib.flows.Bind.apply(
            hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                hydra.rewriting.Rewriting.deannotateType(ftype),
                new hydra.core.Type.Unit()),
              () -> hydra.lib.flows.Pure.apply((java.util.List<hydra.ext.haskell.ast.Type>) (java.util.List.<hydra.ext.haskell.ast.Type>of())),
              () -> hydra.lib.flows.Bind.apply(
                hydra.ext.haskell.coder.Coder.adaptTypeToHaskellAndEncode(
                  namespaces,
                  ftype),
                (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.haskell.ast.Type>>>) (htype -> hydra.lib.flows.Pure.apply(java.util.List.of(htype))))),
            (java.util.function.Function<java.util.List<hydra.ext.haskell.ast.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments>>) (typeList -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.ConstructorWithComments(new hydra.ext.haskell.ast.Constructor.Ordinary(new hydra.ext.haskell.ast.OrdinaryConstructor(hydra.ext.haskell.utils.Utils.simpleName(nm), typeList)), comments))));
        }));
    })));
    return hydra.monads.Monads.withTrace(
      hydra.lib.strings.Cat2.apply(
        "type definition ",
        (elementName).value),
      hydra.lib.flows.Bind.apply(
        hydra.monads.Monads.<hydra.graph.Graph>getState(),
        (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.haskell.ast.DeclarationWithComments>>>) (g -> hydra.lib.flows.Bind.apply(
          hydra.schemas.Schemas.isSerializableByName(elementName),
          (java.util.function.Function<Boolean, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.haskell.ast.DeclarationWithComments>>>) (isSer -> {
            hydra.util.Lazy<hydra.ext.haskell.ast.Deriving> deriv = new hydra.util.Lazy<>(() -> new hydra.ext.haskell.ast.Deriving(hydra.lib.logic.IfElse.lazy(
              isSer,
              () -> hydra.lib.lists.Map.apply(
                hydra.ext.haskell.utils.Utils::rawName,
                java.util.List.of(
                  "Eq",
                  "Ord",
                  "Read",
                  "Show")),
              () -> (java.util.List<hydra.ext.haskell.ast.Name>) (java.util.List.<hydra.ext.haskell.ast.Name>of()))));
            hydra.util.Lazy<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Type>> unpackResult = new hydra.util.Lazy<>(() -> hydra.ext.haskell.utils.Utils.unpackForallType(
              g,
              typ));
            hydra.util.Lazy<java.util.List<hydra.core.Name>> vars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(unpackResult.get()));
            hydra.util.Lazy<hydra.ext.haskell.ast.DeclarationHead> hd = new hydra.util.Lazy<>(() -> ((declHead.get()).apply(hname)).apply(hydra.lib.lists.Reverse.apply(vars.get())));
            hydra.util.Lazy<hydra.core.Type> t_ = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(unpackResult.get()));
            return hydra.lib.flows.Bind.apply(
              (hydra.rewriting.Rewriting.deannotateType(t_.get())).accept(new hydra.core.Type.PartialVisitor<>() {
                @Override
                public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Declaration> otherwise(hydra.core.Type instance) {
                  return hydra.lib.flows.Bind.apply(
                    hydra.ext.haskell.coder.Coder.adaptTypeToHaskellAndEncode(
                      namespaces,
                      typ),
                    (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Declaration>>) (htype -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Declaration.Type(new hydra.ext.haskell.ast.TypeDeclaration(hd.get(), htype)))));
                }
                
                @Override
                public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Declaration> visit(hydra.core.Type.Record rt) {
                  return hydra.lib.flows.Bind.apply(
                    ((recordCons).apply(lname)).apply(((rt).value).fields),
                    (java.util.function.Function<hydra.ext.haskell.ast.ConstructorWithComments, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Declaration>>) (cons -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Declaration.Data(new hydra.ext.haskell.ast.DataDeclaration(new hydra.ext.haskell.ast.DataOrNewtype.Data(), (java.util.List<hydra.ext.haskell.ast.Assertion>) (java.util.List.<hydra.ext.haskell.ast.Assertion>of()), hd.get(), java.util.List.of(cons), java.util.List.of(deriv.get()))))));
                }
                
                @Override
                public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Declaration> visit(hydra.core.Type.Union rt) {
                  return hydra.lib.flows.Bind.apply(
                    hydra.lib.flows.MapList.apply(
                      (java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments>>) (v1 -> (((unionCons).apply(g)).apply(lname)).apply(v1)),
                      ((rt).value).fields),
                    (java.util.function.Function<java.util.List<hydra.ext.haskell.ast.ConstructorWithComments>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Declaration>>) (cons -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Declaration.Data(new hydra.ext.haskell.ast.DataDeclaration(new hydra.ext.haskell.ast.DataOrNewtype.Data(), (java.util.List<hydra.ext.haskell.ast.Assertion>) (java.util.List.<hydra.ext.haskell.ast.Assertion>of()), hd.get(), cons, java.util.List.of(deriv.get()))))));
                }
                
                @Override
                public hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Declaration> visit(hydra.core.Type.Wrap wrapped) {
                  hydra.core.Type wt = ((wrapped).value).body;
                  return hydra.lib.flows.Bind.apply(
                    ((newtypeCons).apply(elementName)).apply(wt),
                    (java.util.function.Function<hydra.ext.haskell.ast.ConstructorWithComments, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Declaration>>) (cons -> hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.Declaration.Data(new hydra.ext.haskell.ast.DataDeclaration(new hydra.ext.haskell.ast.DataOrNewtype.Newtype(), (java.util.List<hydra.ext.haskell.ast.Assertion>) (java.util.List.<hydra.ext.haskell.ast.Assertion>of()), hd.get(), java.util.List.of(cons), java.util.List.of(deriv.get()))))));
                }
              }),
              (java.util.function.Function<hydra.ext.haskell.ast.Declaration, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.haskell.ast.DeclarationWithComments>>>) (decl -> hydra.lib.flows.Bind.apply(
                hydra.annotations.Annotations.getTypeDescription(typ),
                (java.util.function.Function<hydra.util.Maybe<String>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.haskell.ast.DeclarationWithComments>>>) (comments -> hydra.lib.flows.Bind.apply(
                  hydra.lib.logic.IfElse.lazy(
                    hydra.ext.haskell.coder.Coder.includeTypeDefinitions(),
                    () -> hydra.lib.flows.Bind.apply(
                      hydra.ext.haskell.coder.Coder.typeDecl(
                        namespaces,
                        elementName,
                        typ),
                      (java.util.function.Function<hydra.ext.haskell.ast.DeclarationWithComments, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.haskell.ast.DeclarationWithComments>>>) (decl_ -> hydra.lib.flows.Pure.apply(java.util.List.of(decl_)))),
                    () -> hydra.lib.flows.Pure.apply((java.util.List<hydra.ext.haskell.ast.DeclarationWithComments>) (java.util.List.<hydra.ext.haskell.ast.DeclarationWithComments>of()))),
                  (java.util.function.Function<java.util.List<hydra.ext.haskell.ast.DeclarationWithComments>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.ext.haskell.ast.DeclarationWithComments>>>) (tdecls -> {
                    hydra.ext.haskell.ast.DeclarationWithComments mainDecl = new hydra.ext.haskell.ast.DeclarationWithComments(decl, comments);
                    hydra.util.Lazy<java.util.List<hydra.ext.haskell.ast.DeclarationWithComments>> nameDecls_ = new hydra.util.Lazy<>(() -> hydra.ext.haskell.coder.Coder.nameDecls(
                      g,
                      namespaces,
                      elementName,
                      typ));
                    return hydra.lib.flows.Pure.apply(hydra.lib.lists.Concat.apply(java.util.List.of(
                      java.util.List.of(mainDecl),
                      nameDecls_.get(),
                      tdecls)));
                  }))))));
          })))));
  }
  
  static <T0> hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments> typeDecl(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Name name, hydra.core.Type typ) {
    hydra.core.Term rawTerm = hydra.encode.core.Core.type(typ);
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> rewrite = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> {
      java.util.function.Function<hydra.core.Term, hydra.util.Maybe<String>> decodeString = (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<String>>) (term2 -> (hydra.rewriting.Rewriting.deannotateTerm(term2)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<String> otherwise(hydra.core.Term instance) {
          return (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing());
        }
        
        @Override
        public hydra.util.Maybe<String> visit(hydra.core.Term.Literal lit) {
          return ((lit).value).accept(new hydra.core.Literal.PartialVisitor<>() {
            @Override
            public hydra.util.Maybe<String> otherwise(hydra.core.Literal instance) {
              return (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing());
            }
            
            @Override
            public hydra.util.Maybe<String> visit(hydra.core.Literal.String_ s) {
              return hydra.util.Maybe.just((s).value);
            }
          });
        }
      }));
      java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Name>> decodeName = (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Name>>) (term2 -> (hydra.rewriting.Rewriting.deannotateTerm(term2)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<hydra.core.Name> otherwise(hydra.core.Term instance) {
          return (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing());
        }
        
        @Override
        public hydra.util.Maybe<hydra.core.Name> visit(hydra.core.Term.Wrap wt) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              ((wt).value).typeName,
              new hydra.core.Name("hydra.core.Name")),
            () -> hydra.lib.maybes.Map.apply(
              (java.util.function.Function<String, hydra.core.Name>) (x -> new hydra.core.Name(x)),
              (decodeString).apply(((wt).value).body)),
            () -> (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing()));
        }
      }));
      java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>> forVariableType = (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) (vname -> {
        hydra.module.QualifiedName qname = hydra.names.Names.qualifyName(vname);
        String local = (qname).local;
        hydra.util.Maybe<hydra.module.Namespace> mns = (qname).namespace;
        return hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.module.Namespace, hydra.core.Term>) (ns -> new hydra.core.Term.Variable(hydra.names.Names.qname(
            ns,
            hydra.lib.strings.Cat.apply(java.util.List.of(
              "_",
              local,
              "_type_"))))),
          mns);
      });
      java.util.function.Function<hydra.core.Field, hydra.util.Maybe<hydra.core.Term>> forType = (java.util.function.Function<hydra.core.Field, hydra.util.Maybe<hydra.core.Term>>) (field -> {
        hydra.core.Name fname = (field).name;
        hydra.core.Term fterm = (field).term;
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            fname,
            new hydra.core.Name("record")),
          () -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              fname,
              new hydra.core.Name("variable")),
            () -> hydra.lib.maybes.Bind.apply(
              (decodeName).apply(fterm),
              forVariableType),
            () -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())));
      });
      hydra.util.Lazy<hydra.util.Maybe<hydra.core.Field>> variantResult = new hydra.util.Lazy<>(() -> (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<hydra.core.Field> otherwise(hydra.core.Term instance) {
          return (hydra.util.Maybe<hydra.core.Field>) (hydra.util.Maybe.<hydra.core.Field>nothing());
        }
        
        @Override
        public hydra.util.Maybe<hydra.core.Field> visit(hydra.core.Term.Union inj) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              ((inj).value).typeName,
              new hydra.core.Name("hydra.core.Type")),
            () -> hydra.util.Maybe.just(((inj).value).field),
            () -> (hydra.util.Maybe<hydra.core.Field>) (hydra.util.Maybe.<hydra.core.Field>nothing()));
        }
      }));
      return hydra.lib.maybes.FromMaybe.apply(
        (recurse).apply(term),
        hydra.lib.maybes.Bind.apply(
          variantResult.get(),
          forType));
    }));
    hydra.core.Term finalTerm = hydra.rewriting.Rewriting.rewriteTerm(
      rewrite,
      rawTerm);
    java.util.function.Function<hydra.core.Name, String> typeNameLocal = (java.util.function.Function<hydra.core.Name, String>) (name_ -> hydra.lib.strings.Cat.apply(java.util.List.of(
      "_",
      hydra.names.Names.localNameOf(name_),
      "_type_")));
    java.util.function.Function<hydra.module.Namespace, java.util.function.Function<hydra.core.Name, hydra.core.Name>> typeName = (java.util.function.Function<hydra.module.Namespace, java.util.function.Function<hydra.core.Name, hydra.core.Name>>) (ns -> (java.util.function.Function<hydra.core.Name, hydra.core.Name>) (name_ -> hydra.names.Names.qname(
      ns,
      (typeNameLocal).apply(name_))));
    return hydra.lib.flows.Bind.apply(
      hydra.adapt.modules.Modules.constructCoder(
        hydra.ext.haskell.language.Language.haskellLanguage(),
        (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>) (v1 -> hydra.ext.haskell.coder.Coder.encodeTerm(
          namespaces,
          v1)),
        new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type"))),
      (java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T0, hydra.core.Term, hydra.ext.haskell.ast.Expression>, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments>>) (coder -> hydra.lib.flows.Bind.apply(
        (((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T0, hydra.core.Term, hydra.ext.haskell.ast.Expression>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T0, hydra.core.Term, hydra.ext.haskell.ast.Expression>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T0, hydra.core.Term, hydra.ext.haskell.ast.Expression>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T0, hydra.core.Term, hydra.ext.haskell.ast.Expression>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.Expression>>>) (projected -> projected.encode))))).apply(coder)).apply(finalTerm),
        (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.compute.Flow<hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments>>) (expr -> {
          hydra.ext.haskell.ast.Name hname = hydra.ext.haskell.utils.Utils.simpleName((typeNameLocal).apply(name));
          hydra.util.Lazy<hydra.ext.haskell.ast.Pattern> pat = new hydra.util.Lazy<>(() -> hydra.ext.haskell.utils.Utils.applicationPattern(
            hname,
            (java.util.List<hydra.ext.haskell.ast.Pattern>) (java.util.List.<hydra.ext.haskell.ast.Pattern>of())));
          hydra.ext.haskell.ast.RightHandSide rhs = new hydra.ext.haskell.ast.RightHandSide(expr);
          hydra.util.Lazy<hydra.ext.haskell.ast.Declaration> decl = new hydra.util.Lazy<>(() -> new hydra.ext.haskell.ast.Declaration.ValueBinding(new hydra.ext.haskell.ast.ValueBinding.Simple(new hydra.ext.haskell.ast.SimpleValueBinding(pat.get(), rhs, (hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>) (hydra.util.Maybe.<hydra.ext.haskell.ast.LocalBindings>nothing())))));
          return hydra.lib.flows.Pure.apply(new hydra.ext.haskell.ast.DeclarationWithComments(decl.get(), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())));
        }))));
  }
  
  static <T0> java.util.Map<T0, java.util.Set<hydra.classes.TypeClass>> typeSchemeConstraintsToClassMap(hydra.util.Maybe<java.util.Map<T0, hydra.core.TypeVariableMetadata>> maybeConstraints) {
    java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.classes.TypeClass>> nameToTypeClass = (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.classes.TypeClass>>) (className -> {
      String classNameStr = (className).value;
      hydra.util.Lazy<Boolean> isEq = new hydra.util.Lazy<>(() -> hydra.lib.equality.Equal.apply(
        classNameStr,
        (new hydra.core.Name("equality")).value));
      hydra.util.Lazy<Boolean> isOrd = new hydra.util.Lazy<>(() -> hydra.lib.equality.Equal.apply(
        classNameStr,
        (new hydra.core.Name("ordering")).value));
      return hydra.lib.logic.IfElse.lazy(
        isEq.get(),
        () -> hydra.util.Maybe.just(new hydra.classes.TypeClass.Equality()),
        () -> hydra.lib.logic.IfElse.lazy(
          isOrd.get(),
          () -> hydra.util.Maybe.just(new hydra.classes.TypeClass.Ordering()),
          () -> (hydra.util.Maybe<hydra.classes.TypeClass>) (hydra.util.Maybe.<hydra.classes.TypeClass>nothing())));
    });
    return hydra.lib.maybes.Maybe.apply(
      (java.util.Map<T0, java.util.Set<hydra.classes.TypeClass>>) ((java.util.Map<T0, java.util.Set<hydra.classes.TypeClass>>) (hydra.lib.maps.Empty.<T0, java.util.Set<hydra.classes.TypeClass>>apply())),
      (java.util.function.Function<java.util.Map<T0, hydra.core.TypeVariableMetadata>, java.util.Map<T0, java.util.Set<hydra.classes.TypeClass>>>) (constraints -> hydra.lib.maps.Map.apply(
        (java.util.function.Function<hydra.core.TypeVariableMetadata, java.util.Set<hydra.classes.TypeClass>>) (meta -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
          nameToTypeClass,
          hydra.lib.sets.ToList.apply((meta).classes))))),
        constraints)),
      maybeConstraints);
  }
}
