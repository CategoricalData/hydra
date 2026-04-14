// Note: this is an automatically generated file. Do not edit.

package hydra.haskell;

/**
 * Functions for encoding Hydra modules as Haskell modules
 */
public interface Coder {
  static <T0, T1> hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> adaptTypeToHaskellAndEncode(hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.core.Type typ, T0 cx, T1 g) {
    java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>> enc = (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (t -> hydra.haskell.Coder.<T0, T1>encodeType(
      namespaces,
      t,
      cx,
      g));
    return hydra.Strip.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> otherwise(hydra.core.Type instance) {
        return hydra.lib.eithers.Bind.apply(
          hydra.Adapt.adaptTypeForLanguage(
            hydra.haskell.Language.haskellLanguage(),
            typ),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (adaptedType -> (enc).apply(adaptedType)));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.Variable ignored) {
        return (enc).apply(typ);
      }
    });
  }

  static String constantForFieldName(hydra.core.Name tname, hydra.core.Name fname) {
    return hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "_",
      hydra.Names.localNameOf(tname),
      "_",
      (fname).value));
  }

  static String constantForTypeName(hydra.core.Name tname) {
    return hydra.lib.strings.Cat2.apply(
      "_",
      hydra.Names.localNameOf(tname));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Module> constructModule(hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.packaging.Module mod, java.util.List<hydra.packaging.Definition> defs, hydra.context.Context cx, hydra.graph.Graph g) {
    java.util.function.Function<hydra.packaging.Definition, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.DeclarationWithComments>>> createDeclarations = (java.util.function.Function<hydra.packaging.Definition, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.DeclarationWithComments>>>) (def -> (def).accept(new hydra.packaging.Definition.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.DeclarationWithComments>> visit(hydra.packaging.Definition.Type type) {
        hydra.core.Name name = (type).value.name;
        hydra.core.Type typ = (type).value.type.type;
        return hydra.haskell.Coder.toTypeDeclarationsFrom(
          namespaces,
          name,
          typ,
          cx,
          g);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.DeclarationWithComments>> visit(hydra.packaging.Definition.Term term) {
        return hydra.lib.eithers.Bind.apply(
          hydra.haskell.Coder.toDataDeclaration(
            namespaces,
            (term).value,
            cx,
            g),
          (java.util.function.Function<hydra.haskell.syntax.DeclarationWithComments, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.DeclarationWithComments>>>) (d -> hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.DeclarationWithComments>>right(java.util.Arrays.asList(d))));
      }
    }));
    java.util.function.Function<hydra.packaging.Namespace, String> h = (java.util.function.Function<hydra.packaging.Namespace, String>) (namespace -> (namespace).value);
    java.util.function.Function<String, hydra.haskell.syntax.ModuleName> importName = (java.util.function.Function<String, hydra.haskell.syntax.ModuleName>) (name -> new hydra.haskell.syntax.ModuleName(hydra.lib.strings.Intercalate.apply(
      ".",
      hydra.lib.lists.Map.apply(
        hydra.Formatting::capitalize,
        hydra.lib.strings.SplitOn.apply(
          ".",
          name)))));
    java.util.function.Function<hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>, hydra.haskell.syntax.Import> toImport = (java.util.function.Function<hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>, hydra.haskell.syntax.Import>) (pair -> {
      hydra.util.Lazy<hydra.haskell.syntax.ModuleName> alias = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
      hydra.util.Lazy<hydra.packaging.Namespace> namespace = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
      String name = (h).apply(namespace.get());
      return new hydra.haskell.syntax.Import(true, (importName).apply(name), hydra.util.Maybe.just(alias.get()), (hydra.util.Maybe<hydra.haskell.syntax.SpecImport>) (hydra.util.Maybe.<hydra.haskell.syntax.SpecImport>nothing()));
    });
    hydra.util.Lazy<java.util.List<hydra.haskell.syntax.Import>> domainImports = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      toImport,
      hydra.lib.maps.ToList.apply(((java.util.function.Function<hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>, java.util.Map<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>>) (projected -> projected.mapping)).apply(namespaces))));
    hydra.haskell.environment.HaskellModuleMetadata meta = hydra.haskell.Coder.gatherMetadata(defs);
    java.util.function.Function<hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>, hydra.haskell.syntax.Import> toImport2 = (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>, hydra.haskell.syntax.Import>) (triple -> {
      hydra.util.Lazy<java.util.List<String>> hidden = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(triple));
      hydra.util.Lazy<hydra.util.Maybe<String>> malias = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(triple)));
      hydra.util.Lazy<String> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(triple)));
      hydra.util.Lazy<hydra.util.Maybe<hydra.haskell.syntax.SpecImport>> spec = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(hidden.get()),
        () -> (hydra.util.Maybe<hydra.haskell.syntax.SpecImport>) (hydra.util.Maybe.<hydra.haskell.syntax.SpecImport>nothing()),
        () -> hydra.util.Maybe.just(new hydra.haskell.syntax.SpecImport.Hiding(hydra.lib.lists.Map.apply(
          (java.util.function.Function<String, hydra.haskell.syntax.ImportExportSpec>) (n -> new hydra.haskell.syntax.ImportExportSpec((hydra.util.Maybe<hydra.haskell.syntax.ImportModifier>) (hydra.util.Maybe.<hydra.haskell.syntax.ImportModifier>nothing()), hydra.haskell.Utils.simpleName(n), (hydra.util.Maybe<hydra.haskell.syntax.SubspecImportExportSpec>) (hydra.util.Maybe.<hydra.haskell.syntax.SubspecImportExportSpec>nothing()))),
          hidden.get())))));
      return new hydra.haskell.syntax.Import(hydra.lib.maybes.IsJust.apply(malias.get()), new hydra.haskell.syntax.ModuleName(name.get()), hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, hydra.haskell.syntax.ModuleName>) (x -> new hydra.haskell.syntax.ModuleName(x)),
        malias.get()), spec.get());
    });
    hydra.util.Lazy<java.util.List<hydra.haskell.syntax.Import>> standardImports = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      toImport2,
      hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
        java.util.Arrays.asList((hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>) ((hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>) (new hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>((hydra.util.Pair<String, hydra.util.Maybe<String>>) ((hydra.util.Pair<String, hydra.util.Maybe<String>>) (new hydra.util.Pair<String, hydra.util.Maybe<String>>("Prelude", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())))), java.util.Arrays.asList(
          "Enum",
          "Ordering",
          "decodeFloat",
          "encodeFloat",
          "fail",
          "map",
          "pure",
          "sum"))))),
        hydra.haskell.Coder.constructModule_condImport(
          (meta).usesByteString,
          (hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>) ((hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>) (new hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>((hydra.util.Pair<String, hydra.util.Maybe<String>>) ((hydra.util.Pair<String, hydra.util.Maybe<String>>) (new hydra.util.Pair<String, hydra.util.Maybe<String>>("Data.ByteString", hydra.util.Maybe.just("B")))), (java.util.List<String>) (java.util.Collections.<String>emptyList()))))),
        hydra.haskell.Coder.constructModule_condImport(
          (meta).usesInt,
          (hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>) ((hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>) (new hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>((hydra.util.Pair<String, hydra.util.Maybe<String>>) ((hydra.util.Pair<String, hydra.util.Maybe<String>>) (new hydra.util.Pair<String, hydra.util.Maybe<String>>("Data.Int", hydra.util.Maybe.just("I")))), (java.util.List<String>) (java.util.Collections.<String>emptyList()))))),
        hydra.haskell.Coder.constructModule_condImport(
          (meta).usesMap,
          (hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>) ((hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>) (new hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>((hydra.util.Pair<String, hydra.util.Maybe<String>>) ((hydra.util.Pair<String, hydra.util.Maybe<String>>) (new hydra.util.Pair<String, hydra.util.Maybe<String>>("Data.Map", hydra.util.Maybe.just("M")))), (java.util.List<String>) (java.util.Collections.<String>emptyList()))))),
        hydra.haskell.Coder.constructModule_condImport(
          (meta).usesSet,
          (hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>) ((hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>) (new hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>((hydra.util.Pair<String, hydra.util.Maybe<String>>) ((hydra.util.Pair<String, hydra.util.Maybe<String>>) (new hydra.util.Pair<String, hydra.util.Maybe<String>>("Data.Set", hydra.util.Maybe.just("S")))), (java.util.List<String>) (java.util.Collections.<String>emptyList()))))),
        hydra.lib.logic.IfElse.lazy(
          hydra.Analysis.moduleContainsBinaryLiterals(mod),
          () -> java.util.Arrays.asList((hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>) ((hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>) (new hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>((hydra.util.Pair<String, hydra.util.Maybe<String>>) ((hydra.util.Pair<String, hydra.util.Maybe<String>>) (new hydra.util.Pair<String, hydra.util.Maybe<String>>("Hydra.Lib.Literals", hydra.util.Maybe.just("Literals")))), (java.util.List<String>) (java.util.Collections.<String>emptyList()))))),
          () -> (java.util.List<hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>>) (java.util.Collections.<hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, java.util.List<String>>>emptyList()))))));
    hydra.util.Lazy<java.util.List<hydra.haskell.syntax.Import>> imports = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
      domainImports.get(),
      standardImports.get()));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        createDeclarations,
        defs),
      (java.util.function.Function<java.util.List<java.util.List<hydra.haskell.syntax.DeclarationWithComments>>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Module>>) (declLists -> {
        hydra.util.Lazy<java.util.List<hydra.haskell.syntax.DeclarationWithComments>> decls = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(declLists));
        hydra.util.Maybe<String> mc = (mod).description;
        return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Module>right(new hydra.haskell.syntax.Module(hydra.util.Maybe.just(new hydra.haskell.syntax.ModuleHead(mc, (importName).apply((h).apply((mod).namespace)), (java.util.List<hydra.haskell.syntax.Export>) (java.util.Collections.<hydra.haskell.syntax.Export>emptyList()))), imports.get(), decls.get()));
      }));
  }

  static <T0> java.util.List<T0> constructModule_condImport(Boolean flag, T0 triple) {
    return hydra.lib.logic.IfElse.lazy(
      flag,
      () -> java.util.Arrays.asList(triple),
      () -> (java.util.List<T0>) (java.util.Collections.<T0>emptyList()));
  }

  static hydra.haskell.environment.HaskellModuleMetadata emptyMetadata() {
    return new hydra.haskell.environment.HaskellModuleMetadata(false, false, false, false);
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> encodeCaseExpression(Integer depth, hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.core.CaseStatement stmt, hydra.haskell.syntax.Expression scrutinee, T0 cx, hydra.graph.Graph g) {
    hydra.util.Maybe<hydra.core.Term> def = (stmt).default_;
    hydra.core.Name dn = (stmt).typeName;
    java.util.List<hydra.core.Field> fields = (stmt).cases;
    java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.FieldType>, java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Alternative>>> toAlt = (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.FieldType>, java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Alternative>>>) (fieldMap -> (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Alternative>>) (field -> {
      hydra.core.Name fn = (field).name;
      hydra.core.Term fun_ = (field).term;
      hydra.util.Lazy<hydra.haskell.syntax.Name> hname = new hydra.util.Lazy<>(() -> hydra.haskell.Utils.unionFieldReference(
        hydra.lib.sets.Union.apply(
          hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply((g).boundTerms)),
          hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply((g).schemaTypes))),
        namespaces,
        dn,
        fn));
      String v0 = hydra.lib.strings.Cat2.apply(
        "v",
        hydra.lib.literals.ShowInt32.apply(depth));
      hydra.core.Term raw = new hydra.core.Term.Application(new hydra.core.Application(fun_, new hydra.core.Term.Variable(new hydra.core.Name(v0))));
      hydra.core.Term rhsTerm = hydra.Dependencies.simplifyTerm(raw);
      hydra.util.Lazy<String> v1 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
        hydra.Variables.isFreeVariableInTerm(
          new hydra.core.Name(v0),
          rhsTerm),
        () -> hydra.Constants.ignoredVariable(),
        () -> v0));
      return hydra.lib.eithers.Bind.apply(
        hydra.lib.maybes.Cases.applyLazy(
          hydra.lib.maps.Lookup.apply(
            fn,
            fieldMap),
          () -> hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.Pattern>>left(new hydra.errors.Error_.Resolution(new hydra.errors.ResolutionError.NoMatchingField(new hydra.errors.NoMatchingFieldError(fn)))),
          (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.Pattern>>>) (fieldType -> {
            hydra.core.Type ft = (fieldType).type;
            java.util.List<hydra.haskell.syntax.Pattern> singleArg = java.util.Arrays.asList(new hydra.haskell.syntax.Pattern.Name(hydra.haskell.Utils.rawName(v1.get())));
            return hydra.Strip.deannotateType(ft).accept(new hydra.core.Type.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.Pattern>> otherwise(hydra.core.Type instance) {
                return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.Pattern>>right(singleArg);
              }

              @Override
              public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.Pattern>> visit(hydra.core.Type.Unit ignored) {
                return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.Pattern>>right(hydra.haskell.Coder.<hydra.haskell.syntax.Pattern>encodeCaseExpression_noArgs());
              }
            });
          })),
        (java.util.function.Function<java.util.List<hydra.haskell.syntax.Pattern>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Alternative>>) (args -> {
          hydra.haskell.syntax.Pattern lhs = hydra.haskell.Utils.applicationPattern(
            hname.get(),
            args);
          return hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.haskell.syntax.CaseRhs>) (x -> new hydra.haskell.syntax.CaseRhs(x)),
              hydra.haskell.Coder.<T0>encodeTerm(
                hydra.lib.math.Add.apply(
                  depth,
                  1),
                namespaces,
                rhsTerm,
                cx,
                g)),
            (java.util.function.Function<hydra.haskell.syntax.CaseRhs, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Alternative>>) (rhs -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Alternative>right(new hydra.haskell.syntax.Alternative(lhs, rhs, (hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>) (hydra.util.Maybe.<hydra.haskell.syntax.LocalBindings>nothing())))));
        }));
    }));
    return hydra.lib.eithers.Bind.apply(
      hydra.Resolution.<T0>requireUnionType(
        cx,
        g,
        dn),
      (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (rt -> {
        java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<hydra.core.Name, hydra.core.FieldType>> toFieldMapEntry = (java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<hydra.core.Name, hydra.core.FieldType>>) (f -> (hydra.util.Pair<hydra.core.Name, hydra.core.FieldType>) ((hydra.util.Pair<hydra.core.Name, hydra.core.FieldType>) (new hydra.util.Pair<hydra.core.Name, hydra.core.FieldType>((f).name, f))));
        hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.FieldType>> fieldMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          toFieldMapEntry,
          rt)));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Alternative>>) (v1 -> (toAlt).apply(fieldMap.get()).apply(v1)),
            fields),
          (java.util.function.Function<java.util.List<hydra.haskell.syntax.Alternative>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (ecases -> hydra.lib.eithers.Bind.apply(
            hydra.lib.maybes.Cases.applyLazy(
              def,
              () -> hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.Alternative>>right((java.util.List<hydra.haskell.syntax.Alternative>) (java.util.Collections.<hydra.haskell.syntax.Alternative>emptyList())),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.Alternative>>>) (d -> hydra.lib.eithers.Bind.apply(
                hydra.lib.eithers.Map.apply(
                  (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.haskell.syntax.CaseRhs>) (x -> new hydra.haskell.syntax.CaseRhs(x)),
                  hydra.haskell.Coder.<T0>encodeTerm(
                    depth,
                    namespaces,
                    d,
                    cx,
                    g)),
                (java.util.function.Function<hydra.haskell.syntax.CaseRhs, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.Alternative>>>) (cs -> {
                  hydra.haskell.syntax.Pattern lhs = new hydra.haskell.syntax.Pattern.Name(hydra.haskell.Utils.rawName(hydra.Constants.ignoredVariable()));
                  hydra.util.Lazy<hydra.haskell.syntax.Alternative> alt = new hydra.util.Lazy<>(() -> new hydra.haskell.syntax.Alternative(lhs, cs, (hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>) (hydra.util.Maybe.<hydra.haskell.syntax.LocalBindings>nothing())));
                  return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.Alternative>>right(java.util.Arrays.asList(alt.get()));
                })))),
            (java.util.function.Function<java.util.List<hydra.haskell.syntax.Alternative>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (dcases -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(new hydra.haskell.syntax.Expression.Case(new hydra.haskell.syntax.CaseExpression(scrutinee, hydra.lib.lists.Concat2.apply(
              ecases,
              dcases))))))));
      }));
  }

  static <T1> java.util.List<T1> encodeCaseExpression_noArgs() {
    return (java.util.List<T1>) (java.util.Collections.<T1>emptyList());
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> encodeLambdaTerm(Integer depth, hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.core.Lambda lam, T0 cx, hydra.graph.Graph g) {
    hydra.core.Term body = (lam).body;
    hydra.core.Name v = (lam).parameter;
    return hydra.lib.eithers.Bind.apply(
      hydra.haskell.Coder.<T0>encodeTerm(
        depth,
        namespaces,
        body,
        cx,
        g),
      (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (hbody -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hslambda(
        hydra.haskell.Utils.elementReference(
          namespaces,
          v),
        hbody))));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> encodeLiteral(hydra.core.Literal l, T0 cx) {
    return (l).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> otherwise(hydra.core.Literal instance) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("supported literal", hydra.show.Core.literal(l)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Literal.Binary bs) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hsapp(
          hydra.haskell.Utils.hsvar("Literals.stringToBinary"),
          hydra.haskell.Utils.hslit(new hydra.haskell.syntax.Literal.String_(hydra.lib.literals.BinaryToString.apply((bs).value)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Literal.Boolean_ b) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hsvar(hydra.lib.logic.IfElse.lazy(
          (b).value,
          () -> "True",
          () -> "False")));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Literal.Float_ fv) {
        return (fv).value.accept(new hydra.core.FloatValue.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.FloatValue.Float32 f) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hslit(new hydra.haskell.syntax.Literal.Float_((f).value)));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.FloatValue.Float64 f) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hslit(new hydra.haskell.syntax.Literal.Double_((f).value)));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.FloatValue.Bigfloat f) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hslit(new hydra.haskell.syntax.Literal.Double_(hydra.lib.literals.BigfloatToFloat64.apply((f).value))));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Literal.Integer_ iv) {
        return (iv).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.IntegerValue.Bigint i) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hslit(new hydra.haskell.syntax.Literal.Integer_((i).value)));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.IntegerValue.Int8 i) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hslit(new hydra.haskell.syntax.Literal.Integer_(hydra.lib.literals.Int8ToBigint.apply((i).value))));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.IntegerValue.Int16 i) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hslit(new hydra.haskell.syntax.Literal.Integer_(hydra.lib.literals.Int16ToBigint.apply((i).value))));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.IntegerValue.Int32 i) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hslit(new hydra.haskell.syntax.Literal.Int((i).value)));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.IntegerValue.Int64 i) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hslit(new hydra.haskell.syntax.Literal.Integer_(hydra.lib.literals.Int64ToBigint.apply((i).value))));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.IntegerValue.Uint8 i) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hslit(new hydra.haskell.syntax.Literal.Integer_(hydra.lib.literals.Uint8ToBigint.apply((i).value))));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.IntegerValue.Uint16 i) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hslit(new hydra.haskell.syntax.Literal.Integer_(hydra.lib.literals.Uint16ToBigint.apply((i).value))));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.IntegerValue.Uint32 i) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hslit(new hydra.haskell.syntax.Literal.Integer_(hydra.lib.literals.Uint32ToBigint.apply((i).value))));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.IntegerValue.Uint64 i) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hslit(new hydra.haskell.syntax.Literal.Integer_(hydra.lib.literals.Uint64ToBigint.apply((i).value))));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Literal.String_ s) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hslit(new hydra.haskell.syntax.Literal.String_((s).value)));
      }
    });
  }

  static <T0> hydra.util.Either<T0, hydra.haskell.syntax.Expression> encodeProjection(hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.core.Projection proj) {
    hydra.core.Name dn = (proj).typeName;
    hydra.core.Name fname = (proj).field;
    return hydra.util.Either.<T0, hydra.haskell.syntax.Expression>right(new hydra.haskell.syntax.Expression.Variable(hydra.haskell.Utils.recordFieldReference(
      namespaces,
      dn,
      fname)));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> encodeStandaloneCases(Integer depth, hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.core.CaseStatement stmt, T0 cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.haskell.syntax.Expression>) (v1 -> hydra.haskell.Utils.hslambda(
        hydra.haskell.Utils.rawName("x"),
        v1)),
      hydra.haskell.Coder.<T0>encodeCaseExpression(
        depth,
        namespaces,
        stmt,
        hydra.haskell.Utils.hsvar("x"),
        cx,
        g));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> encodeTerm(Integer depth, hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.core.Term term, T0 cx, hydra.graph.Graph g) {
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>> encode = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (t -> hydra.haskell.Coder.<T0>encodeTerm(
      depth,
      namespaces,
      t,
      cx,
      g));
    java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>> nonemptyMap = (java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (m -> {
      java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>> encodePair = (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (pair -> {
        hydra.util.Lazy<hydra.core.Term> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
        hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(k.get()),
          (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (hk -> hydra.lib.eithers.Bind.apply(
            (encode).apply(v.get()),
            (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (hv -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(new hydra.haskell.syntax.Expression.Tuple(java.util.Arrays.asList(
              hk,
              hv)))))));
      });
      hydra.haskell.syntax.Expression lhs = hydra.haskell.Utils.hsvar("M.fromList");
      return hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.util.List<hydra.haskell.syntax.Expression>, hydra.haskell.syntax.Expression>) (x -> new hydra.haskell.syntax.Expression.List(x)),
          hydra.lib.eithers.MapList.apply(
            encodePair,
            hydra.lib.maps.ToList.apply(m))),
        (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (rhs -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hsapp(
          lhs,
          rhs))));
    });
    java.util.function.Function<java.util.Set<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>> nonemptySet = (java.util.function.Function<java.util.Set<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (s -> {
      hydra.haskell.syntax.Expression lhs = hydra.haskell.Utils.hsvar("S.fromList");
      return hydra.lib.eithers.Bind.apply(
        hydra.haskell.Coder.<T0>encodeTerm(
          depth,
          namespaces,
          new hydra.core.Term.List(hydra.lib.sets.ToList.apply(s)),
          cx,
          g),
        (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (rhs -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hsapp(
          lhs,
          rhs))));
    });
    return hydra.Strip.deannotateTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("supported term", hydra.show.Core.term(term)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Application app) {
        hydra.core.Term arg = (app).value.argument;
        hydra.core.Term fun = (app).value.function;
        hydra.core.Term deannotatedFun = hydra.Strip.deannotateTerm(fun);
        return (deannotatedFun).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> otherwise(hydra.core.Term instance) {
            return hydra.lib.eithers.Bind.apply(
              (encode).apply(fun),
              (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (hfun -> hydra.lib.eithers.Bind.apply(
                (encode).apply(arg),
                (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (harg -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hsapp(
                  hfun,
                  harg))))));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Cases stmt) {
            return hydra.lib.eithers.Bind.apply(
              (encode).apply(arg),
              (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (harg -> hydra.haskell.Coder.<T0>encodeCaseExpression(
                depth,
                namespaces,
                (stmt).value,
                harg,
                cx,
                g)));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Cases stmt) {
        return hydra.haskell.Coder.<T0>encodeStandaloneCases(
          depth,
          namespaces,
          (stmt).value,
          cx,
          g);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (l -> hydra.lib.eithers.Bind.apply(
            (encode).apply(l),
            (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (hl -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hsapp(
              hydra.haskell.Utils.hsvar("Left"),
              hl))))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (r -> hydra.lib.eithers.Bind.apply(
            (encode).apply(r),
            (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (hr -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hsapp(
              hydra.haskell.Utils.hsvar("Right"),
              hr))))),
          (e).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Lambda lam) {
        return hydra.haskell.Coder.<T0>encodeLambdaTerm(
          depth,
          namespaces,
          (lam).value,
          cx,
          g);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Project proj) {
        return hydra.haskell.Coder.encodeProjection(
          namespaces,
          (proj).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Unwrap name) {
        return hydra.haskell.Coder.encodeUnwrap(
          namespaces,
          (name).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Let letTerm) {
        java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Let, hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>>> collectBindings = new java.util.concurrent.atomic.AtomicReference<>();
        collectBindings.set((java.util.function.Function<hydra.core.Let, hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>>) (lt -> {
          hydra.core.Term body = (lt).body;
          java.util.List<hydra.core.Binding> bs = (lt).bindings;
          return hydra.Strip.deannotateTerm(body).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term> otherwise(hydra.core.Term instance) {
              return (hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>) ((hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>) (new hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>(bs, body)));
            }

            @Override
            public hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term> visit(hydra.core.Term.Let innerLt) {
              hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term> innerResult = collectBindings.get().apply((innerLt).value);
              return (hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>) ((hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>) (new hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>(hydra.lib.lists.Concat2.apply(
                bs,
                hydra.lib.pairs.First.apply(innerResult)), hydra.lib.pairs.Second.apply(innerResult))));
            }
          });
        }));
        hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term> collected = collectBindings.get().apply((letTerm).value);
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> allBindings = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(collected));
        java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.LocalBinding>> encodeBinding = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.LocalBinding>>) (binding -> {
          hydra.core.Name name = (binding).name;
          hydra.haskell.syntax.Name hname = hydra.haskell.Utils.simpleName((name).value);
          hydra.core.Term term_ = (binding).term;
          return hydra.lib.eithers.Bind.apply(
            (encode).apply(term_),
            (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.LocalBinding>>) (hexpr -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.LocalBinding>right(new hydra.haskell.syntax.LocalBinding.Value(hydra.haskell.Utils.simpleValueBinding(
              hname,
              hexpr,
              (hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>) (hydra.util.Maybe.<hydra.haskell.syntax.LocalBindings>nothing()))))));
        });
        hydra.util.Lazy<hydra.core.Term> finalBody = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(collected));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            encodeBinding,
            allBindings.get()),
          (java.util.function.Function<java.util.List<hydra.haskell.syntax.LocalBinding>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (hbindings -> hydra.lib.eithers.Bind.apply(
            (encode).apply(finalBody.get()),
            (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (hinner -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(new hydra.haskell.syntax.Expression.Let(new hydra.haskell.syntax.LetExpression(hbindings, hinner)))))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.List els) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            encode,
            (els).value),
          (java.util.function.Function<java.util.List<hydra.haskell.syntax.Expression>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (helems -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(new hydra.haskell.syntax.Expression.List(helems))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Literal v) {
        return hydra.haskell.Coder.<T0>encodeLiteral(
          (v).value,
          cx);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Map m) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.maps.Null.apply((m).value),
          () -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hsvar("M.empty")),
          () -> (nonemptyMap).apply((m).value));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.maybes.Cases.applyLazy(
          (m).value,
          () -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hsvar("Nothing")),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (t -> hydra.lib.eithers.Bind.apply(
            (encode).apply(t),
            (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (ht -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hsapp(
              hydra.haskell.Utils.hsvar("Just"),
              ht))))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Pair p) {
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(hydra.lib.pairs.First.apply((p).value)),
          (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (f -> hydra.lib.eithers.Bind.apply(
            (encode).apply(hydra.lib.pairs.Second.apply((p).value)),
            (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (s -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(new hydra.haskell.syntax.Expression.Tuple(java.util.Arrays.asList(
              f,
              s)))))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Record record) {
        java.util.List<hydra.core.Field> fields = (record).value.fields;
        hydra.core.Name sname = (record).value.typeName;
        java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.FieldUpdate>> toFieldUpdate = (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.FieldUpdate>>) (field -> {
          hydra.core.Name fn = (field).name;
          hydra.haskell.syntax.Name fieldRef = hydra.haskell.Utils.recordFieldReference(
            namespaces,
            sname,
            fn);
          hydra.core.Term ft = (field).term;
          return hydra.lib.eithers.Bind.apply(
            (encode).apply(ft),
            (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.FieldUpdate>>) (hft -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.FieldUpdate>right(new hydra.haskell.syntax.FieldUpdate(fieldRef, hft))));
        });
        hydra.haskell.syntax.Name typeName = hydra.haskell.Utils.elementReference(
          namespaces,
          sname);
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            toFieldUpdate,
            fields),
          (java.util.function.Function<java.util.List<hydra.haskell.syntax.FieldUpdate>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (updates -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(new hydra.haskell.syntax.Expression.ConstructRecord(new hydra.haskell.syntax.ConstructRecordExpression(typeName, updates)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Set s) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.sets.Null.apply((s).value),
          () -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hsvar("S.empty")),
          () -> (nonemptySet).apply((s).value));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.TypeLambda abs) {
        hydra.core.Term term1 = (abs).value.body;
        return (encode).apply(term1);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.TypeApplication typed) {
        hydra.core.Term term1 = (typed).value.body;
        return (encode).apply(term1);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Inject injection) {
        hydra.core.Field field = (injection).value.field;
        hydra.core.Term ft = (field).term;
        hydra.core.Name fn = (field).name;
        hydra.core.Name sname = (injection).value.typeName;
        hydra.util.Lazy<hydra.haskell.syntax.Expression> lhs = new hydra.util.Lazy<>(() -> new hydra.haskell.syntax.Expression.Variable(hydra.haskell.Utils.unionFieldReference(
          hydra.lib.sets.Union.apply(
            hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply((g).boundTerms)),
            hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply((g).schemaTypes))),
          namespaces,
          sname,
          fn)));
        hydra.util.Lazy<hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>> dflt = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.haskell.syntax.Expression>) (v1 -> hydra.haskell.Utils.hsapp(
            lhs.get(),
            v1)),
          (encode).apply(ft)));
        return hydra.lib.eithers.Bind.apply(
          hydra.Resolution.<T0>requireUnionField(
            cx,
            g,
            sname,
            fn),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (ftyp -> hydra.Strip.deannotateType(ftyp).accept(new hydra.core.Type.PartialVisitor<>() {
            @Override
            public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> otherwise(hydra.core.Type instance) {
              return dflt.get();
            }

            @Override
            public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Type.Unit ignored) {
              return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(lhs.get());
            }
          })));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Unit ignored) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(new hydra.haskell.syntax.Expression.Tuple((java.util.List<hydra.haskell.syntax.Expression>) (java.util.Collections.<hydra.haskell.syntax.Expression>emptyList())));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Variable name) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(new hydra.haskell.syntax.Expression.Variable(hydra.haskell.Utils.elementReference(
          namespaces,
          (name).value)));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression> visit(hydra.core.Term.Wrap wrapped) {
        hydra.core.Name tname = (wrapped).value.typeName;
        hydra.haskell.syntax.Expression lhs = new hydra.haskell.syntax.Expression.Variable(hydra.haskell.Utils.elementReference(
          namespaces,
          tname));
        hydra.core.Term term_ = (wrapped).value.body;
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(term_),
          (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (rhs -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Expression>right(hydra.haskell.Utils.hsapp(
            lhs,
            rhs))));
      }
    });
  }

  static <T0, T1> hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> encodeType(hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.core.Type typ, T0 cx, T1 g) {
    java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>> encode = (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (t -> hydra.haskell.Coder.<T0, T1>encodeType(
      namespaces,
      t,
      cx,
      g));
    hydra.util.Lazy<hydra.haskell.syntax.Type> unitTuple = new hydra.util.Lazy<>(() -> new hydra.haskell.syntax.Type.Tuple((java.util.List<hydra.haskell.syntax.Type>) (java.util.Collections.<hydra.haskell.syntax.Type>emptyList())));
    return hydra.Strip.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("supported type", hydra.show.Core.type(typ)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.Application app) {
        hydra.core.Type lhs = (app).value.function;
        hydra.core.Type rhs = (app).value.argument;
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(lhs),
          (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (hlhs -> hydra.lib.eithers.Bind.apply(
            (encode).apply(rhs),
            (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (hrhs -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(hydra.haskell.Utils.toTypeApplication(java.util.Arrays.asList(
              hlhs,
              hrhs)))))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.Either eitherType) {
        hydra.core.Type left_ = (eitherType).value.left;
        hydra.core.Type right_ = (eitherType).value.right;
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(left_),
          (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (hleft -> hydra.lib.eithers.Bind.apply(
            (encode).apply(right_),
            (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (hright -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(hydra.haskell.Utils.toTypeApplication(java.util.Arrays.asList(
              new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName("Either")),
              hleft,
              hright)))))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.Function funType) {
        hydra.core.Type cod = (funType).value.codomain;
        hydra.core.Type dom = (funType).value.domain;
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(dom),
          (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (hdom -> hydra.lib.eithers.Bind.apply(
            (encode).apply(cod),
            (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (hcod -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.Function(new hydra.haskell.syntax.FunctionType(hdom, hcod)))))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.Forall forallType) {
        hydra.core.Type body = (forallType).value.body;
        hydra.core.Name v = (forallType).value.parameter;
        return (encode).apply(body);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.List lt) {
        return hydra.lib.eithers.Bind.apply(
          (encode).apply((lt).value),
          (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (hlt -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.List(hlt))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.Literal lt) {
        return (lt).value.accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> otherwise(hydra.core.LiteralType instance) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("supported literal type", hydra.show.Core.literalType((lt).value)))));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.LiteralType.Binary ignored) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName("B.ByteString")));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.LiteralType.Boolean_ ignored) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName("Bool")));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.LiteralType.Float_ ft) {
            return (ft).value.accept(new hydra.core.FloatType.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.FloatType.Float32 ignored) {
                return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName("Float")));
              }

              @Override
              public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.FloatType.Float64 ignored) {
                return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName("Double")));
              }

              @Override
              public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.FloatType.Bigfloat ignored) {
                return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName("Double")));
              }
            });
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.LiteralType.Integer_ it) {
            return (it).value.accept(new hydra.core.IntegerType.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> otherwise(hydra.core.IntegerType instance) {
                return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("supported integer type", hydra.show.Core.integerType((it).value)))));
              }

              @Override
              public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.IntegerType.Bigint ignored) {
                return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName("Integer")));
              }

              @Override
              public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.IntegerType.Int8 ignored) {
                return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName("I.Int8")));
              }

              @Override
              public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.IntegerType.Int16 ignored) {
                return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName("I.Int16")));
              }

              @Override
              public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.IntegerType.Int32 ignored) {
                return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName("Int")));
              }

              @Override
              public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.IntegerType.Int64 ignored) {
                return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName("I.Int64")));
              }
            });
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.LiteralType.String_ ignored) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName("String")));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.Map mapType) {
        hydra.core.Type kt = (mapType).value.keys;
        hydra.core.Type vt = (mapType).value.values;
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(kt),
          (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (hkt -> hydra.lib.eithers.Bind.apply(
            (encode).apply(vt),
            (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (hvt -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(hydra.haskell.Utils.toTypeApplication(java.util.Arrays.asList(
              new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName("M.Map")),
              hkt,
              hvt)))))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.Maybe ot) {
        return hydra.lib.eithers.Bind.apply(
          (encode).apply((ot).value),
          (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (hot -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(hydra.haskell.Utils.toTypeApplication(java.util.Arrays.asList(
            new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName("Maybe")),
            hot)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.Pair pt) {
        return hydra.lib.eithers.Bind.apply(
          (encode).apply((pt).value.first),
          (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (f -> hydra.lib.eithers.Bind.apply(
            (encode).apply((pt).value.second),
            (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (s -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.Tuple(java.util.Arrays.asList(
              f,
              s)))))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.Record ignored) {
        return hydra.haskell.Coder.encodeType_ref(
          (java.util.function.Function<hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>, java.util.function.Function<hydra.core.Name, hydra.haskell.syntax.Name>>) (p0 -> p1 -> hydra.haskell.Utils.elementReference(
            p0,
            p1)),
          namespaces,
          new hydra.core.Name("placeholder"));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.Set st) {
        return hydra.lib.eithers.Bind.apply(
          (encode).apply((st).value),
          (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (hst -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(hydra.haskell.Utils.toTypeApplication(java.util.Arrays.asList(
            new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName("S.Set")),
            hst)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.Union ignored) {
        return hydra.haskell.Coder.encodeType_ref(
          (java.util.function.Function<hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>, java.util.function.Function<hydra.core.Name, hydra.haskell.syntax.Name>>) (p0 -> p1 -> hydra.haskell.Utils.elementReference(
            p0,
            p1)),
          namespaces,
          new hydra.core.Name("placeholder"));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.Unit ignored) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(unitTuple.get());
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.Variable v1) {
        return hydra.haskell.Coder.encodeType_ref(
          (java.util.function.Function<hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>, java.util.function.Function<hydra.core.Name, hydra.haskell.syntax.Name>>) (p0 -> p1 -> hydra.haskell.Utils.elementReference(
            p0,
            p1)),
          namespaces,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.Void_ ignored) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName("Void")));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> visit(hydra.core.Type.Wrap ignored) {
        return hydra.haskell.Coder.encodeType_ref(
          (java.util.function.Function<hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>, java.util.function.Function<hydra.core.Name, hydra.haskell.syntax.Name>>) (p0 -> p1 -> hydra.haskell.Utils.elementReference(
            p0,
            p1)),
          namespaces,
          new hydra.core.Name("placeholder"));
      }
    });
  }

  static <T0, T1> hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type> encodeTypeWithClassAssertions(hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>> explicitClasses, hydra.core.Type typ, T0 cx, T1 g) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>> classes = new hydra.util.Lazy<>(() -> hydra.lib.maps.Union.apply(
      explicitClasses,
      hydra.haskell.Coder.getImplicitTypeClasses(typ)));
    hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.classes.TypeClass>>> assertPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      p0 -> hydra.haskell.Coder.<hydra.core.Name, hydra.classes.TypeClass>encodeTypeWithClassAssertions_toPairs(p0),
      hydra.lib.maps.ToList.apply(classes.get()))));
    java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.classes.TypeClass>, hydra.haskell.syntax.Assertion> encodeAssertion = (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.classes.TypeClass>, hydra.haskell.syntax.Assertion>) (pair -> {
      hydra.util.Lazy<hydra.classes.TypeClass> cls = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
      hydra.haskell.syntax.Name hname = hydra.haskell.Utils.rawName(cls.get().accept(new hydra.classes.TypeClass.PartialVisitor<>() {
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
      hydra.haskell.syntax.Type htype = new hydra.haskell.syntax.Type.Variable(hydra.haskell.Utils.rawName(name.get().value));
      return new hydra.haskell.syntax.Assertion.Class_(new hydra.haskell.syntax.ClassAssertion(hname, java.util.Arrays.asList(htype)));
    });
    java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>> implicitClasses = hydra.haskell.Coder.getImplicitTypeClasses(typ);
    return hydra.lib.eithers.Bind.apply(
      hydra.haskell.Coder.<T0, T1>adaptTypeToHaskellAndEncode(
        namespaces,
        typ,
        cx,
        g),
      (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (htyp -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(assertPairs.get()),
        () -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(htyp),
        () -> ((java.util.function.Supplier<hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Type>>) (() -> {
          hydra.util.Lazy<java.util.List<hydra.haskell.syntax.Assertion>> encoded = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
            encodeAssertion,
            assertPairs.get()));
          hydra.util.Lazy<hydra.haskell.syntax.Assertion> hassert = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              hydra.lib.lists.Length.apply(encoded.get()),
              1),
            () -> hydra.lib.lists.Head.apply(encoded.get()),
            () -> new hydra.haskell.syntax.Assertion.Tuple(encoded.get())));
          return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.Ctx(new hydra.haskell.syntax.ContextType(hassert.get(), htyp)));
        })).get())));
  }

  static <T2, T3> java.util.Set<T3> encodeTypeWithClassAssertions_clsSet(hydra.util.Pair<T2, java.util.Set<T3>> mapEntry) {
    return hydra.lib.pairs.Second.apply(mapEntry);
  }

  static <T2, T3> T2 encodeTypeWithClassAssertions_name(hydra.util.Pair<T2, java.util.Set<T3>> mapEntry) {
    return hydra.lib.pairs.First.apply(mapEntry);
  }

  static <T2, T3, T4> hydra.util.Pair<T2, T4> encodeTypeWithClassAssertions_toPair(hydra.util.Pair<T2, java.util.Set<T3>> mapEntry, T4 c) {
    return (hydra.util.Pair<T2, T4>) ((hydra.util.Pair<T2, T4>) (new hydra.util.Pair<T2, T4>(hydra.haskell.Coder.<T2, T3>encodeTypeWithClassAssertions_name(mapEntry), c)));
  }

  static <T2, T3> java.util.List<hydra.util.Pair<T2, T3>> encodeTypeWithClassAssertions_toPairs(hydra.util.Pair<T2, java.util.Set<T3>> mapEntry) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<T3, hydra.util.Pair<T2, T3>>) (v1 -> hydra.haskell.Coder.<T2, T3, T3>encodeTypeWithClassAssertions_toPair(
        mapEntry,
        v1)),
      hydra.lib.sets.ToList.apply(hydra.haskell.Coder.<T2, T3>encodeTypeWithClassAssertions_clsSet(mapEntry)));
  }

  static <T2> hydra.util.Either<T2, hydra.haskell.syntax.Type> encodeType_ref(java.util.function.Function<hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>, java.util.function.Function<hydra.core.Name, hydra.haskell.syntax.Name>> hydra_haskell_utils_elementReference, hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.core.Name name) {
    return hydra.util.Either.<T2, hydra.haskell.syntax.Type>right(new hydra.haskell.syntax.Type.Variable((hydra_haskell_utils_elementReference).apply(namespaces).apply(name)));
  }

  static <T0> hydra.util.Either<T0, hydra.haskell.syntax.Expression> encodeUnwrap(hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.core.Name name) {
    return hydra.util.Either.<T0, hydra.haskell.syntax.Expression>right(new hydra.haskell.syntax.Expression.Variable(hydra.haskell.Utils.elementReference(
      namespaces,
      hydra.Names.qname(
        hydra.lib.maybes.FromJust.apply(hydra.Names.namespaceOf(name)),
        hydra.haskell.Utils.newtypeAccessorName(name)))));
  }

  static hydra.haskell.environment.HaskellModuleMetadata extendMetaForTerm(hydra.haskell.environment.HaskellModuleMetadata meta, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.haskell.environment.HaskellModuleMetadata otherwise(hydra.core.Term instance) {
        return meta;
      }

      @Override
      public hydra.haskell.environment.HaskellModuleMetadata visit(hydra.core.Term.Map ignored) {
        return hydra.haskell.Coder.setMetaUsesMap(
          true,
          meta);
      }

      @Override
      public hydra.haskell.environment.HaskellModuleMetadata visit(hydra.core.Term.Set ignored) {
        return hydra.haskell.Coder.setMetaUsesSet(
          true,
          meta);
      }
    });
  }

  static hydra.haskell.environment.HaskellModuleMetadata extendMetaForType(hydra.haskell.environment.HaskellModuleMetadata meta, hydra.core.Type typ) {
    return hydra.Strip.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.haskell.environment.HaskellModuleMetadata otherwise(hydra.core.Type instance) {
        return meta;
      }

      @Override
      public hydra.haskell.environment.HaskellModuleMetadata visit(hydra.core.Type.Literal lt) {
        return (lt).value.accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public hydra.haskell.environment.HaskellModuleMetadata otherwise(hydra.core.LiteralType instance) {
            return meta;
          }

          @Override
          public hydra.haskell.environment.HaskellModuleMetadata visit(hydra.core.LiteralType.Binary ignored) {
            return hydra.haskell.Coder.setMetaUsesByteString(
              true,
              meta);
          }

          @Override
          public hydra.haskell.environment.HaskellModuleMetadata visit(hydra.core.LiteralType.Integer_ it) {
            return (it).value.accept(new hydra.core.IntegerType.PartialVisitor<>() {
              @Override
              public hydra.haskell.environment.HaskellModuleMetadata otherwise(hydra.core.IntegerType instance) {
                return meta;
              }

              @Override
              public hydra.haskell.environment.HaskellModuleMetadata visit(hydra.core.IntegerType.Int8 ignored) {
                return hydra.haskell.Coder.setMetaUsesInt(
                  true,
                  meta);
              }

              @Override
              public hydra.haskell.environment.HaskellModuleMetadata visit(hydra.core.IntegerType.Int16 ignored) {
                return hydra.haskell.Coder.setMetaUsesInt(
                  true,
                  meta);
              }

              @Override
              public hydra.haskell.environment.HaskellModuleMetadata visit(hydra.core.IntegerType.Int64 ignored) {
                return hydra.haskell.Coder.setMetaUsesInt(
                  true,
                  meta);
              }
            });
          }
        });
      }

      @Override
      public hydra.haskell.environment.HaskellModuleMetadata visit(hydra.core.Type.Map ignored) {
        return hydra.haskell.Coder.setMetaUsesMap(
          true,
          meta);
      }

      @Override
      public hydra.haskell.environment.HaskellModuleMetadata visit(hydra.core.Type.Set ignored) {
        return hydra.haskell.Coder.setMetaUsesSet(
          true,
          meta);
      }
    });
  }

  static java.util.Set<hydra.core.Name> findOrdVariables(hydra.core.Type typ) {
    java.util.function.Function<hydra.core.Name, Boolean> isTypeVariable = (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.maybes.IsNothing.apply(hydra.Names.namespaceOf(v)));
    java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>> tryType = (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>>) (names -> (java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>) (t -> hydra.Strip.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
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
        hydra.core.Type kt = (mapType).value.keys;
        return (tryType).apply(names).apply(kt);
      }

      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Set et) {
        return (tryType).apply(names).apply((et).value);
      }
    })));
    return hydra.Rewriting.foldOverType(
      new hydra.coders.TraversalOrder.Pre(),
      fold,
      (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      typ);
  }

  static hydra.haskell.environment.HaskellModuleMetadata gatherMetadata(java.util.List<hydra.packaging.Definition> defs) {
    java.util.function.Function<hydra.haskell.environment.HaskellModuleMetadata, java.util.function.Function<hydra.packaging.Definition, hydra.haskell.environment.HaskellModuleMetadata>> addDef = (java.util.function.Function<hydra.haskell.environment.HaskellModuleMetadata, java.util.function.Function<hydra.packaging.Definition, hydra.haskell.environment.HaskellModuleMetadata>>) (meta -> (java.util.function.Function<hydra.packaging.Definition, hydra.haskell.environment.HaskellModuleMetadata>) (def -> (def).accept(new hydra.packaging.Definition.PartialVisitor<>() {
      @Override
      public hydra.haskell.environment.HaskellModuleMetadata visit(hydra.packaging.Definition.Term termDef) {
        hydra.core.Term term = (termDef).value.term;
        hydra.util.Lazy<hydra.haskell.environment.HaskellModuleMetadata> metaWithTerm = new hydra.util.Lazy<>(() -> hydra.Rewriting.foldOverTerm(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<hydra.haskell.environment.HaskellModuleMetadata, java.util.function.Function<hydra.core.Term, hydra.haskell.environment.HaskellModuleMetadata>>) (m -> (java.util.function.Function<hydra.core.Term, hydra.haskell.environment.HaskellModuleMetadata>) (t -> hydra.haskell.Coder.extendMetaForTerm(
            m,
            t))),
          meta,
          term));
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> metaWithTerm.get(),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.haskell.environment.HaskellModuleMetadata>) (ts -> hydra.Rewriting.foldOverType(
            new hydra.coders.TraversalOrder.Pre(),
            (java.util.function.Function<hydra.haskell.environment.HaskellModuleMetadata, java.util.function.Function<hydra.core.Type, hydra.haskell.environment.HaskellModuleMetadata>>) (m -> (java.util.function.Function<hydra.core.Type, hydra.haskell.environment.HaskellModuleMetadata>) (t -> hydra.haskell.Coder.extendMetaForType(
              m,
              t))),
            metaWithTerm.get(),
            (ts).type)),
          (termDef).value.type);
      }

      @Override
      public hydra.haskell.environment.HaskellModuleMetadata visit(hydra.packaging.Definition.Type typeDef) {
        hydra.core.Type typ = (typeDef).value.type.type;
        return hydra.Rewriting.foldOverType(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<hydra.haskell.environment.HaskellModuleMetadata, java.util.function.Function<hydra.core.Type, hydra.haskell.environment.HaskellModuleMetadata>>) (m -> (java.util.function.Function<hydra.core.Type, hydra.haskell.environment.HaskellModuleMetadata>) (t -> hydra.haskell.Coder.extendMetaForType(
            m,
            t))),
          meta,
          typ);
      }
    })));
    return hydra.lib.lists.Foldl.apply(
      addDef,
      hydra.haskell.Coder.emptyMetadata(),
      defs);
  }

  static java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>> getImplicitTypeClasses(hydra.core.Type typ) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      p0 -> hydra.haskell.Coder.<hydra.core.Name>getImplicitTypeClasses_toPair(p0),
      hydra.lib.sets.ToList.apply(hydra.haskell.Coder.findOrdVariables(typ))));
  }

  static <T0> hydra.util.Pair<T0, java.util.Set<hydra.classes.TypeClass>> getImplicitTypeClasses_toPair(T0 name) {
    return (hydra.util.Pair<T0, java.util.Set<hydra.classes.TypeClass>>) ((hydra.util.Pair<T0, java.util.Set<hydra.classes.TypeClass>>) (new hydra.util.Pair<T0, java.util.Set<hydra.classes.TypeClass>>(name, hydra.lib.sets.FromList.apply(java.util.Arrays.asList(new hydra.classes.TypeClass.Ordering())))));
  }

  static Boolean includeTypeDefinitions() {
    return false;
  }

  static hydra.core.Name keyHaskellVar() {
    return new hydra.core.Name("haskellVar");
  }

  static hydra.util.Either<hydra.errors.Error_, java.util.Map<String, String>> moduleToHaskell(hydra.packaging.Module mod, java.util.List<hydra.packaging.Definition> defs, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.haskell.Coder.moduleToHaskellModule(
        mod,
        defs,
        cx,
        g),
      (java.util.function.Function<hydra.haskell.syntax.Module, hydra.util.Either<hydra.errors.Error_, java.util.Map<String, String>>>) (hsmod -> {
        String filepath = hydra.Names.namespaceToFilePath(
          new hydra.util.CaseConvention.Pascal(),
          new hydra.packaging.FileExtension("hs"),
          (mod).namespace);
        String s = hydra.Serialization.printExpr(hydra.Serialization.parenthesize(hydra.haskell.Serde.moduleToExpr(hsmod)));
        return hydra.util.Either.<hydra.errors.Error_, java.util.Map<String, String>>right(hydra.lib.maps.Singleton.apply(
          filepath,
          s));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Module> moduleToHaskellModule(hydra.packaging.Module mod, java.util.List<hydra.packaging.Definition> defs, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.haskell.Utils.namespacesForModule(
        mod,
        cx,
        g),
      (java.util.function.Function<hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Module>>) (namespaces -> hydra.haskell.Coder.constructModule(
        namespaces,
        mod,
        defs,
        cx,
        g)));
  }

  static java.util.List<hydra.haskell.syntax.DeclarationWithComments> nameDecls(hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.core.Name name, hydra.core.Type typ) {
    java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<String, String>> toConstant = (java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<String, String>>) (fieldType -> {
      hydra.core.Name fname = (fieldType).name;
      return (hydra.util.Pair<String, String>) ((hydra.util.Pair<String, String>) (new hydra.util.Pair<String, String>(hydra.haskell.Coder.constantForFieldName(
        name,
        fname), (fname).value)));
    });
    hydra.util.Lazy<java.util.List<hydra.util.Pair<String, String>>> fieldDecls = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      toConstant,
      hydra.Lexical.fieldsOf(typ)));
    String nm = (name).value;
    hydra.util.Lazy<hydra.util.Pair<String, String>> nameDecl = new hydra.util.Lazy<>(() -> (hydra.util.Pair<String, String>) ((hydra.util.Pair<String, String>) (new hydra.util.Pair<String, String>(hydra.haskell.Coder.constantForTypeName(name), nm))));
    java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.util.Pair<String, String>, hydra.haskell.syntax.DeclarationWithComments>> toDecl = (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.util.Pair<String, String>, hydra.haskell.syntax.DeclarationWithComments>>) (n -> (java.util.function.Function<hydra.util.Pair<String, String>, hydra.haskell.syntax.DeclarationWithComments>) (pair -> {
      hydra.util.Lazy<String> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
      hydra.util.Lazy<String> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
      hydra.util.Lazy<hydra.haskell.syntax.Declaration> decl = new hydra.util.Lazy<>(() -> new hydra.haskell.syntax.Declaration.ValueBinding(new hydra.haskell.syntax.ValueBinding.Simple(new hydra.haskell.syntax.SimpleValueBinding(hydra.haskell.Utils.applicationPattern(
        hydra.haskell.Utils.simpleName(k.get()),
        (java.util.List<hydra.haskell.syntax.Pattern>) (java.util.Collections.<hydra.haskell.syntax.Pattern>emptyList())), new hydra.haskell.syntax.RightHandSide(new hydra.haskell.syntax.Expression.Application(new hydra.haskell.syntax.ApplicationExpression(new hydra.haskell.syntax.Expression.Variable(hydra.haskell.Utils.elementReference(
        namespaces,
        n)), new hydra.haskell.syntax.Expression.Literal(new hydra.haskell.syntax.Literal.String_(v.get()))))), (hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>) (hydra.util.Maybe.<hydra.haskell.syntax.LocalBindings>nothing())))));
      return new hydra.haskell.syntax.DeclarationWithComments(decl.get(), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()));
    }));
    return hydra.lib.logic.IfElse.lazy(
      hydra.haskell.Coder.useCoreImport(),
      () -> hydra.lib.lists.Cons.apply(
        (toDecl).apply(new hydra.core.Name("hydra.core.Name")).apply(nameDecl.get()),
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<String, String>, hydra.haskell.syntax.DeclarationWithComments>) (v1 -> (toDecl).apply(new hydra.core.Name("hydra.core.Name")).apply(v1)),
          fieldDecls.get())),
      () -> (java.util.List<hydra.haskell.syntax.DeclarationWithComments>) (java.util.Collections.<hydra.haskell.syntax.DeclarationWithComments>emptyList()));
  }

  static hydra.haskell.environment.HaskellModuleMetadata setMetaUsesByteString(Boolean b, hydra.haskell.environment.HaskellModuleMetadata m) {
    return new hydra.haskell.environment.HaskellModuleMetadata(b, (m).usesInt, (m).usesMap, (m).usesSet);
  }

  static hydra.haskell.environment.HaskellModuleMetadata setMetaUsesInt(Boolean b, hydra.haskell.environment.HaskellModuleMetadata m) {
    return new hydra.haskell.environment.HaskellModuleMetadata((m).usesByteString, b, (m).usesMap, (m).usesSet);
  }

  static hydra.haskell.environment.HaskellModuleMetadata setMetaUsesMap(Boolean b, hydra.haskell.environment.HaskellModuleMetadata m) {
    return new hydra.haskell.environment.HaskellModuleMetadata((m).usesByteString, (m).usesInt, b, (m).usesSet);
  }

  static hydra.haskell.environment.HaskellModuleMetadata setMetaUsesSet(Boolean b, hydra.haskell.environment.HaskellModuleMetadata m) {
    return new hydra.haskell.environment.HaskellModuleMetadata((m).usesByteString, (m).usesInt, (m).usesMap, b);
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments> toDataDeclaration(hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.packaging.TermDefinition def, T0 cx, hydra.graph.Graph g) {
    hydra.core.Name name = (def).name;
    hydra.haskell.syntax.Name hname = hydra.haskell.Utils.simpleName(hydra.Names.localNameOf(name));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.haskell.syntax.ValueBinding, hydra.haskell.syntax.ValueBinding>> rewriteValueBinding = new java.util.concurrent.atomic.AtomicReference<>();
    rewriteValueBinding.set((java.util.function.Function<hydra.haskell.syntax.ValueBinding, hydra.haskell.syntax.ValueBinding>) (vb -> (vb).accept(new hydra.haskell.syntax.ValueBinding.PartialVisitor<>() {
      @Override
      public hydra.haskell.syntax.ValueBinding visit(hydra.haskell.syntax.ValueBinding.Simple simple) {
        hydra.util.Maybe<hydra.haskell.syntax.LocalBindings> bindings = (simple).value.localBindings;
        hydra.haskell.syntax.Pattern pattern_ = (simple).value.pattern;
        hydra.haskell.syntax.RightHandSide rhs = (simple).value.rhs;
        return (pattern_).accept(new hydra.haskell.syntax.Pattern.PartialVisitor<>() {
          @Override
          public hydra.haskell.syntax.ValueBinding otherwise(hydra.haskell.syntax.Pattern instance) {
            return vb;
          }

          @Override
          public hydra.haskell.syntax.ValueBinding visit(hydra.haskell.syntax.Pattern.Application appPat) {
            java.util.List<hydra.haskell.syntax.Pattern> args = (appPat).value.args;
            hydra.haskell.syntax.Name name_ = (appPat).value.name;
            hydra.haskell.syntax.Expression rhsExpr = (rhs).value;
            return (rhsExpr).accept(new hydra.haskell.syntax.Expression.PartialVisitor<>() {
              @Override
              public hydra.haskell.syntax.ValueBinding otherwise(hydra.haskell.syntax.Expression instance) {
                return vb;
              }

              @Override
              public hydra.haskell.syntax.ValueBinding visit(hydra.haskell.syntax.Expression.Lambda lambda_) {
                hydra.haskell.syntax.Expression body = (lambda_).value.inner;
                java.util.List<hydra.haskell.syntax.Pattern> vars = (lambda_).value.bindings;
                hydra.util.Lazy<hydra.haskell.syntax.Pattern> newPattern = new hydra.util.Lazy<>(() -> hydra.haskell.Utils.applicationPattern(
                  name_,
                  hydra.lib.lists.Concat2.apply(
                    args,
                    vars)));
                hydra.haskell.syntax.RightHandSide newRhs = new hydra.haskell.syntax.RightHandSide(body);
                return rewriteValueBinding.get().apply(new hydra.haskell.syntax.ValueBinding.Simple(new hydra.haskell.syntax.SimpleValueBinding(newPattern.get(), newRhs, bindings)));
              }
            });
          }
        });
      }
    })));
    hydra.core.Term term = (def).term;
    hydra.util.Maybe<hydra.core.TypeScheme> typ = (def).type;
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.Maybe<String>, java.util.function.Function<hydra.haskell.syntax.Name, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments>>>>>> toDecl = new java.util.concurrent.atomic.AtomicReference<>();
    toDecl.set((java.util.function.Function<hydra.util.Maybe<String>, java.util.function.Function<hydra.haskell.syntax.Name, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments>>>>>) (comments -> (java.util.function.Function<hydra.haskell.syntax.Name, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments>>>>) (hname_ -> (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments>>>) (term_ -> (java.util.function.Function<hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments>>) (bindings -> hydra.Strip.deannotateTerm(term_).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments> otherwise(hydra.core.Term instance) {
        return hydra.lib.eithers.Bind.apply(
          hydra.haskell.Coder.<T0>encodeTerm(
            0,
            namespaces,
            term_,
            cx,
            g),
          (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments>>) (hterm -> {
            hydra.util.Lazy<hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>> schemeConstraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
              () -> (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()),
              (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (ts -> (ts).constraints),
              typ));
            hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>> schemeClasses = new hydra.util.Lazy<>(() -> hydra.haskell.Coder.typeSchemeConstraintsToClassMap(schemeConstraints.get()));
            hydra.haskell.syntax.ValueBinding vb = hydra.haskell.Utils.simpleValueBinding(
              hname_,
              hterm,
              bindings);
            return hydra.lib.eithers.Bind.apply(
              hydra.Annotations.<T0>getTypeClasses(
                cx,
                g,
                hydra.Strip.removeTypesFromTerm(term)),
              (java.util.function.Function<java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments>>) (explicitClasses -> {
                hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>> combinedClasses = new hydra.util.Lazy<>(() -> hydra.lib.maps.Union.apply(
                  schemeClasses.get(),
                  explicitClasses));
                hydra.util.Lazy<hydra.core.Type> schemeType = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
                  () -> new hydra.core.Type.Unit(),
                  (java.util.function.Function<hydra.core.TypeScheme, hydra.core.Type>) (ts -> (ts).type),
                  typ));
                return hydra.lib.eithers.Bind.apply(
                  hydra.haskell.Coder.encodeTypeWithClassAssertions(
                    namespaces,
                    combinedClasses.get(),
                    schemeType.get(),
                    cx,
                    g),
                  (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments>>) (htype -> {
                    hydra.haskell.syntax.Declaration decl = new hydra.haskell.syntax.Declaration.TypedBinding(new hydra.haskell.syntax.TypedBinding(new hydra.haskell.syntax.TypeSignature(hname_, htype), rewriteValueBinding.get().apply(vb)));
                    return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments>right(new hydra.haskell.syntax.DeclarationWithComments(decl, comments));
                  }));
              }));
          }));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments> visit(hydra.core.Term.Let letTerm) {
        hydra.core.Term env = (letTerm).value.body;
        java.util.List<hydra.core.Binding> lbindings = (letTerm).value.bindings;
        hydra.util.Lazy<java.util.List<hydra.haskell.syntax.Name>> hnames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.haskell.syntax.Name>) (binding -> hydra.haskell.Utils.simpleName((binding).name.value)),
          lbindings));
        hydra.util.Lazy<java.util.List<hydra.core.Term>> terms = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          projected -> projected.term,
          lbindings));
        java.util.function.Function<hydra.haskell.syntax.Name, java.util.function.Function<hydra.haskell.syntax.Expression, hydra.haskell.syntax.LocalBinding>> toTermDefinition = (java.util.function.Function<hydra.haskell.syntax.Name, java.util.function.Function<hydra.haskell.syntax.Expression, hydra.haskell.syntax.LocalBinding>>) (hname_ -> (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.haskell.syntax.LocalBinding>) (hterm_ -> new hydra.haskell.syntax.LocalBinding.Value(hydra.haskell.Utils.simpleValueBinding(
          hname_,
          hterm_,
          (hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>) (hydra.util.Maybe.<hydra.haskell.syntax.LocalBindings>nothing())))));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Expression>>) (t -> hydra.haskell.Coder.<T0>encodeTerm(
              0,
              namespaces,
              t,
              cx,
              g)),
            terms.get()),
          (java.util.function.Function<java.util.List<hydra.haskell.syntax.Expression>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments>>) (hterms -> {
            hydra.util.Lazy<java.util.List<hydra.haskell.syntax.LocalBinding>> hbindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.ZipWith.apply(
              toTermDefinition,
              hnames.get(),
              hterms));
            hydra.util.Lazy<java.util.List<hydra.haskell.syntax.LocalBinding>> prevBindings = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
              () -> (java.util.List<hydra.haskell.syntax.LocalBinding>) (java.util.Collections.<hydra.haskell.syntax.LocalBinding>emptyList()),
              (java.util.function.Function<hydra.haskell.syntax.LocalBindings, java.util.List<hydra.haskell.syntax.LocalBinding>>) (lb -> (lb).value),
              bindings));
            hydra.util.Lazy<java.util.List<hydra.haskell.syntax.LocalBinding>> allBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
              prevBindings.get(),
              hbindings.get()));
            return toDecl.get().apply(comments).apply(hname_).apply(env).apply(hydra.util.Maybe.just(new hydra.haskell.syntax.LocalBindings(allBindings.get())));
          }));
      }
    }))))));
    return hydra.lib.eithers.Bind.apply(
      hydra.Annotations.<T0>getTermDescription(
        cx,
        g,
        term),
      (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments>>) (comments -> toDecl.get().apply(comments).apply(hname).apply(term).apply((hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>) (hydra.util.Maybe.<hydra.haskell.syntax.LocalBindings>nothing()))));
  }

  static hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.DeclarationWithComments>> toTypeDeclarationsFrom(hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.core.Name elementName, hydra.core.Type typ, hydra.context.Context cx, hydra.graph.Graph g) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.haskell.syntax.Name, java.util.function.Function<java.util.List<hydra.core.Name>, hydra.haskell.syntax.DeclarationHead>>> declHead = new java.util.concurrent.atomic.AtomicReference<>();
    declHead.set((java.util.function.Function<hydra.haskell.syntax.Name, java.util.function.Function<java.util.List<hydra.core.Name>, hydra.haskell.syntax.DeclarationHead>>) (name -> (java.util.function.Function<java.util.List<hydra.core.Name>, hydra.haskell.syntax.DeclarationHead>) (vars_ -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(vars_),
      () -> new hydra.haskell.syntax.DeclarationHead.Simple(name),
      () -> ((java.util.function.Supplier<hydra.haskell.syntax.DeclarationHead>) (() -> {
        hydra.util.Lazy<hydra.core.Name> h = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(vars_));
        hydra.haskell.syntax.Variable hvar = new hydra.haskell.syntax.Variable(hydra.haskell.Utils.simpleName(h.get().value));
        hydra.util.Lazy<java.util.List<hydra.core.Name>> rest = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(vars_));
        return new hydra.haskell.syntax.DeclarationHead.Application(new hydra.haskell.syntax.ApplicationDeclarationHead(declHead.get().apply(name).apply(rest.get()), hvar));
      })).get()))));
    String lname = hydra.Names.localNameOf(elementName);
    hydra.haskell.syntax.Name hname = hydra.haskell.Utils.simpleName(lname);
    java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>>> newtypeCons = (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>>>) (tname -> (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>>) (typ_ -> {
      hydra.haskell.syntax.Name hname0 = hydra.haskell.Utils.simpleName(hydra.haskell.Utils.newtypeAccessorName(tname));
      return hydra.lib.eithers.Bind.apply(
        hydra.haskell.Coder.adaptTypeToHaskellAndEncode(
          namespaces,
          typ_,
          cx,
          g),
        (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>>) (htype -> {
          hydra.haskell.syntax.Name constructorName = hydra.haskell.Utils.simpleName(hydra.Names.localNameOf(tname));
          hydra.util.Lazy<hydra.haskell.syntax.FieldWithComments> hfield = new hydra.util.Lazy<>(() -> new hydra.haskell.syntax.FieldWithComments(new hydra.haskell.syntax.Field(hname0, htype), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())));
          return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>right(new hydra.haskell.syntax.ConstructorWithComments(new hydra.haskell.syntax.Constructor.Record(new hydra.haskell.syntax.RecordConstructor(constructorName, java.util.Arrays.asList(hfield.get()))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())));
        }));
    }));
    java.util.function.Function<String, java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>>> recordCons = (java.util.function.Function<String, java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>>>) (lname_ -> (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>>) (fields -> {
      java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.FieldWithComments>> toField = (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.FieldWithComments>>) (fieldType -> {
        hydra.core.Name fname = (fieldType).name;
        hydra.core.Type ftype = (fieldType).type;
        hydra.haskell.syntax.Name hname_ = hydra.haskell.Utils.simpleName(hydra.lib.strings.Cat2.apply(
          hydra.Formatting.decapitalize(lname_),
          hydra.Formatting.capitalize((fname).value)));
        return hydra.lib.eithers.Bind.apply(
          hydra.haskell.Coder.adaptTypeToHaskellAndEncode(
            namespaces,
            ftype,
            cx,
            g),
          (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.FieldWithComments>>) (htype -> hydra.lib.eithers.Bind.apply(
            hydra.Annotations.getTypeDescription(
              cx,
              g,
              ftype),
            (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.FieldWithComments>>) (comments -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.FieldWithComments>right(new hydra.haskell.syntax.FieldWithComments(new hydra.haskell.syntax.Field(hname_, htype), comments))))));
      });
      return hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          toField,
          fields),
        (java.util.function.Function<java.util.List<hydra.haskell.syntax.FieldWithComments>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>>) (hFields -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>right(new hydra.haskell.syntax.ConstructorWithComments(new hydra.haskell.syntax.Constructor.Record(new hydra.haskell.syntax.RecordConstructor(hydra.haskell.Utils.simpleName(lname_), hFields)), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())))));
    }));
    java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<String, java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>>>> unionCons = (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<String, java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>>>>) (boundNames_ -> (java.util.function.Function<String, java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>>>) (lname_ -> (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>>) (fieldType -> {
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<String, String>> deconflict = new java.util.concurrent.atomic.AtomicReference<>();
      deconflict.set((java.util.function.Function<String, String>) (name -> {
        hydra.util.Lazy<hydra.core.Name> tname = new hydra.util.Lazy<>(() -> hydra.Names.unqualifyName(new hydra.packaging.QualifiedName(hydra.util.Maybe.just(hydra.lib.pairs.First.apply(((java.util.function.Function<hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>, hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>>) (projected -> projected.focus)).apply(namespaces))), name)));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.sets.Member.apply(
            tname.get(),
            boundNames_),
          () -> deconflict.get().apply(hydra.lib.strings.Cat2.apply(
            name,
            "_")),
          () -> name);
      }));
      hydra.core.Name fname = (fieldType).name;
      hydra.core.Type ftype = (fieldType).type;
      return hydra.lib.eithers.Bind.apply(
        hydra.Annotations.getTypeDescription(
          cx,
          g,
          ftype),
        (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>>) (comments -> {
          String nm = deconflict.get().apply(hydra.lib.strings.Cat2.apply(
            hydra.Formatting.capitalize(lname_),
            hydra.Formatting.capitalize((fname).value)));
          return hydra.lib.eithers.Bind.apply(
            hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                hydra.Strip.deannotateType(ftype),
                new hydra.core.Type.Unit()),
              () -> hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.Type>>right((java.util.List<hydra.haskell.syntax.Type>) (java.util.Collections.<hydra.haskell.syntax.Type>emptyList())),
              () -> hydra.lib.eithers.Bind.apply(
                hydra.haskell.Coder.adaptTypeToHaskellAndEncode(
                  namespaces,
                  ftype,
                  cx,
                  g),
                (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.Type>>>) (htype -> hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.Type>>right(java.util.Arrays.asList(htype))))),
            (java.util.function.Function<java.util.List<hydra.haskell.syntax.Type>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>>) (typeList -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>right(new hydra.haskell.syntax.ConstructorWithComments(new hydra.haskell.syntax.Constructor.Ordinary(new hydra.haskell.syntax.OrdinaryConstructor(hydra.haskell.Utils.simpleName(nm), typeList)), comments))));
        }));
    })));
    return hydra.lib.eithers.Bind.apply(
      hydra.Predicates.isSerializableByName(
        cx,
        g,
        elementName),
      (java.util.function.Function<Boolean, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.DeclarationWithComments>>>) (isSer -> {
        hydra.util.Lazy<hydra.haskell.syntax.Deriving> deriv = new hydra.util.Lazy<>(() -> new hydra.haskell.syntax.Deriving(hydra.lib.logic.IfElse.lazy(
          isSer,
          () -> hydra.lib.lists.Map.apply(
            hydra.haskell.Utils::rawName,
            java.util.Arrays.asList(
              "Eq",
              "Ord",
              "Read",
              "Show")),
          () -> (java.util.List<hydra.haskell.syntax.Name>) (java.util.Collections.<hydra.haskell.syntax.Name>emptyList()))));
        hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Type> unpackResult = hydra.haskell.Utils.unpackForallType(typ);
        hydra.util.Lazy<java.util.List<hydra.core.Name>> vars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(unpackResult));
        hydra.util.Lazy<hydra.haskell.syntax.DeclarationHead> hd = new hydra.util.Lazy<>(() -> declHead.get().apply(hname).apply(hydra.lib.lists.Reverse.apply(vars.get())));
        hydra.util.Lazy<hydra.core.Type> t_ = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(unpackResult));
        return hydra.lib.eithers.Bind.apply(
          hydra.Strip.deannotateType(t_.get()).accept(new hydra.core.Type.PartialVisitor<>() {
            @Override
            public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Declaration> otherwise(hydra.core.Type instance) {
              return hydra.lib.eithers.Bind.apply(
                hydra.haskell.Coder.adaptTypeToHaskellAndEncode(
                  namespaces,
                  typ,
                  cx,
                  g),
                (java.util.function.Function<hydra.haskell.syntax.Type, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Declaration>>) (htype -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Declaration>right(new hydra.haskell.syntax.Declaration.Type(new hydra.haskell.syntax.TypeDeclaration(hd.get(), htype)))));
            }

            @Override
            public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Declaration> visit(hydra.core.Type.Record rt) {
              return hydra.lib.eithers.Bind.apply(
                (recordCons).apply(lname).apply((rt).value),
                (java.util.function.Function<hydra.haskell.syntax.ConstructorWithComments, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Declaration>>) (cons -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Declaration>right(new hydra.haskell.syntax.Declaration.Data(new hydra.haskell.syntax.DataDeclaration(new hydra.haskell.syntax.DataOrNewtype.Data(), (java.util.List<hydra.haskell.syntax.Assertion>) (java.util.Collections.<hydra.haskell.syntax.Assertion>emptyList()), hd.get(), java.util.Arrays.asList(cons), java.util.Arrays.asList(deriv.get()))))));
            }

            @Override
            public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Declaration> visit(hydra.core.Type.Union rt) {
              return hydra.lib.eithers.Bind.apply(
                hydra.lib.eithers.MapList.apply(
                  (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.ConstructorWithComments>>) (v1 -> (unionCons).apply(hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply((g).boundTerms))).apply(lname).apply(v1)),
                  (rt).value),
                (java.util.function.Function<java.util.List<hydra.haskell.syntax.ConstructorWithComments>, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Declaration>>) (cons -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Declaration>right(new hydra.haskell.syntax.Declaration.Data(new hydra.haskell.syntax.DataDeclaration(new hydra.haskell.syntax.DataOrNewtype.Data(), (java.util.List<hydra.haskell.syntax.Assertion>) (java.util.Collections.<hydra.haskell.syntax.Assertion>emptyList()), hd.get(), cons, java.util.Arrays.asList(deriv.get()))))));
            }

            @Override
            public hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Declaration> visit(hydra.core.Type.Wrap wrapped) {
              return hydra.lib.eithers.Bind.apply(
                (newtypeCons).apply(elementName).apply((wrapped).value),
                (java.util.function.Function<hydra.haskell.syntax.ConstructorWithComments, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.Declaration>>) (cons -> hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.Declaration>right(new hydra.haskell.syntax.Declaration.Data(new hydra.haskell.syntax.DataDeclaration(new hydra.haskell.syntax.DataOrNewtype.Newtype(), (java.util.List<hydra.haskell.syntax.Assertion>) (java.util.Collections.<hydra.haskell.syntax.Assertion>emptyList()), hd.get(), java.util.Arrays.asList(cons), java.util.Arrays.asList(deriv.get()))))));
            }
          }),
          (java.util.function.Function<hydra.haskell.syntax.Declaration, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.DeclarationWithComments>>>) (decl -> hydra.lib.eithers.Bind.apply(
            hydra.Annotations.getTypeDescription(
              cx,
              g,
              typ),
            (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.DeclarationWithComments>>>) (comments -> hydra.lib.eithers.Bind.apply(
              hydra.lib.logic.IfElse.lazy(
                hydra.haskell.Coder.includeTypeDefinitions(),
                () -> hydra.lib.eithers.Bind.apply(
                  hydra.haskell.Coder.typeDecl(
                    namespaces,
                    elementName,
                    typ,
                    cx,
                    g),
                  (java.util.function.Function<hydra.haskell.syntax.DeclarationWithComments, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.DeclarationWithComments>>>) (decl_ -> hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.DeclarationWithComments>>right(java.util.Arrays.asList(decl_)))),
                () -> hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.DeclarationWithComments>>right((java.util.List<hydra.haskell.syntax.DeclarationWithComments>) (java.util.Collections.<hydra.haskell.syntax.DeclarationWithComments>emptyList()))),
              (java.util.function.Function<java.util.List<hydra.haskell.syntax.DeclarationWithComments>, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.DeclarationWithComments>>>) (tdecls -> {
                hydra.haskell.syntax.DeclarationWithComments mainDecl = new hydra.haskell.syntax.DeclarationWithComments(decl, comments);
                java.util.List<hydra.haskell.syntax.DeclarationWithComments> nameDecls_ = hydra.haskell.Coder.nameDecls(
                  namespaces,
                  elementName,
                  typ);
                return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.haskell.syntax.DeclarationWithComments>>right(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
                  java.util.Arrays.asList(mainDecl),
                  nameDecls_,
                  tdecls)));
              }))))));
      }));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments> typeDecl(hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.core.Name name, hydra.core.Type typ, T0 cx, hydra.graph.Graph g) {
    hydra.core.Term rawTerm = hydra.encode.Core.type(typ);
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> rewrite = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> {
      java.util.function.Function<hydra.core.Term, hydra.util.Maybe<String>> decodeString = (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<String>>) (term2 -> hydra.Strip.deannotateTerm(term2).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<String> otherwise(hydra.core.Term instance) {
          return (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing());
        }

        @Override
        public hydra.util.Maybe<String> visit(hydra.core.Term.Literal lit) {
          return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
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
      java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Name>> decodeName = (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Name>>) (term2 -> hydra.Strip.deannotateTerm(term2).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<hydra.core.Name> otherwise(hydra.core.Term instance) {
          return (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing());
        }

        @Override
        public hydra.util.Maybe<hydra.core.Name> visit(hydra.core.Term.Wrap wt) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              (wt).value.typeName,
              new hydra.core.Name("hydra.core.Name")),
            () -> hydra.lib.maybes.Map.apply(
              (java.util.function.Function<String, hydra.core.Name>) (x -> new hydra.core.Name(x)),
              (decodeString).apply((wt).value.body)),
            () -> (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing()));
        }
      }));
      java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>> forVariableType = (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) (vname -> {
        hydra.packaging.QualifiedName qname = hydra.Names.qualifyName(vname);
        String local = (qname).local;
        hydra.util.Maybe<hydra.packaging.Namespace> mns = (qname).namespace;
        return hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.packaging.Namespace, hydra.core.Term>) (ns -> new hydra.core.Term.Variable(hydra.Names.qname(
            ns,
            hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
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
      hydra.util.Lazy<hydra.util.Maybe<hydra.core.Field>> variantResult = new hydra.util.Lazy<>(() -> hydra.Strip.deannotateTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<hydra.core.Field> otherwise(hydra.core.Term instance) {
          return (hydra.util.Maybe<hydra.core.Field>) (hydra.util.Maybe.<hydra.core.Field>nothing());
        }

        @Override
        public hydra.util.Maybe<hydra.core.Field> visit(hydra.core.Term.Inject inj) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              (inj).value.typeName,
              new hydra.core.Name("hydra.core.Type")),
            () -> hydra.util.Maybe.just((inj).value.field),
            () -> (hydra.util.Maybe<hydra.core.Field>) (hydra.util.Maybe.<hydra.core.Field>nothing()));
        }
      }));
      return hydra.lib.maybes.FromMaybe.applyLazy(
        () -> (recurse).apply(term),
        hydra.lib.maybes.Bind.apply(
          variantResult.get(),
          forType));
    }));
    hydra.core.Term finalTerm = hydra.Rewriting.rewriteTerm(
      rewrite,
      rawTerm);
    java.util.function.Function<hydra.core.Name, String> typeNameLocal = (java.util.function.Function<hydra.core.Name, String>) (name_ -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "_",
      hydra.Names.localNameOf(name_),
      "_type_")));
    java.util.function.Function<hydra.packaging.Namespace, java.util.function.Function<hydra.core.Name, hydra.core.Name>> typeName = (java.util.function.Function<hydra.packaging.Namespace, java.util.function.Function<hydra.core.Name, hydra.core.Name>>) (ns -> (java.util.function.Function<hydra.core.Name, hydra.core.Name>) (name_ -> hydra.Names.qname(
      ns,
      (typeNameLocal).apply(name_))));
    return hydra.lib.eithers.Bind.apply(
      hydra.haskell.Coder.<T0>encodeTerm(
        0,
        namespaces,
        finalTerm,
        cx,
        g),
      (java.util.function.Function<hydra.haskell.syntax.Expression, hydra.util.Either<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments>>) (expr -> {
        hydra.haskell.syntax.Name hname = hydra.haskell.Utils.simpleName((typeNameLocal).apply(name));
        hydra.util.Lazy<hydra.haskell.syntax.Pattern> pat = new hydra.util.Lazy<>(() -> hydra.haskell.Utils.applicationPattern(
          hname,
          (java.util.List<hydra.haskell.syntax.Pattern>) (java.util.Collections.<hydra.haskell.syntax.Pattern>emptyList())));
        hydra.haskell.syntax.RightHandSide rhs = new hydra.haskell.syntax.RightHandSide(expr);
        hydra.util.Lazy<hydra.haskell.syntax.Declaration> decl = new hydra.util.Lazy<>(() -> new hydra.haskell.syntax.Declaration.ValueBinding(new hydra.haskell.syntax.ValueBinding.Simple(new hydra.haskell.syntax.SimpleValueBinding(pat.get(), rhs, (hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>) (hydra.util.Maybe.<hydra.haskell.syntax.LocalBindings>nothing())))));
        return hydra.util.Either.<hydra.errors.Error_, hydra.haskell.syntax.DeclarationWithComments>right(new hydra.haskell.syntax.DeclarationWithComments(decl.get(), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())));
      }));
  }

  static <T0> java.util.Map<T0, java.util.Set<hydra.classes.TypeClass>> typeSchemeConstraintsToClassMap(hydra.util.Maybe<java.util.Map<T0, hydra.core.TypeVariableMetadata>> maybeConstraints) {
    java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.classes.TypeClass>> nameToTypeClass = (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.classes.TypeClass>>) (className -> {
      String classNameStr = (className).value;
      hydra.util.Lazy<Boolean> isEq = new hydra.util.Lazy<>(() -> hydra.lib.equality.Equal.apply(
        classNameStr,
        new hydra.core.Name("equality").value));
      hydra.util.Lazy<Boolean> isOrd = new hydra.util.Lazy<>(() -> hydra.lib.equality.Equal.apply(
        classNameStr,
        new hydra.core.Name("ordering").value));
      return hydra.lib.logic.IfElse.lazy(
        isEq.get(),
        () -> hydra.util.Maybe.just(new hydra.classes.TypeClass.Equality()),
        () -> hydra.lib.logic.IfElse.lazy(
          isOrd.get(),
          () -> hydra.util.Maybe.just(new hydra.classes.TypeClass.Ordering()),
          () -> (hydra.util.Maybe<hydra.classes.TypeClass>) (hydra.util.Maybe.<hydra.classes.TypeClass>nothing())));
    });
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> (java.util.Map<T0, java.util.Set<hydra.classes.TypeClass>>) ((java.util.Map<T0, java.util.Set<hydra.classes.TypeClass>>) (hydra.lib.maps.Empty.<T0, java.util.Set<hydra.classes.TypeClass>>apply())),
      (java.util.function.Function<java.util.Map<T0, hydra.core.TypeVariableMetadata>, java.util.Map<T0, java.util.Set<hydra.classes.TypeClass>>>) (constraints -> hydra.lib.maps.Map.apply(
        (java.util.function.Function<hydra.core.TypeVariableMetadata, java.util.Set<hydra.classes.TypeClass>>) (meta -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
          nameToTypeClass,
          hydra.lib.sets.ToList.apply((meta).classes))))),
        constraints)),
      maybeConstraints);
  }

  static Boolean useCoreImport() {
    return true;
  }
}
