// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell;

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

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> adaptTypeToHaskellAndEncode(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Type typ, hydra.context.Context cx, T0 g) {
    java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>> enc = (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (t -> hydra.ext.haskell.Coder.<T0>encodeType(
      namespaces,
      t,
      cx,
      g));
    return hydra.Rewriting.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> otherwise(hydra.core.Type instance) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Bimap.apply(
            (java.util.function.Function<String, hydra.context.InContext<hydra.errors.Error_>>) (_s -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(_s)), cx))),
            (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_x -> _x),
            hydra.Adapt.adaptTypeForLanguage(
              hydra.ext.haskell.Language.haskellLanguage(),
              typ)),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (adaptedType -> (enc).apply(adaptedType)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Variable ignored) {
        return (enc).apply(typ);
      }
    });
  }

  static String constantForFieldName(hydra.core.Name tname, hydra.core.Name fname) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
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

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Module> constructModule(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.module.Module mod, hydra.util.ConsList<hydra.module.Definition> defs, hydra.context.Context cx, hydra.graph.Graph g) {
    java.util.function.Function<hydra.module.Definition, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>>> createDeclarations = (java.util.function.Function<hydra.module.Definition, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>>>) (def -> (def).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>> visit(hydra.module.Definition.Type type) {
        hydra.core.Name name = (type).value.name;
        hydra.core.Type typ = (type).value.type;
        return hydra.ext.haskell.Coder.toTypeDeclarationsFrom(
          namespaces,
          name,
          typ,
          cx,
          g);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>> visit(hydra.module.Definition.Term term) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.haskell.Coder.toDataDeclaration(
            namespaces,
            (term).value,
            cx,
            g),
          (java.util.function.Function<hydra.ext.haskell.ast.DeclarationWithComments, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>>>) (d -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>>right(hydra.util.ConsList.of(d))));
      }
    }));
    java.util.function.Function<hydra.module.Namespace, String> h = (java.util.function.Function<hydra.module.Namespace, String>) (namespace -> (namespace).value);
    java.util.function.Function<String, hydra.ext.haskell.ast.ModuleName> importName = (java.util.function.Function<String, hydra.ext.haskell.ast.ModuleName>) (name -> new hydra.ext.haskell.ast.ModuleName(hydra.lib.strings.Intercalate.apply(
      ".",
      hydra.lib.lists.Map.apply(
        hydra.Formatting::capitalize,
        hydra.lib.strings.SplitOn.apply(
          ".",
          name)))));
    java.util.function.Function<hydra.util.Pair<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>, hydra.ext.haskell.ast.Import> toImport = (java.util.function.Function<hydra.util.Pair<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>, hydra.ext.haskell.ast.Import>) (pair -> {
      hydra.util.Lazy<hydra.ext.haskell.ast.ModuleName> alias = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
      hydra.util.Lazy<hydra.module.Namespace> namespace = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
      String name = (h).apply(namespace.get());
      return new hydra.ext.haskell.ast.Import(true, (importName).apply(name), hydra.util.Maybe.just(alias.get()), (hydra.util.Maybe<hydra.ext.haskell.ast.SpecImport>) (hydra.util.Maybe.<hydra.ext.haskell.ast.SpecImport>nothing()));
    });
    hydra.util.Lazy<hydra.util.ConsList<hydra.ext.haskell.ast.Import>> domainImports = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      toImport,
      hydra.lib.maps.ToList.apply(((java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, hydra.util.PersistentMap<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>>) (projected -> projected.mapping)).apply(namespaces))));
    java.util.function.Function<hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>, hydra.ext.haskell.ast.Import> toImport2 = (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>, hydra.ext.haskell.ast.Import>) (triple -> {
      hydra.util.Lazy<hydra.util.ConsList<String>> hidden = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(triple));
      hydra.util.Lazy<hydra.util.Maybe<String>> malias = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(triple)));
      hydra.util.Lazy<String> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(triple)));
      hydra.util.Lazy<hydra.util.Maybe<hydra.ext.haskell.ast.SpecImport>> spec = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(hidden.get()),
        () -> (hydra.util.Maybe<hydra.ext.haskell.ast.SpecImport>) (hydra.util.Maybe.<hydra.ext.haskell.ast.SpecImport>nothing()),
        () -> hydra.util.Maybe.just(new hydra.ext.haskell.ast.SpecImport.Hiding(hydra.lib.lists.Map.apply(
          (java.util.function.Function<String, hydra.ext.haskell.ast.ImportExportSpec>) (n -> new hydra.ext.haskell.ast.ImportExportSpec((hydra.util.Maybe<hydra.ext.haskell.ast.ImportModifier>) (hydra.util.Maybe.<hydra.ext.haskell.ast.ImportModifier>nothing()), hydra.ext.haskell.Utils.simpleName(n), (hydra.util.Maybe<hydra.ext.haskell.ast.SubspecImportExportSpec>) (hydra.util.Maybe.<hydra.ext.haskell.ast.SubspecImportExportSpec>nothing()))),
          hidden.get())))));
      return new hydra.ext.haskell.ast.Import(hydra.lib.maybes.IsJust.apply(malias.get()), new hydra.ext.haskell.ast.ModuleName(name.get()), hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, hydra.ext.haskell.ast.ModuleName>) (x -> new hydra.ext.haskell.ast.ModuleName(x)),
        malias.get()), spec.get());
    });
    hydra.util.Lazy<hydra.util.ConsList<hydra.ext.haskell.ast.Import>> standardImports = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      toImport2,
      hydra.lib.lists.Concat2.apply(
        hydra.util.ConsList.of(
          (hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>) ((hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>) (new hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>((hydra.util.Pair<String, hydra.util.Maybe<String>>) ((hydra.util.Pair<String, hydra.util.Maybe<String>>) (new hydra.util.Pair<String, hydra.util.Maybe<String>>("Prelude", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())))), hydra.util.ConsList.of(
            "Enum",
            "Ordering",
            "decodeFloat",
            "encodeFloat",
            "fail",
            "map",
            "pure",
            "sum")))),
          (hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>) ((hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>) (new hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>((hydra.util.Pair<String, hydra.util.Maybe<String>>) ((hydra.util.Pair<String, hydra.util.Maybe<String>>) (new hydra.util.Pair<String, hydra.util.Maybe<String>>("Data.ByteString", hydra.util.Maybe.just("B")))), (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty())))),
          (hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>) ((hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>) (new hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>((hydra.util.Pair<String, hydra.util.Maybe<String>>) ((hydra.util.Pair<String, hydra.util.Maybe<String>>) (new hydra.util.Pair<String, hydra.util.Maybe<String>>("Data.Int", hydra.util.Maybe.just("I")))), (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty())))),
          (hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>) ((hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>) (new hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>((hydra.util.Pair<String, hydra.util.Maybe<String>>) ((hydra.util.Pair<String, hydra.util.Maybe<String>>) (new hydra.util.Pair<String, hydra.util.Maybe<String>>("Data.List", hydra.util.Maybe.just("L")))), (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty())))),
          (hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>) ((hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>) (new hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>((hydra.util.Pair<String, hydra.util.Maybe<String>>) ((hydra.util.Pair<String, hydra.util.Maybe<String>>) (new hydra.util.Pair<String, hydra.util.Maybe<String>>("Data.Map", hydra.util.Maybe.just("M")))), (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty())))),
          (hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>) ((hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>) (new hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>((hydra.util.Pair<String, hydra.util.Maybe<String>>) ((hydra.util.Pair<String, hydra.util.Maybe<String>>) (new hydra.util.Pair<String, hydra.util.Maybe<String>>("Data.Set", hydra.util.Maybe.just("S")))), (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty()))))),
        hydra.lib.logic.IfElse.lazy(
          hydra.Schemas.moduleContainsBinaryLiterals(mod),
          () -> hydra.util.ConsList.of((hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>) ((hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>) (new hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>((hydra.util.Pair<String, hydra.util.Maybe<String>>) ((hydra.util.Pair<String, hydra.util.Maybe<String>>) (new hydra.util.Pair<String, hydra.util.Maybe<String>>("Hydra.Lib.Literals", hydra.util.Maybe.just("Literals")))), (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty()))))),
          () -> (hydra.util.ConsList<hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>>) (hydra.util.ConsList.<hydra.util.Pair<hydra.util.Pair<String, hydra.util.Maybe<String>>, hydra.util.ConsList<String>>>empty())))));
    hydra.util.Lazy<hydra.util.ConsList<hydra.ext.haskell.ast.Import>> imports = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
      domainImports.get(),
      standardImports.get()));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        createDeclarations,
        defs),
      (java.util.function.Function<hydra.util.ConsList<hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Module>>) (declLists -> {
        hydra.util.Lazy<hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>> decls = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(declLists));
        hydra.util.Maybe<String> mc = (mod).description;
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Module>right(new hydra.ext.haskell.ast.Module(hydra.util.Maybe.just(new hydra.ext.haskell.ast.ModuleHead(mc, (importName).apply((h).apply((mod).namespace)), (hydra.util.ConsList<hydra.ext.haskell.ast.Export>) (hydra.util.ConsList.<hydra.ext.haskell.ast.Export>empty()))), imports.get(), decls.get()));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> encodeCaseExpression(Integer depth, hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.CaseStatement stmt, hydra.ext.haskell.ast.Expression scrutinee, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.util.Maybe<hydra.core.Term> def = (stmt).default_;
    hydra.core.Name dn = (stmt).typeName;
    hydra.util.ConsList<hydra.core.Field> fields = (stmt).cases;
    java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.FieldType>, java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Alternative>>> toAlt = (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.FieldType>, java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Alternative>>>) (fieldMap -> (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Alternative>>) (field -> {
      hydra.core.Name fn = (field).name;
      hydra.core.Term fun_ = (field).term;
      hydra.util.Lazy<hydra.ext.haskell.ast.Name> hname = new hydra.util.Lazy<>(() -> hydra.ext.haskell.Utils.unionFieldReference(
        hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply((g).boundTerms)),
        namespaces,
        dn,
        fn));
      String v0 = hydra.lib.strings.Cat2.apply(
        "v",
        hydra.lib.literals.ShowInt32.apply(depth));
      hydra.core.Term raw = new hydra.core.Term.Application(new hydra.core.Application(fun_, new hydra.core.Term.Variable(new hydra.core.Name(v0))));
      hydra.core.Term rhsTerm = hydra.Rewriting.simplifyTerm(raw);
      hydra.util.Lazy<String> v1 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
        hydra.Rewriting.isFreeVariableInTerm(
          new hydra.core.Name(v0),
          rhsTerm),
        () -> hydra.Constants.ignoredVariable(),
        () -> v0));
      return hydra.lib.eithers.Bind.apply(
        hydra.lib.maybes.Cases.applyLazy(
          hydra.lib.maps.Lookup.apply(
            fn,
            fieldMap),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.Pattern>>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
            "field ",
            hydra.lib.literals.ShowString.apply((fn).value),
            " not found in ",
            hydra.lib.literals.ShowString.apply((dn).value))))), cx))),
          (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.Pattern>>>) (fieldType -> {
            hydra.core.Type ft = (fieldType).type;
            hydra.util.ConsList<hydra.ext.haskell.ast.Pattern> singleArg = hydra.util.ConsList.of(new hydra.ext.haskell.ast.Pattern.Name(hydra.ext.haskell.Utils.rawName(v1.get())));
            return hydra.Rewriting.deannotateType(ft).accept(new hydra.core.Type.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.Pattern>> otherwise(hydra.core.Type instance) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.Pattern>>right(singleArg);
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.Pattern>> visit(hydra.core.Type.Unit ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.Pattern>>right(hydra.ext.haskell.Coder.<hydra.ext.haskell.ast.Pattern>encodeCaseExpression_noArgs());
              }
            });
          })),
        (java.util.function.Function<hydra.util.ConsList<hydra.ext.haskell.ast.Pattern>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Alternative>>) (args -> {
          hydra.ext.haskell.ast.Pattern lhs = hydra.ext.haskell.Utils.applicationPattern(
            hname.get(),
            args);
          return hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.CaseRhs>) (x -> new hydra.ext.haskell.ast.CaseRhs(x)),
              hydra.ext.haskell.Coder.encodeTerm(
                hydra.lib.math.Add.apply(
                  depth,
                  1),
                namespaces,
                rhsTerm,
                cx,
                g)),
            (java.util.function.Function<hydra.ext.haskell.ast.CaseRhs, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Alternative>>) (rhs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Alternative>right(new hydra.ext.haskell.ast.Alternative(lhs, rhs, (hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>) (hydra.util.Maybe.<hydra.ext.haskell.ast.LocalBindings>nothing())))));
        }));
    }));
    return hydra.lib.eithers.Bind.apply(
      hydra.Schemas.requireUnionType(
        cx,
        g,
        dn),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (rt -> {
        java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<hydra.core.Name, hydra.core.FieldType>> toFieldMapEntry = (java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<hydra.core.Name, hydra.core.FieldType>>) (f -> (hydra.util.Pair<hydra.core.Name, hydra.core.FieldType>) ((hydra.util.Pair<hydra.core.Name, hydra.core.FieldType>) (new hydra.util.Pair<hydra.core.Name, hydra.core.FieldType>((f).name, f))));
        hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.FieldType>> fieldMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          toFieldMapEntry,
          rt)));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Alternative>>) (v1 -> (toAlt).apply(fieldMap.get()).apply(v1)),
            fields),
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.haskell.ast.Alternative>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (ecases -> hydra.lib.eithers.Bind.apply(
            hydra.lib.maybes.Cases.applyLazy(
              def,
              () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.Alternative>>right((hydra.util.ConsList<hydra.ext.haskell.ast.Alternative>) (hydra.util.ConsList.<hydra.ext.haskell.ast.Alternative>empty())),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.Alternative>>>) (d -> hydra.lib.eithers.Bind.apply(
                hydra.lib.eithers.Map.apply(
                  (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.CaseRhs>) (x -> new hydra.ext.haskell.ast.CaseRhs(x)),
                  hydra.ext.haskell.Coder.encodeTerm(
                    depth,
                    namespaces,
                    d,
                    cx,
                    g)),
                (java.util.function.Function<hydra.ext.haskell.ast.CaseRhs, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.Alternative>>>) (cs -> {
                  hydra.ext.haskell.ast.Pattern lhs = new hydra.ext.haskell.ast.Pattern.Name(hydra.ext.haskell.Utils.rawName(hydra.Constants.ignoredVariable()));
                  hydra.util.Lazy<hydra.ext.haskell.ast.Alternative> alt = new hydra.util.Lazy<>(() -> new hydra.ext.haskell.ast.Alternative(lhs, cs, (hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>) (hydra.util.Maybe.<hydra.ext.haskell.ast.LocalBindings>nothing())));
                  return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.Alternative>>right(hydra.util.ConsList.of(alt.get()));
                })))),
            (java.util.function.Function<hydra.util.ConsList<hydra.ext.haskell.ast.Alternative>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (dcases -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(new hydra.ext.haskell.ast.Expression.Case(new hydra.ext.haskell.ast.CaseExpression(scrutinee, hydra.lib.lists.Concat2.apply(
              ecases,
              dcases))))))));
      }));
  }

  static <T0> hydra.util.ConsList<T0> encodeCaseExpression_noArgs() {
    return (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> encodeFunction(Integer depth, hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Function fun, hydra.context.Context cx, hydra.graph.Graph g) {
    return (fun).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Function.Elimination e) {
        return (e).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Elimination.Wrap name) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(new hydra.ext.haskell.ast.Expression.Variable(hydra.ext.haskell.Utils.elementReference(
              namespaces,
              hydra.Names.qname(
                hydra.lib.maybes.FromJust.apply(hydra.Names.namespaceOf((name).value)),
                hydra.ext.haskell.Utils.newtypeAccessorName((name).value)))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Elimination.Record proj) {
            hydra.core.Name dn = (proj).value.typeName;
            hydra.core.Name fname = (proj).value.field;
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(new hydra.ext.haskell.ast.Expression.Variable(hydra.ext.haskell.Utils.recordFieldReference(
              namespaces,
              dn,
              fname)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Elimination.Union stmt) {
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression>) (v1 -> hydra.ext.haskell.Utils.hslambda(
                hydra.ext.haskell.Utils.rawName("x"),
                v1)),
              hydra.ext.haskell.Coder.encodeCaseExpression(
                depth,
                namespaces,
                (stmt).value,
                hydra.ext.haskell.Utils.hsvar("x"),
                cx,
                g));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Function.Lambda lam) {
        hydra.core.Term body = (lam).value.body;
        hydra.core.Name v = (lam).value.parameter;
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.haskell.Coder.encodeTerm(
            depth,
            namespaces,
            body,
            cx,
            g),
          (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (hbody -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hslambda(
            hydra.ext.haskell.Utils.elementReference(
              namespaces,
              v),
            hbody))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Function.Primitive name) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(new hydra.ext.haskell.ast.Expression.Variable(hydra.ext.haskell.Utils.elementReference(
          namespaces,
          (name).value)));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> encodeLiteral(hydra.core.Literal l, hydra.context.Context cx) {
    return (l).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> otherwise(hydra.core.Literal instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          "literal value ",
          hydra.show.Core.literal(l)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Literal.Binary bs) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hsapp(
          hydra.ext.haskell.Utils.hsvar("Literals.stringToBinary"),
          hydra.ext.haskell.Utils.hslit(new hydra.ext.haskell.ast.Literal.String_(hydra.lib.literals.BinaryToString.apply((bs).value)))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Literal.Boolean_ b) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hsvar(hydra.lib.logic.IfElse.lazy(
          (b).value,
          () -> "True",
          () -> "False")));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Literal.Float_ fv) {
        return (fv).value.accept(new hydra.core.FloatValue.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.FloatValue.Float32 f) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hslit(new hydra.ext.haskell.ast.Literal.Float_((f).value)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.FloatValue.Float64 f) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hslit(new hydra.ext.haskell.ast.Literal.Double_((f).value)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.FloatValue.Bigfloat f) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hslit(new hydra.ext.haskell.ast.Literal.Double_(hydra.lib.literals.BigfloatToFloat64.apply((f).value))));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Literal.Integer_ iv) {
        return (iv).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Bigint i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hslit(new hydra.ext.haskell.ast.Literal.Integer_((i).value)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Int8 i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hslit(new hydra.ext.haskell.ast.Literal.Integer_(hydra.lib.literals.Int8ToBigint.apply((i).value))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Int16 i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hslit(new hydra.ext.haskell.ast.Literal.Integer_(hydra.lib.literals.Int16ToBigint.apply((i).value))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Int32 i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hslit(new hydra.ext.haskell.ast.Literal.Int((i).value)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Int64 i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hslit(new hydra.ext.haskell.ast.Literal.Integer_(hydra.lib.literals.Int64ToBigint.apply((i).value))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Uint8 i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hslit(new hydra.ext.haskell.ast.Literal.Integer_(hydra.lib.literals.Uint8ToBigint.apply((i).value))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Uint16 i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hslit(new hydra.ext.haskell.ast.Literal.Integer_(hydra.lib.literals.Uint16ToBigint.apply((i).value))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Uint32 i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hslit(new hydra.ext.haskell.ast.Literal.Integer_(hydra.lib.literals.Uint32ToBigint.apply((i).value))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.IntegerValue.Uint64 i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hslit(new hydra.ext.haskell.ast.Literal.Integer_(hydra.lib.literals.Uint64ToBigint.apply((i).value))));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Literal.String_ s) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hslit(new hydra.ext.haskell.ast.Literal.String_((s).value)));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> encodeTerm(Integer depth, hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Term term, hydra.context.Context cx, hydra.graph.Graph g) {
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>> encode = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (t -> hydra.ext.haskell.Coder.encodeTerm(
      depth,
      namespaces,
      t,
      cx,
      g));
    java.util.function.Function<hydra.util.PersistentMap<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>> nonemptyMap = (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (m -> {
      java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>> encodePair = (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (pair -> {
        hydra.util.Lazy<hydra.core.Term> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
        hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(k.get()),
          (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (hk -> hydra.lib.eithers.Bind.apply(
            (encode).apply(v.get()),
            (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (hv -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(new hydra.ext.haskell.ast.Expression.Tuple(hydra.util.ConsList.of(
              hk,
              hv)))))));
      });
      hydra.ext.haskell.ast.Expression lhs = hydra.ext.haskell.Utils.hsvar("M.fromList");
      return hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.haskell.ast.Expression>, hydra.ext.haskell.ast.Expression>) (x -> new hydra.ext.haskell.ast.Expression.List(x)),
          hydra.lib.eithers.MapList.apply(
            encodePair,
            hydra.lib.maps.ToList.apply(m))),
        (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (rhs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hsapp(
          lhs,
          rhs))));
    });
    java.util.function.Function<hydra.util.PersistentSet<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>> nonemptySet = (java.util.function.Function<hydra.util.PersistentSet<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (s -> {
      hydra.ext.haskell.ast.Expression lhs = hydra.ext.haskell.Utils.hsvar("S.fromList");
      return hydra.lib.eithers.Bind.apply(
        hydra.ext.haskell.Coder.encodeTerm(
          depth,
          namespaces,
          new hydra.core.Term.List(hydra.lib.sets.ToList.apply(s)),
          cx,
          g),
        (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (rhs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hsapp(
          lhs,
          rhs))));
    });
    return hydra.Rewriting.deannotateTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          "unexpected term: ",
          hydra.show.Core.term(term)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Application app) {
        hydra.core.Term arg = (app).value.argument;
        hydra.core.Term fun = (app).value.function;
        hydra.core.Term deannotatedFun = hydra.Rewriting.deannotateTerm(fun);
        return (deannotatedFun).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> otherwise(hydra.core.Term instance) {
            return hydra.lib.eithers.Bind.apply(
              (encode).apply(fun),
              (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (hfun -> hydra.lib.eithers.Bind.apply(
                (encode).apply(arg),
                (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (harg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hsapp(
                  hfun,
                  harg))))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Function f) {
            return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> otherwise(hydra.core.Function instance) {
                return hydra.lib.eithers.Bind.apply(
                  (encode).apply(fun),
                  (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (hfun -> hydra.lib.eithers.Bind.apply(
                    (encode).apply(arg),
                    (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (harg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hsapp(
                      hfun,
                      harg))))));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Function.Elimination e) {
                return (e).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> otherwise(hydra.core.Elimination instance) {
                    return hydra.lib.eithers.Bind.apply(
                      (encode).apply(fun),
                      (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (hfun -> hydra.lib.eithers.Bind.apply(
                        (encode).apply(arg),
                        (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (harg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hsapp(
                          hfun,
                          harg))))));
                  }

                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Elimination.Union stmt) {
                    return hydra.lib.eithers.Bind.apply(
                      (encode).apply(arg),
                      (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (harg -> hydra.ext.haskell.Coder.encodeCaseExpression(
                        depth,
                        namespaces,
                        (stmt).value,
                        harg,
                        cx,
                        g)));
                  }
                });
              }
            });
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (l -> hydra.lib.eithers.Bind.apply(
            (encode).apply(l),
            (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (hl -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hsapp(
              hydra.ext.haskell.Utils.hsvar("Left"),
              hl))))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (r -> hydra.lib.eithers.Bind.apply(
            (encode).apply(r),
            (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (hr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hsapp(
              hydra.ext.haskell.Utils.hsvar("Right"),
              hr))))),
          (e).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Function f) {
        return hydra.ext.haskell.Coder.encodeFunction(
          depth,
          namespaces,
          (f).value,
          cx,
          g);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Let letTerm) {
        java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Let, hydra.util.Pair<hydra.util.ConsList<hydra.core.Binding>, hydra.core.Term>>> collectBindings = new java.util.concurrent.atomic.AtomicReference<>();
        collectBindings.set((java.util.function.Function<hydra.core.Let, hydra.util.Pair<hydra.util.ConsList<hydra.core.Binding>, hydra.core.Term>>) (lt -> {
          hydra.core.Term body = (lt).body;
          hydra.util.ConsList<hydra.core.Binding> bs = (lt).bindings;
          return hydra.Rewriting.deannotateTerm(body).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public hydra.util.Pair<hydra.util.ConsList<hydra.core.Binding>, hydra.core.Term> otherwise(hydra.core.Term instance) {
              return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Binding>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Binding>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Binding>, hydra.core.Term>(bs, body)));
            }

            @Override
            public hydra.util.Pair<hydra.util.ConsList<hydra.core.Binding>, hydra.core.Term> visit(hydra.core.Term.Let innerLt) {
              hydra.util.Pair<hydra.util.ConsList<hydra.core.Binding>, hydra.core.Term> innerResult = collectBindings.get().apply((innerLt).value);
              return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Binding>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Binding>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Binding>, hydra.core.Term>(hydra.lib.lists.Concat2.apply(
                bs,
                hydra.lib.pairs.First.apply(innerResult)), hydra.lib.pairs.Second.apply(innerResult))));
            }
          });
        }));
        hydra.util.Pair<hydra.util.ConsList<hydra.core.Binding>, hydra.core.Term> collected = collectBindings.get().apply((letTerm).value);
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> allBindings = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(collected));
        java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.LocalBinding>> encodeBinding = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.LocalBinding>>) (binding -> {
          hydra.core.Name name = (binding).name;
          hydra.ext.haskell.ast.Name hname = hydra.ext.haskell.Utils.simpleName((name).value);
          hydra.core.Term term_ = (binding).term;
          return hydra.lib.eithers.Bind.apply(
            (encode).apply(term_),
            (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.LocalBinding>>) (hexpr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.LocalBinding>right(new hydra.ext.haskell.ast.LocalBinding.Value(hydra.ext.haskell.Utils.simpleValueBinding(
              hname,
              hexpr,
              (hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>) (hydra.util.Maybe.<hydra.ext.haskell.ast.LocalBindings>nothing()))))));
        });
        hydra.util.Lazy<hydra.core.Term> finalBody = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(collected));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            encodeBinding,
            allBindings.get()),
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.haskell.ast.LocalBinding>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (hbindings -> hydra.lib.eithers.Bind.apply(
            (encode).apply(finalBody.get()),
            (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (hinner -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(new hydra.ext.haskell.ast.Expression.Let(new hydra.ext.haskell.ast.LetExpression(hbindings, hinner)))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.List els) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            encode,
            (els).value),
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.haskell.ast.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (helems -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(new hydra.ext.haskell.ast.Expression.List(helems))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Literal v) {
        return hydra.ext.haskell.Coder.encodeLiteral(
          (v).value,
          cx);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Map m) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.maps.Null.apply((m).value),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hsvar("M.empty")),
          () -> (nonemptyMap).apply((m).value));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.maybes.Cases.applyLazy(
          (m).value,
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hsvar("Nothing")),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (t -> hydra.lib.eithers.Bind.apply(
            (encode).apply(t),
            (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (ht -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hsapp(
              hydra.ext.haskell.Utils.hsvar("Just"),
              ht))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Pair p) {
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(hydra.lib.pairs.First.apply((p).value)),
          (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (f -> hydra.lib.eithers.Bind.apply(
            (encode).apply(hydra.lib.pairs.Second.apply((p).value)),
            (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (s -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(new hydra.ext.haskell.ast.Expression.Tuple(hydra.util.ConsList.of(
              f,
              s)))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Record record) {
        hydra.util.ConsList<hydra.core.Field> fields = (record).value.fields;
        hydra.core.Name sname = (record).value.typeName;
        java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.FieldUpdate>> toFieldUpdate = (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.FieldUpdate>>) (field -> {
          hydra.core.Name fn = (field).name;
          hydra.ext.haskell.ast.Name fieldRef = hydra.ext.haskell.Utils.recordFieldReference(
            namespaces,
            sname,
            fn);
          hydra.core.Term ft = (field).term;
          return hydra.lib.eithers.Bind.apply(
            (encode).apply(ft),
            (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.FieldUpdate>>) (hft -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.FieldUpdate>right(new hydra.ext.haskell.ast.FieldUpdate(fieldRef, hft))));
        });
        hydra.ext.haskell.ast.Name typeName = hydra.ext.haskell.Utils.elementReference(
          namespaces,
          sname);
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            toFieldUpdate,
            fields),
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.haskell.ast.FieldUpdate>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (updates -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(new hydra.ext.haskell.ast.Expression.ConstructRecord(new hydra.ext.haskell.ast.ConstructRecordExpression(typeName, updates)))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Set s) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.sets.Null.apply((s).value),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hsvar("S.empty")),
          () -> (nonemptySet).apply((s).value));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.TypeLambda abs) {
        hydra.core.Term term1 = (abs).value.body;
        return (encode).apply(term1);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.TypeApplication typed) {
        hydra.core.Term term1 = (typed).value.body;
        return (encode).apply(term1);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Union injection) {
        hydra.core.Field field = (injection).value.field;
        hydra.core.Term ft = (field).term;
        hydra.core.Name fn = (field).name;
        hydra.core.Name sname = (injection).value.typeName;
        hydra.util.Lazy<hydra.ext.haskell.ast.Expression> lhs = new hydra.util.Lazy<>(() -> new hydra.ext.haskell.ast.Expression.Variable(hydra.ext.haskell.Utils.unionFieldReference(
          hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply((g).boundTerms)),
          namespaces,
          sname,
          fn)));
        hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>> dflt = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression>) (v1 -> hydra.ext.haskell.Utils.hsapp(
            lhs.get(),
            v1)),
          (encode).apply(ft)));
        return hydra.lib.eithers.Bind.apply(
          hydra.Schemas.requireUnionField(
            cx,
            g,
            sname,
            fn),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (ftyp -> hydra.Rewriting.deannotateType(ftyp).accept(new hydra.core.Type.PartialVisitor<>() {
            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> otherwise(hydra.core.Type instance) {
              return dflt.get();
            }

            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Type.Unit ignored) {
              return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(lhs.get());
            }
          })));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Unit ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(new hydra.ext.haskell.ast.Expression.Tuple((hydra.util.ConsList<hydra.ext.haskell.ast.Expression>) (hydra.util.ConsList.<hydra.ext.haskell.ast.Expression>empty())));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Variable name) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(new hydra.ext.haskell.ast.Expression.Variable(hydra.ext.haskell.Utils.elementReference(
          namespaces,
          (name).value)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression> visit(hydra.core.Term.Wrap wrapped) {
        hydra.core.Name tname = (wrapped).value.typeName;
        hydra.ext.haskell.ast.Expression lhs = new hydra.ext.haskell.ast.Expression.Variable(hydra.ext.haskell.Utils.elementReference(
          namespaces,
          tname));
        hydra.core.Term term_ = (wrapped).value.body;
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(term_),
          (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (rhs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>right(hydra.ext.haskell.Utils.hsapp(
            lhs,
            rhs))));
      }
    });
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> encodeType(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Type typ, hydra.context.Context cx, T0 g) {
    java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>> encode = (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (t -> hydra.ext.haskell.Coder.<T0>encodeType(
      namespaces,
      t,
      cx,
      g));
    hydra.util.Lazy<hydra.ext.haskell.ast.Type> unitTuple = new hydra.util.Lazy<>(() -> new hydra.ext.haskell.ast.Type.Tuple((hydra.util.ConsList<hydra.ext.haskell.ast.Type>) (hydra.util.ConsList.<hydra.ext.haskell.ast.Type>empty())));
    return hydra.Rewriting.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          "unexpected type: ",
          hydra.show.Core.type(typ)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Application app) {
        hydra.core.Type lhs = (app).value.function;
        hydra.core.Type rhs = (app).value.argument;
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(lhs),
          (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (hlhs -> hydra.lib.eithers.Bind.apply(
            (encode).apply(rhs),
            (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (hrhs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(hydra.ext.haskell.Utils.toTypeApplication(hydra.util.ConsList.of(
              hlhs,
              hrhs)))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Either eitherType) {
        hydra.core.Type left_ = (eitherType).value.left;
        hydra.core.Type right_ = (eitherType).value.right;
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(left_),
          (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (hleft -> hydra.lib.eithers.Bind.apply(
            (encode).apply(right_),
            (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (hright -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(hydra.ext.haskell.Utils.toTypeApplication(hydra.util.ConsList.of(
              new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.Utils.rawName("Either")),
              hleft,
              hright)))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Function funType) {
        hydra.core.Type cod = (funType).value.codomain;
        hydra.core.Type dom = (funType).value.domain;
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(dom),
          (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (hdom -> hydra.lib.eithers.Bind.apply(
            (encode).apply(cod),
            (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (hcod -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(new hydra.ext.haskell.ast.Type.Function(new hydra.ext.haskell.ast.FunctionType(hdom, hcod)))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Forall forallType) {
        hydra.core.Type body = (forallType).value.body;
        hydra.core.Name v = (forallType).value.parameter;
        return (encode).apply(body);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.List lt) {
        return hydra.lib.eithers.Bind.apply(
          (encode).apply((lt).value),
          (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (hlt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(new hydra.ext.haskell.ast.Type.List(hlt))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Literal lt) {
        return (lt).value.accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> otherwise(hydra.core.LiteralType instance) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
              "unexpected literal type: ",
              hydra.show.Core.literalType((lt).value)))), cx)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.LiteralType.Binary ignored) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.Utils.rawName("B.ByteString")));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.LiteralType.Boolean_ ignored) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.Utils.rawName("Bool")));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.LiteralType.Float_ ft) {
            return (ft).value.accept(new hydra.core.FloatType.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.FloatType.Float32 ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.Utils.rawName("Float")));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.FloatType.Float64 ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.Utils.rawName("Double")));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.FloatType.Bigfloat ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.Utils.rawName("Double")));
              }
            });
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.LiteralType.Integer_ it) {
            return (it).value.accept(new hydra.core.IntegerType.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> otherwise(hydra.core.IntegerType instance) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
                  "unexpected integer type: ",
                  hydra.show.Core.integerType((it).value)))), cx)));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.IntegerType.Bigint ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.Utils.rawName("Integer")));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.IntegerType.Int8 ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.Utils.rawName("I.Int8")));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.IntegerType.Int16 ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.Utils.rawName("I.Int16")));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.IntegerType.Int32 ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.Utils.rawName("Int")));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.IntegerType.Int64 ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.Utils.rawName("I.Int64")));
              }
            });
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.LiteralType.String_ ignored) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.Utils.rawName("String")));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Map mapType) {
        hydra.core.Type kt = (mapType).value.keys;
        hydra.core.Type vt = (mapType).value.values;
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(kt),
          (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (hkt -> hydra.lib.eithers.Bind.apply(
            (encode).apply(vt),
            (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (hvt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(hydra.ext.haskell.Utils.toTypeApplication(hydra.util.ConsList.of(
              new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.Utils.rawName("M.Map")),
              hkt,
              hvt)))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Maybe ot) {
        return hydra.lib.eithers.Bind.apply(
          (encode).apply((ot).value),
          (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (hot -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(hydra.ext.haskell.Utils.toTypeApplication(hydra.util.ConsList.of(
            new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.Utils.rawName("Maybe")),
            hot)))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Pair pt) {
        return hydra.lib.eithers.Bind.apply(
          (encode).apply((pt).value.first),
          (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (f -> hydra.lib.eithers.Bind.apply(
            (encode).apply((pt).value.second),
            (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (s -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(new hydra.ext.haskell.ast.Type.Tuple(hydra.util.ConsList.of(
              f,
              s)))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Record ignored) {
        return hydra.ext.haskell.Coder.encodeType_ref(
          (java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, java.util.function.Function<hydra.core.Name, hydra.ext.haskell.ast.Name>>) (p0 -> p1 -> hydra.ext.haskell.Utils.elementReference(
            p0,
            p1)),
          namespaces,
          new hydra.core.Name("placeholder"));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Set st) {
        return hydra.lib.eithers.Bind.apply(
          (encode).apply((st).value),
          (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (hst -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(hydra.ext.haskell.Utils.toTypeApplication(hydra.util.ConsList.of(
            new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.Utils.rawName("S.Set")),
            hst)))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Union ignored) {
        return hydra.ext.haskell.Coder.encodeType_ref(
          (java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, java.util.function.Function<hydra.core.Name, hydra.ext.haskell.ast.Name>>) (p0 -> p1 -> hydra.ext.haskell.Utils.elementReference(
            p0,
            p1)),
          namespaces,
          new hydra.core.Name("placeholder"));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Unit ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(unitTuple.get());
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Variable v1) {
        return hydra.ext.haskell.Coder.encodeType_ref(
          (java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, java.util.function.Function<hydra.core.Name, hydra.ext.haskell.ast.Name>>) (p0 -> p1 -> hydra.ext.haskell.Utils.elementReference(
            p0,
            p1)),
          namespaces,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> visit(hydra.core.Type.Wrap ignored) {
        return hydra.ext.haskell.Coder.encodeType_ref(
          (java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, java.util.function.Function<hydra.core.Name, hydra.ext.haskell.ast.Name>>) (p0 -> p1 -> hydra.ext.haskell.Utils.elementReference(
            p0,
            p1)),
          namespaces,
          new hydra.core.Name("placeholder"));
      }
    });
  }

  static <T1> hydra.util.Either<T1, hydra.ext.haskell.ast.Type> encodeType_ref(java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, java.util.function.Function<hydra.core.Name, hydra.ext.haskell.ast.Name>> hydra_ext_haskell_utils_elementReference2, hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Name name) {
    return hydra.util.Either.<T1, hydra.ext.haskell.ast.Type>right(new hydra.ext.haskell.ast.Type.Variable((hydra_ext_haskell_utils_elementReference2).apply(namespaces).apply(name)));
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type> encodeTypeWithClassAssertions(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.util.PersistentMap<hydra.core.Name, hydra.util.PersistentSet<hydra.classes.TypeClass>> explicitClasses, hydra.core.Type typ, hydra.context.Context cx, T0 g) {
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.util.PersistentSet<hydra.classes.TypeClass>>> classes = new hydra.util.Lazy<>(() -> hydra.lib.maps.Union.apply(
      explicitClasses,
      hydra.ext.haskell.Coder.getImplicitTypeClasses(typ)));
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.classes.TypeClass>>> assertPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      p0 -> hydra.ext.haskell.Coder.<hydra.core.Name, hydra.classes.TypeClass>encodeTypeWithClassAssertions_toPairs(p0),
      hydra.lib.maps.ToList.apply(classes.get()))));
    java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.classes.TypeClass>, hydra.ext.haskell.ast.Assertion> encodeAssertion = (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.classes.TypeClass>, hydra.ext.haskell.ast.Assertion>) (pair -> {
      hydra.util.Lazy<hydra.classes.TypeClass> cls = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
      hydra.ext.haskell.ast.Name hname = hydra.ext.haskell.Utils.rawName(cls.get().accept(new hydra.classes.TypeClass.PartialVisitor<>() {
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
      hydra.ext.haskell.ast.Type htype = new hydra.ext.haskell.ast.Type.Variable(hydra.ext.haskell.Utils.rawName(name.get().value));
      return new hydra.ext.haskell.ast.Assertion.Class_(new hydra.ext.haskell.ast.ClassAssertion(hname, hydra.util.ConsList.of(htype)));
    });
    hydra.util.PersistentMap<hydra.core.Name, hydra.util.PersistentSet<hydra.classes.TypeClass>> implicitClasses = hydra.ext.haskell.Coder.getImplicitTypeClasses(typ);
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.haskell.Coder.<T0>adaptTypeToHaskellAndEncode(
        namespaces,
        typ,
        cx,
        g),
      (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (htyp -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(assertPairs.get()),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(htyp),
        () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>>) (() -> {
          hydra.util.Lazy<hydra.util.ConsList<hydra.ext.haskell.ast.Assertion>> encoded = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
            encodeAssertion,
            assertPairs.get()));
          hydra.util.Lazy<hydra.ext.haskell.ast.Assertion> hassert = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              hydra.lib.lists.Length.apply(encoded.get()),
              1),
            () -> hydra.lib.lists.Head.apply(encoded.get()),
            () -> new hydra.ext.haskell.ast.Assertion.Tuple(encoded.get())));
          return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Type>right(new hydra.ext.haskell.ast.Type.Ctx(new hydra.ext.haskell.ast.ContextType(hassert.get(), htyp)));
        })).get())));
  }

  static <T1, T2> hydra.util.ConsList<hydra.util.Pair<T1, T2>> encodeTypeWithClassAssertions_toPairs(hydra.util.Pair<T1, hydra.util.PersistentSet<T2>> mapEntry) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<T2, hydra.util.Pair<T1, T2>>) (v1 -> hydra.ext.haskell.Coder.<T1, T2, T2>encodeTypeWithClassAssertions_toPair(
        mapEntry,
        v1)),
      hydra.lib.sets.ToList.apply(hydra.ext.haskell.Coder.<T1, T2>encodeTypeWithClassAssertions_clsSet(mapEntry)));
  }

  static <T1, T2> T1 encodeTypeWithClassAssertions_name(hydra.util.Pair<T1, hydra.util.PersistentSet<T2>> mapEntry) {
    return hydra.lib.pairs.First.apply(mapEntry);
  }

  static <T1, T2> hydra.util.PersistentSet<T2> encodeTypeWithClassAssertions_clsSet(hydra.util.Pair<T1, hydra.util.PersistentSet<T2>> mapEntry) {
    return hydra.lib.pairs.Second.apply(mapEntry);
  }

  static <T1, T2, T3> hydra.util.Pair<T1, T3> encodeTypeWithClassAssertions_toPair(hydra.util.Pair<T1, hydra.util.PersistentSet<T2>> mapEntry, T3 c) {
    return (hydra.util.Pair<T1, T3>) ((hydra.util.Pair<T1, T3>) (new hydra.util.Pair<T1, T3>(hydra.ext.haskell.Coder.<T1, T2>encodeTypeWithClassAssertions_name(mapEntry), c)));
  }

  static hydra.util.PersistentSet<hydra.core.Name> findOrdVariables(hydra.core.Type typ) {
    java.util.function.Function<hydra.core.Name, Boolean> isTypeVariable = (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.maybes.IsNothing.apply(hydra.Names.namespaceOf(v)));
    java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>> tryType = (java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>>) (names -> (java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>) (t -> hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.PersistentSet<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return names;
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Type.Variable v) {
        return hydra.lib.logic.IfElse.lazy(
          (isTypeVariable).apply((v).value),
          () -> hydra.lib.sets.Insert.apply(
            (v).value,
            names),
          () -> names);
      }
    })));
    java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>> fold = (java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>>) (names -> (java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>) (typ_ -> (typ_).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.PersistentSet<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return names;
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Type.Map mapType) {
        hydra.core.Type kt = (mapType).value.keys;
        return (tryType).apply(names).apply(kt);
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Type.Set et) {
        return (tryType).apply(names).apply((et).value);
      }
    })));
    return hydra.Rewriting.foldOverType(
      new hydra.coders.TraversalOrder.Pre(),
      fold,
      (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      typ);
  }

  static hydra.util.PersistentMap<hydra.core.Name, hydra.util.PersistentSet<hydra.classes.TypeClass>> getImplicitTypeClasses(hydra.core.Type typ) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      p0 -> hydra.ext.haskell.Coder.<hydra.core.Name>getImplicitTypeClasses_toPair(p0),
      hydra.lib.sets.ToList.apply(hydra.ext.haskell.Coder.findOrdVariables(typ))));
  }

  static <T0> hydra.util.Pair<T0, hydra.util.PersistentSet<hydra.classes.TypeClass>> getImplicitTypeClasses_toPair(T0 name) {
    return (hydra.util.Pair<T0, hydra.util.PersistentSet<hydra.classes.TypeClass>>) ((hydra.util.Pair<T0, hydra.util.PersistentSet<hydra.classes.TypeClass>>) (new hydra.util.Pair<T0, hydra.util.PersistentSet<hydra.classes.TypeClass>>(name, hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(new hydra.classes.TypeClass.Ordering())))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Module> moduleToHaskellModule(hydra.module.Module mod, hydra.util.ConsList<hydra.module.Definition> defs, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.haskell.Utils.namespacesForModule(
        mod,
        cx,
        g),
      (java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Module>>) (namespaces -> hydra.ext.haskell.Coder.constructModule(
        namespaces,
        mod,
        defs,
        cx,
        g)));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<String, String>> moduleToHaskell(hydra.module.Module mod, hydra.util.ConsList<hydra.module.Definition> defs, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.haskell.Coder.moduleToHaskellModule(
        mod,
        defs,
        cx,
        g),
      (java.util.function.Function<hydra.ext.haskell.ast.Module, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<String, String>>>) (hsmod -> {
        String filepath = hydra.Names.namespaceToFilePath(
          new hydra.util.CaseConvention.Pascal(),
          new hydra.module.FileExtension("hs"),
          (mod).namespace);
        String s = hydra.Serialization.printExpr(hydra.Serialization.parenthesize(hydra.ext.haskell.Serde.moduleToExpr(hsmod)));
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<String, String>>right(hydra.lib.maps.Singleton.apply(
          filepath,
          s));
      }));
  }

  static hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments> nameDecls(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Name name, hydra.core.Type typ) {
    java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<String, String>> toConstant = (java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<String, String>>) (fieldType -> {
      hydra.core.Name fname = (fieldType).name;
      return (hydra.util.Pair<String, String>) ((hydra.util.Pair<String, String>) (new hydra.util.Pair<String, String>(hydra.ext.haskell.Coder.constantForFieldName(
        name,
        fname), (fname).value)));
    });
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<String, String>>> fieldDecls = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      toConstant,
      hydra.Lexical.fieldsOf(typ)));
    String nm = (name).value;
    hydra.util.Lazy<hydra.util.Pair<String, String>> nameDecl = new hydra.util.Lazy<>(() -> (hydra.util.Pair<String, String>) ((hydra.util.Pair<String, String>) (new hydra.util.Pair<String, String>(hydra.ext.haskell.Coder.constantForTypeName(name), nm))));
    java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.util.Pair<String, String>, hydra.ext.haskell.ast.DeclarationWithComments>> toDecl = (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.util.Pair<String, String>, hydra.ext.haskell.ast.DeclarationWithComments>>) (n -> (java.util.function.Function<hydra.util.Pair<String, String>, hydra.ext.haskell.ast.DeclarationWithComments>) (pair -> {
      hydra.util.Lazy<String> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
      hydra.util.Lazy<String> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
      hydra.util.Lazy<hydra.ext.haskell.ast.Declaration> decl = new hydra.util.Lazy<>(() -> new hydra.ext.haskell.ast.Declaration.ValueBinding(new hydra.ext.haskell.ast.ValueBinding.Simple(new hydra.ext.haskell.ast.SimpleValueBinding(hydra.ext.haskell.Utils.applicationPattern(
        hydra.ext.haskell.Utils.simpleName(k.get()),
        (hydra.util.ConsList<hydra.ext.haskell.ast.Pattern>) (hydra.util.ConsList.<hydra.ext.haskell.ast.Pattern>empty())), new hydra.ext.haskell.ast.RightHandSide(new hydra.ext.haskell.ast.Expression.Application(new hydra.ext.haskell.ast.ApplicationExpression(new hydra.ext.haskell.ast.Expression.Variable(hydra.ext.haskell.Utils.elementReference(
        namespaces,
        n)), new hydra.ext.haskell.ast.Expression.Literal(new hydra.ext.haskell.ast.Literal.String_(v.get()))))), (hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>) (hydra.util.Maybe.<hydra.ext.haskell.ast.LocalBindings>nothing())))));
      return new hydra.ext.haskell.ast.DeclarationWithComments(decl.get(), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()));
    }));
    return hydra.lib.logic.IfElse.lazy(
      hydra.ext.haskell.Coder.useCoreImport(),
      () -> hydra.lib.lists.Cons.apply(
        (toDecl).apply(new hydra.core.Name("hydra.core.Name")).apply(nameDecl.get()),
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<String, String>, hydra.ext.haskell.ast.DeclarationWithComments>) (v1 -> (toDecl).apply(new hydra.core.Name("hydra.core.Name")).apply(v1)),
          fieldDecls.get())),
      () -> (hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>) (hydra.util.ConsList.<hydra.ext.haskell.ast.DeclarationWithComments>empty()));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments> toDataDeclaration(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.module.TermDefinition def, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.core.Name name = (def).name;
    hydra.ext.haskell.ast.Name hname = hydra.ext.haskell.Utils.simpleName(hydra.Names.localNameOf(name));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.ext.haskell.ast.ValueBinding, hydra.ext.haskell.ast.ValueBinding>> rewriteValueBinding = new java.util.concurrent.atomic.AtomicReference<>();
    rewriteValueBinding.set((java.util.function.Function<hydra.ext.haskell.ast.ValueBinding, hydra.ext.haskell.ast.ValueBinding>) (vb -> (vb).accept(new hydra.ext.haskell.ast.ValueBinding.PartialVisitor<>() {
      @Override
      public hydra.ext.haskell.ast.ValueBinding visit(hydra.ext.haskell.ast.ValueBinding.Simple simple) {
        hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings> bindings = (simple).value.localBindings;
        hydra.ext.haskell.ast.Pattern pattern_ = (simple).value.pattern;
        hydra.ext.haskell.ast.RightHandSide rhs = (simple).value.rhs;
        return (pattern_).accept(new hydra.ext.haskell.ast.Pattern.PartialVisitor<>() {
          @Override
          public hydra.ext.haskell.ast.ValueBinding otherwise(hydra.ext.haskell.ast.Pattern instance) {
            return vb;
          }

          @Override
          public hydra.ext.haskell.ast.ValueBinding visit(hydra.ext.haskell.ast.Pattern.Application appPat) {
            hydra.util.ConsList<hydra.ext.haskell.ast.Pattern> args = (appPat).value.args;
            hydra.ext.haskell.ast.Name name_ = (appPat).value.name;
            hydra.ext.haskell.ast.Expression rhsExpr = (rhs).value;
            return (rhsExpr).accept(new hydra.ext.haskell.ast.Expression.PartialVisitor<>() {
              @Override
              public hydra.ext.haskell.ast.ValueBinding otherwise(hydra.ext.haskell.ast.Expression instance) {
                return vb;
              }

              @Override
              public hydra.ext.haskell.ast.ValueBinding visit(hydra.ext.haskell.ast.Expression.Lambda lambda_) {
                hydra.ext.haskell.ast.Expression body = (lambda_).value.inner;
                hydra.util.ConsList<hydra.ext.haskell.ast.Pattern> vars = (lambda_).value.bindings;
                hydra.util.Lazy<hydra.ext.haskell.ast.Pattern> newPattern = new hydra.util.Lazy<>(() -> hydra.ext.haskell.Utils.applicationPattern(
                  name_,
                  hydra.lib.lists.Concat2.apply(
                    args,
                    vars)));
                hydra.ext.haskell.ast.RightHandSide newRhs = new hydra.ext.haskell.ast.RightHandSide(body);
                return rewriteValueBinding.get().apply(new hydra.ext.haskell.ast.ValueBinding.Simple(new hydra.ext.haskell.ast.SimpleValueBinding(newPattern.get(), newRhs, bindings)));
              }
            });
          }
        });
      }
    })));
    hydra.core.Term term = (def).term;
    hydra.core.TypeScheme typ = (def).type;
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.Maybe<String>, java.util.function.Function<hydra.ext.haskell.ast.Name, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments>>>>>> toDecl = new java.util.concurrent.atomic.AtomicReference<>();
    toDecl.set((java.util.function.Function<hydra.util.Maybe<String>, java.util.function.Function<hydra.ext.haskell.ast.Name, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments>>>>>) (comments -> (java.util.function.Function<hydra.ext.haskell.ast.Name, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments>>>>) (hname_ -> (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments>>>) (term_ -> (java.util.function.Function<hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments>>) (bindings -> hydra.Rewriting.deannotateTerm(term_).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments> otherwise(hydra.core.Term instance) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.haskell.Coder.encodeTerm(
            0,
            namespaces,
            term_,
            cx,
            g),
          (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments>>) (hterm -> {
            hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>> schemeConstraints = (typ).constraints;
            hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.util.PersistentSet<hydra.classes.TypeClass>>> schemeClasses = new hydra.util.Lazy<>(() -> hydra.ext.haskell.Coder.typeSchemeConstraintsToClassMap(schemeConstraints));
            hydra.ext.haskell.ast.ValueBinding vb = hydra.ext.haskell.Utils.simpleValueBinding(
              hname_,
              hterm,
              bindings);
            return hydra.lib.eithers.Bind.apply(
              hydra.Annotations.getTypeClasses(
                cx,
                g,
                hydra.Rewriting.removeTypesFromTerm(term)),
              (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.util.PersistentSet<hydra.classes.TypeClass>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments>>) (explicitClasses -> {
                hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.util.PersistentSet<hydra.classes.TypeClass>>> combinedClasses = new hydra.util.Lazy<>(() -> hydra.lib.maps.Union.apply(
                  schemeClasses.get(),
                  explicitClasses));
                return hydra.lib.eithers.Bind.apply(
                  hydra.ext.haskell.Coder.encodeTypeWithClassAssertions(
                    namespaces,
                    combinedClasses.get(),
                    (typ).type,
                    cx,
                    g),
                  (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments>>) (htype -> {
                    hydra.ext.haskell.ast.Declaration decl = new hydra.ext.haskell.ast.Declaration.TypedBinding(new hydra.ext.haskell.ast.TypedBinding(new hydra.ext.haskell.ast.TypeSignature(hname_, htype), rewriteValueBinding.get().apply(vb)));
                    return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments>right(new hydra.ext.haskell.ast.DeclarationWithComments(decl, comments));
                  }));
              }));
          }));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments> visit(hydra.core.Term.Let letTerm) {
        hydra.core.Term env = (letTerm).value.body;
        hydra.util.ConsList<hydra.core.Binding> lbindings = (letTerm).value.bindings;
        hydra.util.Lazy<hydra.util.ConsList<hydra.ext.haskell.ast.Name>> hnames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.ext.haskell.ast.Name>) (binding -> hydra.ext.haskell.Utils.simpleName((binding).name.value)),
          lbindings));
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> terms = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          projected -> projected.term,
          lbindings));
        java.util.function.Function<hydra.ext.haskell.ast.Name, java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.LocalBinding>> toBinding = (java.util.function.Function<hydra.ext.haskell.ast.Name, java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.LocalBinding>>) (hname_ -> (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.LocalBinding>) (hterm_ -> new hydra.ext.haskell.ast.LocalBinding.Value(hydra.ext.haskell.Utils.simpleValueBinding(
          hname_,
          hterm_,
          (hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>) (hydra.util.Maybe.<hydra.ext.haskell.ast.LocalBindings>nothing())))));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Expression>>) (t -> hydra.ext.haskell.Coder.encodeTerm(
              0,
              namespaces,
              t,
              cx,
              g)),
            terms.get()),
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.haskell.ast.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments>>) (hterms -> {
            hydra.util.Lazy<hydra.util.ConsList<hydra.ext.haskell.ast.LocalBinding>> hbindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.ZipWith.apply(
              toBinding,
              hnames.get(),
              hterms));
            hydra.util.Lazy<hydra.util.ConsList<hydra.ext.haskell.ast.LocalBinding>> prevBindings = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
              () -> (hydra.util.ConsList<hydra.ext.haskell.ast.LocalBinding>) (hydra.util.ConsList.<hydra.ext.haskell.ast.LocalBinding>empty()),
              (java.util.function.Function<hydra.ext.haskell.ast.LocalBindings, hydra.util.ConsList<hydra.ext.haskell.ast.LocalBinding>>) (lb -> (lb).value),
              bindings));
            hydra.util.Lazy<hydra.util.ConsList<hydra.ext.haskell.ast.LocalBinding>> allBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
              prevBindings.get(),
              hbindings.get()));
            return toDecl.get().apply(comments).apply(hname_).apply(env).apply(hydra.util.Maybe.just(new hydra.ext.haskell.ast.LocalBindings(allBindings.get())));
          }));
      }
    }))))));
    return hydra.lib.eithers.Bind.apply(
      hydra.Annotations.getTermDescription(
        cx,
        g,
        term),
      (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments>>) (comments -> toDecl.get().apply(comments).apply(hname).apply(term).apply((hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>) (hydra.util.Maybe.<hydra.ext.haskell.ast.LocalBindings>nothing()))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>> toTypeDeclarationsFrom(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Name elementName, hydra.core.Type typ, hydra.context.Context cx, hydra.graph.Graph g) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.ext.haskell.ast.Name, java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.ext.haskell.ast.DeclarationHead>>> declHead = new java.util.concurrent.atomic.AtomicReference<>();
    declHead.set((java.util.function.Function<hydra.ext.haskell.ast.Name, java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.ext.haskell.ast.DeclarationHead>>) (name -> (java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.ext.haskell.ast.DeclarationHead>) (vars_ -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(vars_),
      () -> new hydra.ext.haskell.ast.DeclarationHead.Simple(name),
      () -> ((java.util.function.Supplier<hydra.ext.haskell.ast.DeclarationHead>) (() -> {
        hydra.util.Lazy<hydra.core.Name> h = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(vars_));
        hydra.ext.haskell.ast.Variable hvar = new hydra.ext.haskell.ast.Variable(hydra.ext.haskell.Utils.simpleName(h.get().value));
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> rest = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(vars_));
        return new hydra.ext.haskell.ast.DeclarationHead.Application(new hydra.ext.haskell.ast.ApplicationDeclarationHead(declHead.get().apply(name).apply(rest.get()), hvar));
      })).get()))));
    String lname = hydra.Names.localNameOf(elementName);
    hydra.ext.haskell.ast.Name hname = hydra.ext.haskell.Utils.simpleName(lname);
    java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>>> newtypeCons = (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>>>) (tname -> (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>>) (typ_ -> {
      hydra.ext.haskell.ast.Name hname0 = hydra.ext.haskell.Utils.simpleName(hydra.ext.haskell.Utils.newtypeAccessorName(tname));
      return hydra.lib.eithers.Bind.apply(
        hydra.ext.haskell.Coder.adaptTypeToHaskellAndEncode(
          namespaces,
          typ_,
          cx,
          g),
        (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>>) (htype -> {
          hydra.ext.haskell.ast.Name constructorName = hydra.ext.haskell.Utils.simpleName(hydra.Names.localNameOf(tname));
          hydra.util.Lazy<hydra.ext.haskell.ast.FieldWithComments> hfield = new hydra.util.Lazy<>(() -> new hydra.ext.haskell.ast.FieldWithComments(new hydra.ext.haskell.ast.Field(hname0, htype), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())));
          return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>right(new hydra.ext.haskell.ast.ConstructorWithComments(new hydra.ext.haskell.ast.Constructor.Record(new hydra.ext.haskell.ast.RecordConstructor(constructorName, hydra.util.ConsList.of(hfield.get()))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())));
        }));
    }));
    java.util.function.Function<String, java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>>> recordCons = (java.util.function.Function<String, java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>>>) (lname_ -> (java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>>) (fields -> {
      java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.FieldWithComments>> toField = (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.FieldWithComments>>) (fieldType -> {
        hydra.core.Name fname = (fieldType).name;
        hydra.core.Type ftype = (fieldType).type;
        hydra.ext.haskell.ast.Name hname_ = hydra.ext.haskell.Utils.simpleName(hydra.lib.strings.Cat2.apply(
          hydra.Formatting.decapitalize(lname_),
          hydra.Formatting.capitalize((fname).value)));
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.haskell.Coder.adaptTypeToHaskellAndEncode(
            namespaces,
            ftype,
            cx,
            g),
          (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.FieldWithComments>>) (htype -> hydra.lib.eithers.Bind.apply(
            hydra.Annotations.getTypeDescription(
              cx,
              g,
              ftype),
            (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.FieldWithComments>>) (comments -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.FieldWithComments>right(new hydra.ext.haskell.ast.FieldWithComments(new hydra.ext.haskell.ast.Field(hname_, htype), comments))))));
      });
      return hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          toField,
          fields),
        (java.util.function.Function<hydra.util.ConsList<hydra.ext.haskell.ast.FieldWithComments>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>>) (hFields -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>right(new hydra.ext.haskell.ast.ConstructorWithComments(new hydra.ext.haskell.ast.Constructor.Record(new hydra.ext.haskell.ast.RecordConstructor(hydra.ext.haskell.Utils.simpleName(lname_), hFields)), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())))));
    }));
    java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<String, java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>>>> unionCons = (java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<String, java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>>>>) (boundNames_ -> (java.util.function.Function<String, java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>>>) (lname_ -> (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>>) (fieldType -> {
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<String, String>> deconflict = new java.util.concurrent.atomic.AtomicReference<>();
      deconflict.set((java.util.function.Function<String, String>) (name -> {
        hydra.util.Lazy<hydra.core.Name> tname = new hydra.util.Lazy<>(() -> hydra.Names.unqualifyName(new hydra.module.QualifiedName(hydra.util.Maybe.just(hydra.lib.pairs.First.apply(((java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, hydra.util.Pair<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>>) (projected -> projected.focus)).apply(namespaces))), name)));
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
        (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>>) (comments -> {
          String nm = deconflict.get().apply(hydra.lib.strings.Cat2.apply(
            hydra.Formatting.capitalize(lname_),
            hydra.Formatting.capitalize((fname).value)));
          return hydra.lib.eithers.Bind.apply(
            hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                hydra.Rewriting.deannotateType(ftype),
                new hydra.core.Type.Unit()),
              () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.Type>>right((hydra.util.ConsList<hydra.ext.haskell.ast.Type>) (hydra.util.ConsList.<hydra.ext.haskell.ast.Type>empty())),
              () -> hydra.lib.eithers.Bind.apply(
                hydra.ext.haskell.Coder.adaptTypeToHaskellAndEncode(
                  namespaces,
                  ftype,
                  cx,
                  g),
                (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.Type>>>) (htype -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.Type>>right(hydra.util.ConsList.of(htype))))),
            (java.util.function.Function<hydra.util.ConsList<hydra.ext.haskell.ast.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>>) (typeList -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>right(new hydra.ext.haskell.ast.ConstructorWithComments(new hydra.ext.haskell.ast.Constructor.Ordinary(new hydra.ext.haskell.ast.OrdinaryConstructor(hydra.ext.haskell.Utils.simpleName(nm), typeList)), comments))));
        }));
    })));
    return hydra.lib.eithers.Bind.apply(
      hydra.Schemas.isSerializableByName(
        cx,
        g,
        elementName),
      (java.util.function.Function<Boolean, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>>>) (isSer -> {
        hydra.util.Lazy<hydra.ext.haskell.ast.Deriving> deriv = new hydra.util.Lazy<>(() -> new hydra.ext.haskell.ast.Deriving(hydra.lib.logic.IfElse.lazy(
          isSer,
          () -> hydra.lib.lists.Map.apply(
            hydra.ext.haskell.Utils::rawName,
            hydra.util.ConsList.of(
              "Eq",
              "Ord",
              "Read",
              "Show")),
          () -> (hydra.util.ConsList<hydra.ext.haskell.ast.Name>) (hydra.util.ConsList.<hydra.ext.haskell.ast.Name>empty()))));
        hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type> unpackResult = hydra.ext.haskell.Utils.unpackForallType(typ);
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> vars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(unpackResult));
        hydra.util.Lazy<hydra.ext.haskell.ast.DeclarationHead> hd = new hydra.util.Lazy<>(() -> declHead.get().apply(hname).apply(hydra.lib.lists.Reverse.apply(vars.get())));
        hydra.util.Lazy<hydra.core.Type> t_ = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(unpackResult));
        return hydra.lib.eithers.Bind.apply(
          hydra.Rewriting.deannotateType(t_.get()).accept(new hydra.core.Type.PartialVisitor<>() {
            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Declaration> otherwise(hydra.core.Type instance) {
              return hydra.lib.eithers.Bind.apply(
                hydra.ext.haskell.Coder.adaptTypeToHaskellAndEncode(
                  namespaces,
                  typ,
                  cx,
                  g),
                (java.util.function.Function<hydra.ext.haskell.ast.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Declaration>>) (htype -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Declaration>right(new hydra.ext.haskell.ast.Declaration.Type(new hydra.ext.haskell.ast.TypeDeclaration(hd.get(), htype)))));
            }

            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Declaration> visit(hydra.core.Type.Record rt) {
              return hydra.lib.eithers.Bind.apply(
                (recordCons).apply(lname).apply((rt).value),
                (java.util.function.Function<hydra.ext.haskell.ast.ConstructorWithComments, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Declaration>>) (cons -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Declaration>right(new hydra.ext.haskell.ast.Declaration.Data(new hydra.ext.haskell.ast.DataDeclaration(new hydra.ext.haskell.ast.DataOrNewtype.Data(), (hydra.util.ConsList<hydra.ext.haskell.ast.Assertion>) (hydra.util.ConsList.<hydra.ext.haskell.ast.Assertion>empty()), hd.get(), hydra.util.ConsList.of(cons), hydra.util.ConsList.of(deriv.get()))))));
            }

            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Declaration> visit(hydra.core.Type.Union rt) {
              return hydra.lib.eithers.Bind.apply(
                hydra.lib.eithers.MapList.apply(
                  (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.ConstructorWithComments>>) (v1 -> (unionCons).apply(hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply((g).boundTerms))).apply(lname).apply(v1)),
                  (rt).value),
                (java.util.function.Function<hydra.util.ConsList<hydra.ext.haskell.ast.ConstructorWithComments>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Declaration>>) (cons -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Declaration>right(new hydra.ext.haskell.ast.Declaration.Data(new hydra.ext.haskell.ast.DataDeclaration(new hydra.ext.haskell.ast.DataOrNewtype.Data(), (hydra.util.ConsList<hydra.ext.haskell.ast.Assertion>) (hydra.util.ConsList.<hydra.ext.haskell.ast.Assertion>empty()), hd.get(), cons, hydra.util.ConsList.of(deriv.get()))))));
            }

            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Declaration> visit(hydra.core.Type.Wrap wrapped) {
              return hydra.lib.eithers.Bind.apply(
                (newtypeCons).apply(elementName).apply((wrapped).value),
                (java.util.function.Function<hydra.ext.haskell.ast.ConstructorWithComments, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Declaration>>) (cons -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.Declaration>right(new hydra.ext.haskell.ast.Declaration.Data(new hydra.ext.haskell.ast.DataDeclaration(new hydra.ext.haskell.ast.DataOrNewtype.Newtype(), (hydra.util.ConsList<hydra.ext.haskell.ast.Assertion>) (hydra.util.ConsList.<hydra.ext.haskell.ast.Assertion>empty()), hd.get(), hydra.util.ConsList.of(cons), hydra.util.ConsList.of(deriv.get()))))));
            }
          }),
          (java.util.function.Function<hydra.ext.haskell.ast.Declaration, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>>>) (decl -> hydra.lib.eithers.Bind.apply(
            hydra.Annotations.getTypeDescription(
              cx,
              g,
              typ),
            (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>>>) (comments -> hydra.lib.eithers.Bind.apply(
              hydra.lib.logic.IfElse.lazy(
                hydra.ext.haskell.Coder.includeTypeDefinitions(),
                () -> hydra.lib.eithers.Bind.apply(
                  hydra.ext.haskell.Coder.typeDecl(
                    namespaces,
                    elementName,
                    typ,
                    cx,
                    g),
                  (java.util.function.Function<hydra.ext.haskell.ast.DeclarationWithComments, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>>>) (decl_ -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>>right(hydra.util.ConsList.of(decl_)))),
                () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>>right((hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>) (hydra.util.ConsList.<hydra.ext.haskell.ast.DeclarationWithComments>empty()))),
              (java.util.function.Function<hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>>>) (tdecls -> {
                hydra.ext.haskell.ast.DeclarationWithComments mainDecl = new hydra.ext.haskell.ast.DeclarationWithComments(decl, comments);
                hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments> nameDecls_ = hydra.ext.haskell.Coder.nameDecls(
                  namespaces,
                  elementName,
                  typ);
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments>>right(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
                  hydra.util.ConsList.of(mainDecl),
                  nameDecls_,
                  tdecls)));
              }))))));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments> typeDecl(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Name name, hydra.core.Type typ, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.core.Term rawTerm = hydra.encode.Core.type(typ);
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> rewrite = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> {
      java.util.function.Function<hydra.core.Term, hydra.util.Maybe<String>> decodeString = (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<String>>) (term2 -> hydra.Rewriting.deannotateTerm(term2).accept(new hydra.core.Term.PartialVisitor<>() {
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
      java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Name>> decodeName = (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Name>>) (term2 -> hydra.Rewriting.deannotateTerm(term2).accept(new hydra.core.Term.PartialVisitor<>() {
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
        hydra.module.QualifiedName qname = hydra.Names.qualifyName(vname);
        String local = (qname).local;
        hydra.util.Maybe<hydra.module.Namespace> mns = (qname).namespace;
        return hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.module.Namespace, hydra.core.Term>) (ns -> new hydra.core.Term.Variable(hydra.Names.qname(
            ns,
            hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
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
      hydra.util.Lazy<hydra.util.Maybe<hydra.core.Field>> variantResult = new hydra.util.Lazy<>(() -> hydra.Rewriting.deannotateTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<hydra.core.Field> otherwise(hydra.core.Term instance) {
          return (hydra.util.Maybe<hydra.core.Field>) (hydra.util.Maybe.<hydra.core.Field>nothing());
        }

        @Override
        public hydra.util.Maybe<hydra.core.Field> visit(hydra.core.Term.Union inj) {
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
    java.util.function.Function<hydra.core.Name, String> typeNameLocal = (java.util.function.Function<hydra.core.Name, String>) (name_ -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "_",
      hydra.Names.localNameOf(name_),
      "_type_")));
    java.util.function.Function<hydra.module.Namespace, java.util.function.Function<hydra.core.Name, hydra.core.Name>> typeName = (java.util.function.Function<hydra.module.Namespace, java.util.function.Function<hydra.core.Name, hydra.core.Name>>) (ns -> (java.util.function.Function<hydra.core.Name, hydra.core.Name>) (name_ -> hydra.Names.qname(
      ns,
      (typeNameLocal).apply(name_))));
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.haskell.Coder.encodeTerm(
        0,
        namespaces,
        finalTerm,
        cx,
        g),
      (java.util.function.Function<hydra.ext.haskell.ast.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments>>) (expr -> {
        hydra.ext.haskell.ast.Name hname = hydra.ext.haskell.Utils.simpleName((typeNameLocal).apply(name));
        hydra.util.Lazy<hydra.ext.haskell.ast.Pattern> pat = new hydra.util.Lazy<>(() -> hydra.ext.haskell.Utils.applicationPattern(
          hname,
          (hydra.util.ConsList<hydra.ext.haskell.ast.Pattern>) (hydra.util.ConsList.<hydra.ext.haskell.ast.Pattern>empty())));
        hydra.ext.haskell.ast.RightHandSide rhs = new hydra.ext.haskell.ast.RightHandSide(expr);
        hydra.util.Lazy<hydra.ext.haskell.ast.Declaration> decl = new hydra.util.Lazy<>(() -> new hydra.ext.haskell.ast.Declaration.ValueBinding(new hydra.ext.haskell.ast.ValueBinding.Simple(new hydra.ext.haskell.ast.SimpleValueBinding(pat.get(), rhs, (hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings>) (hydra.util.Maybe.<hydra.ext.haskell.ast.LocalBindings>nothing())))));
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.haskell.ast.DeclarationWithComments>right(new hydra.ext.haskell.ast.DeclarationWithComments(decl.get(), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())));
      }));
  }

  static <T0> hydra.util.PersistentMap<T0, hydra.util.PersistentSet<hydra.classes.TypeClass>> typeSchemeConstraintsToClassMap(hydra.util.Maybe<hydra.util.PersistentMap<T0, hydra.core.TypeVariableMetadata>> maybeConstraints) {
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
      () -> (hydra.util.PersistentMap<T0, hydra.util.PersistentSet<hydra.classes.TypeClass>>) ((hydra.util.PersistentMap<T0, hydra.util.PersistentSet<hydra.classes.TypeClass>>) (hydra.lib.maps.Empty.<T0, hydra.util.PersistentSet<hydra.classes.TypeClass>>apply())),
      (java.util.function.Function<hydra.util.PersistentMap<T0, hydra.core.TypeVariableMetadata>, hydra.util.PersistentMap<T0, hydra.util.PersistentSet<hydra.classes.TypeClass>>>) (constraints -> hydra.lib.maps.Map.apply(
        (java.util.function.Function<hydra.core.TypeVariableMetadata, hydra.util.PersistentSet<hydra.classes.TypeClass>>) (meta -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
          nameToTypeClass,
          hydra.lib.sets.ToList.apply((meta).classes))))),
        constraints)),
      maybeConstraints);
  }
}
