// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala;

/**
 * Scala code generator: converts Hydra modules to Scala source code
 */
public interface Coder {
  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<String, String>> moduleToScala(hydra.module.Module mod, hydra.util.ConsList<hydra.module.Definition> defs, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.scala.Coder.constructModule(
        cx,
        g,
        mod,
        defs),
      (java.util.function.Function<hydra.ext.scala.meta.Pkg, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<String, String>>>) (pkg -> {
        String s = hydra.Serialization.printExpr(hydra.Serialization.parenthesize(hydra.ext.scala.Serde.writePkg(pkg)));
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<String, String>>right(hydra.lib.maps.Singleton.apply(
          hydra.Names.namespaceToFilePath(
            new hydra.util.CaseConvention.Camel(),
            new hydra.module.FileExtension("scala"),
            (mod).namespace),
          s));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Pkg> constructModule(hydra.context.Context cx, hydra.graph.Graph g, hydra.module.Module mod, hydra.util.ConsList<hydra.module.Definition> defs) {
    String nsName = (mod).namespace.value;
    hydra.util.Pair<hydra.util.ConsList<hydra.module.TypeDefinition>, hydra.util.ConsList<hydra.module.TermDefinition>> partitioned = hydra.Schemas.partitionDefinitions(defs);
    hydra.ext.scala.meta.Data_Name pname = new hydra.ext.scala.meta.Data_Name(new hydra.ext.scala.meta.PredefString(hydra.lib.strings.Intercalate.apply(
      ".",
      hydra.lib.strings.SplitOn.apply(
        ".",
        nsName))));
    hydra.ext.scala.meta.Data_Ref pref = new hydra.ext.scala.meta.Data_Ref.Name(pname);
    hydra.util.Lazy<hydra.util.ConsList<hydra.module.TermDefinition>> termDefs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(partitioned));
    hydra.util.Lazy<hydra.util.ConsList<hydra.module.TypeDefinition>> typeDefs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(partitioned));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.module.TypeDefinition, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (td -> hydra.ext.scala.Coder.encodeTypeDefinition(
          cx,
          g,
          td)),
        typeDefs.get()),
      (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Stat>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Pkg>>) (typeDeclStats -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.module.TermDefinition, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (td -> hydra.ext.scala.Coder.encodeTermDefinition(
            cx,
            g,
            td)),
          termDefs.get()),
        (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Stat>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Pkg>>) (termDeclStats -> hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.findImports(
            cx,
            g,
            mod),
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Stat>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Pkg>>) (imports -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Pkg>right(new hydra.ext.scala.meta.Pkg(pname, pref, hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
            imports,
            typeDeclStats,
            termDeclStats))))))))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.scala.meta.Stat>> findImports(hydra.context.Context cx, hydra.graph.Graph g, hydra.module.Module mod) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Schemas.moduleDependencyNamespaces(
        cx,
        g,
        false,
        false,
        true,
        false,
        mod),
      (java.util.function.Function<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.scala.meta.Stat>>>) (elImps -> hydra.lib.eithers.Bind.apply(
        hydra.Schemas.moduleDependencyNamespaces(
          cx,
          g,
          false,
          true,
          false,
          false,
          mod),
        (java.util.function.Function<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.scala.meta.Stat>>>) (primImps -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.scala.meta.Stat>>right(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.lib.lists.Map.apply(
            hydra.ext.scala.Coder::toElImport,
            hydra.lib.sets.ToList.apply(elImps)),
          hydra.lib.lists.Map.apply(
            hydra.ext.scala.Coder::toPrimImport,
            hydra.lib.sets.ToList.apply(primImps)))))))));
  }

  static hydra.ext.scala.meta.Stat toElImport(hydra.module.Namespace ns) {
    return new hydra.ext.scala.meta.Stat.ImportExport(new hydra.ext.scala.meta.ImportExportStat.Import(new hydra.ext.scala.meta.Import(hydra.util.ConsList.of(new hydra.ext.scala.meta.Importer(new hydra.ext.scala.meta.Data_Ref.Name(new hydra.ext.scala.meta.Data_Name(new hydra.ext.scala.meta.PredefString(hydra.lib.strings.Intercalate.apply(
      ".",
      hydra.lib.strings.SplitOn.apply(
        ".",
        (ns).value))))), hydra.util.ConsList.of(new hydra.ext.scala.meta.Importee.Wildcard()))))));
  }

  static hydra.ext.scala.meta.Stat toPrimImport(hydra.module.Namespace ns) {
    return new hydra.ext.scala.meta.Stat.ImportExport(new hydra.ext.scala.meta.ImportExportStat.Import(new hydra.ext.scala.meta.Import(hydra.util.ConsList.of(new hydra.ext.scala.meta.Importer(new hydra.ext.scala.meta.Data_Ref.Name(new hydra.ext.scala.meta.Data_Name(new hydra.ext.scala.meta.PredefString(hydra.lib.strings.Intercalate.apply(
      ".",
      hydra.lib.strings.SplitOn.apply(
        ".",
        (ns).value))))), (hydra.util.ConsList<hydra.ext.scala.meta.Importee>) (hydra.util.ConsList.<hydra.ext.scala.meta.Importee>empty()))))));
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat> encodeTypeDefinition(hydra.context.Context cx, T0 g, hydra.module.TypeDefinition td) {
    hydra.core.Name name = (td).name;
    String lname = hydra.Names.localNameOf(name);
    hydra.ext.scala.meta.Data_Name dname = new hydra.ext.scala.meta.Data_Name(new hydra.ext.scala.meta.PredefString(lname));
    hydra.core.Type typ = (td).type;
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> freeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.logic.Not.apply(hydra.lib.lists.Elem.apply(
        46,
        hydra.lib.strings.ToList.apply((v).value)))),
      hydra.lib.sets.ToList.apply(hydra.Rewriting.freeVariablesInType(typ))));
    hydra.ext.scala.meta.Type_Name tname = new hydra.ext.scala.meta.Type_Name(lname);
    hydra.util.Lazy<hydra.util.ConsList<hydra.ext.scala.meta.Type_Param>> tparams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.ext.scala.meta.Type_Param>) (_v -> {
        String vn = hydra.Formatting.capitalize((_v).value);
        return new hydra.ext.scala.meta.Type_Param((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), new hydra.ext.scala.meta.Name.Value(vn), (hydra.util.ConsList<hydra.ext.scala.meta.Type_Param>) (hydra.util.ConsList.<hydra.ext.scala.meta.Type_Param>empty()), (hydra.util.ConsList<hydra.ext.scala.meta.TypeBounds>) (hydra.util.ConsList.<hydra.ext.scala.meta.TypeBounds>empty()), (hydra.util.ConsList<hydra.ext.scala.meta.Type>) (hydra.util.ConsList.<hydra.ext.scala.meta.Type>empty()), (hydra.util.ConsList<hydra.ext.scala.meta.Type>) (hydra.util.ConsList.<hydra.ext.scala.meta.Type>empty()));
      }),
      freeVars.get()));
    return hydra.Rewriting.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat> otherwise(hydra.core.Type instance) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (ignored -> hydra.ext.scala.Coder.encodeTypeDefinition_mkAlias(
            lname,
            tparams.get(),
            hydra.ext.scala.Utils.stref("Any"))),
          (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (v1 -> hydra.ext.scala.Coder.encodeTypeDefinition_mkAlias(
            lname,
            tparams.get(),
            v1)),
          hydra.ext.scala.Coder.<T0>encodeType(
            cx,
            g,
            typ));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat> visit(hydra.core.Type.Forall ft) {
        java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type>>>> collectForallParams = new java.util.concurrent.atomic.AtomicReference<>();
        collectForallParams.set((java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type>>>) (t -> (java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type>>) (acc -> hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type>(acc, t)));
          }

          @Override
          public hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type> visit(hydra.core.Type.Forall ft2) {
            return collectForallParams.get().apply((ft2).value.body).apply(hydra.lib.lists.Cons.apply(
              (ft2).value.parameter,
              acc));
          }
        }))));
        hydra.core.Type forallBody = (ft).value.body;
        hydra.core.Name forallParam = (ft).value.parameter;
        hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type> collected = collectForallParams.get().apply(forallBody).apply(hydra.util.ConsList.of(forallParam));
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> allForallParams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Reverse.apply(hydra.lib.pairs.First.apply(collected)));
        hydra.util.Lazy<hydra.util.ConsList<hydra.ext.scala.meta.Type_Param>> allTparams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Name, hydra.ext.scala.meta.Type_Param>) (_v -> {
            String vn = hydra.Formatting.capitalize((_v).value);
            return new hydra.ext.scala.meta.Type_Param((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), new hydra.ext.scala.meta.Name.Value(vn), (hydra.util.ConsList<hydra.ext.scala.meta.Type_Param>) (hydra.util.ConsList.<hydra.ext.scala.meta.Type_Param>empty()), (hydra.util.ConsList<hydra.ext.scala.meta.TypeBounds>) (hydra.util.ConsList.<hydra.ext.scala.meta.TypeBounds>empty()), (hydra.util.ConsList<hydra.ext.scala.meta.Type>) (hydra.util.ConsList.<hydra.ext.scala.meta.Type>empty()), (hydra.util.ConsList<hydra.ext.scala.meta.Type>) (hydra.util.ConsList.<hydra.ext.scala.meta.Type>empty()));
          }),
          allForallParams.get()));
        hydra.util.Lazy<hydra.core.Type> innerBody = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(collected));
        return hydra.Rewriting.deannotateType(innerBody.get()).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat> otherwise(hydra.core.Type instance) {
            return hydra.lib.eithers.Either.apply(
              (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (ignored -> hydra.ext.scala.Coder.encodeTypeDefinition_mkAlias2(
                allTparams.get(),
                lname,
                hydra.ext.scala.Utils.stref("Any"))),
              (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (v1 -> hydra.ext.scala.Coder.encodeTypeDefinition_mkAlias2(
                allTparams.get(),
                lname,
                v1)),
              hydra.ext.scala.Coder.<T0>encodeType(
                cx,
                g,
                innerBody.get()));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat> visit(hydra.core.Type.Record rt2) {
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.MapList.apply(
                (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data_Param>>) (f -> hydra.ext.scala.Coder.<T0>fieldToParam(
                  cx,
                  g,
                  f)),
                (rt2).value),
              (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (params -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>right(new hydra.ext.scala.meta.Stat.Defn(new hydra.ext.scala.meta.Defn.Class_(new hydra.ext.scala.meta.Defn_Class(hydra.util.ConsList.of(new hydra.ext.scala.meta.Mod.Case()), tname, allTparams.get(), new hydra.ext.scala.meta.Ctor_Primary((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), new hydra.ext.scala.meta.Name.Value(""), hydra.util.ConsList.of(params)), new hydra.ext.scala.meta.Template((hydra.util.ConsList<hydra.ext.scala.meta.Stat>) (hydra.util.ConsList.<hydra.ext.scala.meta.Stat>empty()), (hydra.util.ConsList<hydra.ext.scala.meta.Init>) (hydra.util.ConsList.<hydra.ext.scala.meta.Init>empty()), new hydra.ext.scala.meta.Self(null), (hydra.util.ConsList<hydra.ext.scala.meta.Stat>) (hydra.util.ConsList.<hydra.ext.scala.meta.Stat>empty()))))))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat> visit(hydra.core.Type.Union rt2) {
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.MapList.apply(
                (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (f -> hydra.ext.scala.Coder.<T0>fieldToEnumCase(
                  cx,
                  g,
                  lname,
                  allTparams.get(),
                  f)),
                (rt2).value),
              (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Stat>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (cases -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>right(new hydra.ext.scala.meta.Stat.Defn(new hydra.ext.scala.meta.Defn.Enum_(new hydra.ext.scala.meta.Defn_Enum((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), tname, allTparams.get(), new hydra.ext.scala.meta.Ctor_Primary((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), new hydra.ext.scala.meta.Name.Value(""), (hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>>) (hydra.util.ConsList.<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>>empty())), new hydra.ext.scala.meta.Template((hydra.util.ConsList<hydra.ext.scala.meta.Stat>) (hydra.util.ConsList.<hydra.ext.scala.meta.Stat>empty()), (hydra.util.ConsList<hydra.ext.scala.meta.Init>) (hydra.util.ConsList.<hydra.ext.scala.meta.Init>empty()), new hydra.ext.scala.meta.Self(null), cases)))))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat> visit(hydra.core.Type.Wrap wt2) {
            return hydra.lib.eithers.Bind.apply(
              hydra.ext.scala.Coder.<T0>encodeType(
                cx,
                g,
                (wt2).value),
              (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (styp -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>right(new hydra.ext.scala.meta.Stat.Defn(new hydra.ext.scala.meta.Defn.Type(new hydra.ext.scala.meta.Defn_Type((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), tname, allTparams.get(), styp))))));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat> visit(hydra.core.Type.Record rt) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data_Param>>) (f -> hydra.ext.scala.Coder.<T0>fieldToParam(
              cx,
              g,
              f)),
            (rt).value),
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (params -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>right(new hydra.ext.scala.meta.Stat.Defn(new hydra.ext.scala.meta.Defn.Class_(new hydra.ext.scala.meta.Defn_Class(hydra.util.ConsList.of(new hydra.ext.scala.meta.Mod.Case()), tname, tparams.get(), new hydra.ext.scala.meta.Ctor_Primary((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), new hydra.ext.scala.meta.Name.Value(""), hydra.util.ConsList.of(params)), new hydra.ext.scala.meta.Template((hydra.util.ConsList<hydra.ext.scala.meta.Stat>) (hydra.util.ConsList.<hydra.ext.scala.meta.Stat>empty()), (hydra.util.ConsList<hydra.ext.scala.meta.Init>) (hydra.util.ConsList.<hydra.ext.scala.meta.Init>empty()), new hydra.ext.scala.meta.Self(null), (hydra.util.ConsList<hydra.ext.scala.meta.Stat>) (hydra.util.ConsList.<hydra.ext.scala.meta.Stat>empty()))))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat> visit(hydra.core.Type.Union rt) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (f -> hydra.ext.scala.Coder.<T0>fieldToEnumCase(
              cx,
              g,
              lname,
              tparams.get(),
              f)),
            (rt).value),
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Stat>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (cases -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>right(new hydra.ext.scala.meta.Stat.Defn(new hydra.ext.scala.meta.Defn.Enum_(new hydra.ext.scala.meta.Defn_Enum((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), tname, tparams.get(), new hydra.ext.scala.meta.Ctor_Primary((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), new hydra.ext.scala.meta.Name.Value(""), (hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>>) (hydra.util.ConsList.<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>>empty())), new hydra.ext.scala.meta.Template((hydra.util.ConsList<hydra.ext.scala.meta.Stat>) (hydra.util.ConsList.<hydra.ext.scala.meta.Stat>empty()), (hydra.util.ConsList<hydra.ext.scala.meta.Init>) (hydra.util.ConsList.<hydra.ext.scala.meta.Init>empty()), new hydra.ext.scala.meta.Self(null), cases)))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat> visit(hydra.core.Type.Wrap wt) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.<T0>encodeType(
            cx,
            g,
            (wt).value),
          (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (styp -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>right(new hydra.ext.scala.meta.Stat.Defn(new hydra.ext.scala.meta.Defn.Type(new hydra.ext.scala.meta.Defn_Type((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), tname, tparams.get(), styp))))));
      }
    });
  }

  static <T1> hydra.util.Either<T1, hydra.ext.scala.meta.Stat> encodeTypeDefinition_mkAlias(String lname, hydra.util.ConsList<hydra.ext.scala.meta.Type_Param> tparams, hydra.ext.scala.meta.Type styp) {
    return hydra.util.Either.<T1, hydra.ext.scala.meta.Stat>right(new hydra.ext.scala.meta.Stat.Defn(new hydra.ext.scala.meta.Defn.Type(new hydra.ext.scala.meta.Defn_Type((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), new hydra.ext.scala.meta.Type_Name(lname), tparams, styp))));
  }

  static <T1> hydra.util.Either<T1, hydra.ext.scala.meta.Stat> encodeTypeDefinition_mkAlias2(hydra.util.ConsList<hydra.ext.scala.meta.Type_Param> allTparams, String lname, hydra.ext.scala.meta.Type styp) {
    return hydra.util.Either.<T1, hydra.ext.scala.meta.Stat>right(new hydra.ext.scala.meta.Stat.Defn(new hydra.ext.scala.meta.Defn.Type(new hydra.ext.scala.meta.Defn_Type((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), new hydra.ext.scala.meta.Type_Name(lname), allTparams, styp))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> encodeFunction(hydra.context.Context cx, hydra.graph.Graph g, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> meta, hydra.core.Function fun, hydra.util.Maybe<hydra.core.Term> arg) {
    return (fun).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> otherwise(hydra.core.Function instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("unsupported function")), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Function.Lambda lam) {
        hydra.core.Term body = (lam).value.body;
        hydra.util.Maybe<hydra.core.Type> rawMdom = (lam).value.domain;
        hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> mdom = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Bind.apply(
          rawMdom,
          (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (dom -> {
            hydra.util.PersistentSet<hydra.core.Name> freeVars = hydra.Rewriting.freeVariablesInType(dom);
            hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> unqualifiedFreeVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Filter.apply(
              (java.util.function.Function<hydra.core.Name, Boolean>) (n -> hydra.lib.logic.Not.apply(hydra.lib.lists.Elem.apply(
                46,
                hydra.lib.strings.ToList.apply((n).value)))),
              hydra.lib.sets.ToList.apply(freeVars))));
            hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> unresolvedVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.Difference.apply(
              unqualifiedFreeVars.get(),
              (g).typeVariables));
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.sets.Null.apply(unresolvedVars.get()),
              () -> hydra.util.Maybe.just(dom),
              () -> (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()));
          })));
        hydra.core.Name param = (lam).value.parameter;
        String v = hydra.ext.scala.Utils.scalaEscapeName((param).value);
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.encodeTerm(
            cx,
            g,
            body),
          (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sbody -> hydra.lib.eithers.Bind.apply(
            hydra.lib.maybes.Maybe.applyLazy(
              () -> hydra.ext.scala.Coder.findSdom(
                cx,
                g,
                meta),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>>) (dom -> hydra.lib.eithers.Bind.apply(
                hydra.ext.scala.Coder.encodeType(
                  cx,
                  g,
                  dom),
                (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>>) (sdom -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>right(hydra.util.Maybe.just(sdom))))),
              mdom.get()),
            (java.util.function.Function<hydra.util.Maybe<hydra.ext.scala.meta.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sdom -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.slambda(
              v,
              sbody,
              sdom))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Function.Primitive name) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sprim((name).value));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Function.Elimination e) {
        return (e).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> otherwise(hydra.core.Elimination instance) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("unsupported elimination")), cx)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Elimination.Wrap name) {
            return hydra.lib.maybes.Maybe.applyLazy(
              () -> hydra.lib.eithers.Bind.apply(
                hydra.ext.scala.Coder.findSdom(
                  cx,
                  g,
                  meta),
                (java.util.function.Function<hydra.util.Maybe<hydra.ext.scala.meta.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sdom -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.slambda(
                  "x",
                  hydra.ext.scala.Utils.sname("x"),
                  sdom)))),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (a -> hydra.ext.scala.Coder.encodeTerm(
                cx,
                g,
                a)),
              arg);
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Elimination.Record proj) {
            String fname = hydra.ext.scala.Utils.scalaEscapeName((proj).value.field.value);
            String pv = "x";
            hydra.core.Name typeName = (proj).value.typeName;
            return hydra.lib.maybes.Maybe.applyLazy(
              () -> hydra.lib.eithers.Bind.apply(
                hydra.lib.eithers.Either.apply(
                  (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>>) (ignored -> hydra.lib.eithers.Bind.apply(
                    hydra.ext.scala.Coder.encodeType(
                      cx,
                      g,
                      new hydra.core.Type.Variable(typeName)),
                    (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>>) (st -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>right(hydra.util.Maybe.just(st))))),
                  (java.util.function.Function<hydra.util.Maybe<hydra.ext.scala.meta.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>>) (msdom -> hydra.lib.maybes.Maybe.applyLazy(
                    () -> hydra.lib.eithers.Bind.apply(
                      hydra.ext.scala.Coder.encodeType(
                        cx,
                        g,
                        new hydra.core.Type.Variable(typeName)),
                      (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>>) (st -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>right(hydra.util.Maybe.just(st)))),
                    (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>>) (sdom -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>right(hydra.util.Maybe.just(sdom))),
                    msdom)),
                  hydra.ext.scala.Coder.findSdom(
                    cx,
                    g,
                    meta)),
                (java.util.function.Function<hydra.util.Maybe<hydra.ext.scala.meta.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (msdom -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.slambda(
                  pv,
                  new hydra.ext.scala.meta.Data.Ref(new hydra.ext.scala.meta.Data_Ref.Select(new hydra.ext.scala.meta.Data_Select(hydra.ext.scala.Utils.sname(pv), new hydra.ext.scala.meta.Data_Name(new hydra.ext.scala.meta.PredefString(fname))))),
                  msdom)))),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (a -> hydra.lib.eithers.Bind.apply(
                hydra.ext.scala.Coder.encodeTerm(
                  cx,
                  g,
                  a),
                (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sa -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(new hydra.ext.scala.meta.Data.Ref(new hydra.ext.scala.meta.Data_Ref.Select(new hydra.ext.scala.meta.Data_Select(sa, new hydra.ext.scala.meta.Data_Name(new hydra.ext.scala.meta.PredefString(fname))))))))),
              arg);
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Elimination.Union cs) {
            hydra.util.ConsList<hydra.core.Field> cases = (cs).value.cases;
            hydra.util.Maybe<hydra.core.Term> dflt = (cs).value.default_;
            hydra.core.Name tname = (cs).value.typeName;
            hydra.core.Type dom = new hydra.core.Type.Variable(tname);
            hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> ftypes = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Either.apply(
              (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>) (ignored -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()))),
              (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>) (x_ -> x_),
              hydra.Schemas.fieldTypes(
                cx,
                g,
                dom)));
            hydra.util.Lazy<hydra.util.Maybe<hydra.core.Name>> sn = new hydra.util.Lazy<>(() -> hydra.ext.scala.Utils.nameOfType(
              g,
              dom));
            String v = "v";
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.MapList.apply(
                (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Case>>) (f -> hydra.ext.scala.Coder.encodeCase(
                  cx,
                  g,
                  ftypes.get(),
                  sn.get(),
                  f)),
                cases),
              (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Case>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (fieldCases -> hydra.lib.eithers.Bind.apply(
                hydra.lib.maybes.Maybe.applyLazy(
                  () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.scala.meta.Case>>right(fieldCases),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.scala.meta.Case>>>) (dfltTerm -> hydra.lib.eithers.Bind.apply(
                    hydra.ext.scala.Coder.encodeTerm(
                      cx,
                      g,
                      dfltTerm),
                    (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.scala.meta.Case>>>) (sdflt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.scala.meta.Case>>right(hydra.lib.lists.Concat2.apply(
                      fieldCases,
                      hydra.util.ConsList.of(new hydra.ext.scala.meta.Case(new hydra.ext.scala.meta.Pat.Wildcard(), (hydra.util.Maybe<hydra.ext.scala.meta.Data>) (hydra.util.Maybe.<hydra.ext.scala.meta.Data>nothing()), sdflt))))))),
                  dflt),
                (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Case>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (scases -> hydra.lib.maybes.Maybe.applyLazy(
                  () -> hydra.lib.eithers.Bind.apply(
                    hydra.ext.scala.Coder.findSdom(
                      cx,
                      g,
                      meta),
                    (java.util.function.Function<hydra.util.Maybe<hydra.ext.scala.meta.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sdom -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.slambda(
                      v,
                      new hydra.ext.scala.meta.Data.Match(new hydra.ext.scala.meta.Data_Match(hydra.ext.scala.Utils.sname(v), scases)),
                      sdom)))),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (a -> hydra.lib.eithers.Bind.apply(
                    hydra.ext.scala.Coder.encodeTerm(
                      cx,
                      g,
                      a),
                    (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sa -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(new hydra.ext.scala.meta.Data.Match(new hydra.ext.scala.meta.Data_Match(sa, scases)))))),
                  arg)))));
          }
        });
      }
    });
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> encodeLiteral(hydra.context.Context cx, T0 g, hydra.core.Literal av) {
    return (av).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> otherwise(hydra.core.Literal instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("unexpected literal")), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.Literal.Binary ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>right(new hydra.ext.scala.meta.Lit.String_("<binary>"));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.Literal.Boolean_ b) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>right(new hydra.ext.scala.meta.Lit.Boolean_((b).value));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.Literal.Float_ fv) {
        return (fv).value.accept(new hydra.core.FloatValue.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> otherwise(hydra.core.FloatValue instance) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("unexpected float value")), cx)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.FloatValue.Bigfloat bf) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>right(new hydra.ext.scala.meta.Lit.Double_(hydra.lib.literals.BigfloatToFloat64.apply((bf).value)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.FloatValue.Float32 f) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>right(new hydra.ext.scala.meta.Lit.Float_((f).value));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.FloatValue.Float64 f) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>right(new hydra.ext.scala.meta.Lit.Double_((f).value));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.Literal.Integer_ iv) {
        return (iv).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> otherwise(hydra.core.IntegerValue instance) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("unexpected integer value")), cx)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.IntegerValue.Bigint i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>right(new hydra.ext.scala.meta.Lit.Long_(hydra.lib.literals.BigintToInt64.apply((i).value)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.IntegerValue.Int8 i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>right(new hydra.ext.scala.meta.Lit.Byte_((i).value));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.IntegerValue.Int16 i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>right(new hydra.ext.scala.meta.Lit.Short_((i).value));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.IntegerValue.Int32 i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>right(new hydra.ext.scala.meta.Lit.Int((i).value));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.IntegerValue.Int64 i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>right(new hydra.ext.scala.meta.Lit.Long_((i).value));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.IntegerValue.Uint8 i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>right(new hydra.ext.scala.meta.Lit.Byte_(hydra.lib.literals.BigintToInt8.apply(hydra.lib.literals.Uint8ToBigint.apply((i).value))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.IntegerValue.Uint16 i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>right(new hydra.ext.scala.meta.Lit.Int(hydra.lib.literals.BigintToInt32.apply(hydra.lib.literals.Uint16ToBigint.apply((i).value))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.IntegerValue.Uint32 i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>right(new hydra.ext.scala.meta.Lit.Long_(hydra.lib.literals.BigintToInt64.apply(hydra.lib.literals.Uint32ToBigint.apply((i).value))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.IntegerValue.Uint64 i) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>right(new hydra.ext.scala.meta.Lit.Long_(hydra.lib.literals.BigintToInt64.apply(hydra.lib.literals.Uint64ToBigint.apply((i).value))));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit> visit(hydra.core.Literal.String_ s) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Lit>right(new hydra.ext.scala.meta.Lit.String_((s).value));
      }
    });
  }

  static hydra.core.Term stripWrapEliminations(hydra.core.Term t) {
    return hydra.Rewriting.deannotateAndDetypeTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Application app) {
        hydra.core.Term appArg = (app).value.argument;
        hydra.core.Term appFun = (app).value.function;
        return hydra.Rewriting.deannotateAndDetypeTerm(appFun).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Term instance) {
            return t;
          }

          @Override
          public hydra.core.Term visit(hydra.core.Term.Function f) {
            return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
              @Override
              public hydra.core.Term otherwise(hydra.core.Function instance) {
                return t;
              }

              @Override
              public hydra.core.Term visit(hydra.core.Function.Elimination e) {
                return (e).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
                  @Override
                  public hydra.core.Term otherwise(hydra.core.Elimination instance) {
                    return t;
                  }

                  @Override
                  public hydra.core.Term visit(hydra.core.Elimination.Wrap ignored) {
                    return hydra.ext.scala.Coder.stripWrapEliminations(appArg);
                  }
                });
              }
            });
          }

          @Override
          public hydra.core.Term visit(hydra.core.Term.Application innerApp) {
            hydra.core.Term innerArg = (innerApp).value.argument;
            hydra.core.Term innerFun = (innerApp).value.function;
            return hydra.Rewriting.deannotateAndDetypeTerm(innerFun).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public hydra.core.Term otherwise(hydra.core.Term instance) {
                return t;
              }

              @Override
              public hydra.core.Term visit(hydra.core.Term.Function innerF) {
                return (innerF).value.accept(new hydra.core.Function.PartialVisitor<>() {
                  @Override
                  public hydra.core.Term otherwise(hydra.core.Function instance) {
                    return t;
                  }

                  @Override
                  public hydra.core.Term visit(hydra.core.Function.Elimination innerE) {
                    return (innerE).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
                      @Override
                      public hydra.core.Term otherwise(hydra.core.Elimination instance) {
                        return t;
                      }

                      @Override
                      public hydra.core.Term visit(hydra.core.Elimination.Wrap ignored) {
                        return hydra.ext.scala.Coder.stripWrapEliminations(new hydra.core.Term.Application(new hydra.core.Application(innerArg, appArg)));
                      }
                    });
                  }
                });
              }
            });
          }
        });
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> encodeTerm(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term term0) {
    hydra.core.Term term = hydra.ext.scala.Coder.stripWrapEliminations(term0);
    return hydra.Rewriting.deannotateTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("unexpected term")), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.TypeApplication ta) {
        java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Term>>>> collectTypeLambdas = new java.util.concurrent.atomic.AtomicReference<>();
        collectTypeLambdas.set((java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Term>>>) (t -> (java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Term>>) (acc -> hydra.Rewriting.deannotateTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Term> otherwise(hydra.core.Term instance) {
            return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Term>(acc, t)));
          }

          @Override
          public hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Term> visit(hydra.core.Term.TypeLambda tl) {
            return collectTypeLambdas.get().apply((tl).value.body).apply(hydra.lib.lists.Cons.apply(
              (tl).value.parameter,
              acc));
          }
        }))));
        java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>>>> collectTypeArgs = new java.util.concurrent.atomic.AtomicReference<>();
        collectTypeArgs.set((java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>>>) (t -> (java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>>) (acc -> hydra.Rewriting.deannotateTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term> otherwise(hydra.core.Term instance) {
            return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>(acc, t)));
          }

          @Override
          public hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term> visit(hydra.core.Term.TypeApplication ta2) {
            return collectTypeArgs.get().apply((ta2).value.body).apply(hydra.lib.lists.Cons.apply(
              (ta2).value.type,
              acc));
          }
        }))));
        hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term> collected = collectTypeArgs.get().apply((ta).value.body).apply(hydra.util.ConsList.of((ta).value.type));
        hydra.util.Lazy<hydra.core.Term> innerTerm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(collected));
        hydra.util.Lazy<hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Term>> tlCollected = new hydra.util.Lazy<>(() -> collectTypeLambdas.get().apply(innerTerm.get()).apply((hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty())));
        hydra.util.Lazy<hydra.core.Term> bodyAfterTypeLambdas = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tlCollected.get()));
        hydra.core.Term substitutedBody = bodyAfterTypeLambdas.get();
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> typeArgs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(collected));
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> typeParams = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tlCollected.get()));
        return hydra.Rewriting.deannotateTerm(substitutedBody).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> otherwise(hydra.core.Term instance) {
            return hydra.ext.scala.Coder.encodeTerm(
              cx,
              g,
              substitutedBody);
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Function f) {
            return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> otherwise(hydra.core.Function instance) {
                return hydra.ext.scala.Coder.encodeTerm(
                  cx,
                  g,
                  substitutedBody);
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Function.Primitive pname) {
                return hydra.lib.eithers.Bind.apply(
                  hydra.lib.eithers.MapList.apply(
                    (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>>) (targ -> hydra.ext.scala.Coder.encodeType(
                      cx,
                      g,
                      targ)),
                    typeArgs.get()),
                  (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (stypeArgs -> {
                    hydra.util.Lazy<hydra.util.PersistentSet<String>> inScopeTypeVarNames = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
                      (java.util.function.Function<hydra.core.Name, String>) (n -> hydra.Formatting.capitalize((n).value)),
                      hydra.lib.sets.ToList.apply((g).typeVariables))));
                    hydra.util.Lazy<Boolean> hasForallResidual = new hydra.util.Lazy<>(() -> hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.lists.Filter.apply(
                      (java.util.function.Function<hydra.ext.scala.meta.Type, Boolean>) (st -> (st).accept(new hydra.ext.scala.meta.Type.PartialVisitor<>() {
                        @Override
                        public Boolean otherwise(hydra.ext.scala.meta.Type instance) {
                          return false;
                        }

                        @Override
                        public Boolean visit(hydra.ext.scala.meta.Type.Var tv) {
                          String tvName = (tv).value.name.value;
                          return hydra.lib.logic.And.apply(
                            hydra.lib.logic.Not.apply(hydra.lib.lists.Elem.apply(
                              46,
                              hydra.lib.strings.ToList.apply(tvName))),
                            hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
                              tvName,
                              inScopeTypeVarNames.get())));
                        }
                      })),
                      stypeArgs))));
                    return hydra.lib.logic.IfElse.lazy(
                      hasForallResidual.get(),
                      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sprim((pname).value)),
                      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapplyTypes(
                        hydra.ext.scala.Utils.sprim((pname).value),
                        stypeArgs)));
                  }));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Function.Elimination ignored) {
                return hydra.ext.scala.Coder.encodeTerm(
                  cx,
                  g,
                  substitutedBody);
              }
            });
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.TypeLambda tl) {
        return hydra.ext.scala.Coder.encodeTerm(
          cx,
          hydra.Rewriting.extendGraphForTypeLambda(
            g,
            (tl).value),
          (tl).value.body);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Application app) {
        hydra.core.Term arg = (app).value.argument;
        hydra.core.Term fun = (app).value.function;
        return hydra.Rewriting.deannotateAndDetypeTerm(fun).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> otherwise(hydra.core.Term instance) {
            return hydra.lib.eithers.Bind.apply(
              hydra.ext.scala.Coder.encodeTerm(
                cx,
                g,
                fun),
              (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sfun -> hydra.lib.eithers.Bind.apply(
                hydra.ext.scala.Coder.encodeTerm(
                  cx,
                  g,
                  arg),
                (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sarg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
                  sfun,
                  hydra.util.ConsList.of(sarg)))))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Function f) {
            return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> otherwise(hydra.core.Function instance) {
                return hydra.lib.eithers.Bind.apply(
                  hydra.ext.scala.Coder.encodeTerm(
                    cx,
                    g,
                    fun),
                  (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sfun -> hydra.lib.eithers.Bind.apply(
                    hydra.ext.scala.Coder.encodeTerm(
                      cx,
                      g,
                      arg),
                    (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sarg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
                      sfun,
                      hydra.util.ConsList.of(sarg)))))));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Function.Lambda lam) {
                hydra.core.Term lamBody = (lam).value.body;
                return hydra.Rewriting.deannotateAndDetypeTerm(lamBody).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> otherwise(hydra.core.Term instance) {
                    return hydra.lib.eithers.Bind.apply(
                      hydra.ext.scala.Coder.encodeTerm(
                        cx,
                        g,
                        fun),
                      (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sfun -> hydra.lib.eithers.Bind.apply(
                        hydra.ext.scala.Coder.encodeTerm(
                          cx,
                          g,
                          arg),
                        (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sarg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
                          sfun,
                          hydra.util.ConsList.of(sarg)))))));
                  }

                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Application innerApp) {
                    hydra.core.Term innerFun = (innerApp).value.function;
                    return hydra.Rewriting.deannotateAndDetypeTerm(innerFun).accept(new hydra.core.Term.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> otherwise(hydra.core.Term instance) {
                        return hydra.lib.eithers.Bind.apply(
                          hydra.ext.scala.Coder.encodeTerm(
                            cx,
                            g,
                            fun),
                          (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sfun -> hydra.lib.eithers.Bind.apply(
                            hydra.ext.scala.Coder.encodeTerm(
                              cx,
                              g,
                              arg),
                            (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sarg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
                              sfun,
                              hydra.util.ConsList.of(sarg)))))));
                      }

                      @Override
                      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Function innerF) {
                        return (innerF).value.accept(new hydra.core.Function.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> otherwise(hydra.core.Function instance) {
                            return hydra.lib.eithers.Bind.apply(
                              hydra.ext.scala.Coder.encodeTerm(
                                cx,
                                g,
                                fun),
                              (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sfun -> hydra.lib.eithers.Bind.apply(
                                hydra.ext.scala.Coder.encodeTerm(
                                  cx,
                                  g,
                                  arg),
                                (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sarg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
                                  sfun,
                                  hydra.util.ConsList.of(sarg)))))));
                          }

                          @Override
                          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Function.Elimination innerE) {
                            return (innerE).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
                              @Override
                              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> otherwise(hydra.core.Elimination instance) {
                                return hydra.lib.eithers.Bind.apply(
                                  hydra.ext.scala.Coder.encodeTerm(
                                    cx,
                                    g,
                                    fun),
                                  (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sfun -> hydra.lib.eithers.Bind.apply(
                                    hydra.ext.scala.Coder.encodeTerm(
                                      cx,
                                      g,
                                      arg),
                                    (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sarg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
                                      sfun,
                                      hydra.util.ConsList.of(sarg)))))));
                              }

                              @Override
                              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Elimination.Union ignored) {
                                return hydra.ext.scala.Coder.encodeFunction(
                                  cx,
                                  g,
                                  hydra.Annotations.termAnnotationInternal(innerFun),
                                  (innerF).value,
                                  hydra.util.Maybe.just(arg));
                              }
                            });
                          }
                        });
                      }
                    });
                  }
                });
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Function.Elimination e) {
                return (e).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> otherwise(hydra.core.Elimination instance) {
                    return hydra.lib.eithers.Bind.apply(
                      hydra.ext.scala.Coder.encodeTerm(
                        cx,
                        g,
                        fun),
                      (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sfun -> hydra.lib.eithers.Bind.apply(
                        hydra.ext.scala.Coder.encodeTerm(
                          cx,
                          g,
                          arg),
                        (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sarg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
                          sfun,
                          hydra.util.ConsList.of(sarg)))))));
                  }

                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Elimination.Record proj) {
                    String fname = hydra.ext.scala.Utils.scalaEscapeName((proj).value.field.value);
                    return hydra.lib.eithers.Bind.apply(
                      hydra.ext.scala.Coder.encodeTerm(
                        cx,
                        g,
                        arg),
                      (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sarg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(new hydra.ext.scala.meta.Data.Ref(new hydra.ext.scala.meta.Data_Ref.Select(new hydra.ext.scala.meta.Data_Select(sarg, new hydra.ext.scala.meta.Data_Name(new hydra.ext.scala.meta.PredefString(fname))))))));
                  }

                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Elimination.Union ignored) {
                    return hydra.ext.scala.Coder.encodeFunction(
                      cx,
                      g,
                      hydra.Annotations.termAnnotationInternal(fun),
                      (f).value,
                      hydra.util.Maybe.just(arg));
                  }
                });
              }
            });
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Function f) {
        return hydra.ext.scala.Coder.encodeFunction(
          cx,
          g,
          hydra.Annotations.termAnnotationInternal(term),
          (f).value,
          (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.List els) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (e -> hydra.ext.scala.Coder.encodeTerm(
              cx,
              g,
              e)),
            (els).value),
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Data>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sels -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
            hydra.ext.scala.Utils.sname("Seq"),
            sels))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Literal v) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.encodeLiteral(
            cx,
            g,
            (v).value),
          (java.util.function.Function<hydra.ext.scala.meta.Lit, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (slit -> {
            hydra.ext.scala.meta.Data litData = new hydra.ext.scala.meta.Data.Lit(slit);
            return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> otherwise(hydra.core.Literal instance) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(litData);
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Literal.Integer_ iv) {
                return (iv).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> otherwise(hydra.core.IntegerValue instance) {
                    return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(litData);
                  }

                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.IntegerValue.Bigint ignored) {
                    return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
                      hydra.ext.scala.Utils.sname("BigInt"),
                      hydra.util.ConsList.of(litData)));
                  }

                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.IntegerValue.Uint64 ignored) {
                    return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
                      hydra.ext.scala.Utils.sname("BigInt"),
                      hydra.util.ConsList.of(litData)));
                  }
                });
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Literal.Float_ fv) {
                return (fv).value.accept(new hydra.core.FloatValue.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> otherwise(hydra.core.FloatValue instance) {
                    return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(litData);
                  }

                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.FloatValue.Bigfloat ignored) {
                    return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
                      hydra.ext.scala.Utils.sname("BigDecimal"),
                      hydra.util.ConsList.of(litData)));
                  }
                });
              }
            });
          }));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Map m) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (kv -> hydra.lib.eithers.Bind.apply(
              hydra.ext.scala.Coder.encodeTerm(
                cx,
                g,
                hydra.lib.pairs.First.apply(kv)),
              (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sk -> hydra.lib.eithers.Bind.apply(
                hydra.ext.scala.Coder.encodeTerm(
                  cx,
                  g,
                  hydra.lib.pairs.Second.apply(kv)),
                (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sv -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sassign(
                  sk,
                  sv))))))),
            hydra.lib.maps.ToList.apply((m).value)),
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Data>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (spairs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
            hydra.ext.scala.Utils.sname("Map"),
            spairs))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Wrap wt) {
        return hydra.ext.scala.Coder.encodeTerm(
          cx,
          g,
          (wt).value.body);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sname("None")),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (t -> hydra.lib.eithers.Bind.apply(
            hydra.ext.scala.Coder.encodeTerm(
              cx,
              g,
              t),
            (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (s -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
              hydra.ext.scala.Utils.sname("Some"),
              hydra.util.ConsList.of(s)))))),
          (m).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Record rec) {
        hydra.util.ConsList<hydra.core.Field> fields = (rec).value.fields;
        hydra.core.Name rname = (rec).value.typeName;
        String n = hydra.ext.scala.Utils.scalaTypeName(
          true,
          rname);
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (f -> hydra.ext.scala.Coder.encodeTerm(
              cx,
              g,
              (f).term)),
            fields),
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Data>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (args -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
            hydra.ext.scala.Utils.sname(n),
            args))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Set s) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (e -> hydra.ext.scala.Coder.encodeTerm(
              cx,
              g,
              e)),
            hydra.lib.sets.ToList.apply((s).value)),
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Data>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sels -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
            hydra.ext.scala.Utils.sname("scala.collection.immutable.Set"),
            sels))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Union inj) {
        hydra.core.Name fn = (inj).value.field.name;
        hydra.core.Term ft = (inj).value.field.term;
        hydra.core.Name sn = (inj).value.typeName;
        hydra.ext.scala.meta.Data lhs = hydra.ext.scala.Utils.sname(hydra.ext.scala.Utils.qualifyUnionFieldName(
          "UNION.",
          hydra.util.Maybe.just(sn),
          fn));
        hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> unionFtypes = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>) (ignored -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()))),
          (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>) (x_ -> x_),
          hydra.Schemas.fieldTypes(
            cx,
            g,
            new hydra.core.Type.Variable(sn))));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.Rewriting.deannotateAndDetypeTerm(ft).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public Boolean otherwise(hydra.core.Term instance) {
                return false;
              }

              @Override
              public Boolean visit(hydra.core.Term.Unit ignored) {
                return true;
              }

              @Override
              public Boolean visit(hydra.core.Term.Record rec) {
                return hydra.lib.equality.Equal.apply(
                  hydra.lib.lists.Length.apply((rec).value.fields),
                  0);
              }
            }),
            (java.util.function.Function<hydra.core.Type, Boolean>) (dom -> hydra.Rewriting.deannotateType(dom).accept(new hydra.core.Type.PartialVisitor<>() {
              @Override
              public Boolean otherwise(hydra.core.Type instance) {
                return false;
              }

              @Override
              public Boolean visit(hydra.core.Type.Unit ignored) {
                return true;
              }

              @Override
              public Boolean visit(hydra.core.Type.Record rt) {
                return hydra.lib.equality.Equal.apply(
                  hydra.lib.lists.Length.apply((rt).value),
                  0);
              }
            })),
            hydra.lib.maps.Lookup.apply(
              fn,
              unionFtypes.get())),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(lhs),
          () -> hydra.lib.eithers.Bind.apply(
            hydra.ext.scala.Coder.encodeTerm(
              cx,
              g,
              ft),
            (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sarg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
              lhs,
              hydra.util.ConsList.of(sarg))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Variable v) {
        String fullName = (v).value.value;
        String localName = hydra.Names.localNameOf((v).value);
        hydra.util.ConsList<String> parts = hydra.lib.strings.SplitOn.apply(
          ".",
          fullName);
        hydra.util.Lazy<Integer> numParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(parts));
        hydra.util.Lazy<String> escaped = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Lte.apply(
            numParts.get(),
            1),
          () -> hydra.ext.scala.Utils.scalaEscapeName(fullName),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              numParts.get(),
              2),
            () -> hydra.lib.strings.Cat2.apply(
              hydra.lib.lists.Head.apply(parts),
              hydra.lib.strings.Cat2.apply(
                ".",
                hydra.ext.scala.Utils.scalaEscapeName(localName))),
            () -> hydra.lib.strings.Intercalate.apply(
              ".",
              hydra.lib.lists.Concat2.apply(
                hydra.lib.lists.Take.apply(
                  hydra.lib.math.Sub.apply(
                    numParts.get(),
                    1),
                  parts),
                hydra.util.ConsList.of(hydra.ext.scala.Utils.scalaEscapeName(localName)))))));
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sname(escaped.get()));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Annotated at) {
        return hydra.ext.scala.Coder.encodeTerm(
          cx,
          g,
          (at).value.body);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (l -> hydra.lib.eithers.Bind.apply(
            hydra.ext.scala.Coder.encodeTerm(
              cx,
              g,
              l),
            (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sl -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
              hydra.ext.scala.Utils.sname("Left"),
              hydra.util.ConsList.of(sl)))))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (r -> hydra.lib.eithers.Bind.apply(
            hydra.ext.scala.Coder.encodeTerm(
              cx,
              g,
              r),
            (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
              hydra.ext.scala.Utils.sname("Right"),
              hydra.util.ConsList.of(sr)))))),
          (e).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Pair p) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.encodeTerm(
            cx,
            g,
            hydra.lib.pairs.First.apply((p).value)),
          (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sf -> hydra.lib.eithers.Bind.apply(
            hydra.ext.scala.Coder.encodeTerm(
              cx,
              g,
              hydra.lib.pairs.Second.apply((p).value)),
            (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (ss -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(hydra.ext.scala.Utils.sapply(
              hydra.ext.scala.Utils.sname("Tuple2"),
              hydra.util.ConsList.of(
                sf,
                ss)))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Unit ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(new hydra.ext.scala.meta.Data.Lit(new hydra.ext.scala.meta.Lit.Unit()));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.TypeApplication ta) {
        return hydra.ext.scala.Coder.encodeTerm(
          cx,
          g,
          (ta).value.body);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.TypeLambda tl) {
        return hydra.ext.scala.Coder.encodeTerm(
          cx,
          hydra.Rewriting.extendGraphForTypeLambda(
            g,
            (tl).value),
          (tl).value.body);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> visit(hydra.core.Term.Let lt) {
        hydra.util.ConsList<hydra.core.Binding> bindings = (lt).value.bindings;
        hydra.core.Term body = (lt).value.body;
        hydra.graph.Graph gLet = hydra.Rewriting.extendGraphForLet(
          (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (p0 -> p1 -> hydra.CoderUtils.bindingMetadata(
            p0,
            p1)),
          g,
          (lt).value);
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (v1 -> hydra.ext.scala.Coder.encodeLetBinding(
              cx,
              gLet,
              (gLet).typeVariables,
              v1)),
            bindings),
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Stat>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sbindings -> hydra.lib.eithers.Bind.apply(
            hydra.ext.scala.Coder.encodeTerm(
              cx,
              gLet,
              body),
            (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (sbody -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>right(new hydra.ext.scala.meta.Data.Block(new hydra.ext.scala.meta.Data_Block(hydra.lib.lists.Concat2.apply(
              sbindings,
              hydra.util.ConsList.of(new hydra.ext.scala.meta.Stat.Term(sbody))))))))));
      }
    });
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> encodeType(hydra.context.Context cx, T0 g, hydra.core.Type t) {
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("unsupported type")), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.Type.Application at) {
        java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, hydra.util.Pair<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>>>> collectTypeArgs = new java.util.concurrent.atomic.AtomicReference<>();
        collectTypeArgs.set((java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, hydra.util.Pair<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>>>) (t2 -> (java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, hydra.util.Pair<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>>) (acc -> hydra.Rewriting.deannotateType(t2).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Pair<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>> otherwise(hydra.core.Type instance) {
            return (hydra.util.Pair<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>(t2, acc)));
          }

          @Override
          public hydra.util.Pair<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>> visit(hydra.core.Type.Application at2) {
            hydra.core.Type a2 = (at2).value.argument;
            hydra.core.Type f2 = (at2).value.function;
            return collectTypeArgs.get().apply(f2).apply(hydra.lib.lists.Cons.apply(
              a2,
              acc));
          }
        }))));
        hydra.util.Lazy<hydra.util.Pair<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>> collected = new hydra.util.Lazy<>(() -> collectTypeArgs.get().apply(new hydra.core.Type.Application((at).value)).apply((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty())));
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> allArgs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(collected.get()));
        hydra.util.Lazy<hydra.core.Type> baseFun = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(collected.get()));
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.<T0>encodeType(
            cx,
            g,
            baseFun.get()),
          (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>>) (sfun -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>>) (a -> hydra.ext.scala.Coder.<T0>encodeType(
                cx,
                g,
                a)),
              allArgs.get()),
            (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>>) (sargs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(hydra.ext.scala.Utils.stapply(
              sfun,
              sargs))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.Type.Unit ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Unit"))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.Type.Either et) {
        hydra.core.Type lt = (et).value.left;
        hydra.core.Type rt = (et).value.right;
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.<T0>encodeType(
            cx,
            g,
            lt),
          (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>>) (slt -> hydra.lib.eithers.Bind.apply(
            hydra.ext.scala.Coder.<T0>encodeType(
              cx,
              g,
              rt),
            (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>>) (srt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(hydra.ext.scala.Utils.stapply2(
              new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Either"))),
              slt,
              srt))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.Type.Function ft) {
        hydra.core.Type cod = (ft).value.codomain;
        hydra.core.Type dom = (ft).value.domain;
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.<T0>encodeType(
            cx,
            g,
            dom),
          (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>>) (sdom -> hydra.lib.eithers.Bind.apply(
            hydra.ext.scala.Coder.<T0>encodeType(
              cx,
              g,
              cod),
            (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>>) (scod -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.FunctionType(new hydra.ext.scala.meta.Type_FunctionType.Function(new hydra.ext.scala.meta.Type_Function(hydra.util.ConsList.of(sdom), scod))))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.Type.List lt) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.<T0>encodeType(
            cx,
            g,
            (lt).value),
          (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>>) (slt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(hydra.ext.scala.Utils.stapply1(
            new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Seq"))),
            slt))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.Type.Literal lt) {
        return (lt).value.accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> otherwise(hydra.core.LiteralType instance) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("unsupported literal type")), cx)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.LiteralType.Binary ignored) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(hydra.ext.scala.Utils.stapply(
              new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Array"))),
              hydra.util.ConsList.of(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Byte"))))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.LiteralType.Boolean_ ignored) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Boolean"))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.LiteralType.Float_ ft) {
            return (ft).value.accept(new hydra.core.FloatType.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> otherwise(hydra.core.FloatType instance) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("unsupported float type")), cx)));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.FloatType.Bigfloat ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("BigDecimal"))));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.FloatType.Float32 ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Float"))));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.FloatType.Float64 ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Double"))));
              }
            });
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.LiteralType.Integer_ it) {
            return (it).value.accept(new hydra.core.IntegerType.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> otherwise(hydra.core.IntegerType instance) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("unsupported integer type")), cx)));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.IntegerType.Bigint ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("BigInt"))));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.IntegerType.Int8 ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Byte"))));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.IntegerType.Int16 ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Short"))));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.IntegerType.Int32 ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Int"))));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.IntegerType.Int64 ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Long"))));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.IntegerType.Uint8 ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Byte"))));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.IntegerType.Uint16 ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Int"))));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.IntegerType.Uint32 ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Long"))));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.IntegerType.Uint64 ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("BigInt"))));
              }
            });
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.LiteralType.String_ ignored) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("scala.Predef.String"))));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.Type.Map mt) {
        hydra.core.Type kt = (mt).value.keys;
        hydra.core.Type vt = (mt).value.values;
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.<T0>encodeType(
            cx,
            g,
            kt),
          (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>>) (skt -> hydra.lib.eithers.Bind.apply(
            hydra.ext.scala.Coder.<T0>encodeType(
              cx,
              g,
              vt),
            (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>>) (svt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(hydra.ext.scala.Utils.stapply2(
              new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Map"))),
              skt,
              svt))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.Type.Maybe ot) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.<T0>encodeType(
            cx,
            g,
            (ot).value),
          (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>>) (sot -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(hydra.ext.scala.Utils.stapply1(
            new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Option"))),
            sot))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.Type.Pair pt) {
        hydra.core.Type ft = (pt).value.first;
        hydra.core.Type st = (pt).value.second;
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.<T0>encodeType(
            cx,
            g,
            ft),
          (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>>) (sft -> hydra.lib.eithers.Bind.apply(
            hydra.ext.scala.Coder.<T0>encodeType(
              cx,
              g,
              st),
            (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>>) (sst -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(hydra.ext.scala.Utils.stapply2(
              new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("Tuple2"))),
              sft,
              sst))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.Type.Record ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("unexpected anonymous record type")), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.Type.Set st) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.<T0>encodeType(
            cx,
            g,
            (st).value),
          (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>>) (sst -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(hydra.ext.scala.Utils.stapply1(
            new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name("scala.collection.immutable.Set"))),
            sst))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.Type.Union ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("unexpected anonymous union type")), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.Type.Wrap ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("unexpected anonymous wrap type")), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.Type.Forall ft) {
        hydra.core.Type body = (ft).value.body;
        hydra.core.Name v = (ft).value.parameter;
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.<T0>encodeType(
            cx,
            g,
            body),
          (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>>) (sbody -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Lambda(new hydra.ext.scala.meta.Type_Lambda(hydra.util.ConsList.of(hydra.ext.scala.Utils.stparam(v)), sbody)))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type> visit(hydra.core.Type.Variable v) {
        String rawName = (v).value.value;
        hydra.util.Lazy<String> typeName = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Elem.apply(
            46,
            hydra.lib.strings.ToList.apply(rawName)),
          () -> rawName,
          () -> hydra.Formatting.capitalize(rawName)));
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Type>right(new hydra.ext.scala.meta.Type.Var(new hydra.ext.scala.meta.Type_Var(new hydra.ext.scala.meta.Type_Name(typeName.get()))));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data> encodeUntypeApplicationTerm(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Inference.inferInGraphContext(
        cx,
        g,
        term),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data>>) (result -> hydra.ext.scala.Coder.encodeTerm(
        cx,
        g,
        (result).term)));
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data_Param> fieldToParam(hydra.context.Context cx, T0 g, hydra.core.FieldType ft) {
    String fname = hydra.ext.scala.Utils.scalaEscapeName((ft).name.value);
    hydra.core.Type ftyp = (ft).type;
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.scala.Coder.<T0>encodeType(
        cx,
        g,
        ftyp),
      (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data_Param>>) (sftyp -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data_Param>right(new hydra.ext.scala.meta.Data_Param((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), new hydra.ext.scala.meta.Name.Value(fname), hydra.util.Maybe.just(sftyp), (hydra.util.Maybe<hydra.ext.scala.meta.Data>) (hydra.util.Maybe.<hydra.ext.scala.meta.Data>nothing())))));
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat> fieldToEnumCase(hydra.context.Context cx, T0 g, String parentName, hydra.util.ConsList<hydra.ext.scala.meta.Type_Param> tparams, hydra.core.FieldType ft) {
    String fname = hydra.ext.scala.Utils.scalaEscapeName((ft).name.value);
    hydra.ext.scala.meta.Data_Name caseName = new hydra.ext.scala.meta.Data_Name(new hydra.ext.scala.meta.PredefString(fname));
    hydra.core.Type ftyp = (ft).type;
    hydra.util.Lazy<Boolean> isUnit = new hydra.util.Lazy<>(() -> hydra.Rewriting.deannotateType(ftyp).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Type.Unit ignored) {
        return true;
      }

      @Override
      public Boolean visit(hydra.core.Type.Record rt) {
        return hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply((rt).value),
          0);
      }
    }));
    hydra.util.Lazy<hydra.ext.scala.meta.Type> parentType = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(tparams),
      () -> new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name(parentName))),
      () -> new hydra.ext.scala.meta.Type.Apply(new hydra.ext.scala.meta.Type_Apply(new hydra.ext.scala.meta.Type.Ref(new hydra.ext.scala.meta.Type_Ref.Name(new hydra.ext.scala.meta.Type_Name(parentName))), hydra.lib.lists.Map.apply(
        hydra.ext.scala.Coder::typeParamToTypeVar,
        tparams)))));
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.scala.Coder.<T0>encodeType(
        cx,
        g,
        ftyp),
      (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (sftyp -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>right(new hydra.ext.scala.meta.Stat.Defn(new hydra.ext.scala.meta.Defn.EnumCase(new hydra.ext.scala.meta.Defn_EnumCase((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), caseName, (hydra.util.ConsList<hydra.ext.scala.meta.Type_Param>) (hydra.util.ConsList.<hydra.ext.scala.meta.Type_Param>empty()), new hydra.ext.scala.meta.Ctor_Primary((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), new hydra.ext.scala.meta.Name.Value(""), hydra.util.ConsList.of(hydra.lib.logic.IfElse.lazy(
        isUnit.get(),
        () -> (hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>) (hydra.util.ConsList.<hydra.ext.scala.meta.Data_Param>empty()),
        () -> hydra.util.ConsList.of(new hydra.ext.scala.meta.Data_Param((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), new hydra.ext.scala.meta.Name.Value("value"), hydra.util.Maybe.just(sftyp), (hydra.util.Maybe<hydra.ext.scala.meta.Data>) (hydra.util.Maybe.<hydra.ext.scala.meta.Data>nothing())))))), hydra.util.ConsList.of(new hydra.ext.scala.meta.Init(parentType.get(), new hydra.ext.scala.meta.Name.Value(""), (hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.meta.Data>>) (hydra.util.ConsList.<hydra.util.ConsList<hydra.ext.scala.meta.Data>>empty())))))))));
  }

  static hydra.ext.scala.meta.Type typeParamToTypeVar(hydra.ext.scala.meta.Type_Param tp) {
    hydra.ext.scala.meta.Name n = (tp).name;
    String s = (n).accept(new hydra.ext.scala.meta.Name.PartialVisitor<>() {
      @Override
      public String otherwise(hydra.ext.scala.meta.Name instance) {
        return "";
      }

      @Override
      public String visit(hydra.ext.scala.meta.Name.Value v) {
        return (v).value;
      }
    });
    return new hydra.ext.scala.meta.Type.Var(new hydra.ext.scala.meta.Type_Var(new hydra.ext.scala.meta.Type_Name(s)));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat> encodeTermDefinition(hydra.context.Context cx, hydra.graph.Graph g, hydra.module.TermDefinition td) {
    hydra.core.TypeScheme typ = (td).type;
    hydra.core.Type typ_ = (typ).type;
    Boolean isFunctionType = hydra.Rewriting.deannotateType(typ_).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Type.Function ignored) {
        return true;
      }

      @Override
      public Boolean visit(hydra.core.Type.Forall fa) {
        return hydra.Rewriting.deannotateType((fa).value.body).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Type instance) {
            return false;
          }

          @Override
          public Boolean visit(hydra.core.Type.Function ignored) {
            return true;
          }
        });
      }
    });
    hydra.core.Name name = (td).name;
    String lname = hydra.ext.scala.Utils.scalaEscapeName(hydra.Names.localNameOf(name));
    hydra.core.Term term = (td).term;
    return hydra.lib.logic.IfElse.lazy(
      isFunctionType,
      () -> hydra.ext.scala.Coder.encodeComplexTermDef(
        cx,
        g,
        lname,
        term,
        typ_),
      () -> hydra.lib.eithers.Bind.apply(
        hydra.ext.scala.Coder.encodeType(
          cx,
          g,
          typ_),
        (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (stype -> hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.encodeTerm(
            cx,
            g,
            term),
          (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (rhs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>right(new hydra.ext.scala.meta.Stat.Defn(new hydra.ext.scala.meta.Defn.Val(new hydra.ext.scala.meta.Defn_Val(hydra.util.ConsList.of(new hydra.ext.scala.meta.Mod.Lazy()), hydra.util.ConsList.of(new hydra.ext.scala.meta.Pat.Var(new hydra.ext.scala.meta.Pat_Var(new hydra.ext.scala.meta.Data_Name(new hydra.ext.scala.meta.PredefString(lname))))), hydra.util.Maybe.just(stype), rhs)))))))));
  }

  static hydra.util.ConsList<hydra.core.Type> extractDomains(hydra.core.Type t) {
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty());
      }

      @Override
      public hydra.util.ConsList<hydra.core.Type> visit(hydra.core.Type.Function ft) {
        return hydra.lib.lists.Cons.apply(
          (ft).value.domain,
          hydra.ext.scala.Coder.extractDomains((ft).value.codomain));
      }

      @Override
      public hydra.util.ConsList<hydra.core.Type> visit(hydra.core.Type.Forall fa) {
        return hydra.ext.scala.Coder.extractDomains((fa).value.body);
      }
    });
  }

  static hydra.core.Type extractCodomain(hydra.core.Type t) {
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return t;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Function ft) {
        return hydra.ext.scala.Coder.extractCodomain((ft).value.codomain);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall fa) {
        return hydra.ext.scala.Coder.extractCodomain((fa).value.body);
      }
    });
  }

  static hydra.util.ConsList<hydra.core.Name> extractParams(hydra.core.Term t) {
    return hydra.Rewriting.deannotateAndDetypeTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.core.Name> otherwise(hydra.core.Term instance) {
        return (hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty());
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.ConsList<hydra.core.Name> otherwise(hydra.core.Function instance) {
            return (hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty());
          }

          @Override
          public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Function.Lambda lam) {
            return hydra.lib.lists.Cons.apply(
              (lam).value.parameter,
              hydra.ext.scala.Coder.extractParams((lam).value.body));
          }
        });
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Term.TypeLambda tl) {
        return hydra.ext.scala.Coder.extractParams((tl).value.body);
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Term.TypeApplication ta) {
        return hydra.ext.scala.Coder.extractParams((ta).value.body);
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Term.Let lt) {
        return hydra.ext.scala.Coder.extractParams((lt).value.body);
      }
    });
  }

  static hydra.core.Term extractBody(hydra.core.Term t) {
    return hydra.Rewriting.deannotateAndDetypeTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return t;
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda lam) {
            return hydra.ext.scala.Coder.extractBody((lam).value.body);
          }
        });
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
        return hydra.ext.scala.Coder.extractBody((tl).value.body);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication ta) {
        return hydra.ext.scala.Coder.extractBody((ta).value.body);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        return hydra.ext.scala.Coder.extractBody((lt).value.body);
      }
    });
  }

  static hydra.util.ConsList<hydra.core.Binding> extractLetBindings(hydra.core.Term t) {
    return hydra.Rewriting.deannotateAndDetypeTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.core.Binding> otherwise(hydra.core.Term instance) {
        return (hydra.util.ConsList<hydra.core.Binding>) (hydra.util.ConsList.<hydra.core.Binding>empty());
      }

      @Override
      public hydra.util.ConsList<hydra.core.Binding> visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.ConsList<hydra.core.Binding> otherwise(hydra.core.Function instance) {
            return (hydra.util.ConsList<hydra.core.Binding>) (hydra.util.ConsList.<hydra.core.Binding>empty());
          }

          @Override
          public hydra.util.ConsList<hydra.core.Binding> visit(hydra.core.Function.Lambda lam) {
            return hydra.ext.scala.Coder.extractLetBindings((lam).value.body);
          }
        });
      }

      @Override
      public hydra.util.ConsList<hydra.core.Binding> visit(hydra.core.Term.TypeLambda tl) {
        return hydra.ext.scala.Coder.extractLetBindings((tl).value.body);
      }

      @Override
      public hydra.util.ConsList<hydra.core.Binding> visit(hydra.core.Term.TypeApplication ta) {
        return hydra.ext.scala.Coder.extractLetBindings((ta).value.body);
      }

      @Override
      public hydra.util.ConsList<hydra.core.Binding> visit(hydra.core.Term.Let lt) {
        return hydra.lib.lists.Concat2.apply(
          (lt).value.bindings,
          hydra.ext.scala.Coder.extractLetBindings((lt).value.body));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat> encodeComplexTermDef(hydra.context.Context cx, hydra.graph.Graph g, String lname, hydra.core.Term term, hydra.core.Type typ) {
    hydra.core.Type cod = hydra.ext.scala.Coder.extractCodomain(typ);
    hydra.util.ConsList<hydra.core.Type> doms = hydra.ext.scala.Coder.extractDomains(typ);
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> freeTypeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.logic.Not.apply(hydra.lib.lists.Elem.apply(
        46,
        hydra.lib.strings.ToList.apply((v).value)))),
      hydra.lib.sets.ToList.apply(hydra.Rewriting.freeVariablesInType(typ))));
    hydra.util.Lazy<hydra.graph.Graph> gWithTypeVars = new hydra.util.Lazy<>(() -> new hydra.graph.Graph((g).boundTerms, (g).boundTypes, (g).classConstraints, (g).lambdaVariables, (g).metadata, (g).primitives, (g).schemaTypes, hydra.lib.sets.Union.apply(
      hydra.lib.sets.FromList.apply(freeTypeVars.get()),
      (g).typeVariables)));
    hydra.util.ConsList<hydra.core.Binding> letBindings = hydra.ext.scala.Coder.extractLetBindings(term);
    hydra.util.ConsList<hydra.core.Name> paramNames = hydra.ext.scala.Coder.extractParams(term);
    hydra.util.Lazy<Integer> paramCount = new hydra.util.Lazy<>(() -> hydra.lib.math.Min.apply(
      hydra.lib.lists.Length.apply(paramNames),
      hydra.lib.lists.Length.apply(doms)));
    hydra.util.Lazy<hydra.util.ConsList<hydra.ext.scala.meta.Type_Param>> tparams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.ext.scala.meta.Type_Param>) (tv -> hydra.ext.scala.Utils.stparam(tv)),
      freeTypeVars.get()));
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Type>>> zippedParams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Zip.apply(
      hydra.lib.lists.Take.apply(
        paramCount.get(),
        paramNames),
      hydra.lib.lists.Take.apply(
        paramCount.get(),
        doms)));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data_Param>>) (v1 -> hydra.ext.scala.Coder.encodeTypedParam(
          cx,
          gWithTypeVars.get(),
          v1)),
        zippedParams.get()),
      (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (sparams -> hydra.lib.eithers.Bind.apply(
        hydra.ext.scala.Coder.encodeTerm(
          cx,
          gWithTypeVars.get(),
          hydra.ext.scala.Coder.extractBody(term)),
        (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (sbody -> hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.encodeType(
            cx,
            g,
            cod),
          (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (scod -> {
            hydra.util.Lazy<hydra.graph.Graph> gForLets = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply(letBindings),
              () -> gWithTypeVars.get(),
              () -> hydra.Rewriting.extendGraphForLet(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (p0 -> p1 -> hydra.CoderUtils.bindingMetadata(
                  p0,
                  p1)),
                gWithTypeVars.get(),
                new hydra.core.Let(letBindings, new hydra.core.Term.Variable(new hydra.core.Name("dummy"))))));
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.MapList.apply(
                (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (v1 -> hydra.ext.scala.Coder.encodeLetBinding(
                  cx,
                  gForLets.get(),
                  hydra.lib.sets.FromList.apply(freeTypeVars.get()),
                  v1)),
                letBindings),
              (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Stat>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (sbindings -> {
                hydra.util.Lazy<hydra.ext.scala.meta.Data> defBody = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.lists.Null.apply(sbindings),
                  () -> sbody,
                  () -> new hydra.ext.scala.meta.Data.Block(new hydra.ext.scala.meta.Data_Block(hydra.lib.lists.Concat2.apply(
                    sbindings,
                    hydra.util.ConsList.of(new hydra.ext.scala.meta.Stat.Term(sbody)))))));
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>right(new hydra.ext.scala.meta.Stat.Defn(new hydra.ext.scala.meta.Defn.Def(new hydra.ext.scala.meta.Defn_Def((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), new hydra.ext.scala.meta.Data_Name(new hydra.ext.scala.meta.PredefString(lname)), tparams.get(), hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.ext.scala.meta.Data_Param, hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>>) (p -> hydra.util.ConsList.of(p)),
                  sparams), hydra.util.Maybe.just(scod), defBody.get()))));
              }));
          }))))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat> encodeLocalDef(hydra.context.Context cx, hydra.graph.Graph g, hydra.util.PersistentSet<hydra.core.Name> outerTypeVars, String lname, hydra.core.Term term, hydra.core.Type typ) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> freeTypeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.logic.And.apply(
        hydra.lib.logic.Not.apply(hydra.lib.lists.Elem.apply(
          46,
          hydra.lib.strings.ToList.apply((v).value))),
        hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
          v,
          outerTypeVars)))),
      hydra.lib.sets.ToList.apply(hydra.Rewriting.freeVariablesInType(typ))));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> allTypeVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.Union.apply(
      outerTypeVars,
      hydra.lib.sets.FromList.apply(freeTypeVars.get())));
    hydra.core.Type cod = hydra.ext.scala.Coder.extractCodomain(typ);
    hydra.util.ConsList<hydra.core.Type> doms = hydra.ext.scala.Coder.extractDomains(typ);
    hydra.util.Lazy<hydra.graph.Graph> gWithTypeVars = new hydra.util.Lazy<>(() -> new hydra.graph.Graph((g).boundTerms, (g).boundTypes, (g).classConstraints, (g).lambdaVariables, (g).metadata, (g).primitives, (g).schemaTypes, hydra.lib.sets.Union.apply(
      allTypeVars.get(),
      (g).typeVariables)));
    hydra.util.ConsList<hydra.core.Binding> letBindings = hydra.ext.scala.Coder.extractLetBindings(term);
    hydra.util.ConsList<hydra.core.Name> paramNames = hydra.ext.scala.Coder.extractParams(term);
    hydra.util.Lazy<Integer> paramCount = new hydra.util.Lazy<>(() -> hydra.lib.math.Min.apply(
      hydra.lib.lists.Length.apply(paramNames),
      hydra.lib.lists.Length.apply(doms)));
    hydra.util.Lazy<hydra.util.ConsList<hydra.ext.scala.meta.Type_Param>> tparams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.ext.scala.meta.Type_Param>) (tv -> hydra.ext.scala.Utils.stparam(tv)),
      freeTypeVars.get()));
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Type>>> zippedParams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Zip.apply(
      hydra.lib.lists.Take.apply(
        paramCount.get(),
        paramNames),
      hydra.lib.lists.Take.apply(
        paramCount.get(),
        doms)));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data_Param>>) (v1 -> hydra.ext.scala.Coder.encodeTypedParam(
          cx,
          gWithTypeVars.get(),
          v1)),
        zippedParams.get()),
      (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (sparams -> hydra.lib.eithers.Bind.apply(
        hydra.ext.scala.Coder.encodeTerm(
          cx,
          gWithTypeVars.get(),
          hydra.ext.scala.Coder.extractBody(term)),
        (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (sbody -> hydra.lib.eithers.Bind.apply(
          hydra.ext.scala.Coder.encodeType(
            cx,
            gWithTypeVars.get(),
            cod),
          (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (scod -> {
            hydra.util.Lazy<hydra.graph.Graph> gForLets = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply(letBindings),
              () -> gWithTypeVars.get(),
              () -> hydra.Rewriting.extendGraphForLet(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (p0 -> p1 -> hydra.CoderUtils.bindingMetadata(
                  p0,
                  p1)),
                gWithTypeVars.get(),
                new hydra.core.Let(letBindings, new hydra.core.Term.Variable(new hydra.core.Name("dummy"))))));
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.MapList.apply(
                (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (v1 -> hydra.ext.scala.Coder.encodeLetBinding(
                  cx,
                  gForLets.get(),
                  allTypeVars.get(),
                  v1)),
                letBindings),
              (java.util.function.Function<hydra.util.ConsList<hydra.ext.scala.meta.Stat>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (sbindings -> {
                hydra.util.Lazy<hydra.ext.scala.meta.Data> defBody = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.lists.Null.apply(sbindings),
                  () -> sbody,
                  () -> new hydra.ext.scala.meta.Data.Block(new hydra.ext.scala.meta.Data_Block(hydra.lib.lists.Concat2.apply(
                    sbindings,
                    hydra.util.ConsList.of(new hydra.ext.scala.meta.Stat.Term(sbody)))))));
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>right(new hydra.ext.scala.meta.Stat.Defn(new hydra.ext.scala.meta.Defn.Def(new hydra.ext.scala.meta.Defn_Def((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), new hydra.ext.scala.meta.Data_Name(new hydra.ext.scala.meta.PredefString(lname)), tparams.get(), hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.ext.scala.meta.Data_Param, hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>>) (p -> hydra.util.ConsList.of(p)),
                  sparams), hydra.util.Maybe.just(scod), defBody.get()))));
              }));
          }))))));
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data_Param> encodeTypedParam(hydra.context.Context cx, T0 g, hydra.util.Pair<hydra.core.Name, hydra.core.Type> pair) {
    hydra.util.Lazy<hydra.core.Type> pdom = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
    hydra.util.Lazy<String> pname = new hydra.util.Lazy<>(() -> hydra.ext.scala.Utils.scalaEscapeName(hydra.Names.localNameOf(hydra.lib.pairs.First.apply(pair))));
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.scala.Coder.<T0>encodeType(
        cx,
        g,
        pdom.get()),
      (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data_Param>>) (sdom -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Data_Param>right(new hydra.ext.scala.meta.Data_Param((hydra.util.ConsList<hydra.ext.scala.meta.Mod>) (hydra.util.ConsList.<hydra.ext.scala.meta.Mod>empty()), new hydra.ext.scala.meta.Name.Value(pname.get()), hydra.util.Maybe.just(sdom), (hydra.util.Maybe<hydra.ext.scala.meta.Data>) (hydra.util.Maybe.<hydra.ext.scala.meta.Data>nothing())))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat> encodeLetBinding(hydra.context.Context cx, hydra.graph.Graph g, hydra.util.PersistentSet<hydra.core.Name> outerTypeVars, hydra.core.Binding b) {
    String bname = hydra.ext.scala.Utils.scalaEscapeName((b).name.value);
    hydra.core.Term bterm = (b).term;
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.TypeScheme>> mts = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.lib.maps.Lookup.apply(
        (b).name,
        (g).boundTypes),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Maybe<hydra.core.TypeScheme>>) (ts -> hydra.util.Maybe.just(ts)),
      (b).type));
    hydra.util.Lazy<Boolean> isFn = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> false,
      (java.util.function.Function<hydra.core.TypeScheme, Boolean>) (ts -> hydra.Rewriting.deannotateType((ts).type).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public Boolean otherwise(hydra.core.Type instance) {
          return false;
        }

        @Override
        public Boolean visit(hydra.core.Type.Function ignored) {
          return true;
        }

        @Override
        public Boolean visit(hydra.core.Type.Forall fa) {
          return hydra.Rewriting.deannotateType((fa).value.body).accept(new hydra.core.Type.PartialVisitor<>() {
            @Override
            public Boolean otherwise(hydra.core.Type instance) {
              return false;
            }

            @Override
            public Boolean visit(hydra.core.Type.Function ignored) {
              return true;
            }
          });
        }
      })),
      mts.get()));
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.lib.eithers.Bind.apply(
        hydra.ext.scala.Coder.encodeTerm(
          cx,
          g,
          bterm),
        (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (srhs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>right(new hydra.ext.scala.meta.Stat.Defn(new hydra.ext.scala.meta.Defn.Val(new hydra.ext.scala.meta.Defn_Val(hydra.util.ConsList.of(new hydra.ext.scala.meta.Mod.Lazy()), hydra.util.ConsList.of(new hydra.ext.scala.meta.Pat.Var(new hydra.ext.scala.meta.Pat_Var(new hydra.ext.scala.meta.Data_Name(new hydra.ext.scala.meta.PredefString(bname))))), (hydra.util.Maybe<hydra.ext.scala.meta.Type>) (hydra.util.Maybe.<hydra.ext.scala.meta.Type>nothing()), srhs)))))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (ts -> {
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> newVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
            v,
            outerTypeVars))),
          (ts).variables));
        hydra.util.Lazy<Boolean> useDef = new hydra.util.Lazy<>(() -> hydra.lib.logic.Or.apply(
          isFn.get(),
          hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(newVars.get()))));
        return hydra.lib.logic.IfElse.lazy(
          useDef.get(),
          () -> hydra.ext.scala.Coder.encodeLocalDef(
            cx,
            g,
            outerTypeVars,
            bname,
            bterm,
            (ts).type),
          () -> hydra.lib.eithers.Bind.apply(
            hydra.ext.scala.Coder.encodeTerm(
              cx,
              g,
              bterm),
            (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (srhs -> hydra.lib.eithers.Bind.apply(
              hydra.ext.scala.Coder.encodeType(
                cx,
                g,
                (ts).type),
              (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>>) (styp -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Stat>right(new hydra.ext.scala.meta.Stat.Defn(new hydra.ext.scala.meta.Defn.Val(new hydra.ext.scala.meta.Defn_Val(hydra.util.ConsList.of(new hydra.ext.scala.meta.Mod.Lazy()), hydra.util.ConsList.of(new hydra.ext.scala.meta.Pat.Var(new hydra.ext.scala.meta.Pat_Var(new hydra.ext.scala.meta.Data_Name(new hydra.ext.scala.meta.PredefString(bname))))), hydra.util.Maybe.just(styp), srhs)))))))));
      }),
      mts.get());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>> findSdom(hydra.context.Context cx, hydra.graph.Graph g, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> meta) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.errors.DecodingError, hydra.context.InContext<hydra.errors.Error_>>) (_de -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError((_de).value)), cx))),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (_a -> _a),
        hydra.Annotations.getType(
          g,
          meta)),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>>) (mtyp -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>right((hydra.util.Maybe<hydra.ext.scala.meta.Type>) (hydra.util.Maybe.<hydra.ext.scala.meta.Type>nothing())),
        (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>>) (t -> hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>> otherwise(hydra.core.Type instance) {
            return hydra.lib.eithers.Bind.apply(
              hydra.ext.scala.Coder.encodeType(
                cx,
                g,
                t),
              (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>>) (st -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>right(hydra.util.Maybe.just(st))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>> visit(hydra.core.Type.Function ft) {
            hydra.core.Type dom = (ft).value.domain;
            return hydra.lib.eithers.Bind.apply(
              hydra.ext.scala.Coder.encodeType(
                cx,
                g,
                dom),
              (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>>) (sdom -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>right(hydra.util.Maybe.just(sdom))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>> visit(hydra.core.Type.Forall fa) {
            return hydra.Rewriting.deannotateType((fa).value.body).accept(new hydra.core.Type.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>> otherwise(hydra.core.Type instance) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>right((hydra.util.Maybe<hydra.ext.scala.meta.Type>) (hydra.util.Maybe.<hydra.ext.scala.meta.Type>nothing()));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>> visit(hydra.core.Type.Function ft2) {
                hydra.core.Type dom2 = (ft2).value.domain;
                return hydra.lib.eithers.Bind.apply(
                  hydra.ext.scala.Coder.encodeType(
                    cx,
                    g,
                    dom2),
                  (java.util.function.Function<hydra.ext.scala.meta.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>>) (sdom2 -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.scala.meta.Type>>right(hydra.util.Maybe.just(sdom2))));
              }
            });
          }
        })),
        mtyp)));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type> findDomain(hydra.context.Context cx, hydra.graph.Graph g, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> meta) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.errors.DecodingError, hydra.context.InContext<hydra.errors.Error_>>) (_de -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError((_de).value)), cx))),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (_a -> _a),
        hydra.Annotations.getType(
          g,
          meta)),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (r -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("expected a typed term")), cx))),
        (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (t -> hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("expected a function type")), cx)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type> visit(hydra.core.Type.Function ft) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>right((ft).value.domain);
          }
        })),
        r)));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Case> encodeCase(hydra.context.Context cx, hydra.graph.Graph g, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> ftypes, hydra.util.Maybe<hydra.core.Name> sn, hydra.core.Field f) {
    hydra.core.Term fterm = (f).term;
    hydra.core.Name fname = (f).name;
    hydra.util.Lazy<String> lamParamSuffix = new hydra.util.Lazy<>(() -> hydra.Rewriting.deannotateAndDetypeTerm(fterm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public String otherwise(hydra.core.Term instance) {
        return "";
      }

      @Override
      public String visit(hydra.core.Term.Function fn) {
        return (fn).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public String otherwise(hydra.core.Function instance) {
            return "";
          }

          @Override
          public String visit(hydra.core.Function.Lambda lam) {
            String rawName = (lam).value.parameter.value;
            hydra.util.Lazy<String> safeName = new hydra.util.Lazy<>(() -> hydra.lib.strings.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<Integer, Integer>) (c -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.equality.Equal.apply(
                  c,
                  39),
                () -> 95,
                () -> c)),
              hydra.lib.strings.ToList.apply(rawName))));
            return hydra.lib.strings.Cat2.apply(
              "_",
              safeName.get());
          }
        });
      }
    }));
    hydra.util.Lazy<String> shortTypeName = new hydra.util.Lazy<>(() -> hydra.lib.lists.Last.apply(hydra.lib.strings.SplitOn.apply(
      ".",
      hydra.lib.maybes.Maybe.applyLazy(
        () -> "x",
        (java.util.function.Function<hydra.core.Name, String>) (n -> (n).value),
        sn))));
    hydra.core.Name v = new hydra.core.Name(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "v_",
      shortTypeName.get(),
      "_",
      (fname).value,
      lamParamSuffix.get())));
    hydra.core.Term applied = hydra.ext.scala.Coder.applyVar(
      fterm,
      v);
    hydra.util.Lazy<Boolean> isUnit = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.Rewriting.deannotateAndDetypeTerm(fterm).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public Boolean otherwise(hydra.core.Term instance) {
          return false;
        }

        @Override
        public Boolean visit(hydra.core.Term.Function fn) {
          return (fn).value.accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public Boolean otherwise(hydra.core.Function instance) {
              return false;
            }

            @Override
            public Boolean visit(hydra.core.Function.Lambda lam) {
              hydra.core.Term lamBody = (lam).value.body;
              hydra.core.Name lamParam = (lam).value.parameter;
              Boolean bodyIgnoresParam = hydra.Rewriting.isFreeVariableInTerm(
                lamParam,
                lamBody);
              hydra.util.Lazy<Boolean> domIsUnit = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
                () -> false,
                (java.util.function.Function<hydra.core.Type, Boolean>) (dom -> hydra.lib.equality.Equal.apply(
                  dom,
                  new hydra.core.Type.Unit())),
                (lam).value.domain));
              return hydra.lib.logic.Or.apply(
                domIsUnit.get(),
                bodyIgnoresParam);
            }
          });
        }

        @Override
        public Boolean visit(hydra.core.Term.Record r) {
          return hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply((r).value.fields),
            0);
        }

        @Override
        public Boolean visit(hydra.core.Term.Unit ignored) {
          return true;
        }
      }),
      (java.util.function.Function<hydra.core.Type, Boolean>) (dom -> hydra.Rewriting.deannotateType(dom).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public Boolean otherwise(hydra.core.Type instance) {
          return false;
        }

        @Override
        public Boolean visit(hydra.core.Type.Unit ignored) {
          return true;
        }

        @Override
        public Boolean visit(hydra.core.Type.Record rt) {
          return hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply((rt).value),
            0);
        }
      })),
      hydra.lib.maps.Lookup.apply(
        fname,
        ftypes)));
    hydra.util.Lazy<hydra.util.ConsList<hydra.ext.scala.meta.Pat>> patArgs = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      isUnit.get(),
      () -> (hydra.util.ConsList<hydra.ext.scala.meta.Pat>) (hydra.util.ConsList.<hydra.ext.scala.meta.Pat>empty()),
      () -> hydra.util.ConsList.of(hydra.ext.scala.Utils.svar(v))));
    hydra.ext.scala.meta.Pat pat = new hydra.ext.scala.meta.Pat.Extract(new hydra.ext.scala.meta.Pat_Extract(hydra.ext.scala.Utils.sname(hydra.ext.scala.Utils.qualifyUnionFieldName(
      "MATCHED.",
      sn,
      fname)), patArgs.get()));
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.scala.Coder.encodeTerm(
        cx,
        g,
        applied),
      (java.util.function.Function<hydra.ext.scala.meta.Data, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Case>>) (body -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.scala.meta.Case>right(new hydra.ext.scala.meta.Case(pat, (hydra.util.Maybe<hydra.ext.scala.meta.Data>) (hydra.util.Maybe.<hydra.ext.scala.meta.Data>nothing()), body))));
  }

  static hydra.core.Term applyVar(hydra.core.Term fterm, hydra.core.Name avar) {
    String v = (avar).value;
    return hydra.Rewriting.deannotateAndDetypeTerm(fterm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return new hydra.core.Term.Application(new hydra.core.Application(fterm, new hydra.core.Term.Variable(avar)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return new hydra.core.Term.Application(new hydra.core.Application(fterm, new hydra.core.Term.Variable(avar)));
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda lam) {
            hydra.core.Term lamBody = (lam).value.body;
            hydra.core.Name lamParam = (lam).value.parameter;
            return hydra.lib.logic.IfElse.lazy(
              hydra.Rewriting.isFreeVariableInTerm(
                lamParam,
                lamBody),
              () -> lamBody,
              () -> hydra.Rewriting.substituteVariable(
                lamParam,
                avar,
                lamBody));
          }
        });
      }
    });
  }
}
