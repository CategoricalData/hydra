// Note: this is an automatically generated file. Do not edit.

package hydra.validate;

/**
 * Validation functions for modules and packages
 */
public interface Packaging {
  static hydra.util.Maybe<hydra.error.packaging.InvalidPackageError> checkConflictingModuleNamespaces(hydra.packaging.Package_ pkg) {
    hydra.util.Lazy<hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>> result = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>, java.util.function.Function<hydra.module.Module, hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>>>) (acc -> (java.util.function.Function<hydra.module.Module, hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>>) (mod -> {
        hydra.util.Lazy<hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>> err = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(acc));
        hydra.util.Lazy<hydra.util.PersistentMap<String, hydra.module.Namespace>> seen = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(acc));
        return hydra.lib.maybes.Cases.applyLazy(
          err.get(),
          () -> ((java.util.function.Supplier<hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>>) (() -> {
            hydra.module.Namespace ns = (mod).namespace;
            return ((java.util.function.Supplier<hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>>) (() -> {
              String key = hydra.lib.strings.ToLower.apply((ns).value);
              return ((java.util.function.Supplier<hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>>) (() -> {
                hydra.util.Lazy<hydra.util.Maybe<hydra.module.Namespace>> existing = new hydra.util.Lazy<>(() -> hydra.lib.maps.Lookup.apply(
                  key,
                  seen.get()));
                return hydra.lib.maybes.Cases.applyLazy(
                  existing.get(),
                  () -> (hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) ((hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) (new hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>(hydra.lib.maps.Insert.apply(
                    key,
                    ns,
                    seen.get()), (hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>) (hydra.util.Maybe.<hydra.error.packaging.InvalidPackageError>nothing())))),
                  (java.util.function.Function<hydra.module.Namespace, hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>>) (first -> (hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) ((hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) (new hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>(seen.get(), hydra.util.Maybe.just(new hydra.error.packaging.InvalidPackageError.ConflictingModuleNamespace(new hydra.error.packaging.ConflictingModuleNamespaceError(first, ns))))))));
              })).get();
            })).get();
          })).get(),
          (java.util.function.Function<hydra.error.packaging.InvalidPackageError, hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>>) (ignored -> acc));
      })),
      (hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) ((hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) (new hydra.util.Pair<hydra.util.PersistentMap<String, hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>((hydra.util.PersistentMap<String, hydra.module.Namespace>) ((hydra.util.PersistentMap<String, hydra.module.Namespace>) (hydra.lib.maps.Empty.<String, hydra.module.Namespace>apply())), (hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>) (hydra.util.Maybe.<hydra.error.packaging.InvalidPackageError>nothing())))),
      (pkg).modules));
    return hydra.lib.pairs.Second.apply(result.get());
  }

  static hydra.util.Maybe<hydra.error.packaging.InvalidModuleError> checkConflictingVariantNames(hydra.module.Module mod) {
    hydra.util.ConsList<hydra.module.Definition> defs = (mod).definitions;
    hydra.util.Lazy<hydra.util.PersistentSet<String>> defNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.PersistentSet<String>, java.util.function.Function<hydra.module.Definition, hydra.util.PersistentSet<String>>>) (acc -> (java.util.function.Function<hydra.module.Definition, hydra.util.PersistentSet<String>>) (def -> hydra.lib.sets.Insert.apply(
        hydra.Names.localNameOf(hydra.validate.Packaging.definitionName(def)),
        acc))),
      (hydra.util.PersistentSet<String>) (hydra.lib.sets.Empty.<String>apply()),
      defs));
    hydra.module.Namespace ns = (mod).namespace;
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>, java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>>) (acc -> (java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (def -> hydra.lib.maybes.Cases.applyLazy(
        acc,
        () -> (def).accept(new hydra.module.Definition.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.error.packaging.InvalidModuleError> otherwise(hydra.module.Definition instance) {
            return (hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>) (hydra.util.Maybe.<hydra.error.packaging.InvalidModuleError>nothing());
          }

          @Override
          public hydra.util.Maybe<hydra.error.packaging.InvalidModuleError> visit(hydra.module.Definition.Type td) {
            hydra.core.Name typeName = (td).value.name;
            String localTypeName = hydra.Names.localNameOf(typeName);
            hydra.core.Type typ = (td).value.type;
            return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
              @Override
              public hydra.util.Maybe<hydra.error.packaging.InvalidModuleError> otherwise(hydra.core.Type instance) {
                return (hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>) (hydra.util.Maybe.<hydra.error.packaging.InvalidModuleError>nothing());
              }

              @Override
              public hydra.util.Maybe<hydra.error.packaging.InvalidModuleError> visit(hydra.core.Type.Union fields) {
                return hydra.lib.lists.Foldl.apply(
                  (java.util.function.Function<hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>, java.util.function.Function<hydra.core.FieldType, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>>) (innerAcc -> (java.util.function.Function<hydra.core.FieldType, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (field -> hydra.lib.maybes.Cases.applyLazy(
                    innerAcc,
                    () -> ((java.util.function.Supplier<hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (() -> {
                      hydra.core.Name fieldName = (field).name;
                      return ((java.util.function.Supplier<hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (() -> {
                        String localFieldName = hydra.Names.localNameOf(fieldName);
                        return ((java.util.function.Supplier<hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (() -> {
                          String constructorName = hydra.lib.strings.Cat2.apply(
                            hydra.Formatting.capitalize(localTypeName),
                            hydra.Formatting.capitalize(localFieldName));
                          return hydra.lib.logic.IfElse.lazy(
                            hydra.lib.sets.Member.apply(
                              constructorName,
                              defNames.get()),
                            () -> hydra.util.Maybe.just(new hydra.error.packaging.InvalidModuleError.ConflictingVariantName(new hydra.error.packaging.ConflictingVariantNameError(ns, typeName, fieldName, new hydra.core.Name(constructorName)))),
                            () -> (hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>) (hydra.util.Maybe.<hydra.error.packaging.InvalidModuleError>nothing()));
                        })).get();
                      })).get();
                    })).get(),
                    (java.util.function.Function<hydra.error.packaging.InvalidModuleError, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (ignored -> innerAcc)))),
                  (hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>) (hydra.util.Maybe.<hydra.error.packaging.InvalidModuleError>nothing()),
                  (fields).value);
              }
            });
          }
        }),
        (java.util.function.Function<hydra.error.packaging.InvalidModuleError, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (ignored -> acc)))),
      (hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>) (hydra.util.Maybe.<hydra.error.packaging.InvalidModuleError>nothing()),
      defs);
  }

  static hydra.util.Maybe<hydra.error.packaging.InvalidModuleError> checkDefinitionNamespaces(hydra.module.Module mod) {
    hydra.module.Namespace ns = (mod).namespace;
    String prefix = hydra.lib.strings.Cat2.apply(
      (ns).value,
      ".");
    Integer prefixLen = hydra.lib.strings.Length.apply(prefix);
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>, java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>>) (acc -> (java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (def -> hydra.lib.maybes.Cases.applyLazy(
        acc,
        () -> ((java.util.function.Supplier<hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (() -> {
          hydra.core.Name name = hydra.validate.Packaging.definitionName(def);
          return ((java.util.function.Supplier<hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (() -> {
            String nameStr = (name).value;
            return ((java.util.function.Supplier<hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (() -> {
              hydra.util.Lazy<hydra.util.ConsList<Integer>> namePrefix = new hydra.util.Lazy<>(() -> hydra.lib.lists.Take.apply(
                prefixLen,
                hydra.lib.strings.ToList.apply(nameStr)));
              return hydra.lib.logic.IfElse.lazy(
                hydra.lib.equality.Equal.apply(
                  hydra.lib.strings.FromList.apply(namePrefix.get()),
                  prefix),
                () -> (hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>) (hydra.util.Maybe.<hydra.error.packaging.InvalidModuleError>nothing()),
                () -> hydra.util.Maybe.just(new hydra.error.packaging.InvalidModuleError.DefinitionNotInModuleNamespace(new hydra.error.packaging.DefinitionNotInModuleNamespaceError(ns, name))));
            })).get();
          })).get();
        })).get(),
        (java.util.function.Function<hydra.error.packaging.InvalidModuleError, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (ignored -> acc)))),
      (hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>) (hydra.util.Maybe.<hydra.error.packaging.InvalidModuleError>nothing()),
      (mod).definitions);
  }

  static hydra.util.Maybe<hydra.error.packaging.InvalidModuleError> checkDuplicateDefinitionNames(hydra.module.Module mod) {
    hydra.module.Namespace ns = (mod).namespace;
    hydra.util.Lazy<hydra.util.Pair<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>> result = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>, java.util.function.Function<hydra.module.Definition, hydra.util.Pair<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>>>) (acc -> (java.util.function.Function<hydra.module.Definition, hydra.util.Pair<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>>) (def -> {
        hydra.util.Lazy<hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>> err = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(acc));
        hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> seen = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(acc));
        return hydra.lib.maybes.Cases.applyLazy(
          err.get(),
          () -> ((java.util.function.Supplier<hydra.util.Pair<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>>) (() -> {
            hydra.core.Name name = hydra.validate.Packaging.definitionName(def);
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.sets.Member.apply(
                name,
                seen.get()),
              () -> (hydra.util.Pair<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) ((hydra.util.Pair<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (new hydra.util.Pair<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>(seen.get(), hydra.util.Maybe.just(new hydra.error.packaging.InvalidModuleError.DuplicateDefinitionName(new hydra.error.packaging.DuplicateDefinitionNameError(ns, name)))))),
              () -> (hydra.util.Pair<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) ((hydra.util.Pair<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (new hydra.util.Pair<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>(hydra.lib.sets.Insert.apply(
                name,
                seen.get()), (hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>) (hydra.util.Maybe.<hydra.error.packaging.InvalidModuleError>nothing())))));
          })).get(),
          (java.util.function.Function<hydra.error.packaging.InvalidModuleError, hydra.util.Pair<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>>) (ignored -> acc));
      })),
      (hydra.util.Pair<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) ((hydra.util.Pair<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (new hydra.util.Pair<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>((hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>) (hydra.util.Maybe.<hydra.error.packaging.InvalidModuleError>nothing())))),
      (mod).definitions));
    return hydra.lib.pairs.Second.apply(result.get());
  }

  static hydra.util.Maybe<hydra.error.packaging.InvalidPackageError> checkDuplicateModuleNamespaces(hydra.packaging.Package_ pkg) {
    hydra.util.Lazy<hydra.util.Pair<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>> result = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>, java.util.function.Function<hydra.module.Module, hydra.util.Pair<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>>>) (acc -> (java.util.function.Function<hydra.module.Module, hydra.util.Pair<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>>) (mod -> {
        hydra.util.Lazy<hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>> err = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(acc));
        hydra.util.Lazy<hydra.util.PersistentSet<hydra.module.Namespace>> seen = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(acc));
        return hydra.lib.maybes.Cases.applyLazy(
          err.get(),
          () -> ((java.util.function.Supplier<hydra.util.Pair<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>>) (() -> {
            hydra.module.Namespace ns = (mod).namespace;
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.sets.Member.apply(
                ns,
                seen.get()),
              () -> (hydra.util.Pair<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) ((hydra.util.Pair<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) (new hydra.util.Pair<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>(seen.get(), hydra.util.Maybe.just(new hydra.error.packaging.InvalidPackageError.DuplicateModuleNamespace(new hydra.error.packaging.DuplicateModuleNamespaceError(ns)))))),
              () -> (hydra.util.Pair<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) ((hydra.util.Pair<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) (new hydra.util.Pair<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>(hydra.lib.sets.Insert.apply(
                ns,
                seen.get()), (hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>) (hydra.util.Maybe.<hydra.error.packaging.InvalidPackageError>nothing())))));
          })).get(),
          (java.util.function.Function<hydra.error.packaging.InvalidPackageError, hydra.util.Pair<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>>) (ignored -> acc));
      })),
      (hydra.util.Pair<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) ((hydra.util.Pair<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) (new hydra.util.Pair<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>((hydra.util.PersistentSet<hydra.module.Namespace>) (hydra.lib.sets.Empty.<hydra.module.Namespace>apply()), (hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>) (hydra.util.Maybe.<hydra.error.packaging.InvalidPackageError>nothing())))),
      (pkg).modules));
    return hydra.lib.pairs.Second.apply(result.get());
  }

  static hydra.core.Name definitionName(hydra.module.Definition def) {
    return (def).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public hydra.core.Name visit(hydra.module.Definition.Term td) {
        return (td).value.name;
      }

      @Override
      public hydra.core.Name visit(hydra.module.Definition.Type td) {
        return (td).value.name;
      }
    });
  }

  static hydra.util.Maybe<hydra.error.packaging.InvalidModuleError> module(hydra.module.Module mod) {
    hydra.util.Maybe<hydra.error.packaging.InvalidModuleError> r1 = hydra.validate.Packaging.checkDefinitionNamespaces(mod);
    return hydra.lib.maybes.Cases.applyLazy(
      r1,
      () -> ((java.util.function.Supplier<hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (() -> {
        hydra.util.Maybe<hydra.error.packaging.InvalidModuleError> r2 = hydra.validate.Packaging.checkDuplicateDefinitionNames(mod);
        return hydra.lib.maybes.Cases.applyLazy(
          r2,
          () -> hydra.validate.Packaging.checkConflictingVariantNames(mod),
          (java.util.function.Function<hydra.error.packaging.InvalidModuleError, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (ignored -> r2));
      })).get(),
      (java.util.function.Function<hydra.error.packaging.InvalidModuleError, hydra.util.Maybe<hydra.error.packaging.InvalidModuleError>>) (ignored -> r1));
  }

  static hydra.util.Maybe<hydra.error.packaging.InvalidPackageError> package_(hydra.packaging.Package_ pkg) {
    hydra.util.Maybe<hydra.error.packaging.InvalidPackageError> r1 = hydra.validate.Packaging.checkDuplicateModuleNamespaces(pkg);
    return hydra.lib.maybes.Cases.applyLazy(
      r1,
      () -> ((java.util.function.Supplier<hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) (() -> {
        hydra.util.Maybe<hydra.error.packaging.InvalidPackageError> r2 = hydra.validate.Packaging.checkConflictingModuleNamespaces(pkg);
        return hydra.lib.maybes.Cases.applyLazy(
          r2,
          () -> hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>, java.util.function.Function<hydra.module.Module, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>>) (acc -> (java.util.function.Function<hydra.module.Module, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) (mod -> hydra.lib.maybes.Cases.applyLazy(
              acc,
              () -> hydra.lib.maybes.Map.apply(
                (java.util.function.Function<hydra.error.packaging.InvalidModuleError, hydra.error.packaging.InvalidPackageError>) (err -> new hydra.error.packaging.InvalidPackageError.InvalidModule(err)),
                hydra.validate.Packaging.module(mod)),
              (java.util.function.Function<hydra.error.packaging.InvalidPackageError, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) (ignored -> acc)))),
            (hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>) (hydra.util.Maybe.<hydra.error.packaging.InvalidPackageError>nothing()),
            (pkg).modules),
          (java.util.function.Function<hydra.error.packaging.InvalidPackageError, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) (ignored -> r2));
      })).get(),
      (java.util.function.Function<hydra.error.packaging.InvalidPackageError, hydra.util.Maybe<hydra.error.packaging.InvalidPackageError>>) (ignored -> r1));
  }
}
