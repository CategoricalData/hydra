// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * A utility for converting a BNF grammar to a Hydra module.
 */
public interface Grammars {
  static String childName(String lname, String n) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      lname,
      "_",
      hydra.Formatting.capitalize(n)));
  }

  static hydra.util.ConsList<String> findNames(hydra.util.ConsList<hydra.grammar.Pattern> pats) {
    java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<String>, hydra.util.PersistentMap<String, Integer>>, java.util.function.Function<hydra.grammar.Pattern, hydra.util.Pair<hydra.util.ConsList<String>, hydra.util.PersistentMap<String, Integer>>>> nextName = (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<String>, hydra.util.PersistentMap<String, Integer>>, java.util.function.Function<hydra.grammar.Pattern, hydra.util.Pair<hydra.util.ConsList<String>, hydra.util.PersistentMap<String, Integer>>>>) (acc -> (java.util.function.Function<hydra.grammar.Pattern, hydra.util.Pair<hydra.util.ConsList<String>, hydra.util.PersistentMap<String, Integer>>>) (pat -> {
      hydra.util.Lazy<hydra.util.PersistentMap<String, Integer>> nameMap = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(acc));
      String rn = hydra.Grammars.rawName(pat);
      hydra.util.Lazy<hydra.util.Pair<String, Integer>> nameAndIndex = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
        () -> (hydra.util.Pair<String, Integer>) ((hydra.util.Pair<String, Integer>) (new hydra.util.Pair<String, Integer>(rn, 1))),
        (java.util.function.Function<Integer, hydra.util.Pair<String, Integer>>) (i -> (hydra.util.Pair<String, Integer>) ((hydra.util.Pair<String, Integer>) (new hydra.util.Pair<String, Integer>(hydra.lib.strings.Cat2.apply(
          rn,
          hydra.lib.literals.ShowInt32.apply(hydra.lib.math.Add.apply(
            i,
            1))), hydra.lib.math.Add.apply(
          i,
          1))))),
        hydra.lib.maps.Lookup.apply(
          rn,
          nameMap.get())));
      hydra.util.Lazy<hydra.util.ConsList<String>> names = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(acc));
      hydra.util.Lazy<Integer> ni = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nameAndIndex.get()));
      hydra.util.Lazy<String> nn = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(nameAndIndex.get()));
      return (hydra.util.Pair<hydra.util.ConsList<String>, hydra.util.PersistentMap<String, Integer>>) ((hydra.util.Pair<hydra.util.ConsList<String>, hydra.util.PersistentMap<String, Integer>>) (new hydra.util.Pair<hydra.util.ConsList<String>, hydra.util.PersistentMap<String, Integer>>(hydra.lib.lists.Cons.apply(
        nn.get(),
        names.get()), hydra.lib.maps.Insert.apply(
        rn,
        ni.get(),
        nameMap.get()))));
    }));
    return hydra.lib.lists.Reverse.apply(hydra.lib.pairs.First.apply(hydra.lib.lists.Foldl.apply(
      nextName,
      (hydra.util.Pair<hydra.util.ConsList<String>, hydra.util.PersistentMap<String, Integer>>) ((hydra.util.Pair<hydra.util.ConsList<String>, hydra.util.PersistentMap<String, Integer>>) (new hydra.util.Pair<hydra.util.ConsList<String>, hydra.util.PersistentMap<String, Integer>>((hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty()), (hydra.util.PersistentMap<String, Integer>) ((hydra.util.PersistentMap<String, Integer>) (hydra.lib.maps.Empty.<String, Integer>apply()))))),
      pats)));
  }

  static hydra.module.Module grammarToModule(hydra.module.Namespace ns, hydra.grammar.Grammar grammar, hydra.util.Maybe<String> desc) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<String, hydra.grammar.Pattern>>> prodPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.grammar.Production, hydra.util.Pair<String, hydra.grammar.Pattern>>) (prod -> (hydra.util.Pair<String, hydra.grammar.Pattern>) ((hydra.util.Pair<String, hydra.grammar.Pattern>) (new hydra.util.Pair<String, hydra.grammar.Pattern>((prod).symbol.value, (prod).pattern)))),
      (grammar).value));
    hydra.util.Lazy<hydra.util.ConsList<String>> capitalizedNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<String, hydra.grammar.Pattern>, String>) (pair -> hydra.Formatting.capitalize(hydra.lib.pairs.First.apply(pair))),
      prodPairs.get()));
    hydra.util.Lazy<hydra.util.ConsList<hydra.grammar.Pattern>> patterns = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<String, hydra.grammar.Pattern>, hydra.grammar.Pattern>) (pair -> hydra.lib.pairs.Second.apply(pair)),
      prodPairs.get()));
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>> elementPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.ZipWith.apply(
      (java.util.function.Function<String, java.util.function.Function<hydra.grammar.Pattern, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>>) (v1 -> (java.util.function.Function<hydra.grammar.Pattern, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>) (v2 -> hydra.Grammars.makeElements(
        false,
        ns,
        v1,
        v2))),
      capitalizedNames.get(),
      patterns.get())));
    hydra.util.Lazy<hydra.util.ConsList<hydra.module.Definition>> typeDefs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<String, hydra.core.Type>, hydra.module.Definition>) (pair -> {
        hydra.util.Lazy<String> lname = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
        hydra.core.Name elName = hydra.Grammars.toName(
          ns,
          lname.get());
        hydra.util.Lazy<hydra.core.Type> typ = new hydra.util.Lazy<>(() -> hydra.Grammars.wrapType(hydra.lib.pairs.Second.apply(pair)));
        return new hydra.module.Definition.Type(new hydra.module.TypeDefinition(elName, typ.get()));
      }),
      elementPairs.get()));
    return new hydra.module.Module(ns, typeDefs.get(), (hydra.util.ConsList<hydra.module.Namespace>) (hydra.util.ConsList.<hydra.module.Namespace>empty()), (hydra.util.ConsList<hydra.module.Namespace>) (hydra.util.ConsList.<hydra.module.Namespace>empty()), desc);
  }

  static Boolean isComplex(hydra.grammar.Pattern pat) {
    return (pat).accept(new hydra.grammar.Pattern.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.grammar.Pattern instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.grammar.Pattern.Labeled lp) {
        return hydra.Grammars.isComplex((lp).value.pattern);
      }

      @Override
      public Boolean visit(hydra.grammar.Pattern.Sequence pats) {
        return hydra.Grammars.isNontrivial(
          true,
          (pats).value);
      }

      @Override
      public Boolean visit(hydra.grammar.Pattern.Alternatives pats) {
        return hydra.Grammars.isNontrivial(
          false,
          (pats).value);
      }
    });
  }

  static Boolean isNontrivial(Boolean isRecord, hydra.util.ConsList<hydra.grammar.Pattern> pats) {
    java.util.function.Function<hydra.grammar.Pattern, Boolean> isLabeled = (java.util.function.Function<hydra.grammar.Pattern, Boolean>) (p -> (p).accept(new hydra.grammar.Pattern.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.grammar.Pattern instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.grammar.Pattern.Labeled ignored) {
        return true;
      }
    }));
    hydra.util.ConsList<hydra.grammar.Pattern> minPats = hydra.Grammars.simplify(
      isRecord,
      pats);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply(minPats),
        1),
      () -> (isLabeled).apply(hydra.lib.lists.Head.apply(minPats)),
      () -> true);
  }

  static hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>> makeElements(Boolean omitTrivial, hydra.module.Namespace ns, String lname, hydra.grammar.Pattern pat) {
    java.util.function.Function<String, java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.grammar.Pattern, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>>> mod = (java.util.function.Function<String, java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.grammar.Pattern, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>>>) (n -> (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.grammar.Pattern, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>>) (f -> (java.util.function.Function<hydra.grammar.Pattern, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>) (p -> hydra.Grammars.makeElements_descend(
      (java.util.function.Function<String, java.util.function.Function<String, String>>) (p0 -> p1 -> hydra.Grammars.childName(
        p0,
        p1)),
      hydra.Grammars::isComplex,
      (java.util.function.Function<Boolean, java.util.function.Function<hydra.module.Namespace, java.util.function.Function<String, java.util.function.Function<hydra.grammar.Pattern, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>>>>) (p0 -> p1 -> p2 -> p3 -> hydra.Grammars.makeElements(
        p0,
        p1,
        p2,
        p3)),
      (java.util.function.Function<hydra.module.Namespace, java.util.function.Function<String, hydra.core.Name>>) (p0 -> p1 -> hydra.Grammars.toName(
        p0,
        p1)),
      lname,
      ns,
      n,
      (java.util.function.Function<hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>) (pairs -> hydra.lib.lists.Cons.apply(
        (hydra.util.Pair<String, hydra.core.Type>) ((hydra.util.Pair<String, hydra.core.Type>) (new hydra.util.Pair<String, hydra.core.Type>(lname, (f).apply(hydra.lib.pairs.Second.apply(hydra.lib.lists.Head.apply(pairs)))))),
        hydra.lib.lists.Tail.apply(pairs))),
      p))));
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>> trivial = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      omitTrivial,
      () -> (hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>) (hydra.util.ConsList.<hydra.util.Pair<String, hydra.core.Type>>empty()),
      () -> hydra.util.ConsList.of((hydra.util.Pair<String, hydra.core.Type>) ((hydra.util.Pair<String, hydra.core.Type>) (new hydra.util.Pair<String, hydra.core.Type>(lname, new hydra.core.Type.Unit()))))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.grammar.Pattern, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>> forPat = new java.util.concurrent.atomic.AtomicReference<>();
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Boolean, java.util.function.Function<java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, hydra.core.Type>, java.util.function.Function<hydra.util.ConsList<hydra.grammar.Pattern>, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>>>> forRecordOrUnion = new java.util.concurrent.atomic.AtomicReference<>();
    forPat.set((java.util.function.Function<hydra.grammar.Pattern, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>) (pat2 -> (pat2).accept(new hydra.grammar.Pattern.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Alternatives pats) {
        return forRecordOrUnion.get().apply(false).apply((java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, hydra.core.Type>) (fields -> new hydra.core.Type.Union(fields))).apply((pats).value);
      }

      @Override
      public hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Constant ignored) {
        return trivial.get();
      }

      @Override
      public hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Ignored ignored) {
        return (hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>) (hydra.util.ConsList.<hydra.util.Pair<String, hydra.core.Type>>empty());
      }

      @Override
      public hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Labeled lp) {
        return forPat.get().apply((lp).value.pattern);
      }

      @Override
      public hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Nil ignored) {
        return trivial.get();
      }

      @Override
      public hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Nonterminal s) {
        return hydra.util.ConsList.of((hydra.util.Pair<String, hydra.core.Type>) ((hydra.util.Pair<String, hydra.core.Type>) (new hydra.util.Pair<String, hydra.core.Type>(lname, new hydra.core.Type.Variable(hydra.Grammars.toName(
          ns,
          (s).value.value))))));
      }

      @Override
      public hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Option p) {
        return (mod).apply("Option").apply((java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.Maybe(x))).apply((p).value);
      }

      @Override
      public hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Plus p) {
        return (mod).apply("Elmt").apply((java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.List(x))).apply((p).value);
      }

      @Override
      public hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Regex ignored) {
        return hydra.util.ConsList.of((hydra.util.Pair<String, hydra.core.Type>) ((hydra.util.Pair<String, hydra.core.Type>) (new hydra.util.Pair<String, hydra.core.Type>(lname, new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))));
      }

      @Override
      public hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Sequence pats) {
        return forRecordOrUnion.get().apply(true).apply((java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, hydra.core.Type>) (fields -> new hydra.core.Type.Record(fields))).apply((pats).value);
      }

      @Override
      public hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Star p) {
        return (mod).apply("Elmt").apply((java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.List(x))).apply((p).value);
      }
    })));
    forRecordOrUnion.set((java.util.function.Function<Boolean, java.util.function.Function<java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, hydra.core.Type>, java.util.function.Function<hydra.util.ConsList<hydra.grammar.Pattern>, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>>>) (isRecord -> (java.util.function.Function<java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, hydra.core.Type>, java.util.function.Function<hydra.util.ConsList<hydra.grammar.Pattern>, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>>) (construct -> (java.util.function.Function<hydra.util.ConsList<hydra.grammar.Pattern>, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>) (pats -> {
      hydra.util.ConsList<hydra.grammar.Pattern> minPats = hydra.Grammars.simplify(
        isRecord,
        pats);
      hydra.util.ConsList<String> fieldNames = hydra.Grammars.findNames(minPats);
      java.util.function.Function<String, java.util.function.Function<hydra.grammar.Pattern, hydra.util.Pair<hydra.core.FieldType, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>>> toField = (java.util.function.Function<String, java.util.function.Function<hydra.grammar.Pattern, hydra.util.Pair<hydra.core.FieldType, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>>>) (n -> (java.util.function.Function<hydra.grammar.Pattern, hydra.util.Pair<hydra.core.FieldType, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>>) (p -> hydra.Grammars.makeElements_descend(
        (java.util.function.Function<String, java.util.function.Function<String, String>>) (p0 -> p1 -> hydra.Grammars.childName(
          p0,
          p1)),
        hydra.Grammars::isComplex,
        (java.util.function.Function<Boolean, java.util.function.Function<hydra.module.Namespace, java.util.function.Function<String, java.util.function.Function<hydra.grammar.Pattern, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>>>>) (p0 -> p1 -> p2 -> p3 -> hydra.Grammars.makeElements(
          p0,
          p1,
          p2,
          p3)),
        (java.util.function.Function<hydra.module.Namespace, java.util.function.Function<String, hydra.core.Name>>) (p0 -> p1 -> hydra.Grammars.toName(
          p0,
          p1)),
        lname,
        ns,
        n,
        (java.util.function.Function<hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>, hydra.util.Pair<hydra.core.FieldType, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>>) (pairs -> (hydra.util.Pair<hydra.core.FieldType, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.FieldType, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.FieldType, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>(new hydra.core.FieldType(new hydra.core.Name(n), hydra.lib.pairs.Second.apply(hydra.lib.lists.Head.apply(pairs))), hydra.lib.lists.Tail.apply(pairs))))),
        p)));
      hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<hydra.core.FieldType, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>>> fieldPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.ZipWith.apply(
        toField,
        fieldNames,
        minPats));
      hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>> els = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>) ((java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>) (hydra.lib.pairs.Second::apply)),
        fieldPairs.get())));
      hydra.util.Lazy<hydra.util.ConsList<hydra.core.FieldType>> fields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>, hydra.core.FieldType>) ((java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>, hydra.core.FieldType>) (hydra.lib.pairs.First::apply)),
        fieldPairs.get()));
      return hydra.lib.logic.IfElse.lazy(
        hydra.Grammars.isNontrivial(
          isRecord,
          pats),
        () -> hydra.lib.lists.Cons.apply(
          (hydra.util.Pair<String, hydra.core.Type>) ((hydra.util.Pair<String, hydra.core.Type>) (new hydra.util.Pair<String, hydra.core.Type>(lname, (construct).apply(fields.get())))),
          els.get()),
        () -> forPat.get().apply(hydra.lib.lists.Head.apply(minPats)));
    }))));
    return forPat.get().apply(pat);
  }

  static <T0> T0 makeElements_descend(java.util.function.Function<String, java.util.function.Function<String, String>> hydra_grammars_childName2, java.util.function.Function<hydra.grammar.Pattern, Boolean> hydra_grammars_isComplex2, java.util.function.Function<Boolean, java.util.function.Function<hydra.module.Namespace, java.util.function.Function<String, java.util.function.Function<hydra.grammar.Pattern, hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>>>>> hydra_grammars_makeElements2, java.util.function.Function<hydra.module.Namespace, java.util.function.Function<String, hydra.core.Name>> hydra_grammars_toName2, String lname, hydra.module.Namespace ns, String n, java.util.function.Function<hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>>, T0> f, hydra.grammar.Pattern p) {
    hydra.util.ConsList<hydra.util.Pair<String, hydra.core.Type>> cpairs = (hydra_grammars_makeElements2).apply(false).apply(ns).apply((hydra_grammars_childName2).apply(lname).apply(n)).apply(p);
    return (f).apply(hydra.lib.logic.IfElse.lazy(
      (hydra_grammars_isComplex2).apply(p),
      () -> hydra.lib.lists.Cons.apply(
        (hydra.util.Pair<String, hydra.core.Type>) ((hydra.util.Pair<String, hydra.core.Type>) (new hydra.util.Pair<String, hydra.core.Type>(lname, new hydra.core.Type.Variable((hydra_grammars_toName2).apply(ns).apply(hydra.lib.pairs.First.apply(hydra.lib.lists.Head.apply(cpairs))))))),
        cpairs),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(cpairs),
        () -> hydra.util.ConsList.of((hydra.util.Pair<String, hydra.core.Type>) ((hydra.util.Pair<String, hydra.core.Type>) (new hydra.util.Pair<String, hydra.core.Type>(lname, new hydra.core.Type.Unit())))),
        () -> hydra.lib.lists.Cons.apply(
          (hydra.util.Pair<String, hydra.core.Type>) ((hydra.util.Pair<String, hydra.core.Type>) (new hydra.util.Pair<String, hydra.core.Type>(lname, hydra.lib.pairs.Second.apply(hydra.lib.lists.Head.apply(cpairs))))),
          hydra.lib.lists.Tail.apply(cpairs)))));
  }

  static String rawName(hydra.grammar.Pattern pat) {
    return (pat).accept(new hydra.grammar.Pattern.PartialVisitor<>() {
      @Override
      public String visit(hydra.grammar.Pattern.Alternatives ignored) {
        return "alts";
      }

      @Override
      public String visit(hydra.grammar.Pattern.Constant c) {
        return hydra.Formatting.capitalize(hydra.Formatting.withCharacterAliases((c).value.value));
      }

      @Override
      public String visit(hydra.grammar.Pattern.Ignored ignored) {
        return "ignored";
      }

      @Override
      public String visit(hydra.grammar.Pattern.Labeled lp) {
        return (lp).value.label.value;
      }

      @Override
      public String visit(hydra.grammar.Pattern.Nil ignored) {
        return "none";
      }

      @Override
      public String visit(hydra.grammar.Pattern.Nonterminal s) {
        return hydra.Formatting.capitalize((s).value.value);
      }

      @Override
      public String visit(hydra.grammar.Pattern.Option p) {
        return hydra.Formatting.capitalize(hydra.Grammars.rawName((p).value));
      }

      @Override
      public String visit(hydra.grammar.Pattern.Plus p) {
        return hydra.lib.strings.Cat2.apply(
          "listOf",
          hydra.Formatting.capitalize(hydra.Grammars.rawName((p).value)));
      }

      @Override
      public String visit(hydra.grammar.Pattern.Regex ignored) {
        return "regex";
      }

      @Override
      public String visit(hydra.grammar.Pattern.Sequence ignored) {
        return "sequence";
      }

      @Override
      public String visit(hydra.grammar.Pattern.Star p) {
        return hydra.lib.strings.Cat2.apply(
          "listOf",
          hydra.Formatting.capitalize(hydra.Grammars.rawName((p).value)));
      }
    });
  }

  static <T0, T1> T1 replacePlaceholders(T0 elName, T1 typ) {
    return typ;
  }

  static hydra.util.ConsList<hydra.grammar.Pattern> simplify(Boolean isRecord, hydra.util.ConsList<hydra.grammar.Pattern> pats) {
    java.util.function.Function<hydra.grammar.Pattern, Boolean> isConstant = (java.util.function.Function<hydra.grammar.Pattern, Boolean>) (p -> (p).accept(new hydra.grammar.Pattern.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.grammar.Pattern instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.grammar.Pattern.Constant ignored) {
        return true;
      }
    }));
    return hydra.lib.logic.IfElse.lazy(
      isRecord,
      () -> hydra.lib.lists.Filter.apply(
        (java.util.function.Function<hydra.grammar.Pattern, Boolean>) (p -> hydra.lib.logic.Not.apply((isConstant).apply(p))),
        pats),
      () -> pats);
  }

  static hydra.core.Name toName(hydra.module.Namespace ns, String local) {
    return hydra.Names.unqualifyName(new hydra.module.QualifiedName(hydra.util.Maybe.just(ns), local));
  }

  static hydra.core.Type wrapType(hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return new hydra.core.Type.Wrap(t);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Record ignored) {
        return t;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Union ignored) {
        return t;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Wrap ignored) {
        return t;
      }
    });
  }
}
