// Note: this is an automatically generated file. Do not edit.

package hydra.grammars;

/**
 * A utility for converting a BNF grammar to a Hydra module.
 */
public interface Grammars {
  static String childName(String lname, String n) {
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      lname,
      "_",
      hydra.formatting.Formatting.capitalize(n)));
  }
  
  static java.util.List<String> findNames(java.util.List<hydra.grammar.Pattern> pats) {
    java.util.function.Function<hydra.util.Pair<java.util.List<String>, java.util.Map<String, Integer>>, java.util.function.Function<hydra.grammar.Pattern, hydra.util.Pair<java.util.List<String>, java.util.Map<String, Integer>>>> nextName = (java.util.function.Function<hydra.util.Pair<java.util.List<String>, java.util.Map<String, Integer>>, java.util.function.Function<hydra.grammar.Pattern, hydra.util.Pair<java.util.List<String>, java.util.Map<String, Integer>>>>) (acc -> (java.util.function.Function<hydra.grammar.Pattern, hydra.util.Pair<java.util.List<String>, java.util.Map<String, Integer>>>) (pat -> {
      hydra.util.Lazy<java.util.Map<String, Integer>> nameMap = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(acc));
      String rn = hydra.grammars.Grammars.rawName(pat);
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
      hydra.util.Lazy<java.util.List<String>> names = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(acc));
      hydra.util.Lazy<Integer> ni = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nameAndIndex.get()));
      hydra.util.Lazy<String> nn = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(nameAndIndex.get()));
      return (hydra.util.Pair<java.util.List<String>, java.util.Map<String, Integer>>) ((hydra.util.Pair<java.util.List<String>, java.util.Map<String, Integer>>) (new hydra.util.Pair<java.util.List<String>, java.util.Map<String, Integer>>(hydra.lib.lists.Cons.apply(
        nn.get(),
        names.get()), hydra.lib.maps.Insert.apply(
        rn,
        ni.get(),
        nameMap.get()))));
    }));
    return hydra.lib.lists.Reverse.apply(hydra.lib.pairs.First.apply(hydra.lib.lists.Foldl.apply(
      nextName,
      (hydra.util.Pair<java.util.List<String>, java.util.Map<String, Integer>>) ((hydra.util.Pair<java.util.List<String>, java.util.Map<String, Integer>>) (new hydra.util.Pair<java.util.List<String>, java.util.Map<String, Integer>>((java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<String, Integer>) ((java.util.Map<String, Integer>) (hydra.lib.maps.Empty.<String, Integer>apply()))))),
      pats)));
  }
  
  static hydra.module.Module grammarToModule(hydra.module.Namespace ns, hydra.grammar.Grammar grammar, hydra.util.Maybe<String> desc) {
    hydra.util.Lazy<java.util.List<hydra.util.Pair<String, hydra.grammar.Pattern>>> prodPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.grammar.Production, hydra.util.Pair<String, hydra.grammar.Pattern>>) (prod -> (hydra.util.Pair<String, hydra.grammar.Pattern>) ((hydra.util.Pair<String, hydra.grammar.Pattern>) (new hydra.util.Pair<String, hydra.grammar.Pattern>(((prod).symbol).value, (prod).pattern)))),
      (grammar).value));
    hydra.util.Lazy<java.util.List<String>> capitalizedNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<String, hydra.grammar.Pattern>, String>) (pair -> hydra.formatting.Formatting.capitalize(hydra.lib.pairs.First.apply(pair))),
      prodPairs.get()));
    hydra.util.Lazy<java.util.List<hydra.grammar.Pattern>> patterns = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<String, hydra.grammar.Pattern>, hydra.grammar.Pattern>) (pair -> hydra.lib.pairs.Second.apply(pair)),
      prodPairs.get()));
    hydra.util.Lazy<java.util.List<hydra.util.Pair<String, hydra.core.Type>>> elementPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.ZipWith.apply(
      (java.util.function.Function<String, java.util.function.Function<hydra.grammar.Pattern, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>>) (v1 -> (java.util.function.Function<hydra.grammar.Pattern, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>) (v2 -> hydra.grammars.Grammars.makeElements(
        false,
        ns,
        v1,
        v2))),
      capitalizedNames.get(),
      patterns.get())));
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> elements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<String, hydra.core.Type>, hydra.core.Binding>) (pair -> {
        hydra.util.Lazy<String> lname = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
        hydra.core.Name elName = hydra.grammars.Grammars.toName(
          ns,
          lname.get());
        hydra.util.Lazy<hydra.core.Type> typ = new hydra.util.Lazy<>(() -> hydra.grammars.Grammars.replacePlaceholders(
          elName,
          hydra.grammars.Grammars.wrapType(hydra.lib.pairs.Second.apply(pair))));
        return hydra.annotations.Annotations.typeElement(
          elName,
          typ.get());
      }),
      elementPairs.get()));
    return new hydra.module.Module(ns, elements.get(), (java.util.List<hydra.module.Namespace>) (java.util.List.<hydra.module.Namespace>of()), (java.util.List<hydra.module.Namespace>) (java.util.List.<hydra.module.Namespace>of()), desc);
  }
  
  static Boolean isComplex(hydra.grammar.Pattern pat) {
    return (pat).accept(new hydra.grammar.Pattern.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.grammar.Pattern instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.grammar.Pattern.Labeled lp) {
        return hydra.grammars.Grammars.isComplex(((lp).value).pattern);
      }
      
      @Override
      public Boolean visit(hydra.grammar.Pattern.Sequence pats) {
        return hydra.grammars.Grammars.isNontrivial(
          true,
          (pats).value);
      }
      
      @Override
      public Boolean visit(hydra.grammar.Pattern.Alternatives pats) {
        return hydra.grammars.Grammars.isNontrivial(
          false,
          (pats).value);
      }
    });
  }
  
  static Boolean isNontrivial(Boolean isRecord, java.util.List<hydra.grammar.Pattern> pats) {
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
    java.util.List<hydra.grammar.Pattern> minPats = hydra.grammars.Grammars.simplify(
      isRecord,
      pats);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply(minPats),
        1),
      () -> (isLabeled).apply(hydra.lib.lists.Head.apply(minPats)),
      () -> true);
  }
  
  static java.util.List<hydra.util.Pair<String, hydra.core.Type>> makeElements(Boolean omitTrivial, hydra.module.Namespace ns, String lname, hydra.grammar.Pattern pat) {
    java.util.function.Function<String, java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.grammar.Pattern, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>>> mod = (java.util.function.Function<String, java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.grammar.Pattern, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>>>) (n -> (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.grammar.Pattern, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>>) (f -> (java.util.function.Function<hydra.grammar.Pattern, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>) (p -> hydra.grammars.Grammars.makeElements_descend(
      (java.util.function.Function<String, java.util.function.Function<String, String>>) (p0 -> p1 -> hydra.grammars.Grammars.childName(
        p0,
        p1)),
      hydra.grammars.Grammars::isComplex,
      (java.util.function.Function<Boolean, java.util.function.Function<hydra.module.Namespace, java.util.function.Function<String, java.util.function.Function<hydra.grammar.Pattern, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>>>>) (p0 -> p1 -> p2 -> p3 -> hydra.grammars.Grammars.makeElements(
        p0,
        p1,
        p2,
        p3)),
      (java.util.function.Function<hydra.module.Namespace, java.util.function.Function<String, hydra.core.Name>>) (p0 -> p1 -> hydra.grammars.Grammars.toName(
        p0,
        p1)),
      lname,
      ns,
      n,
      (java.util.function.Function<java.util.List<hydra.util.Pair<String, hydra.core.Type>>, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>) (pairs -> hydra.lib.lists.Cons.apply(
        (hydra.util.Pair<String, hydra.core.Type>) ((hydra.util.Pair<String, hydra.core.Type>) (new hydra.util.Pair<String, hydra.core.Type>(lname, (f).apply(hydra.lib.pairs.Second.apply(hydra.lib.lists.Head.apply(pairs)))))),
        hydra.lib.lists.Tail.apply(pairs))),
      p))));
    hydra.util.Lazy<java.util.List<hydra.util.Pair<String, hydra.core.Type>>> trivial = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      omitTrivial,
      () -> (java.util.List<hydra.util.Pair<String, hydra.core.Type>>) (java.util.List.<hydra.util.Pair<String, hydra.core.Type>>of()),
      () -> java.util.List.of((hydra.util.Pair<String, hydra.core.Type>) ((hydra.util.Pair<String, hydra.core.Type>) (new hydra.util.Pair<String, hydra.core.Type>(lname, new hydra.core.Type.Unit()))))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.grammar.Pattern, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>> forPat = new java.util.concurrent.atomic.AtomicReference<>();
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Boolean, java.util.function.Function<java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.core.Type>, java.util.function.Function<java.util.List<hydra.grammar.Pattern>, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>>>> forRecordOrUnion = new java.util.concurrent.atomic.AtomicReference<>();
    forPat.set((java.util.function.Function<hydra.grammar.Pattern, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>) (pat2 -> (pat2).accept(new hydra.grammar.Pattern.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Alternatives pats) {
        return (((forRecordOrUnion.get()).apply(false)).apply((java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.core.Type>) (fields -> new hydra.core.Type.Union(new hydra.core.RowType(hydra.constants.Constants.placeholderName(), fields))))).apply((pats).value);
      }
      
      @Override
      public java.util.List<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Constant ignored) {
        return trivial.get();
      }
      
      @Override
      public java.util.List<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Ignored ignored) {
        return (java.util.List<hydra.util.Pair<String, hydra.core.Type>>) (java.util.List.<hydra.util.Pair<String, hydra.core.Type>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Labeled lp) {
        return (forPat.get()).apply(((lp).value).pattern);
      }
      
      @Override
      public java.util.List<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Nil ignored) {
        return trivial.get();
      }
      
      @Override
      public java.util.List<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Nonterminal s) {
        return java.util.List.of((hydra.util.Pair<String, hydra.core.Type>) ((hydra.util.Pair<String, hydra.core.Type>) (new hydra.util.Pair<String, hydra.core.Type>(lname, new hydra.core.Type.Variable(hydra.grammars.Grammars.toName(
          ns,
          ((s).value).value))))));
      }
      
      @Override
      public java.util.List<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Option p) {
        return (((mod).apply("Option")).apply((java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.Maybe(x)))).apply((p).value);
      }
      
      @Override
      public java.util.List<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Plus p) {
        return (((mod).apply("Elmt")).apply((java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.List(x)))).apply((p).value);
      }
      
      @Override
      public java.util.List<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Regex ignored) {
        return java.util.List.of((hydra.util.Pair<String, hydra.core.Type>) ((hydra.util.Pair<String, hydra.core.Type>) (new hydra.util.Pair<String, hydra.core.Type>(lname, new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))));
      }
      
      @Override
      public java.util.List<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Sequence pats) {
        return (((forRecordOrUnion.get()).apply(true)).apply((java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.core.Type>) (fields -> new hydra.core.Type.Record(new hydra.core.RowType(hydra.constants.Constants.placeholderName(), fields))))).apply((pats).value);
      }
      
      @Override
      public java.util.List<hydra.util.Pair<String, hydra.core.Type>> visit(hydra.grammar.Pattern.Star p) {
        return (((mod).apply("Elmt")).apply((java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.List(x)))).apply((p).value);
      }
    })));
    forRecordOrUnion.set((java.util.function.Function<Boolean, java.util.function.Function<java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.core.Type>, java.util.function.Function<java.util.List<hydra.grammar.Pattern>, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>>>) (isRecord -> (java.util.function.Function<java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.core.Type>, java.util.function.Function<java.util.List<hydra.grammar.Pattern>, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>>) (construct -> (java.util.function.Function<java.util.List<hydra.grammar.Pattern>, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>) (pats -> {
      java.util.List<hydra.grammar.Pattern> minPats = hydra.grammars.Grammars.simplify(
        isRecord,
        pats);
      java.util.List<String> fieldNames = hydra.grammars.Grammars.findNames(minPats);
      java.util.function.Function<String, java.util.function.Function<hydra.grammar.Pattern, hydra.util.Pair<hydra.core.FieldType, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>>> toField = (java.util.function.Function<String, java.util.function.Function<hydra.grammar.Pattern, hydra.util.Pair<hydra.core.FieldType, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>>>) (n -> (java.util.function.Function<hydra.grammar.Pattern, hydra.util.Pair<hydra.core.FieldType, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>>) (p -> hydra.grammars.Grammars.makeElements_descend(
        (java.util.function.Function<String, java.util.function.Function<String, String>>) (p0 -> p1 -> hydra.grammars.Grammars.childName(
          p0,
          p1)),
        hydra.grammars.Grammars::isComplex,
        (java.util.function.Function<Boolean, java.util.function.Function<hydra.module.Namespace, java.util.function.Function<String, java.util.function.Function<hydra.grammar.Pattern, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>>>>) (p0 -> p1 -> p2 -> p3 -> hydra.grammars.Grammars.makeElements(
          p0,
          p1,
          p2,
          p3)),
        (java.util.function.Function<hydra.module.Namespace, java.util.function.Function<String, hydra.core.Name>>) (p0 -> p1 -> hydra.grammars.Grammars.toName(
          p0,
          p1)),
        lname,
        ns,
        n,
        (java.util.function.Function<java.util.List<hydra.util.Pair<String, hydra.core.Type>>, hydra.util.Pair<hydra.core.FieldType, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>>) (pairs -> (hydra.util.Pair<hydra.core.FieldType, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.FieldType, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.FieldType, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>(new hydra.core.FieldType(new hydra.core.Name(n), hydra.lib.pairs.Second.apply(hydra.lib.lists.Head.apply(pairs))), hydra.lib.lists.Tail.apply(pairs))))),
        p)));
      hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.FieldType, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>>> fieldPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.ZipWith.apply(
        toField,
        fieldNames,
        minPats));
      hydra.util.Lazy<java.util.List<hydra.util.Pair<String, hydra.core.Type>>> els = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>) ((java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>) (hydra.lib.pairs.Second::apply)),
        fieldPairs.get())));
      hydra.util.Lazy<java.util.List<hydra.core.FieldType>> fields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>, hydra.core.FieldType>) ((java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>, hydra.core.FieldType>) (hydra.lib.pairs.First::apply)),
        fieldPairs.get()));
      return hydra.lib.logic.IfElse.lazy(
        hydra.grammars.Grammars.isNontrivial(
          isRecord,
          pats),
        () -> hydra.lib.lists.Cons.apply(
          (hydra.util.Pair<String, hydra.core.Type>) ((hydra.util.Pair<String, hydra.core.Type>) (new hydra.util.Pair<String, hydra.core.Type>(lname, (construct).apply(fields.get())))),
          els.get()),
        () -> (forPat.get()).apply(hydra.lib.lists.Head.apply(minPats)));
    }))));
    return (forPat.get()).apply(pat);
  }
  
  static <T0> T0 makeElements_descend(java.util.function.Function<String, java.util.function.Function<String, String>> hydra_grammars_childName2, java.util.function.Function<hydra.grammar.Pattern, Boolean> hydra_grammars_isComplex2, java.util.function.Function<Boolean, java.util.function.Function<hydra.module.Namespace, java.util.function.Function<String, java.util.function.Function<hydra.grammar.Pattern, java.util.List<hydra.util.Pair<String, hydra.core.Type>>>>>> hydra_grammars_makeElements2, java.util.function.Function<hydra.module.Namespace, java.util.function.Function<String, hydra.core.Name>> hydra_grammars_toName2, String lname, hydra.module.Namespace ns, String n, java.util.function.Function<java.util.List<hydra.util.Pair<String, hydra.core.Type>>, T0> f, hydra.grammar.Pattern p) {
    java.util.List<hydra.util.Pair<String, hydra.core.Type>> cpairs = ((((hydra_grammars_makeElements2).apply(false)).apply(ns)).apply(((hydra_grammars_childName2).apply(lname)).apply(n))).apply(p);
    return (f).apply(hydra.lib.logic.IfElse.lazy(
      (hydra_grammars_isComplex2).apply(p),
      () -> hydra.lib.lists.Cons.apply(
        (hydra.util.Pair<String, hydra.core.Type>) ((hydra.util.Pair<String, hydra.core.Type>) (new hydra.util.Pair<String, hydra.core.Type>(lname, new hydra.core.Type.Variable(((hydra_grammars_toName2).apply(ns)).apply(hydra.lib.pairs.First.apply(hydra.lib.lists.Head.apply(cpairs))))))),
        cpairs),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(cpairs),
        () -> java.util.List.of((hydra.util.Pair<String, hydra.core.Type>) ((hydra.util.Pair<String, hydra.core.Type>) (new hydra.util.Pair<String, hydra.core.Type>(lname, new hydra.core.Type.Unit())))),
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
        return hydra.formatting.Formatting.capitalize(hydra.formatting.Formatting.withCharacterAliases(((c).value).value));
      }
      
      @Override
      public String visit(hydra.grammar.Pattern.Ignored ignored) {
        return "ignored";
      }
      
      @Override
      public String visit(hydra.grammar.Pattern.Labeled lp) {
        return (((lp).value).label).value;
      }
      
      @Override
      public String visit(hydra.grammar.Pattern.Nil ignored) {
        return "none";
      }
      
      @Override
      public String visit(hydra.grammar.Pattern.Nonterminal s) {
        return hydra.formatting.Formatting.capitalize(((s).value).value);
      }
      
      @Override
      public String visit(hydra.grammar.Pattern.Option p) {
        return hydra.formatting.Formatting.capitalize(hydra.grammars.Grammars.rawName((p).value));
      }
      
      @Override
      public String visit(hydra.grammar.Pattern.Plus p) {
        return hydra.lib.strings.Cat2.apply(
          "listOf",
          hydra.formatting.Formatting.capitalize(hydra.grammars.Grammars.rawName((p).value)));
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
          hydra.formatting.Formatting.capitalize(hydra.grammars.Grammars.rawName((p).value)));
      }
    });
  }
  
  static hydra.core.Type replacePlaceholders(hydra.core.Name elName, hydra.core.Type typ) {
    return hydra.rewriting.Rewriting.rewriteType(
      (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.core.Type otherwise(hydra.core.Type instance) {
          return t;
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Record rt) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              ((rt).value).typeName,
              hydra.constants.Constants.placeholderName()),
            () -> new hydra.core.Type.Record(new hydra.core.RowType(elName, ((rt).value).fields)),
            () -> t);
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Union ut) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              ((ut).value).typeName,
              hydra.constants.Constants.placeholderName()),
            () -> new hydra.core.Type.Union(new hydra.core.RowType(elName, ((ut).value).fields)),
            () -> t);
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Wrap wt) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              ((wt).value).typeName,
              hydra.constants.Constants.placeholderName()),
            () -> new hydra.core.Type.Wrap(new hydra.core.WrappedType(elName, ((wt).value).body)),
            () -> t);
        }
      }))),
      typ);
  }
  
  static java.util.List<hydra.grammar.Pattern> simplify(Boolean isRecord, java.util.List<hydra.grammar.Pattern> pats) {
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
    return hydra.names.Names.unqualifyName(new hydra.module.QualifiedName(hydra.util.Maybe.just(ns), local));
  }
  
  static hydra.core.Type wrapType(hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return new hydra.core.Type.Wrap(new hydra.core.WrappedType(new hydra.core.Name("Placeholder"), t));
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
