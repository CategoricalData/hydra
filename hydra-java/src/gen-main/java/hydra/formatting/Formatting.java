// Note: this is an automatically generated file. Do not edit.

package hydra.formatting;

/**
 * String formatting types and functions.
 */
public interface Formatting {
  static String capitalize(String v1) {
    return hydra.formatting.Formatting.mapFirstLetter(
      hydra.lib.strings.ToUpper::apply,
      v1);
  }
  
  static String convertCase(hydra.util.CaseConvention from, hydra.util.CaseConvention to, String original) {
    java.util.function.Function<java.util.List<java.util.List<Integer>>, java.util.function.Function<Integer, java.util.List<java.util.List<Integer>>>> splitOnUppercase = (java.util.function.Function<java.util.List<java.util.List<Integer>>, java.util.function.Function<Integer, java.util.List<java.util.List<Integer>>>>) (acc -> (java.util.function.Function<Integer, java.util.List<java.util.List<Integer>>>) (c -> hydra.lib.lists.Concat2.apply(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.chars.IsUpper.apply(c),
        () -> java.util.List.of((java.util.List<Integer>) (java.util.List.<Integer>of())),
        () -> (java.util.List<java.util.List<Integer>>) (java.util.List.<java.util.List<Integer>>of())),
      hydra.lib.lists.Cons.apply(
        hydra.lib.lists.Cons.apply(
          c,
          hydra.lib.lists.Head.apply(acc)),
        hydra.lib.lists.Tail.apply(acc)))));
    hydra.util.Lazy<java.util.List<String>> byCaps = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.lib.strings.FromList::apply,
      hydra.lib.lists.Foldl.apply(
        splitOnUppercase,
        java.util.List.of((java.util.List<Integer>) (java.util.List.<Integer>of())),
        hydra.lib.lists.Reverse.apply(hydra.lib.strings.ToList.apply(hydra.formatting.Formatting.decapitalize(original))))));
    java.util.List<String> byUnderscores = hydra.lib.strings.SplitOn.apply(
      "_",
      original);
    java.util.List<String> parts = (from).accept(new hydra.util.CaseConvention.PartialVisitor<>() {
      @Override
      public java.util.List<String> visit(hydra.util.CaseConvention.Camel ignored) {
        return byCaps.get();
      }
      
      @Override
      public java.util.List<String> visit(hydra.util.CaseConvention.Pascal ignored) {
        return byCaps.get();
      }
      
      @Override
      public java.util.List<String> visit(hydra.util.CaseConvention.LowerSnake ignored) {
        return byUnderscores;
      }
      
      @Override
      public java.util.List<String> visit(hydra.util.CaseConvention.UpperSnake ignored) {
        return byUnderscores;
      }
    });
    return (to).accept(new hydra.util.CaseConvention.PartialVisitor<>() {
      @Override
      public String visit(hydra.util.CaseConvention.Camel ignored) {
        return hydra.formatting.Formatting.decapitalize(hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<String, String>) (arg_ -> hydra.formatting.Formatting.capitalize(hydra.lib.strings.ToLower.apply(arg_))),
          parts)));
      }
      
      @Override
      public String visit(hydra.util.CaseConvention.Pascal ignored) {
        return hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<String, String>) (arg_ -> hydra.formatting.Formatting.capitalize(hydra.lib.strings.ToLower.apply(arg_))),
          parts));
      }
      
      @Override
      public String visit(hydra.util.CaseConvention.LowerSnake ignored) {
        return hydra.lib.strings.Intercalate.apply(
          "_",
          hydra.lib.lists.Map.apply(
            hydra.lib.strings.ToLower::apply,
            parts));
      }
      
      @Override
      public String visit(hydra.util.CaseConvention.UpperSnake ignored) {
        return hydra.lib.strings.Intercalate.apply(
          "_",
          hydra.lib.lists.Map.apply(
            hydra.lib.strings.ToUpper::apply,
            parts));
      }
    });
  }
  
  static String convertCaseCamelToLowerSnake(String v1) {
    return hydra.formatting.Formatting.convertCase(
      new hydra.util.CaseConvention.Camel(),
      new hydra.util.CaseConvention.LowerSnake(),
      v1);
  }
  
  static String convertCaseCamelToUpperSnake(String v1) {
    return hydra.formatting.Formatting.convertCase(
      new hydra.util.CaseConvention.Camel(),
      new hydra.util.CaseConvention.UpperSnake(),
      v1);
  }
  
  static String convertCasePascalToUpperSnake(String v1) {
    return hydra.formatting.Formatting.convertCase(
      new hydra.util.CaseConvention.Pascal(),
      new hydra.util.CaseConvention.UpperSnake(),
      v1);
  }
  
  static String decapitalize(String v1) {
    return hydra.formatting.Formatting.mapFirstLetter(
      hydra.lib.strings.ToLower::apply,
      v1);
  }
  
  static String escapeWithUnderscore(java.util.Set<String> reserved, String s) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        s,
        reserved),
      () -> hydra.lib.strings.Cat2.apply(
        s,
        "_"),
      () -> s);
  }
  
  static String indentLines(String s) {
    java.util.function.Function<String, String> indent = (java.util.function.Function<String, String>) (l -> hydra.lib.strings.Cat2.apply(
      "    ",
      l));
    return hydra.lib.strings.Unlines.apply(hydra.lib.lists.Map.apply(
      indent,
      hydra.lib.strings.Lines.apply(s)));
  }
  
  static String javaStyleComment(String s) {
    return hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "/**\n",
          " * "),
        s),
      "\n */");
  }
  
  static String mapFirstLetter(java.util.function.Function<String, String> mapping, String s) {
    java.util.List<Integer> list = hydra.lib.strings.ToList.apply(s);
    hydra.util.Lazy<String> firstLetter = new hydra.util.Lazy<>(() -> (mapping).apply(hydra.lib.strings.FromList.apply(hydra.lib.lists.Pure.apply(hydra.lib.lists.Head.apply(list)))));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.strings.Null.apply(s),
      () -> s,
      () -> hydra.lib.strings.Cat2.apply(
        firstLetter.get(),
        hydra.lib.strings.FromList.apply(hydra.lib.lists.Tail.apply(list))));
  }
  
  static String nonAlnumToUnderscores(String input) {
    java.util.function.Function<Integer, Boolean> isAlnum = (java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.logic.Or.apply(
      hydra.lib.logic.And.apply(
        hydra.lib.equality.Gte.apply(
          c,
          65),
        hydra.lib.equality.Lte.apply(
          c,
          90)),
      hydra.lib.logic.Or.apply(
        hydra.lib.logic.And.apply(
          hydra.lib.equality.Gte.apply(
            c,
            97),
          hydra.lib.equality.Lte.apply(
            c,
            122)),
        hydra.lib.logic.And.apply(
          hydra.lib.equality.Gte.apply(
            c,
            48),
          hydra.lib.equality.Lte.apply(
            c,
            57)))));
    java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>, java.util.function.Function<Integer, hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>>> replace = (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>, java.util.function.Function<Integer, hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>>>) (p -> (java.util.function.Function<Integer, hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>>) (c -> {
      hydra.util.Lazy<Boolean> b = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(p));
      hydra.util.Lazy<java.util.List<Integer>> s = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
      return hydra.lib.logic.IfElse.lazy(
        (isAlnum).apply(c),
        () -> (hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>) ((hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>) (new hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>(hydra.lib.lists.Cons.apply(
          c,
          s.get()), false))),
        () -> hydra.lib.logic.IfElse.lazy(
          b.get(),
          () -> (hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>) ((hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>) (new hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>(s.get(), true))),
          () -> (hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>) ((hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>) (new hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>(hydra.lib.lists.Cons.apply(
            95,
            s.get()), true)))));
    }));
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>> result = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      replace,
      (hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>) ((hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>) (new hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>((java.util.List<Integer>) (java.util.List.<Integer>of()), false))),
      hydra.lib.strings.ToList.apply(input)));
    return hydra.lib.strings.FromList.apply(hydra.lib.lists.Reverse.apply(hydra.lib.pairs.First.apply(result.get())));
  }
  
  static String sanitizeWithUnderscores(java.util.Set<String> reserved, String s) {
    return hydra.formatting.Formatting.escapeWithUnderscore(
      reserved,
      hydra.formatting.Formatting.nonAlnumToUnderscores(s));
  }
  
  static <T0> String showList(java.util.function.Function<T0, String> f, java.util.List<T0> els) {
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      "[",
      hydra.lib.strings.Intercalate.apply(
        ", ",
        hydra.lib.lists.Map.apply(
          f,
          els)),
      "]"));
  }
  
  static String stripLeadingAndTrailingWhitespace(String s) {
    return hydra.lib.strings.FromList.apply(hydra.lib.lists.DropWhile.apply(
      hydra.lib.chars.IsSpace::apply,
      hydra.lib.lists.Reverse.apply(hydra.lib.lists.DropWhile.apply(
        hydra.lib.chars.IsSpace::apply,
        hydra.lib.lists.Reverse.apply(hydra.lib.strings.ToList.apply(s))))));
  }
  
  static String withCharacterAliases(String original) {
    hydra.util.Lazy<java.util.Map<Integer, String>> aliases = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of(
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(32, "sp"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(33, "excl"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(34, "quot"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(35, "num"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(36, "dollar"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(37, "percnt"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(38, "amp"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(39, "apos"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(40, "lpar"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(41, "rpar"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(42, "ast"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(43, "plus"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(44, "comma"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(45, "minus"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(46, "period"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(47, "sol"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(58, "colon"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(59, "semi"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(60, "lt"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(61, "equals"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(62, "gt"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(63, "quest"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(64, "commat"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(91, "lsqb"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(92, "bsol"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(93, "rsqb"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(94, "circ"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(95, "lowbar"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(96, "grave"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(123, "lcub"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(124, "verbar"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(125, "rcub"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(126, "tilde"))))));
    java.util.function.Function<Integer, java.util.List<Integer>> alias = (java.util.function.Function<Integer, java.util.List<Integer>>) (c -> hydra.lib.maybes.FromMaybe.apply(
      hydra.lib.lists.Pure.apply(c),
      hydra.lib.maybes.Map.apply(
        hydra.lib.strings.ToList::apply,
        hydra.lib.maps.Lookup.apply(
          c,
          aliases.get()))));
    return hydra.lib.strings.FromList.apply(hydra.lib.lists.Filter.apply(
      hydra.lib.chars.IsAlphaNum::apply,
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        alias,
        hydra.lib.strings.ToList.apply(original)))));
  }
  
  static String wrapLine(Integer maxlen, String input) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<java.util.List<Integer>>, java.util.function.Function<java.util.List<Integer>, java.util.List<java.util.List<Integer>>>>> helper = new java.util.concurrent.atomic.AtomicReference<>();
    helper.set((java.util.function.Function<java.util.List<java.util.List<Integer>>, java.util.function.Function<java.util.List<Integer>, java.util.List<java.util.List<Integer>>>>) (prev -> (java.util.function.Function<java.util.List<Integer>, java.util.List<java.util.List<Integer>>>) (rem -> {
      hydra.util.Lazy<java.util.List<Integer>> trunc = new hydra.util.Lazy<>(() -> hydra.lib.lists.Take.apply(
        maxlen,
        rem));
      hydra.util.Lazy<hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<Integer>>> spanResult = new hydra.util.Lazy<>(() -> hydra.lib.lists.Span.apply(
        (java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.logic.And.apply(
          hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
            c,
            32)),
          hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
            c,
            9)))),
        hydra.lib.lists.Reverse.apply(trunc.get())));
      hydra.util.Lazy<java.util.List<Integer>> prefix = new hydra.util.Lazy<>(() -> hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(spanResult.get())));
      hydra.util.Lazy<java.util.List<Integer>> suffix = new hydra.util.Lazy<>(() -> hydra.lib.lists.Reverse.apply(hydra.lib.pairs.First.apply(spanResult.get())));
      return hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Lte.apply(
          hydra.lib.lists.Length.apply(rem),
          maxlen),
        () -> hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
          rem,
          prev)),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(prefix.get()),
          () -> ((helper.get()).apply(hydra.lib.lists.Cons.apply(
            trunc.get(),
            prev))).apply(hydra.lib.lists.Drop.apply(
            maxlen,
            rem)),
          () -> ((helper.get()).apply(hydra.lib.lists.Cons.apply(
            hydra.lib.lists.Init.apply(prefix.get()),
            prev))).apply(hydra.lib.lists.Concat2.apply(
            suffix.get(),
            hydra.lib.lists.Drop.apply(
              maxlen,
              rem)))));
    })));
    return hydra.lib.strings.FromList.apply(hydra.lib.lists.Intercalate.apply(
      java.util.List.of(10),
      ((helper.get()).apply((java.util.List<java.util.List<Integer>>) (java.util.List.<java.util.List<Integer>>of()))).apply(hydra.lib.strings.ToList.apply(input))));
  }
}
