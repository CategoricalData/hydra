// Note: this is an automatically generated file. Do not edit.

package hydra.formatting;

/**
 * String formatting types and functions.
 */
public interface Formatting {
  static String capitalize(String v1) {
    return hydra.formatting.Formatting.mapFirstLetter(
      (hydra.lib.strings.ToUpper::apply),
      (v1));
  }
  
  static String convertCase(hydra.util.CaseConvention from, hydra.util.CaseConvention to, String original) {
    java.util.function.Function<java.util.List<java.util.List<Integer>>, java.util.function.Function<Integer, java.util.List<java.util.List<Integer>>>> splitOnUppercase = (java.util.function.Function<java.util.List<java.util.List<Integer>>, java.util.function.Function<Integer, java.util.List<java.util.List<Integer>>>>) (acc -> (java.util.function.Function<Integer, java.util.List<java.util.List<Integer>>>) (c -> hydra.lib.lists.Concat2.apply(
      hydra.lib.logic.IfElse.apply(
        hydra.lib.chars.IsUpper.apply((c)),
        java.util.List.of((java.util.List<Integer>) (java.util.List.<Integer>of())),
        (java.util.List<java.util.List<Integer>>) (java.util.List.<java.util.List<Integer>>of())),
      hydra.lib.lists.Cons.apply(
        hydra.lib.lists.Cons.apply(
          (c),
          hydra.lib.lists.Head.apply((acc))),
        hydra.lib.lists.Tail.apply((acc))))));
    java.util.List<String> byCaps = hydra.lib.lists.Map.apply(
      (hydra.lib.strings.FromList::apply),
      hydra.lib.lists.Foldl.apply(
        (splitOnUppercase),
        java.util.List.of((java.util.List<Integer>) (java.util.List.<Integer>of())),
        hydra.lib.lists.Reverse.apply(hydra.lib.strings.ToList.apply(hydra.formatting.Formatting.decapitalize((original))))));
    java.util.List<String> byUnderscores = hydra.lib.strings.SplitOn.apply(
      "_",
      (original));
    java.util.List<String> parts = ((from)).accept(new hydra.util.CaseConvention.Visitor<>() {
      @Override
      public java.util.List<String> visit(hydra.util.CaseConvention.Camel ignored) {
        return (byCaps);
      }
      
      @Override
      public java.util.List<String> visit(hydra.util.CaseConvention.Pascal ignored) {
        return (byCaps);
      }
      
      @Override
      public java.util.List<String> visit(hydra.util.CaseConvention.LowerSnake ignored) {
        return (byUnderscores);
      }
      
      @Override
      public java.util.List<String> visit(hydra.util.CaseConvention.UpperSnake ignored) {
        return (byUnderscores);
      }
    });
    return ((to)).accept(new hydra.util.CaseConvention.Visitor<>() {
      @Override
      public String visit(hydra.util.CaseConvention.Camel ignored) {
        return hydra.formatting.Formatting.decapitalize(hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<String, String>) (arg_ -> hydra.formatting.Formatting.capitalize(hydra.lib.strings.ToLower.apply((arg_)))),
          (parts))));
      }
      
      @Override
      public String visit(hydra.util.CaseConvention.Pascal ignored) {
        return hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<String, String>) (arg_ -> hydra.formatting.Formatting.capitalize(hydra.lib.strings.ToLower.apply((arg_)))),
          (parts)));
      }
      
      @Override
      public String visit(hydra.util.CaseConvention.LowerSnake ignored) {
        return hydra.lib.strings.Intercalate.apply(
          "_",
          hydra.lib.lists.Map.apply(
            (hydra.lib.strings.ToLower::apply),
            (parts)));
      }
      
      @Override
      public String visit(hydra.util.CaseConvention.UpperSnake ignored) {
        return hydra.lib.strings.Intercalate.apply(
          "_",
          hydra.lib.lists.Map.apply(
            (hydra.lib.strings.ToUpper::apply),
            (parts)));
      }
    });
  }
  
  static String convertCaseCamelToLowerSnake(String v1) {
    return hydra.formatting.Formatting.convertCase(
      new hydra.util.CaseConvention.Camel(true),
      new hydra.util.CaseConvention.LowerSnake(true),
      (v1));
  }
  
  static String convertCaseCamelToUpperSnake(String v1) {
    return hydra.formatting.Formatting.convertCase(
      new hydra.util.CaseConvention.Camel(true),
      new hydra.util.CaseConvention.UpperSnake(true),
      (v1));
  }
  
  static String convertCasePascalToUpperSnake(String v1) {
    return hydra.formatting.Formatting.convertCase(
      new hydra.util.CaseConvention.Pascal(true),
      new hydra.util.CaseConvention.UpperSnake(true),
      (v1));
  }
  
  static String decapitalize(String v1) {
    return hydra.formatting.Formatting.mapFirstLetter(
      (hydra.lib.strings.ToLower::apply),
      (v1));
  }
  
  static String escapeWithUnderscore(java.util.Set<String> reserved, String s) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.sets.Member.apply(
        (s),
        (reserved)),
      hydra.lib.strings.Cat.apply(java.util.List.of(
        (s),
        "_")),
      (s));
  }
  
  static String indentLines(String s) {
    java.util.function.Function<String, String> indent = (java.util.function.Function<String, String>) (l -> hydra.lib.strings.Cat.apply(java.util.List.of(
      "    ",
      (l))));
    return hydra.lib.strings.Unlines.apply(hydra.lib.lists.Map.apply(
      (indent),
      hydra.lib.strings.Lines.apply((s))));
  }
  
  static String javaStyleComment(String s) {
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      hydra.lib.strings.Cat.apply(java.util.List.of(
        hydra.lib.strings.Cat.apply(java.util.List.of(
          "/**\n",
          " * ")),
        (s))),
      "\n */"));
  }
  
  static String mapFirstLetter(java.util.function.Function<String, String> mapping, String s) {
    java.util.List<Integer> list = hydra.lib.strings.ToList.apply((s));
    String firstLetter = ((mapping)).apply(hydra.lib.strings.FromList.apply(hydra.lib.lists.Pure.apply(hydra.lib.lists.Head.apply((list)))));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.strings.Null.apply((s)),
      (s),
      hydra.lib.strings.Cat2.apply(
        (firstLetter),
        hydra.lib.strings.FromList.apply(hydra.lib.lists.Tail.apply((list)))));
  }
  
  static String nonAlnumToUnderscores(String input) {
    java.util.function.Function<Integer, Boolean> isAlnum = (java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.logic.Or.apply(
      hydra.lib.logic.And.apply(
        hydra.lib.equality.Gte.apply(
          (c),
          65),
        hydra.lib.equality.Lte.apply(
          (c),
          90)),
      hydra.lib.logic.Or.apply(
        hydra.lib.logic.And.apply(
          hydra.lib.equality.Gte.apply(
            (c),
            97),
          hydra.lib.equality.Lte.apply(
            (c),
            122)),
        hydra.lib.logic.And.apply(
          hydra.lib.equality.Gte.apply(
            (c),
            48),
          hydra.lib.equality.Lte.apply(
            (c),
            57)))));
    java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>, java.util.function.Function<Integer, hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>>> replace = (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>, java.util.function.Function<Integer, hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>>>) (p -> (java.util.function.Function<Integer, hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean>>) (c -> {
      Boolean b = ((p)).object2;
      java.util.List<Integer> s = ((p)).object1;
      return hydra.lib.logic.IfElse.apply(
        ((isAlnum)).apply((c)),
        new hydra.util.Tuple.Tuple2(hydra.lib.lists.Cons.apply(
          (c),
          (s)), false),
        hydra.lib.logic.IfElse.apply(
          (b),
          new hydra.util.Tuple.Tuple2((s), true),
          new hydra.util.Tuple.Tuple2(hydra.lib.lists.Cons.apply(
            95,
            (s)), true)));
    }));
    hydra.util.Tuple.Tuple2<java.util.List<Integer>, Boolean> result = hydra.lib.lists.Foldl.apply(
      (replace),
      new hydra.util.Tuple.Tuple2((java.util.List<Integer>) (java.util.List.<Integer>of()), false),
      hydra.lib.strings.ToList.apply((input)));
    return hydra.lib.strings.FromList.apply(hydra.lib.lists.Reverse.apply(((result)).object1));
  }
  
  static String sanitizeWithUnderscores(java.util.Set<String> reserved, String s) {
    return hydra.formatting.Formatting.escapeWithUnderscore(
      (reserved),
      hydra.formatting.Formatting.nonAlnumToUnderscores((s)));
  }
  
  static <T0> String showList(java.util.function.Function<T0, String> f, java.util.List<T0> els) {
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      "[",
      hydra.lib.strings.Intercalate.apply(
        ", ",
        hydra.lib.lists.Map.apply(
          (f),
          (els))),
      "]"));
  }
  
  static String stripLeadingAndTrailingWhitespace(String s) {
    return hydra.lib.strings.FromList.apply(hydra.lib.lists.DropWhile.apply(
      (hydra.lib.chars.IsSpace::apply),
      hydra.lib.lists.Reverse.apply(hydra.lib.lists.DropWhile.apply(
        (hydra.lib.chars.IsSpace::apply),
        hydra.lib.lists.Reverse.apply(hydra.lib.strings.ToList.apply((s)))))));
  }
  
  static String withCharacterAliases(String original) {
    java.util.Map<Integer, String> aliases = hydra.lib.maps.FromList.apply(java.util.List.of(
      new hydra.util.Tuple.Tuple2(32, "sp"),
      new hydra.util.Tuple.Tuple2(33, "excl"),
      new hydra.util.Tuple.Tuple2(34, "quot"),
      new hydra.util.Tuple.Tuple2(35, "num"),
      new hydra.util.Tuple.Tuple2(36, "dollar"),
      new hydra.util.Tuple.Tuple2(37, "percnt"),
      new hydra.util.Tuple.Tuple2(38, "amp"),
      new hydra.util.Tuple.Tuple2(39, "apos"),
      new hydra.util.Tuple.Tuple2(40, "lpar"),
      new hydra.util.Tuple.Tuple2(41, "rpar"),
      new hydra.util.Tuple.Tuple2(42, "ast"),
      new hydra.util.Tuple.Tuple2(43, "plus"),
      new hydra.util.Tuple.Tuple2(44, "comma"),
      new hydra.util.Tuple.Tuple2(45, "minus"),
      new hydra.util.Tuple.Tuple2(46, "period"),
      new hydra.util.Tuple.Tuple2(47, "sol"),
      new hydra.util.Tuple.Tuple2(58, "colon"),
      new hydra.util.Tuple.Tuple2(59, "semi"),
      new hydra.util.Tuple.Tuple2(60, "lt"),
      new hydra.util.Tuple.Tuple2(61, "equals"),
      new hydra.util.Tuple.Tuple2(62, "gt"),
      new hydra.util.Tuple.Tuple2(63, "quest"),
      new hydra.util.Tuple.Tuple2(64, "commat"),
      new hydra.util.Tuple.Tuple2(91, "lsqb"),
      new hydra.util.Tuple.Tuple2(92, "bsol"),
      new hydra.util.Tuple.Tuple2(93, "rsqb"),
      new hydra.util.Tuple.Tuple2(94, "circ"),
      new hydra.util.Tuple.Tuple2(95, "lowbar"),
      new hydra.util.Tuple.Tuple2(96, "grave"),
      new hydra.util.Tuple.Tuple2(123, "lcub"),
      new hydra.util.Tuple.Tuple2(124, "verbar"),
      new hydra.util.Tuple.Tuple2(125, "rcub"),
      new hydra.util.Tuple.Tuple2(126, "tilde")));
    java.util.function.Function<Integer, java.util.List<Integer>> alias = (java.util.function.Function<Integer, java.util.List<Integer>>) (c -> hydra.lib.maybes.FromMaybe.apply(
      hydra.lib.lists.Pure.apply((c)),
      hydra.lib.maybes.Map.apply(
        (hydra.lib.strings.ToList::apply),
        hydra.lib.maps.Lookup.apply(
          (c),
          (aliases)))));
    return hydra.lib.strings.FromList.apply(hydra.lib.lists.Filter.apply(
      (hydra.lib.chars.IsAlphaNum::apply),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (alias),
        hydra.lib.strings.ToList.apply((original))))));
  }
  
  static String wrapLine(Integer maxlen, String input) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<java.util.List<Integer>>, java.util.function.Function<java.util.List<Integer>, java.util.List<java.util.List<Integer>>>>> helper = new java.util.concurrent.atomic.AtomicReference<>();
    helper.set((java.util.function.Function<java.util.List<java.util.List<Integer>>, java.util.function.Function<java.util.List<Integer>, java.util.List<java.util.List<Integer>>>>) (prev -> (java.util.function.Function<java.util.List<Integer>, java.util.List<java.util.List<Integer>>>) (rem -> {
      java.util.List<Integer> trunc = hydra.lib.lists.Take.apply(
        (maxlen),
        (rem));
      hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<Integer>> spanResult = hydra.lib.lists.Span.apply(
        (java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.logic.And.apply(
          hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
            (c),
            32)),
          hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
            (c),
            9)))),
        hydra.lib.lists.Reverse.apply((trunc)));
      java.util.List<Integer> prefix = hydra.lib.lists.Reverse.apply(spanResult.object2);
      java.util.List<Integer> suffix = hydra.lib.lists.Reverse.apply(spanResult.object1);
      return hydra.lib.logic.IfElse.apply(
        hydra.lib.equality.Lte.apply(
          hydra.lib.lists.Length.apply((rem)),
          (maxlen)),
        hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
          (rem),
          (prev))),
        hydra.lib.logic.IfElse.apply(
          hydra.lib.lists.Null.apply((prefix)),
          ((helper.get()).apply(hydra.lib.lists.Cons.apply(
            (trunc),
            (prev)))).apply(hydra.lib.lists.Drop.apply(
            (maxlen),
            (rem))),
          ((helper.get()).apply(hydra.lib.lists.Cons.apply(
            hydra.lib.lists.Init.apply((prefix)),
            (prev)))).apply(hydra.lib.lists.Concat2.apply(
            (suffix),
            hydra.lib.lists.Drop.apply(
              (maxlen),
              (rem))))));
    })));
    return hydra.lib.strings.FromList.apply(hydra.lib.lists.Intercalate.apply(
      java.util.List.of(10),
      ((helper.get()).apply((java.util.List<java.util.List<Integer>>) (java.util.List.<java.util.List<Integer>>of()))).apply(hydra.lib.strings.ToList.apply((input)))));
  }
}
