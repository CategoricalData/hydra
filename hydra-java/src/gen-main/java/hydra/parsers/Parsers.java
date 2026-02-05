// Note: this is an automatically generated file. Do not edit.

package hydra.parsers;

/**
 * General-purpose parser combinators
 */
public interface Parsers {
  static <T0> hydra.parsing.Parser<T0> alt(hydra.parsing.Parser<T0> p1, hydra.parsing.Parser<T0> p2) {
    return (hydra.parsing.Parser<T0>) (new hydra.parsing.Parser((java.util.function.Function<String, hydra.parsing.ParseResult<T0>>) (v1 -> hydra.parsers.Parsers.<T0>alt_parse(
      (p1),
      (p2),
      (v1)))));
  }
  
  static <T0> hydra.parsing.ParseResult<T0> alt_parse(hydra.parsing.Parser<T0> p1, hydra.parsing.Parser<T0> p2, String input) {
    return ((java.util.function.Function<hydra.parsing.ParseResult<T0>, hydra.parsing.ParseResult<T0>>) (v1 -> ((java.util.function.Function<hydra.parsing.ParseResult<T0>, hydra.parsing.ParseResult<T0>>) ((java.util.function.Function<hydra.parsing.ParseResult<T0>, hydra.parsing.ParseResult<T0>>) (u -> ((u)).accept(new hydra.parsing.ParseResult.PartialVisitor<>() {
      @Override
      public hydra.parsing.ParseResult<T0> visit(hydra.parsing.ParseResult.Success<T0> s) {
        return (hydra.parsing.ParseResult<T0>) (new hydra.parsing.ParseResult.Success(((s)).value));
      }
      
      @Override
      public hydra.parsing.ParseResult<T0> visit(hydra.parsing.ParseResult.Failure<T0> e) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (((e)).value).remainder,
            (input)),
          () -> (((java.util.function.Function<hydra.parsing.Parser<T0>, java.util.function.Function<String, hydra.parsing.ParseResult<T0>>>) (wrapped -> ((wrapped)).value)).apply((p2))).apply((input)),
          () -> (hydra.parsing.ParseResult<T0>) (new hydra.parsing.ParseResult.Failure(((e)).value)));
      }
    })))).apply((v1)))).apply((((java.util.function.Function<hydra.parsing.Parser<T0>, java.util.function.Function<String, hydra.parsing.ParseResult<T0>>>) (wrapped -> ((wrapped)).value)).apply((p1))).apply((input)));
  }
  
  static hydra.parsing.Parser<Integer> anyChar() {
    return hydra.parsers.Parsers.satisfy((java.util.function.Function<Integer, Boolean>) (ignored -> true));
  }
  
  static <T0, T1> hydra.parsing.Parser<T1> apply(hydra.parsing.Parser<java.util.function.Function<T0, T1>> pf, hydra.parsing.Parser<T0> pa) {
    return (hydra.parsing.Parser<T1>) (new hydra.parsing.Parser((java.util.function.Function<String, hydra.parsing.ParseResult<T1>>) (v1 -> hydra.parsers.Parsers.<T0, T1>apply_parse(
      (pa),
      (pf),
      (v1)))));
  }
  
  static <T0, T1> hydra.parsing.ParseResult<T1> apply_parse(hydra.parsing.Parser<T0> pa, hydra.parsing.Parser<java.util.function.Function<T0, T1>> pf, String input) {
    return ((java.util.function.Function<hydra.parsing.ParseResult<java.util.function.Function<T0, T1>>, hydra.parsing.ParseResult<T1>>) (v1 -> ((java.util.function.Function<hydra.parsing.ParseResult<java.util.function.Function<T0, T1>>, hydra.parsing.ParseResult<T1>>) ((java.util.function.Function<hydra.parsing.ParseResult<java.util.function.Function<T0, T1>>, hydra.parsing.ParseResult<T1>>) (u -> ((u)).accept(new hydra.parsing.ParseResult.PartialVisitor<>() {
      @Override
      public hydra.parsing.ParseResult<T1> visit(hydra.parsing.ParseResult.Success<java.util.function.Function<T0, T1>> sf) {
        return ((java.util.function.Function<hydra.parsing.ParseResult<T0>, hydra.parsing.ParseResult<T1>>) (v12 -> ((java.util.function.Function<hydra.parsing.ParseResult<T0>, hydra.parsing.ParseResult<T1>>) ((java.util.function.Function<hydra.parsing.ParseResult<T0>, hydra.parsing.ParseResult<T1>>) (u -> ((u)).accept(new hydra.parsing.ParseResult.PartialVisitor<>() {
          @Override
          public hydra.parsing.ParseResult<T1> visit(hydra.parsing.ParseResult.Success<T0> sa) {
            return (hydra.parsing.ParseResult<T1>) (new hydra.parsing.ParseResult.Success((hydra.parsing.ParseSuccess<T1>) (new hydra.parsing.ParseSuccess<T1>((((java.util.function.Function<hydra.parsing.ParseSuccess<java.util.function.Function<T0, T1>>, java.util.function.Function<T0, T1>>) (projected -> projected.value)).apply(((sf)).value)).apply(((java.util.function.Function<hydra.parsing.ParseSuccess<T0>, T0>) (projected -> projected.value)).apply(((sa)).value)), ((java.util.function.Function<hydra.parsing.ParseSuccess<T0>, String>) (projected -> projected.remainder)).apply(((sa)).value)))));
          }
          
          @Override
          public hydra.parsing.ParseResult<T1> visit(hydra.parsing.ParseResult.Failure<T0> e) {
            return (hydra.parsing.ParseResult<T1>) (new hydra.parsing.ParseResult.Failure(((e)).value));
          }
        })))).apply((v12)))).apply((((java.util.function.Function<hydra.parsing.Parser<T0>, java.util.function.Function<String, hydra.parsing.ParseResult<T0>>>) (wrapped -> ((wrapped)).value)).apply((pa))).apply(((java.util.function.Function<hydra.parsing.ParseSuccess<java.util.function.Function<T0, T1>>, String>) (projected -> projected.remainder)).apply(((sf)).value)));
      }
      
      @Override
      public hydra.parsing.ParseResult<T1> visit(hydra.parsing.ParseResult.Failure<java.util.function.Function<T0, T1>> e) {
        return (hydra.parsing.ParseResult<T1>) (new hydra.parsing.ParseResult.Failure(((e)).value));
      }
    })))).apply((v1)))).apply((((java.util.function.Function<hydra.parsing.Parser<java.util.function.Function<T0, T1>>, java.util.function.Function<String, hydra.parsing.ParseResult<java.util.function.Function<T0, T1>>>>) (wrapped -> ((wrapped)).value)).apply((pf))).apply((input)));
  }
  
  static <T0, T1, T2> hydra.parsing.Parser<T2> between(hydra.parsing.Parser<T0> open, hydra.parsing.Parser<T1> close, hydra.parsing.Parser<T2> p) {
    return hydra.parsers.Parsers.<T0, T2>bind(
      (open),
      (java.util.function.Function<T0, hydra.parsing.Parser<T2>>) (ignored -> hydra.parsers.Parsers.<T2, T2>bind(
        (p),
        (java.util.function.Function<T2, hydra.parsing.Parser<T2>>) (x -> hydra.parsers.Parsers.<T1, T2>bind(
          (close),
          (java.util.function.Function<T1, hydra.parsing.Parser<T2>>) (_2 -> hydra.parsers.Parsers.<T2>pure((x))))))));
  }
  
  static <T0, T1> hydra.parsing.Parser<T1> bind(hydra.parsing.Parser<T0> pa, java.util.function.Function<T0, hydra.parsing.Parser<T1>> f) {
    return (hydra.parsing.Parser<T1>) (new hydra.parsing.Parser((java.util.function.Function<String, hydra.parsing.ParseResult<T1>>) (v1 -> hydra.parsers.Parsers.<T0, T1>bind_parse(
      (f),
      (pa),
      (v1)))));
  }
  
  static <T0, T1> hydra.parsing.ParseResult<T1> bind_parse(java.util.function.Function<T0, hydra.parsing.Parser<T1>> f, hydra.parsing.Parser<T0> pa, String input) {
    return ((java.util.function.Function<hydra.parsing.ParseResult<T0>, hydra.parsing.ParseResult<T1>>) (v1 -> ((java.util.function.Function<hydra.parsing.ParseResult<T0>, hydra.parsing.ParseResult<T1>>) ((java.util.function.Function<hydra.parsing.ParseResult<T0>, hydra.parsing.ParseResult<T1>>) (u -> ((u)).accept(new hydra.parsing.ParseResult.PartialVisitor<>() {
      @Override
      public hydra.parsing.ParseResult<T1> visit(hydra.parsing.ParseResult.Success<T0> s) {
        return (((java.util.function.Function<hydra.parsing.Parser<T1>, java.util.function.Function<String, hydra.parsing.ParseResult<T1>>>) (wrapped -> ((wrapped)).value)).apply(((f)).apply(((java.util.function.Function<hydra.parsing.ParseSuccess<T0>, T0>) (projected -> projected.value)).apply(((s)).value)))).apply(((java.util.function.Function<hydra.parsing.ParseSuccess<T0>, String>) (projected -> projected.remainder)).apply(((s)).value));
      }
      
      @Override
      public hydra.parsing.ParseResult<T1> visit(hydra.parsing.ParseResult.Failure<T0> e) {
        return (hydra.parsing.ParseResult<T1>) (new hydra.parsing.ParseResult.Failure(((e)).value));
      }
    })))).apply((v1)))).apply((((java.util.function.Function<hydra.parsing.Parser<T0>, java.util.function.Function<String, hydra.parsing.ParseResult<T0>>>) (wrapped -> ((wrapped)).value)).apply((pa))).apply((input)));
  }
  
  static hydra.parsing.Parser<Integer> char_(Integer c) {
    return hydra.parsers.Parsers.satisfy((java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Equal.apply(
      (x),
      (c))));
  }
  
  static <T0> hydra.parsing.Parser<T0> choice(java.util.List<hydra.parsing.Parser<T0>> ps) {
    return hydra.lib.lists.Foldl.apply(
      p0 -> p1 -> hydra.parsers.Parsers.<T0>alt(
        (p0),
        (p1)),
      hydra.parsers.Parsers.<T0>fail("no choice matched"),
      (ps));
  }
  
  static hydra.parsing.Parser<java.lang.Void> eof() {
    return (hydra.parsing.Parser<java.lang.Void>) (new hydra.parsing.Parser((java.util.function.Function<String, hydra.parsing.ParseResult<java.lang.Void>>) (input -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        (input),
        ""),
      () -> (hydra.parsing.ParseResult<java.lang.Void>) (new hydra.parsing.ParseResult.Success((hydra.parsing.ParseSuccess<java.lang.Void>) (new hydra.parsing.ParseSuccess<java.lang.Void>(null, "")))),
      () -> (hydra.parsing.ParseResult<java.lang.Void>) (new hydra.parsing.ParseResult.Failure(new hydra.parsing.ParseError("expected end of input", (input))))))));
  }
  
  static <T0> hydra.parsing.Parser<T0> fail(String msg) {
    return (hydra.parsing.Parser<T0>) (new hydra.parsing.Parser((java.util.function.Function<String, hydra.parsing.ParseResult<T0>>) (input -> (hydra.parsing.ParseResult<T0>) (new hydra.parsing.ParseResult.Failure(new hydra.parsing.ParseError((msg), (input)))))));
  }
  
  static <T0> hydra.parsing.Parser<T0> lazy(java.util.function.Function<java.lang.Void, hydra.parsing.Parser<T0>> f) {
    return (hydra.parsing.Parser<T0>) (new hydra.parsing.Parser((java.util.function.Function<String, hydra.parsing.ParseResult<T0>>) (input -> (((java.util.function.Function<hydra.parsing.Parser<T0>, java.util.function.Function<String, hydra.parsing.ParseResult<T0>>>) (wrapped -> ((wrapped)).value)).apply(((f)).apply(null))).apply((input)))));
  }
  
  static <T0> hydra.parsing.Parser<java.util.List<T0>> many(hydra.parsing.Parser<T0> p) {
    return hydra.parsers.Parsers.<java.util.List<T0>>alt(
      hydra.parsers.Parsers.<T0>some((p)),
      hydra.parsers.Parsers.<java.util.List<T0>>pure((java.util.List<T0>) (java.util.List.<T0>of())));
  }
  
  static <T0, T1> hydra.parsing.Parser<T1> map(java.util.function.Function<T0, T1> f, hydra.parsing.Parser<T0> pa) {
    return (hydra.parsing.Parser<T1>) (new hydra.parsing.Parser((java.util.function.Function<String, hydra.parsing.ParseResult<T1>>) (v1 -> hydra.parsers.Parsers.<T0, T1>map_parse(
      (f),
      (pa),
      (v1)))));
  }
  
  static <T0, T1> hydra.parsing.ParseResult<T1> map_parse(java.util.function.Function<T0, T1> f, hydra.parsing.Parser<T0> pa, String input) {
    return ((java.util.function.Function<hydra.parsing.ParseResult<T0>, hydra.parsing.ParseResult<T1>>) (v1 -> ((java.util.function.Function<hydra.parsing.ParseResult<T0>, hydra.parsing.ParseResult<T1>>) ((java.util.function.Function<hydra.parsing.ParseResult<T0>, hydra.parsing.ParseResult<T1>>) (u -> ((u)).accept(new hydra.parsing.ParseResult.PartialVisitor<>() {
      @Override
      public hydra.parsing.ParseResult<T1> visit(hydra.parsing.ParseResult.Success<T0> s) {
        return (hydra.parsing.ParseResult<T1>) (new hydra.parsing.ParseResult.Success((hydra.parsing.ParseSuccess<T1>) (new hydra.parsing.ParseSuccess<T1>(((f)).apply(((java.util.function.Function<hydra.parsing.ParseSuccess<T0>, T0>) (projected -> projected.value)).apply(((s)).value)), ((java.util.function.Function<hydra.parsing.ParseSuccess<T0>, String>) (projected -> projected.remainder)).apply(((s)).value)))));
      }
      
      @Override
      public hydra.parsing.ParseResult<T1> visit(hydra.parsing.ParseResult.Failure<T0> e) {
        return (hydra.parsing.ParseResult<T1>) (new hydra.parsing.ParseResult.Failure(((e)).value));
      }
    })))).apply((v1)))).apply((((java.util.function.Function<hydra.parsing.Parser<T0>, java.util.function.Function<String, hydra.parsing.ParseResult<T0>>>) (wrapped -> ((wrapped)).value)).apply((pa))).apply((input)));
  }
  
  static <T0> hydra.parsing.Parser<hydra.util.Maybe<T0>> optional(hydra.parsing.Parser<T0> p) {
    return hydra.parsers.Parsers.<hydra.util.Maybe<T0>>alt(
      hydra.parsers.Parsers.<T0, hydra.util.Maybe<T0>>map(
        (java.util.function.Function<T0, hydra.util.Maybe<T0>>) ((hydra.lib.maybes.Pure::apply)),
        (p)),
      hydra.parsers.Parsers.<hydra.util.Maybe<T0>>pure((hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing())));
  }
  
  static <T0> hydra.parsing.Parser<T0> pure(T0 a) {
    return (hydra.parsing.Parser<T0>) (new hydra.parsing.Parser((java.util.function.Function<String, hydra.parsing.ParseResult<T0>>) (input -> (hydra.parsing.ParseResult<T0>) (new hydra.parsing.ParseResult.Success((hydra.parsing.ParseSuccess<T0>) (new hydra.parsing.ParseSuccess<T0>((a), (input))))))));
  }
  
  static <T0> hydra.parsing.ParseResult<T0> runParser(hydra.parsing.Parser<T0> p, String input) {
    return (((java.util.function.Function<hydra.parsing.Parser<T0>, java.util.function.Function<String, hydra.parsing.ParseResult<T0>>>) (wrapped -> ((wrapped)).value)).apply((p))).apply((input));
  }
  
  static hydra.parsing.Parser<Integer> satisfy(java.util.function.Function<Integer, Boolean> pred) {
    java.util.function.Function<String, hydra.parsing.ParseResult<Integer>> parse = (java.util.function.Function<String, hydra.parsing.ParseResult<Integer>>) (input -> {
      java.util.List<Integer> codes = hydra.lib.strings.ToList.apply((input));
      return hydra.lib.maybes.Maybe.apply(
        (hydra.parsing.ParseResult<Integer>) (new hydra.parsing.ParseResult.Failure(new hydra.parsing.ParseError("unexpected end of input", (input)))),
        (java.util.function.Function<Integer, hydra.parsing.ParseResult<Integer>>) (c -> {
          hydra.util.Lazy<String> rest = new hydra.util.Lazy<>(() -> hydra.lib.strings.FromList.apply(hydra.lib.lists.Drop.apply(
            1,
            (codes))));
          return hydra.lib.logic.IfElse.lazy(
            ((pred)).apply((c)),
            () -> (hydra.parsing.ParseResult<Integer>) (new hydra.parsing.ParseResult.Success((hydra.parsing.ParseSuccess<Integer>) (new hydra.parsing.ParseSuccess<Integer>((c), rest.get())))),
            () -> (hydra.parsing.ParseResult<Integer>) (new hydra.parsing.ParseResult.Failure(new hydra.parsing.ParseError("character did not satisfy predicate", (input)))));
        }),
        hydra.lib.lists.SafeHead.apply((codes)));
    });
    return (hydra.parsing.Parser<Integer>) (new hydra.parsing.Parser((parse)));
  }
  
  static <T0, T1> hydra.parsing.Parser<java.util.List<T0>> sepBy(hydra.parsing.Parser<T0> p, hydra.parsing.Parser<T1> sep) {
    return hydra.parsers.Parsers.<java.util.List<T0>>alt(
      hydra.parsers.Parsers.<T0, T1>sepBy1(
        (p),
        (sep)),
      hydra.parsers.Parsers.<java.util.List<T0>>pure((java.util.List<T0>) (java.util.List.<T0>of())));
  }
  
  static <T0, T1> hydra.parsing.Parser<java.util.List<T0>> sepBy1(hydra.parsing.Parser<T0> p, hydra.parsing.Parser<T1> sep) {
    return hydra.parsers.Parsers.<T0, java.util.List<T0>>bind(
      (p),
      (java.util.function.Function<T0, hydra.parsing.Parser<java.util.List<T0>>>) (x -> hydra.parsers.Parsers.<java.util.List<T0>, java.util.List<T0>>bind(
        hydra.parsers.Parsers.<T0>many(hydra.parsers.Parsers.<T1, T0>bind(
          (sep),
          (java.util.function.Function<T1, hydra.parsing.Parser<T0>>) (ignored -> (p)))),
        (java.util.function.Function<java.util.List<T0>, hydra.parsing.Parser<java.util.List<T0>>>) (xs -> hydra.parsers.Parsers.<java.util.List<T0>>pure(hydra.lib.lists.Cons.apply(
          (x),
          (xs)))))));
  }
  
  static <T0> hydra.parsing.Parser<java.util.List<T0>> some(hydra.parsing.Parser<T0> p) {
    return hydra.parsers.Parsers.<T0, java.util.List<T0>>bind(
      (p),
      (java.util.function.Function<T0, hydra.parsing.Parser<java.util.List<T0>>>) (x -> hydra.parsers.Parsers.<java.util.List<T0>, java.util.List<T0>>bind(
        hydra.parsers.Parsers.<T0>many((p)),
        (java.util.function.Function<java.util.List<T0>, hydra.parsing.Parser<java.util.List<T0>>>) (xs -> hydra.parsers.Parsers.<java.util.List<T0>>pure(hydra.lib.lists.Cons.apply(
          (x),
          (xs)))))));
  }
  
  static hydra.parsing.Parser<String> string(String str) {
    return (hydra.parsing.Parser<String>) (new hydra.parsing.Parser((java.util.function.Function<String, hydra.parsing.ParseResult<String>>) (input -> {
      java.util.List<Integer> inputCodes = hydra.lib.strings.ToList.apply((input));
      java.util.List<Integer> strCodes = hydra.lib.strings.ToList.apply((str));
      hydra.util.Lazy<Integer> strLen = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply((strCodes)));
      hydra.util.Lazy<java.util.List<Integer>> inputPrefix = new hydra.util.Lazy<>(() -> hydra.lib.lists.Take.apply(
        strLen.get(),
        (inputCodes)));
      return hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          (strCodes),
          inputPrefix.get()),
        () -> (hydra.parsing.ParseResult<String>) (new hydra.parsing.ParseResult.Success((hydra.parsing.ParseSuccess<String>) (new hydra.parsing.ParseSuccess<String>((str), hydra.lib.strings.FromList.apply(hydra.lib.lists.Drop.apply(
          strLen.get(),
          (inputCodes))))))),
        () -> (hydra.parsing.ParseResult<String>) (new hydra.parsing.ParseResult.Failure(new hydra.parsing.ParseError(hydra.lib.strings.Cat2.apply(
          "expected: ",
          (str)), (input)))));
    })));
  }
}
