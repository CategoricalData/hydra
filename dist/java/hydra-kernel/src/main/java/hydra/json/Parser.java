// Note: this is an automatically generated file. Do not edit.

package hydra.json;

/**
 * JSON parser using Hydra parser combinators
 */
public interface Parser {
  static hydra.parsing.Parser<Integer> digit() {
    return hydra.Parsers.satisfy((java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.logic.And.apply(
      hydra.lib.equality.Gte.apply(
        c,
        48),
      hydra.lib.equality.Lte.apply(
        c,
        57))));
  }

  static hydra.parsing.Parser<String> digits() {
    return hydra.Parsers.map(
      hydra.lib.strings.FromList::apply,
      hydra.Parsers.some(hydra.json.Parser.digit()));
  }

  static hydra.parsing.Parser<hydra.json.model.Value> jsonArray() {
    return hydra.Parsers.map(
      (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.json.model.Value>) (x -> new hydra.json.model.Value.Array(x)),
      hydra.Parsers.between(
        hydra.json.Parser.token(hydra.Parsers.char_(91)),
        hydra.json.Parser.token(hydra.Parsers.char_(93)),
        hydra.Parsers.sepBy(
          hydra.Parsers.lazy((java.util.function.Function<java.lang.Void, hydra.parsing.Parser<hydra.json.model.Value>>) (ignored -> hydra.json.Parser.jsonValue())),
          hydra.json.Parser.token(hydra.Parsers.char_(44)))));
  }

  static hydra.parsing.Parser<hydra.json.model.Value> jsonBool() {
    return hydra.Parsers.alt(
      hydra.Parsers.map(
        (java.util.function.Function<String, hydra.json.model.Value>) (ignored -> new hydra.json.model.Value.Boolean_(true)),
        hydra.json.Parser.token(hydra.Parsers.string("true"))),
      hydra.Parsers.map(
        (java.util.function.Function<String, hydra.json.model.Value>) (ignored -> new hydra.json.model.Value.Boolean_(false)),
        hydra.json.Parser.token(hydra.Parsers.string("false"))));
  }

  static hydra.parsing.Parser<Integer> jsonEscapeChar() {
    return hydra.Parsers.choice(java.util.Arrays.asList(
      hydra.Parsers.map(
        (java.util.function.Function<Integer, Integer>) (ignored -> 34),
        hydra.Parsers.char_(34)),
      hydra.Parsers.map(
        (java.util.function.Function<Integer, Integer>) (ignored -> 92),
        hydra.Parsers.char_(92)),
      hydra.Parsers.map(
        (java.util.function.Function<Integer, Integer>) (ignored -> 47),
        hydra.Parsers.char_(47)),
      hydra.Parsers.map(
        (java.util.function.Function<Integer, Integer>) (ignored -> 8),
        hydra.Parsers.char_(98)),
      hydra.Parsers.map(
        (java.util.function.Function<Integer, Integer>) (ignored -> 12),
        hydra.Parsers.char_(102)),
      hydra.Parsers.map(
        (java.util.function.Function<Integer, Integer>) (ignored -> 10),
        hydra.Parsers.char_(110)),
      hydra.Parsers.map(
        (java.util.function.Function<Integer, Integer>) (ignored -> 13),
        hydra.Parsers.char_(114)),
      hydra.Parsers.map(
        (java.util.function.Function<Integer, Integer>) (ignored -> 9),
        hydra.Parsers.char_(116))));
  }

  static hydra.parsing.Parser<hydra.util.Maybe<String>> jsonExponentPart() {
    return hydra.Parsers.optional(hydra.Parsers.bind(
      hydra.Parsers.satisfy((java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.logic.Or.apply(
        hydra.lib.equality.Equal.apply(
          c,
          101),
        hydra.lib.equality.Equal.apply(
          c,
          69)))),
      (java.util.function.Function<Integer, hydra.parsing.Parser<String>>) (ignored -> hydra.Parsers.bind(
        hydra.Parsers.optional(hydra.Parsers.satisfy((java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.logic.Or.apply(
          hydra.lib.equality.Equal.apply(
            c,
            43),
          hydra.lib.equality.Equal.apply(
            c,
            45))))),
        (java.util.function.Function<hydra.util.Maybe<Integer>, hydra.parsing.Parser<String>>) (sign -> hydra.Parsers.map(
          (java.util.function.Function<String, String>) (digits -> hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "e",
              hydra.lib.maybes.Maybe.applyLazy(
                () -> "",
                (java.util.function.Function<Integer, String>) (arg_ -> hydra.lib.strings.FromList.apply(hydra.lib.lists.Pure.apply(arg_))),
                sign)),
            digits)),
          hydra.json.Parser.digits()))))));
  }

  static hydra.parsing.Parser<hydra.util.Maybe<String>> jsonFractionPart() {
    return hydra.Parsers.optional(hydra.Parsers.bind(
      hydra.Parsers.char_(46),
      (java.util.function.Function<Integer, hydra.parsing.Parser<String>>) (ignored -> hydra.Parsers.map(
        (java.util.function.Function<String, String>) (d -> hydra.lib.strings.Cat2.apply(
          ".",
          d)),
        hydra.json.Parser.digits()))));
  }

  static hydra.parsing.Parser<String> jsonIntegerPart() {
    return hydra.Parsers.bind(
      hydra.Parsers.optional(hydra.Parsers.char_(45)),
      (java.util.function.Function<hydra.util.Maybe<Integer>, hydra.parsing.Parser<String>>) (sign -> hydra.Parsers.bind(
        hydra.json.Parser.digits(),
        (java.util.function.Function<String, hydra.parsing.Parser<String>>) (digits -> hydra.Parsers.pure(hydra.lib.maybes.Maybe.applyLazy(
          () -> digits,
          (java.util.function.Function<Integer, String>) (ignored -> hydra.lib.strings.Cat2.apply(
            "-",
            digits)),
          sign))))));
  }

  static hydra.parsing.Parser<hydra.util.Pair<String, hydra.json.model.Value>> jsonKeyValue() {
    return hydra.Parsers.bind(
      hydra.json.Parser.token(hydra.Parsers.bind(
        hydra.Parsers.char_(34),
        (java.util.function.Function<Integer, hydra.parsing.Parser<String>>) (ignored -> hydra.Parsers.bind(
          hydra.Parsers.many(hydra.json.Parser.jsonStringChar()),
          (java.util.function.Function<java.util.List<Integer>, hydra.parsing.Parser<String>>) (chars -> hydra.Parsers.bind(
            hydra.Parsers.char_(34),
            (java.util.function.Function<Integer, hydra.parsing.Parser<String>>) (_2 -> hydra.Parsers.pure(hydra.lib.strings.FromList.apply(chars))))))))),
      (java.util.function.Function<String, hydra.parsing.Parser<hydra.util.Pair<String, hydra.json.model.Value>>>) (key -> hydra.Parsers.bind(
        hydra.json.Parser.token(hydra.Parsers.char_(58)),
        (java.util.function.Function<Integer, hydra.parsing.Parser<hydra.util.Pair<String, hydra.json.model.Value>>>) (ignored -> hydra.Parsers.map(
          (java.util.function.Function<hydra.json.model.Value, hydra.util.Pair<String, hydra.json.model.Value>>) (v -> (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>(key, v)))),
          hydra.Parsers.lazy((java.util.function.Function<java.lang.Void, hydra.parsing.Parser<hydra.json.model.Value>>) (_2 -> hydra.json.Parser.jsonValue())))))));
  }

  static hydra.parsing.Parser<hydra.json.model.Value> jsonNull() {
    return hydra.Parsers.map(
      (java.util.function.Function<String, hydra.json.model.Value>) (ignored -> new hydra.json.model.Value.Null()),
      hydra.json.Parser.token(hydra.Parsers.string("null")));
  }

  static hydra.parsing.Parser<hydra.json.model.Value> jsonNumber() {
    return hydra.json.Parser.token(hydra.Parsers.bind(
      hydra.json.Parser.jsonIntegerPart(),
      (java.util.function.Function<String, hydra.parsing.Parser<hydra.json.model.Value>>) (intPart -> hydra.Parsers.bind(
        hydra.json.Parser.jsonFractionPart(),
        (java.util.function.Function<hydra.util.Maybe<String>, hydra.parsing.Parser<hydra.json.model.Value>>) (fracPart -> hydra.Parsers.bind(
          hydra.json.Parser.jsonExponentPart(),
          (java.util.function.Function<hydra.util.Maybe<String>, hydra.parsing.Parser<hydra.json.model.Value>>) (expPart -> {
            hydra.util.Lazy<String> numStr = new hydra.util.Lazy<>(() -> hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                intPart,
                hydra.lib.maybes.Maybe.applyLazy(
                  () -> "",
                  (java.util.function.Function<String, String>) (hydra.lib.equality.Identity::apply),
                  fracPart)),
              hydra.lib.maybes.Maybe.applyLazy(
                () -> "",
                (java.util.function.Function<String, String>) (hydra.lib.equality.Identity::apply),
                expPart)));
            return hydra.Parsers.pure(new hydra.json.model.Value.Number_(hydra.lib.maybes.Maybe.applyLazy(
              () -> new java.math.BigDecimal("0.0"),
              (java.util.function.Function<java.math.BigDecimal, java.math.BigDecimal>) (hydra.lib.equality.Identity::apply),
              hydra.lib.literals.ReadDecimal.apply(numStr.get()))));
          })))))));
  }

  static hydra.parsing.Parser<hydra.json.model.Value> jsonObject() {
    return hydra.Parsers.map(
      (java.util.function.Function<java.util.List<hydra.util.Pair<String, hydra.json.model.Value>>, hydra.json.model.Value>) (arg_ -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(arg_))),
      hydra.Parsers.between(
        hydra.json.Parser.token(hydra.Parsers.char_(123)),
        hydra.json.Parser.token(hydra.Parsers.char_(125)),
        hydra.Parsers.sepBy(
          hydra.json.Parser.jsonKeyValue(),
          hydra.json.Parser.token(hydra.Parsers.char_(44)))));
  }

  static hydra.parsing.Parser<hydra.json.model.Value> jsonString() {
    return hydra.json.Parser.token(hydra.Parsers.bind(
      hydra.Parsers.char_(34),
      (java.util.function.Function<Integer, hydra.parsing.Parser<hydra.json.model.Value>>) (ignored -> hydra.Parsers.bind(
        hydra.Parsers.many(hydra.json.Parser.jsonStringChar()),
        (java.util.function.Function<java.util.List<Integer>, hydra.parsing.Parser<hydra.json.model.Value>>) (chars -> hydra.Parsers.bind(
          hydra.Parsers.char_(34),
          (java.util.function.Function<Integer, hydra.parsing.Parser<hydra.json.model.Value>>) (_2 -> hydra.Parsers.pure(new hydra.json.model.Value.String_(hydra.lib.strings.FromList.apply(chars))))))))));
  }

  static hydra.parsing.Parser<Integer> jsonStringChar() {
    return hydra.Parsers.alt(
      hydra.Parsers.bind(
        hydra.Parsers.char_(92),
        (java.util.function.Function<Integer, hydra.parsing.Parser<Integer>>) (ignored -> hydra.json.Parser.jsonEscapeChar())),
      hydra.Parsers.satisfy((java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.logic.And.apply(
        hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
          c,
          34)),
        hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
          c,
          92))))));
  }

  static hydra.parsing.Parser<hydra.json.model.Value> jsonValue() {
    return hydra.Parsers.choice(java.util.Arrays.asList(
      hydra.json.Parser.jsonNull(),
      hydra.json.Parser.jsonBool(),
      hydra.json.Parser.jsonNumber(),
      hydra.json.Parser.jsonString(),
      hydra.json.Parser.jsonArray(),
      hydra.json.Parser.jsonObject()));
  }

  static hydra.parsing.ParseResult<hydra.json.model.Value> parseJson(String input) {
    return ((java.util.function.Function<hydra.parsing.Parser<hydra.json.model.Value>, java.util.function.Function<String, hydra.parsing.ParseResult<hydra.json.model.Value>>>) (wrapped -> (wrapped).value)).apply(hydra.Parsers.bind(
      hydra.json.Parser.whitespace(),
      (java.util.function.Function<java.lang.Void, hydra.parsing.Parser<hydra.json.model.Value>>) (ignored -> hydra.Parsers.bind(
        hydra.json.Parser.jsonValue(),
        (java.util.function.Function<hydra.json.model.Value, hydra.parsing.Parser<hydra.json.model.Value>>) (v -> hydra.Parsers.bind(
          hydra.json.Parser.whitespace(),
          (java.util.function.Function<java.lang.Void, hydra.parsing.Parser<hydra.json.model.Value>>) (_2 -> hydra.Parsers.bind(
            hydra.Parsers.eof(),
            (java.util.function.Function<java.lang.Void, hydra.parsing.Parser<hydra.json.model.Value>>) (_3 -> hydra.Parsers.pure(v)))))))))).apply(input);
  }

  static <T0> hydra.parsing.Parser<T0> token(hydra.parsing.Parser<T0> p) {
    return hydra.Parsers.<T0, T0>bind(
      p,
      (java.util.function.Function<T0, hydra.parsing.Parser<T0>>) (x -> hydra.Parsers.<java.lang.Void, T0>bind(
        hydra.json.Parser.whitespace(),
        (java.util.function.Function<java.lang.Void, hydra.parsing.Parser<T0>>) (ignored -> hydra.Parsers.<T0>pure(x)))));
  }

  static hydra.parsing.Parser<java.lang.Void> whitespace() {
    return hydra.Parsers.map(
      (java.util.function.Function<java.util.List<Integer>, java.lang.Void>) (ignored -> null),
      hydra.Parsers.many(hydra.Parsers.satisfy((java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<Boolean, java.util.function.Function<Boolean, Boolean>>) (p0 -> p1 -> hydra.lib.logic.Or.apply(
          p0,
          p1)),
        false,
        java.util.Arrays.asList(
          hydra.lib.equality.Equal.apply(
            c,
            32),
          hydra.lib.equality.Equal.apply(
            c,
            9),
          hydra.lib.equality.Equal.apply(
            c,
            10),
          hydra.lib.equality.Equal.apply(
            c,
            13)))))));
  }
}
