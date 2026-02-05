// Note: this is an automatically generated file. Do not edit.

package hydra.json.parser;

/**
 * JSON parser using Hydra parser combinators
 */
public interface Parser {
  static hydra.parsing.Parser<java.lang.Void> whitespace() {
    return hydra.parsers.Parsers.map(
      (java.util.function.Function<java.util.List<Integer>, java.lang.Void>) (ignored -> null),
      hydra.parsers.Parsers.many(hydra.parsers.Parsers.satisfy((java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<Boolean, java.util.function.Function<Boolean, Boolean>>) (p0 -> p1 -> hydra.lib.logic.Or.apply(
          (p0),
          (p1))),
        false,
        java.util.List.of(
          hydra.lib.equality.Equal.apply(
            (c),
            32),
          hydra.lib.equality.Equal.apply(
            (c),
            9),
          hydra.lib.equality.Equal.apply(
            (c),
            10),
          hydra.lib.equality.Equal.apply(
            (c),
            13)))))));
  }
  
  static <T0> hydra.parsing.Parser<T0> token(hydra.parsing.Parser<T0> p) {
    return hydra.parsers.Parsers.<T0, T0>bind(
      (p),
      (java.util.function.Function<T0, hydra.parsing.Parser<T0>>) (x -> hydra.parsers.Parsers.<java.lang.Void, T0>bind(
        hydra.json.parser.Parser.whitespace(),
        (java.util.function.Function<java.lang.Void, hydra.parsing.Parser<T0>>) (ignored -> hydra.parsers.Parsers.<T0>pure((x))))));
  }
  
  static hydra.parsing.Parser<hydra.json.model.Value> jsonNull() {
    return hydra.parsers.Parsers.map(
      (java.util.function.Function<String, hydra.json.model.Value>) (ignored -> new hydra.json.model.Value.Null()),
      hydra.json.parser.Parser.token(hydra.parsers.Parsers.string("null")));
  }
  
  static hydra.parsing.Parser<hydra.json.model.Value> jsonBool() {
    return hydra.parsers.Parsers.alt(
      hydra.parsers.Parsers.map(
        (java.util.function.Function<String, hydra.json.model.Value>) (ignored -> new hydra.json.model.Value.Boolean_(true)),
        hydra.json.parser.Parser.token(hydra.parsers.Parsers.string("true"))),
      hydra.parsers.Parsers.map(
        (java.util.function.Function<String, hydra.json.model.Value>) (ignored -> new hydra.json.model.Value.Boolean_(false)),
        hydra.json.parser.Parser.token(hydra.parsers.Parsers.string("false"))));
  }
  
  static hydra.parsing.Parser<Integer> digit() {
    return hydra.parsers.Parsers.satisfy((java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.logic.And.apply(
      hydra.lib.equality.Gte.apply(
        (c),
        48),
      hydra.lib.equality.Lte.apply(
        (c),
        57))));
  }
  
  static hydra.parsing.Parser<String> digits() {
    return hydra.parsers.Parsers.map(
      (hydra.lib.strings.FromList::apply),
      hydra.parsers.Parsers.some(hydra.json.parser.Parser.digit()));
  }
  
  static hydra.parsing.Parser<String> jsonIntegerPart() {
    return hydra.parsers.Parsers.bind(
      hydra.parsers.Parsers.optional(hydra.parsers.Parsers.char_(45)),
      (java.util.function.Function<hydra.util.Maybe<Integer>, hydra.parsing.Parser<String>>) (sign -> hydra.parsers.Parsers.bind(
        hydra.json.parser.Parser.digits(),
        (java.util.function.Function<String, hydra.parsing.Parser<String>>) (digits -> hydra.parsers.Parsers.pure(hydra.lib.maybes.Maybe.apply(
          (digits),
          (java.util.function.Function<Integer, String>) (ignored -> hydra.lib.strings.Cat2.apply(
            "-",
            (digits))),
          (sign)))))));
  }
  
  static hydra.parsing.Parser<hydra.util.Maybe<String>> jsonFractionPart() {
    return hydra.parsers.Parsers.optional(hydra.parsers.Parsers.bind(
      hydra.parsers.Parsers.char_(46),
      (java.util.function.Function<Integer, hydra.parsing.Parser<String>>) (ignored -> hydra.parsers.Parsers.map(
        (java.util.function.Function<String, String>) (d -> hydra.lib.strings.Cat2.apply(
          ".",
          (d))),
        hydra.json.parser.Parser.digits()))));
  }
  
  static hydra.parsing.Parser<hydra.util.Maybe<String>> jsonExponentPart() {
    return hydra.parsers.Parsers.optional(hydra.parsers.Parsers.bind(
      hydra.parsers.Parsers.satisfy((java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.logic.Or.apply(
        hydra.lib.equality.Equal.apply(
          (c),
          101),
        hydra.lib.equality.Equal.apply(
          (c),
          69)))),
      (java.util.function.Function<Integer, hydra.parsing.Parser<String>>) (ignored -> hydra.parsers.Parsers.bind(
        hydra.parsers.Parsers.optional(hydra.parsers.Parsers.satisfy((java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.logic.Or.apply(
          hydra.lib.equality.Equal.apply(
            (c),
            43),
          hydra.lib.equality.Equal.apply(
            (c),
            45))))),
        (java.util.function.Function<hydra.util.Maybe<Integer>, hydra.parsing.Parser<String>>) (sign -> hydra.parsers.Parsers.map(
          (java.util.function.Function<String, String>) (digits -> hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "e",
              hydra.lib.maybes.Maybe.apply(
                "",
                (java.util.function.Function<Integer, String>) (arg_ -> hydra.lib.strings.FromList.apply(hydra.lib.lists.Pure.apply((arg_)))),
                (sign))),
            (digits))),
          hydra.json.parser.Parser.digits()))))));
  }
  
  static hydra.parsing.Parser<hydra.json.model.Value> jsonNumber() {
    return hydra.json.parser.Parser.token(hydra.parsers.Parsers.bind(
      hydra.json.parser.Parser.jsonIntegerPart(),
      (java.util.function.Function<String, hydra.parsing.Parser<hydra.json.model.Value>>) (intPart -> hydra.parsers.Parsers.bind(
        hydra.json.parser.Parser.jsonFractionPart(),
        (java.util.function.Function<hydra.util.Maybe<String>, hydra.parsing.Parser<hydra.json.model.Value>>) (fracPart -> hydra.parsers.Parsers.bind(
          hydra.json.parser.Parser.jsonExponentPart(),
          (java.util.function.Function<hydra.util.Maybe<String>, hydra.parsing.Parser<hydra.json.model.Value>>) (expPart -> {
            hydra.util.Lazy<String> numStr = new hydra.util.Lazy<>(() -> hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                (intPart),
                hydra.lib.maybes.Maybe.apply(
                  "",
                  (java.util.function.Function<String, String>) ((hydra.lib.equality.Identity::apply)),
                  (fracPart))),
              hydra.lib.maybes.Maybe.apply(
                "",
                (java.util.function.Function<String, String>) ((hydra.lib.equality.Identity::apply)),
                (expPart))));
            return hydra.parsers.Parsers.pure(new hydra.json.model.Value.Number_(hydra.lib.maybes.Maybe.apply(
              new java.math.BigDecimal("0.0"),
              (java.util.function.Function<java.math.BigDecimal, java.math.BigDecimal>) ((hydra.lib.equality.Identity::apply)),
              hydra.lib.literals.ReadBigfloat.apply(numStr.get()))));
          })))))));
  }
  
  static hydra.parsing.Parser<Integer> jsonEscapeChar() {
    return hydra.parsers.Parsers.choice(java.util.List.of(
      hydra.parsers.Parsers.map(
        (java.util.function.Function<Integer, Integer>) (ignored -> 34),
        hydra.parsers.Parsers.char_(34)),
      hydra.parsers.Parsers.map(
        (java.util.function.Function<Integer, Integer>) (ignored -> 92),
        hydra.parsers.Parsers.char_(92)),
      hydra.parsers.Parsers.map(
        (java.util.function.Function<Integer, Integer>) (ignored -> 47),
        hydra.parsers.Parsers.char_(47)),
      hydra.parsers.Parsers.map(
        (java.util.function.Function<Integer, Integer>) (ignored -> 8),
        hydra.parsers.Parsers.char_(98)),
      hydra.parsers.Parsers.map(
        (java.util.function.Function<Integer, Integer>) (ignored -> 12),
        hydra.parsers.Parsers.char_(102)),
      hydra.parsers.Parsers.map(
        (java.util.function.Function<Integer, Integer>) (ignored -> 10),
        hydra.parsers.Parsers.char_(110)),
      hydra.parsers.Parsers.map(
        (java.util.function.Function<Integer, Integer>) (ignored -> 13),
        hydra.parsers.Parsers.char_(114)),
      hydra.parsers.Parsers.map(
        (java.util.function.Function<Integer, Integer>) (ignored -> 9),
        hydra.parsers.Parsers.char_(116))));
  }
  
  static hydra.parsing.Parser<Integer> jsonStringChar() {
    return hydra.parsers.Parsers.alt(
      hydra.parsers.Parsers.bind(
        hydra.parsers.Parsers.char_(92),
        (java.util.function.Function<Integer, hydra.parsing.Parser<Integer>>) (ignored -> hydra.json.parser.Parser.jsonEscapeChar())),
      hydra.parsers.Parsers.satisfy((java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.logic.And.apply(
        hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
          (c),
          34)),
        hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
          (c),
          92))))));
  }
  
  static hydra.parsing.Parser<hydra.json.model.Value> jsonString() {
    return hydra.json.parser.Parser.token(hydra.parsers.Parsers.bind(
      hydra.parsers.Parsers.char_(34),
      (java.util.function.Function<Integer, hydra.parsing.Parser<hydra.json.model.Value>>) (ignored -> hydra.parsers.Parsers.bind(
        hydra.parsers.Parsers.many(hydra.json.parser.Parser.jsonStringChar()),
        (java.util.function.Function<java.util.List<Integer>, hydra.parsing.Parser<hydra.json.model.Value>>) (chars -> hydra.parsers.Parsers.bind(
          hydra.parsers.Parsers.char_(34),
          (java.util.function.Function<Integer, hydra.parsing.Parser<hydra.json.model.Value>>) (_2 -> hydra.parsers.Parsers.pure(new hydra.json.model.Value.String_(hydra.lib.strings.FromList.apply((chars)))))))))));
  }
  
  static hydra.parsing.Parser<hydra.json.model.Value> jsonArray() {
    return hydra.parsers.Parsers.map(
      (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.json.model.Value>) (x -> new hydra.json.model.Value.Array((x))),
      hydra.parsers.Parsers.between(
        hydra.json.parser.Parser.token(hydra.parsers.Parsers.char_(91)),
        hydra.json.parser.Parser.token(hydra.parsers.Parsers.char_(93)),
        hydra.parsers.Parsers.sepBy(
          hydra.parsers.Parsers.lazy((java.util.function.Function<java.lang.Void, hydra.parsing.Parser<hydra.json.model.Value>>) (ignored -> hydra.json.parser.Parser.jsonValue())),
          hydra.json.parser.Parser.token(hydra.parsers.Parsers.char_(44)))));
  }
  
  static hydra.parsing.Parser<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>> jsonKeyValue() {
    return hydra.parsers.Parsers.bind(
      hydra.json.parser.Parser.token(hydra.parsers.Parsers.bind(
        hydra.parsers.Parsers.char_(34),
        (java.util.function.Function<Integer, hydra.parsing.Parser<String>>) (ignored -> hydra.parsers.Parsers.bind(
          hydra.parsers.Parsers.many(hydra.json.parser.Parser.jsonStringChar()),
          (java.util.function.Function<java.util.List<Integer>, hydra.parsing.Parser<String>>) (chars -> hydra.parsers.Parsers.bind(
            hydra.parsers.Parsers.char_(34),
            (java.util.function.Function<Integer, hydra.parsing.Parser<String>>) (_2 -> hydra.parsers.Parsers.pure(hydra.lib.strings.FromList.apply((chars)))))))))),
      (java.util.function.Function<String, hydra.parsing.Parser<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>>) (key -> hydra.parsers.Parsers.bind(
        hydra.json.parser.Parser.token(hydra.parsers.Parsers.char_(58)),
        (java.util.function.Function<Integer, hydra.parsing.Parser<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>>) (ignored -> hydra.parsers.Parsers.map(
          (java.util.function.Function<hydra.json.model.Value, hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>) (v -> (hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) ((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) (new hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>((key), (v))))),
          hydra.parsers.Parsers.lazy((java.util.function.Function<java.lang.Void, hydra.parsing.Parser<hydra.json.model.Value>>) (_2 -> hydra.json.parser.Parser.jsonValue())))))));
  }
  
  static hydra.parsing.Parser<hydra.json.model.Value> jsonObject() {
    return hydra.parsers.Parsers.map(
      (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>, hydra.json.model.Value>) (arg_ -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply((arg_)))),
      hydra.parsers.Parsers.between(
        hydra.json.parser.Parser.token(hydra.parsers.Parsers.char_(123)),
        hydra.json.parser.Parser.token(hydra.parsers.Parsers.char_(125)),
        hydra.parsers.Parsers.sepBy(
          hydra.json.parser.Parser.jsonKeyValue(),
          hydra.json.parser.Parser.token(hydra.parsers.Parsers.char_(44)))));
  }
  
  static hydra.parsing.Parser<hydra.json.model.Value> jsonValue() {
    return hydra.parsers.Parsers.choice(java.util.List.of(
      hydra.json.parser.Parser.jsonNull(),
      hydra.json.parser.Parser.jsonBool(),
      hydra.json.parser.Parser.jsonNumber(),
      hydra.json.parser.Parser.jsonString(),
      hydra.json.parser.Parser.jsonArray(),
      hydra.json.parser.Parser.jsonObject()));
  }
  
  static hydra.parsing.ParseResult<hydra.json.model.Value> parseJson(String input) {
    return (((java.util.function.Function<hydra.parsing.Parser<hydra.json.model.Value>, java.util.function.Function<String, hydra.parsing.ParseResult<hydra.json.model.Value>>>) (wrapped -> ((wrapped)).value)).apply(hydra.parsers.Parsers.bind(
      hydra.json.parser.Parser.whitespace(),
      (java.util.function.Function<java.lang.Void, hydra.parsing.Parser<hydra.json.model.Value>>) (ignored -> hydra.parsers.Parsers.bind(
        hydra.json.parser.Parser.jsonValue(),
        (java.util.function.Function<hydra.json.model.Value, hydra.parsing.Parser<hydra.json.model.Value>>) (v -> hydra.parsers.Parsers.bind(
          hydra.json.parser.Parser.whitespace(),
          (java.util.function.Function<java.lang.Void, hydra.parsing.Parser<hydra.json.model.Value>>) (_2 -> hydra.parsers.Parsers.bind(
            hydra.parsers.Parsers.eof(),
            (java.util.function.Function<java.lang.Void, hydra.parsing.Parser<hydra.json.model.Value>>) (_3 -> hydra.parsers.Parsers.pure((v)))))))))))).apply((input));
  }
}
