// Note: this is an automatically generated file. Do not edit.

package hydra.test.lib;

/**
 * Test cases for hydra.lib.strings primitives
 */
public interface Strings {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("hydra.lib.strings primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), hydra.util.ConsList.of(
      new hydra.testing.TestGroup("cat", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("basic concatenation", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "one",
          "two",
          "three")), "onetwothree")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("single string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of("hello")), "hello")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat.apply((hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty())), "")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("with empty strings", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "",
          "one",
          "",
          "")), "one")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("all empty strings", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "",
          "",
          "",
          "")), "")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unicode strings", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "\u00F1",
          "\u4E16",
          "\uD83C\uDF0D")), "\u00F1\u4E16\uD83C\uDF0D")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("combining characters", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "e",
          "\u0301")), "e\u0301")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("control characters", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "\n",
          "\t",
          "\r")), "\n\t\r")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("null character", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "hello",
          "\u0000",
          "world")), "hello\u0000world")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("cat2", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("basic concatenation", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat2.apply(
          "hello",
          "world"), "helloworld")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty first string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat2.apply(
          "",
          "world"), "world")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty second string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat2.apply(
          "hello",
          ""), "hello")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("both empty strings", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat2.apply(
          "",
          ""), "")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unicode characters", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat2.apply(
          "\u00F1",
          "\u4E16"), "\u00F1\u4E16")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("special characters", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat2.apply(
          "\n",
          "\t"), "\n\t")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("null characters", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat2.apply(
          "hello\u0000",
          "world"), "hello\u0000world")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("charAt", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("first character", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.strings.CharAt.apply(
          0,
          "hello")), hydra.lib.literals.ShowInt32.apply(104))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("middle character", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.strings.CharAt.apply(
          2,
          "hello")), hydra.lib.literals.ShowInt32.apply(108))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("last character", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.strings.CharAt.apply(
          4,
          "hello")), hydra.lib.literals.ShowInt32.apply(111))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("single character string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.strings.CharAt.apply(
          0,
          "a")), hydra.lib.literals.ShowInt32.apply(97))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unicode character", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.strings.CharAt.apply(
          0,
          "\u00F1")), hydra.lib.literals.ShowInt32.apply(241))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("multi-byte unicode", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.strings.CharAt.apply(
          0,
          "\u4E16")), hydra.lib.literals.ShowInt32.apply(19990))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("second of combining pair", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.strings.CharAt.apply(
          1,
          "e\u0301")), hydra.lib.literals.ShowInt32.apply(769))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("fromList", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("basic ascii string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(
          104,
          101,
          108,
          108,
          111)), "hello")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty code point list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.FromList.apply((hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())), "")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("single character", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(97)), "a")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unicode characters", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(
          241,
          19990,
          127757)), "\u00F1\u4E16\uD83C\uDF0D")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("combining character sequence", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(
          101,
          769)), "e\u0301")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("special characters", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(
          10,
          9,
          13)), "\n\t\r")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("null character", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(
          104,
          0,
          105)), "h\u0000i")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("intercalate", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("comma separator", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Intercalate.apply(
          ",",
          hydra.util.ConsList.of(
            "one",
            "two",
            "three")), "one,two,three")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty separator", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Intercalate.apply(
          "",
          hydra.util.ConsList.of(
            "a",
            "b",
            "c")), "abc")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("multi-character separator", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Intercalate.apply(
          " | ",
          hydra.util.ConsList.of(
            "A",
            "B",
            "C")), "A | B | C")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty string list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Intercalate.apply(
          ",",
          (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty())), "")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("single item list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Intercalate.apply(
          ",",
          hydra.util.ConsList.of("only")), "only")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty strings in list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Intercalate.apply(
          ",",
          hydra.util.ConsList.of(
            "",
            "a",
            "")), ",a,")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unicode separator", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Intercalate.apply(
          "\uD83C\uDF0D",
          hydra.util.ConsList.of(
            "link1",
            "link2")), "link1\uD83C\uDF0Dlink2")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("newline separator", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Intercalate.apply(
          "\n",
          hydra.util.ConsList.of(
            "line1",
            "line2")), "line1\nline2")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("length", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("empty string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.strings.Length.apply("")), hydra.lib.literals.ShowInt32.apply(0))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("single character", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.strings.Length.apply("a")), hydra.lib.literals.ShowInt32.apply(1))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("basic word", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.strings.Length.apply("hello")), hydra.lib.literals.ShowInt32.apply(5))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unicode characters", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.strings.Length.apply("\u00F1\u4E16\uD83C\uDF0D")), hydra.lib.literals.ShowInt32.apply(3))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("combining character sequence", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.strings.Length.apply("e\u0301")), hydra.lib.literals.ShowInt32.apply(2))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("special characters", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.strings.Length.apply("\n\t\r")), hydra.lib.literals.ShowInt32.apply(3))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("lines", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("single line", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.Lines.apply("hello world"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of("hello world"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("two lines", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.Lines.apply("hello\nworld"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "hello",
            "world"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("three lines", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.Lines.apply("one\ntwo\nthree"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "one",
            "two",
            "three"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.Lines.apply(""))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("just newline", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.Lines.apply("\n"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(""))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("trailing newline", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.Lines.apply("hello\n"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of("hello"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("leading newline", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.Lines.apply("\nhello"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "",
            "hello"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("multiple consecutive newlines", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.Lines.apply("a\n\nb"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "a",
            "",
            "b"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unicode content", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.Lines.apply("\u00F1\n\u4E16"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "\u00F1",
            "\u4E16"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("tabs not split", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.Lines.apply("a\tb\nc"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "a\tb",
            "c"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("maybeCharAt", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("first character", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.maybeCharAt"))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(104)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("middle character", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.maybeCharAt"))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(108)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("last character", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.maybeCharAt"))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(4))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(111)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("out of bounds", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.maybeCharAt"))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))))), hydra.show.Core.term(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("negative index", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.maybeCharAt"))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-1))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))))), hydra.show.Core.term(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.maybeCharAt"))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("")))))), hydra.show.Core.term(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("null", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("empty string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.strings.Null.apply("")), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("single character", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.strings.Null.apply("a")), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("space", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.strings.Null.apply(" ")), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unicode space", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.strings.Null.apply("\u00A0")), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("newline", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.strings.Null.apply("\n")), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("null character", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.strings.Null.apply("\u0000")), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("multi-character", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.strings.Null.apply("hello")), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("splitOn", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("basic separator", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            "ss",
            "Mississippi"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "Mi",
            "i",
            "ippi"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("single char separator", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            " ",
            "one two three"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "one",
            "two",
            "three"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("multi-char separator", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            "  ",
            "a  b  c"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "a",
            "b",
            "c"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("separator not found", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            "x",
            "hello"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of("hello"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("separator at start", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            "h",
            "hello"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "",
            "ello"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("separator at end", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            "o",
            "hello"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "hell",
            ""))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("leading and trailing separator", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            " ",
            " one two "))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "",
            "one",
            "two",
            ""))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("whole string as separator", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            "Mississippi",
            "Mississippi"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "",
            ""))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("consecutive separators", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            " ",
            "a  b"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "a",
            "",
            "b"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("multiple occurrences", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            "l",
            "hello"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "he",
            "",
            "o"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("overlapping pattern", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            "aa",
            "aaa"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "",
            "a"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty separator", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            "",
            "abc"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "",
            "a",
            "b",
            "c"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("separator on empty string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            "x",
            ""))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(""))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("both empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            "",
            ""))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(""))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("single char both", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            "a",
            "a"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "",
            ""))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unicode separator", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            "\u4E16",
            "hello\u4E16world"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "hello",
            "world"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unicode content", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            ",",
            "\u00F1,\u4E16,\uD83C\uDF0D"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "\u00F1",
            "\u4E16",
            "\uD83C\uDF0D"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("newline separator", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.strings.SplitOn.apply(
            "\n",
            "line1\nline2\nline3"))), hydra.lib.literals.ShowString.apply(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.util.ConsList.of(
            "line1",
            "line2",
            "line3"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("toList", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("empty string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            hydra.lib.strings.ToList.apply(""))), hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("single character", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            hydra.lib.strings.ToList.apply("a"))), hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            hydra.util.ConsList.of(97))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("basic word", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            hydra.lib.strings.ToList.apply("hello"))), hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            hydra.util.ConsList.of(
              104,
              101,
              108,
              108,
              111))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unicode characters", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            hydra.lib.strings.ToList.apply("\u00F1\u4E16\uD83C\uDF0D"))), hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            hydra.util.ConsList.of(
              241,
              19990,
              127757))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("combining character sequence", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            hydra.lib.strings.ToList.apply("e\u0301"))), hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            hydra.util.ConsList.of(
              101,
              769))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("control characters", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            hydra.lib.strings.ToList.apply("\n\t\r"))), hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            hydra.util.ConsList.of(
              10,
              9,
              13))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("null character", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            hydra.lib.strings.ToList.apply("h\u0000i"))), hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            hydra.util.ConsList.of(
              104,
              0,
              105))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("toLower", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("mixed case", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.ToLower.apply("Hello World"), "hello world")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("all uppercase", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.ToLower.apply("HELLO"), "hello")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("all lowercase", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.ToLower.apply("hello"), "hello")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.ToLower.apply(""), "")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("with numbers and punctuation", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.ToLower.apply("Abc123, XYZ!"), "abc123, xyz!")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("control characters", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.ToLower.apply("\n\t\r"), "\n\t\r")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unicode accented chars", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.ToLower.apply("\u00D1\u00C1\u00C9\u00CD\u00D3\u00DA"), "\u00F1\u00E1\u00E9\u00ED\u00F3\u00FA")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("toUpper", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("mixed case", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.ToUpper.apply("hello World"), "HELLO WORLD")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("all lowercase", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.ToUpper.apply("hello"), "HELLO")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("all uppercase", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.ToUpper.apply("HELLO"), "HELLO")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.ToUpper.apply(""), "")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("with numbers and punctuation", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.ToUpper.apply("abc123, xyz!"), "ABC123, XYZ!")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("control characters", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.ToUpper.apply("\n\t\r"), "\n\t\r")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unicode accented chars", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.ToUpper.apply("\u00F1\u00E1\u00E9\u00ED\u00F3\u00FA"), "\u00D1\u00C1\u00C9\u00CD\u00D3\u00DA")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("unlines", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("multiple lines", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Unlines.apply(hydra.util.ConsList.of(
          "one",
          "two",
          "three")), "one\ntwo\nthree\n")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("single line", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Unlines.apply(hydra.util.ConsList.of("hello")), "hello\n")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Unlines.apply((hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty())), "")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("with empty lines", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Unlines.apply(hydra.util.ConsList.of(
          "hello",
          "",
          "world")), "hello\n\nworld\n")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("all empty lines", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Unlines.apply(hydra.util.ConsList.of(
          "",
          "",
          "")), "\n\n\n")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unicode content", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Unlines.apply(hydra.util.ConsList.of(
          "\u00F1o\u00F1o",
          "\u4E16\u754C")), "\u00F1o\u00F1o\n\u4E16\u754C\n")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty()))))), (hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>) (hydra.util.ConsList.<hydra.testing.TestCaseWithMetadata>empty()));
  }
}
