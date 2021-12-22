package hydra.lib.strings

import hydra.HydraSpecBase

class StringsSpec extends HydraSpecBase {
  "cat" should exp in {
    assert(cat(Seq("one", "two")) === "onetwo")
    assert(cat(Seq()) === "")
    assert(cat(Seq("one", "", "two", "three", "")) === "onetwothree")
  }

  "length" should exp in {
    assert(length("one") === 3)
    assert(length("") === 0)
  }

  "splitOn" should exp in {
    assert(splitOn(",", "one,two,three") === Seq("one", "two", "three"))
    assert(splitOn(",", ",one,two,") === Seq("", "one", "two", ""))
    assert(splitOn(",", ",one,two,,") === Seq("", "one", "two", "", ""))
    assert(splitOn(",", ",,") === Seq("", "", ""))
    assert(splitOn("--", "-one--two---three") === Seq("-one", "two", "-three"))
  }

  "toLower" should exp in {
    assert(toLower("OneTwo5") === "onetwo5")
    assert(toLower("onetwo5") === "onetwo5")
    assert(toLower("") === "")
  }

  "toUpper" should exp in {
    assert(toUpper("OneTwo5") === "ONETWO5")
    assert(toUpper("ONETWO5") === "ONETWO5")
    assert(toUpper("") === "")
  }
}
