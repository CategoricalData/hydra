package hydra.test.lib.maybes

import hydra.testing.*

lazy val allTests: hydra.testing.TestGroup = hydra.testing.TestGroup("hydra.lib.maybes primitives",
   None, Seq(hydra.testing.TestGroup("apply", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("both just",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.apply[Int,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Int](Some((x: Int) => hydra.lib.math.add(3)(x)))(Some(5))), hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(Some(8)))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("nothing function", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.apply[Int,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Int](None)(Some(5))), hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(None))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("nothing value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.apply[Int,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Int](Some((x: Int) => hydra.lib.math.add(3)(x)))(None)), hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(None))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("bind", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("just to just",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.bind[Int,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Int](Some(5))((x: Int) => Some(hydra.lib.math.mul(x)(2)))), hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(Some(10)))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("nothing to nothing", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.bind[Int,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Int](None)((x: Int) => Some(hydra.lib.math.mul(x)(2)))), hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(None))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("cases", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("just applies function",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.literals.showInt32(hydra.lib.maybes.cases[Int,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Int](Some(5))(0)((x: Int) => hydra.lib.math.mul(x)(2))), hydra.lib.literals.showInt32(10))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("nothing returns default", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.literals.showInt32(hydra.lib.maybes.cases[Int,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Int](None)(99)((x: Int) => hydra.lib.math.mul(x)(2))), hydra.lib.literals.showInt32(99))),
   None, Seq()))), hydra.testing.TestGroup("cat", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("filters nothings",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.cat[Int](Seq(Some(1),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Some(2)))), hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(Seq(1,
   2)))), None, Seq()), hydra.testing.TestCaseWithMetadata("all justs", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.cat[Int](Seq(Some(1),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Some(2)))), hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(Seq(1,
   2)))), None, Seq()), hydra.testing.TestCaseWithMetadata("all nothings", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.cat[Int](Seq(None,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None))), hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(Seq()))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("empty list", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.cat[Int](Seq())),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(Seq()))), None,
   Seq()))), hydra.testing.TestGroup("compose", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("both succeed",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.compose[Int,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Int, Int]((x: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.lte[Int](x)(5))(Some(hydra.lib.math.add(x)(1)))(None))((y: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.gte[Int](y)(5))(Some(hydra.lib.math.mul(y)(2)))(None))(5)),
     hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(Some(12)))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("first fails", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.compose[Int,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     Int, Int]((x: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.lte[Int](x)(5))(Some(hydra.lib.math.add(x)(1)))(None))((y: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.gte[Int](y)(5))(Some(hydra.lib.math.mul(y)(2)))(None))(10)),
     hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(None))), None,
     Seq()), hydra.testing.TestCaseWithMetadata("second fails", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.compose[Int,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     Int, Int]((x: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.lte[Int](x)(5))(Some(hydra.lib.math.add(x)(1)))(None))((y: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.gte[Int](y)(5))(Some(hydra.lib.math.mul(y)(2)))(None))(3)),
     hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(None))), None,
     Seq()))), hydra.testing.TestGroup("fromMaybe", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("just value",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.literals.showInt32(hydra.lib.maybes.fromMaybe[Int](0)(Some(42))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.lib.literals.showInt32(42))), None, Seq()), hydra.testing.TestCaseWithMetadata("nothing with default",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.literals.showInt32(hydra.lib.maybes.fromMaybe[Int](99)(None)),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.lib.literals.showInt32(99))), None, Seq()))), hydra.testing.TestGroup("isJust",
     None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("just value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.literals.showBoolean(hydra.lib.maybes.isJust[Int](Some(42))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.lib.literals.showBoolean(true))), None, Seq()), hydra.testing.TestCaseWithMetadata("nothing",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.literals.showBoolean(hydra.lib.maybes.isJust[Unit](None)),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.lib.literals.showBoolean(false))), None, Seq()))), hydra.testing.TestGroup("isNothing",
     None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("just value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.literals.showBoolean(hydra.lib.maybes.isNothing[Int](Some(42))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.lib.literals.showBoolean(false))), None, Seq()), hydra.testing.TestCaseWithMetadata("nothing",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.literals.showBoolean(hydra.lib.maybes.isNothing[Unit](None)),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.lib.literals.showBoolean(true))), None, Seq()))), hydra.testing.TestGroup("map",
     None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("maps just value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.map[Int,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     Int]((x: Int) => hydra.lib.math.mul(x)(2))(Some(5))), hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(Some(10)))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     None, Seq()), hydra.testing.TestCaseWithMetadata("nothing unchanged", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.map[Int,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     Int]((x: Int) => hydra.lib.math.mul(x)(2))(None)), hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(None))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     None, Seq()))), hydra.testing.TestGroup("mapMaybe", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("filter and transform",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.mapMaybe[Int,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     Int]((x: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.gt[Int](x)(2))(Some(hydra.lib.math.mul(x)(2)))(None))(Seq(1,
     2, 3, 4, 5))), hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(Seq(6,
     8, 10)))), None, Seq()), hydra.testing.TestCaseWithMetadata("empty result", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.mapMaybe[Int,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     Int]((x: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.gt[Int](x)(2))(Some(hydra.lib.math.mul(x)(2)))(None))(Seq(1,
     2))), hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(Seq()))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("empty input", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.mapMaybe[Int,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     Int]((x: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.gt[Int](x)(2))(Some(hydra.lib.math.mul(x)(2)))(None))(Seq())),
     hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(Seq()))), None,
     Seq()))), hydra.testing.TestGroup("maybe", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("just value applies function",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.literals.showInt32(hydra.lib.maybes.maybe[Int,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     Int](0)((x: Int) => hydra.lib.math.mul(x)(2))(Some(5))), hydra.lib.literals.showInt32(10))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("nothing returns default", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.literals.showInt32(hydra.lib.maybes.maybe[Int,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     Int](99)((x: Int) => hydra.lib.math.mul(x)(2))(None)), hydra.lib.literals.showInt32(99))),
     None, Seq()))), hydra.testing.TestGroup("pure", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("wraps integer",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.pure[Int](42)),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.show.core.maybe((n: Int) => hydra.lib.literals.showInt32(n))(Some(42)))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("wraps string", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.maybe((s: scala.Predef.String) => hydra.lib.literals.showString(s))(hydra.lib.maybes.pure[scala.Predef.String]("hello")),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.show.core.maybe((s: scala.Predef.String) => hydra.lib.literals.showString(s))(Some("hello")))),
     None, Seq()))), hydra.testing.TestGroup("toList", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("just value",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.toList[Int](Some(42))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(Seq(42)))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("nothing", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(hydra.lib.maybes.toList[Int](None)),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.show.core.list((n: Int) => hydra.lib.literals.showInt32(n))(Seq()))), None,
     Seq())))), Seq())
