package hydra.test.sorting

import hydra.testing.*

lazy val allTests: hydra.testing.TestGroup = hydra.testing.TestGroup("sorting", None,
   Seq(hydra.testing.TestGroup("topological sort", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("empty set",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[Seq[Seq[Int]],
   Seq[Int], scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(hydra.sorting.topologicalSort(Seq())),
     hydra.lib.eithers.either[Seq[Seq[Int]], Seq[Int], scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(Right(Seq())))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("singleton set", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[Seq[Seq[Int]],
     Seq[Int], scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(hydra.sorting.topologicalSort(Seq(Tuple2(1,
     Seq())))), hydra.lib.eithers.either[Seq[Seq[Int]], Seq[Int], scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(Right(Seq(1))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("discrete set with multiple elements",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[Seq[Seq[Int]],
     Seq[Int], scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(hydra.sorting.topologicalSort(Seq(Tuple2(3,
     Seq()), Tuple2(1, Seq()), Tuple2(2, Seq())))), hydra.lib.eithers.either[Seq[Seq[Int]],
     Seq[Int], scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(Right(Seq(1,
     2, 3))))), None, Seq()), hydra.testing.TestCaseWithMetadata("linked list", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[Seq[Seq[Int]],
     Seq[Int], scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(hydra.sorting.topologicalSort(Seq(Tuple2(3,
     Seq(1)), Tuple2(2, Seq(3)), Tuple2(1, Seq())))), hydra.lib.eithers.either[Seq[Seq[Int]],
     Seq[Int], scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(Right(Seq(1,
     3, 2))))), None, Seq()), hydra.testing.TestCaseWithMetadata("binary tree", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[Seq[Seq[Int]],
     Seq[Int], scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(hydra.sorting.topologicalSort(Seq(Tuple2(3,
     Seq(1, 4)), Tuple2(4, Seq(6, 2)), Tuple2(1, Seq(5)), Tuple2(2, Seq()), Tuple2(6,
     Seq()), Tuple2(5, Seq())))), hydra.lib.eithers.either[Seq[Seq[Int]], Seq[Int],
     scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(Right(Seq(5,
     1, 2, 6, 4, 3))))), None, Seq()), hydra.testing.TestCaseWithMetadata("two trees",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[Seq[Seq[Int]],
     Seq[Int], scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(hydra.sorting.topologicalSort(Seq(Tuple2(3,
     Seq(1, 4)), Tuple2(5, Seq(6, 2)), Tuple2(2, Seq(7)), Tuple2(1, Seq()), Tuple2(4,
     Seq()), Tuple2(6, Seq()), Tuple2(7, Seq())))), hydra.lib.eithers.either[Seq[Seq[Int]],
     Seq[Int], scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(Right(Seq(1,
     7, 2, 4, 3, 6, 5))))), None, Seq()), hydra.testing.TestCaseWithMetadata("diamond DAG",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[Seq[Seq[Int]],
     Seq[Int], scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(hydra.sorting.topologicalSort(Seq(Tuple2(1,
     Seq(3, 4)), Tuple2(3, Seq(2)), Tuple2(4, Seq(2)), Tuple2(2, Seq(5)), Tuple2(5,
     Seq())))), hydra.lib.eithers.either[Seq[Seq[Int]], Seq[Int], scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(Right(Seq(5,
     2, 3, 4, 1))))), None, Seq()), hydra.testing.TestCaseWithMetadata("two-node cycle",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[Seq[Seq[Int]],
     Seq[Int], scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(hydra.sorting.topologicalSort(Seq(Tuple2(1,
     Seq(2)), Tuple2(2, Seq(1))))), hydra.lib.eithers.either[Seq[Seq[Int]], Seq[Int],
     scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(Left(Seq(Seq(1,
     2)))))), None, Seq()), hydra.testing.TestCaseWithMetadata("cycle with incoming and outgoing edges",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[Seq[Seq[Int]],
     Seq[Int], scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(hydra.sorting.topologicalSort(Seq(Tuple2(1,
     Seq(3)), Tuple2(3, Seq(2)), Tuple2(2, Seq(3, 4)), Tuple2(4, Seq(5)), Tuple2(5,
     Seq())))), hydra.lib.eithers.either[Seq[Seq[Int]], Seq[Int], scala.Predef.String]((cs: Seq[Seq[Int]]) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(cs))(")")))((xs: Seq[Int]) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(hydra.show.core.list(hydra.lib.literals.showInt32)(xs))(")")))(Left(Seq(Seq(2,
     3)))))), None, Seq()))), hydra.testing.TestGroup("topological sort SCC", None,
     Seq(), Seq(hydra.testing.TestCaseWithMetadata("empty set", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(hydra.sorting.topologicalSortComponents(Seq())),
     hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(Seq()))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("singleton set", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(hydra.sorting.topologicalSortComponents(Seq(Tuple2(1,
     Seq())))), hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(Seq(Seq(1))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("discrete set with multiple elements",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(hydra.sorting.topologicalSortComponents(Seq(Tuple2(3,
     Seq()), Tuple2(1, Seq()), Tuple2(2, Seq())))), hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(Seq(Seq(1),
     Seq(2), Seq(3))))), None, Seq()), hydra.testing.TestCaseWithMetadata("single two-element component #1",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(hydra.sorting.topologicalSortComponents(Seq(Tuple2(1,
     Seq(2)), Tuple2(2, Seq())))), hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(Seq(Seq(2),
     Seq(1))))), None, Seq()), hydra.testing.TestCaseWithMetadata("single two-element component #2",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(hydra.sorting.topologicalSortComponents(Seq(Tuple2(2,
     Seq(1)), Tuple2(1, Seq())))), hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(Seq(Seq(1),
     Seq(2))))), None, Seq()), hydra.testing.TestCaseWithMetadata("multiple-element component",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(hydra.sorting.topologicalSortComponents(Seq(Tuple2(2,
     Seq(1, 3)), Tuple2(1, Seq(3)), Tuple2(3, Seq())))), hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(Seq(Seq(3),
     Seq(1), Seq(2))))), None, Seq()), hydra.testing.TestCaseWithMetadata("cycle of two nodes #1",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(hydra.sorting.topologicalSortComponents(Seq(Tuple2(1,
     Seq(2)), Tuple2(2, Seq(1))))), hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(Seq(Seq(1,
     2))))), None, Seq()), hydra.testing.TestCaseWithMetadata("cycle of two nodes #2",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(hydra.sorting.topologicalSortComponents(Seq(Tuple2(2,
     Seq(1)), Tuple2(1, Seq(2))))), hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(Seq(Seq(1,
     2))))), None, Seq()), hydra.testing.TestCaseWithMetadata("cycle of three nodes #1",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(hydra.sorting.topologicalSortComponents(Seq(Tuple2(1,
     Seq(2)), Tuple2(2, Seq(3)), Tuple2(3, Seq(1))))), hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(Seq(Seq(1,
     2, 3))))), None, Seq()), hydra.testing.TestCaseWithMetadata("cycle of three nodes #2",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(hydra.sorting.topologicalSortComponents(Seq(Tuple2(2,
     Seq(1)), Tuple2(3, Seq(2)), Tuple2(1, Seq(3))))), hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(Seq(Seq(1,
     2, 3))))), None, Seq()), hydra.testing.TestCaseWithMetadata("multiple disconnected cycles",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(hydra.sorting.topologicalSortComponents(Seq(Tuple2(200,
     Seq()), Tuple2(100, Seq()), Tuple2(300, Seq()), Tuple2(10, Seq(20)), Tuple2(20,
     Seq(10)), Tuple2(1, Seq(2)), Tuple2(2, Seq(3)), Tuple2(3, Seq(1))))), hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(Seq(Seq(1,
     2, 3), Seq(10, 20), Seq(100), Seq(200), Seq(300))))), None, Seq()), hydra.testing.TestCaseWithMetadata("complex cycles",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(hydra.sorting.topologicalSortComponents(Seq(Tuple2(1,
     Seq(2, 3)), Tuple2(2, Seq(3)), Tuple2(3, Seq(1))))), hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(Seq(Seq(1,
     2, 3))))), None, Seq()), hydra.testing.TestCaseWithMetadata("chain of three SCCs",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(hydra.sorting.topologicalSortComponents(Seq(Tuple2(1,
     Seq(2, 10)), Tuple2(2, Seq(3)), Tuple2(3, Seq(1)), Tuple2(10, Seq(20)), Tuple2(20,
     Seq(100, 10)), Tuple2(100, Seq())))), hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(Seq(Seq(100),
     Seq(10, 20), Seq(1, 2, 3))))), None, Seq()), hydra.testing.TestCaseWithMetadata("SCCs with dependencies to/from non-SCC nodes",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(hydra.sorting.topologicalSortComponents(Seq(Tuple2(1,
     Seq(2, 3, 10)), Tuple2(2, Seq(3)), Tuple2(3, Seq(1)), Tuple2(10, Seq(20, 30)),
     Tuple2(20, Seq(30)), Tuple2(30, Seq()), Tuple2(100, Seq(200, 2)), Tuple2(200,
     Seq()), Tuple2(300, Seq(100)), Tuple2(1000, Seq()), Tuple2(2000, Seq())))), hydra.show.core.list((v1: Seq[Int]) => hydra.show.core.list(hydra.lib.literals.showInt32)(v1))(Seq(Seq(30),
     Seq(20), Seq(10), Seq(1, 2, 3), Seq(200), Seq(100), Seq(300), Seq(1000), Seq(2000))))),
     None, Seq())))), Seq())
