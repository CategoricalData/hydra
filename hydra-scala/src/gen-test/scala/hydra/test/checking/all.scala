package hydra.test.checking.all

import hydra.testing.*

lazy val allTests: hydra.testing.TestGroup = hydra.testing.TestGroup("checking", None, Seq(hydra.test.checking.advanced.allTests, hydra.test.checking.algebraicTypes.allTests, hydra.test.checking.collections.allTests, hydra.test.checking.failures.allTests, hydra.test.checking.fundamentals.allTests, hydra.test.checking.nominalTypes.allTests), Seq())
