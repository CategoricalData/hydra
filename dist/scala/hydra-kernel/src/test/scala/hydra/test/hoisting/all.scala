package hydra.test.hoisting.all

import hydra.testing.*

lazy val allTests: hydra.testing.TestGroup = hydra.testing.TestGroup("hoisting", None, Seq(hydra.test.hoisting.cases.allTests, hydra.test.hoisting.let.allTests), Seq())
