package hydra.test.checking.failures

import hydra.testing.*

lazy val allTests: hydra.testing.TestGroup = hydra.testing.TestGroup("Failures", None, Seq(hydra.test.checking.failures.failOnUntypedTests), Seq())

lazy val failOnUntypedTests: hydra.testing.TestGroup = hydra.testing.TestGroup("Fail on untyped (pre-inference) terms", None, Seq(hydra.test.checking.failures.untypedLambdasTests), Seq())

lazy val untypedLambdasTests: hydra.testing.TestGroup = hydra.testing.TestGroup("Untyped lambdas", None, Seq(), Seq())
