package hydra.test.inference.all

import hydra.testing.*

lazy val allTests: hydra.testing.TestGroup = hydra.testing.TestGroup("inference", None, Seq(hydra.test.inference.algebraicTypes.allTests, hydra.test.inference.algorithmW.allTests, hydra.test.inference.classes.allTests, hydra.test.inference.failures.allTests, hydra.test.inference.fundamentals.allTests, hydra.test.inference.kernelExamples.allTests, hydra.test.inference.nominalTypes.allTests), Seq())
