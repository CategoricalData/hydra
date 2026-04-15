package hydra.test.validate.all

import hydra.testing.*

lazy val allTests: hydra.testing.TestGroup = hydra.testing.TestGroup("validation",
   None, Seq(hydra.test.validate.core.allTests), Seq())
