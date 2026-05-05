// Note: this is an automatically generated file. Do not edit.

package testcheckingall

import (
  testcheckingadvanced "hydra.dev/hydra/test/checking/advanced"
  testcheckingalgebraicTypes "hydra.dev/hydra/test/checking/algebraicTypes"
  testcheckingcollections "hydra.dev/hydra/test/checking/collections"
  testcheckingfailures "hydra.dev/hydra/test/checking/failures"
  testcheckingfundamentals "hydra.dev/hydra/test/checking/fundamentals"
  testcheckingnominalTypes "hydra.dev/hydra/test/checking/nominalTypes"
  "hydra.dev/hydra/testing"
)

var AllTests = testing.TestGroup{Name: "checking", Description: nil, Subgroups: []any{testcheckingadvanced.AllTests, testcheckingalgebraicTypes.AllTests, testcheckingcollections.AllTests, testcheckingfailures.AllTests, testcheckingfundamentals.AllTests, testcheckingnominalTypes.AllTests}, Cases: []any{}}
