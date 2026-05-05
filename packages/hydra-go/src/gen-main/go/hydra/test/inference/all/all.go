// Note: this is an automatically generated file. Do not edit.

package testinferenceall

import (
  testinferencealgebraicTypes "hydra.dev/hydra/test/inference/algebraicTypes"
  testinferencealgorithmW "hydra.dev/hydra/test/inference/algorithmW"
  testinferenceclasses "hydra.dev/hydra/test/inference/classes"
  testinferencefailures "hydra.dev/hydra/test/inference/failures"
  testinferencefundamentals "hydra.dev/hydra/test/inference/fundamentals"
  testinferencekernelExamples "hydra.dev/hydra/test/inference/kernelExamples"
  testinferencenominalTypes "hydra.dev/hydra/test/inference/nominalTypes"
  "hydra.dev/hydra/testing"
)

var AllTests = testing.TestGroup{Name: "inference", Description: nil, Subgroups: []any{testinferencealgebraicTypes.AllTests, testinferencealgorithmW.AllTests, testinferenceclasses.AllTests, testinferencefailures.AllTests, testinferencefundamentals.AllTests, testinferencekernelExamples.AllTests, testinferencenominalTypes.AllTests}, Cases: []any{}}
