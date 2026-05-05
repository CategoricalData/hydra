// Note: this is an automatically generated file. Do not edit.

package testhoistingall

import (
  testhoistingcases "hydra.dev/hydra/test/hoisting/cases"
  testhoistinglet "hydra.dev/hydra/test/hoisting/let"
  "hydra.dev/hydra/testing"
)

var AllTests = testing.TestGroup{Name: "hoisting", Description: nil, Subgroups: []any{testhoistingcases.AllTests, testhoistinglet.AllTests}, Cases: []any{}}
