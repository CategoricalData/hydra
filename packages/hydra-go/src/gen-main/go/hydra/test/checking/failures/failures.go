// Note: this is an automatically generated file. Do not edit.

package testcheckingfailures

import "hydra.dev/hydra/testing"

var AllTests = testing.TestGroup{Name: "Failures", Description: nil, Subgroups: []any{FailOnUntypedTests}, Cases: []any{}}

var FailOnUntypedTests = testing.TestGroup{Name: "Fail on untyped (pre-inference) terms", Description: nil, Subgroups: []any{UntypedLambdasTests}, Cases: []any{}}

var UntypedLambdasTests = testing.TestGroup{Name: "Untyped lambdas", Description: nil, Subgroups: []any{}, Cases: []any{}}
