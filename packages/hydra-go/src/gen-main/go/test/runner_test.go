// Test runner for Hydra's language-agnostic test suite.
//
// This file traverses the generated test groups and dispatches each test case.
// Initially it only counts and reports; full evaluation will be added incrementally.

package test

import (
	"fmt"
	"hydra/test/testSuite"
	"hydra/testing"
	"testing"
)

func TestHydraTestSuite(t *testing.T) {
	suite := testSuite.AllTests
	runTestGroup(t, suite)
}

func runTestGroup(t *testing.T, group testing.TestGroup) {
	t.Run(group.Name, func(t *testing.T) {
		// Run subgroups
		for _, sub := range group.Subgroups {
			subGroup, ok := sub.(testing.TestGroup)
			if !ok {
				t.Logf("Skipping non-TestGroup subgroup")
				continue
			}
			runTestGroup(t, subGroup)
		}

		// Run test cases
		for _, tc := range group.Cases {
			tcm, ok := tc.(testing.TestCaseWithMetadata)
			if !ok {
				continue
			}
			runTestCase(t, tcm)
		}
	})
}

func runTestCase(t *testing.T, tcm testing.TestCaseWithMetadata) {
	t.Run(tcm.Name, func(t *testing.T) {
		switch c := tcm.Case_.(type) {
		case testing.TestCaseEvaluation:
			runEvaluationTest(t, c.Value)
		case testing.TestCaseDelegatedEvaluation:
			t.Skip("delegated evaluation not yet implemented")
		default:
			t.Skipf("test case type %T not yet implemented", tcm.Case_)
		}
	})
}

func runEvaluationTest(t *testing.T, tc testing.EvaluationTestCase) {
	// TODO: call reduction.ReduceTerm and compare with tc.Output
	t.Skipf("evaluation test pending reducer implementation")
}

// termSummary returns a brief description of a term for logging.
func termSummary(term any) string {
	if term == nil {
		return "nil"
	}
	return fmt.Sprintf("%T", term)
}
