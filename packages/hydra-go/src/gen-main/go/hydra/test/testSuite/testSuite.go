// Note: this is an automatically generated file. Do not edit.

package testtestSuite

import (
  testannotations "hydra.dev/hydra/test/annotations"
  testcheckingall "hydra.dev/hydra/test/checking/all"
  testetaExpansion "hydra.dev/hydra/test/etaExpansion"
  testformatting "hydra.dev/hydra/test/formatting"
  testhoistingall "hydra.dev/hydra/test/hoisting/all"
  testinferenceall "hydra.dev/hydra/test/inference/all"
  testjsonparser "hydra.dev/hydra/test/json/parser"
  testjsonroundtrip "hydra.dev/hydra/test/json/roundtrip"
  testjsonwriter "hydra.dev/hydra/test/json/writer"
  testlibchars "hydra.dev/hydra/test/lib/chars"
  testlibeithers "hydra.dev/hydra/test/lib/eithers"
  testlibequality "hydra.dev/hydra/test/lib/equality"
  testliblists "hydra.dev/hydra/test/lib/lists"
  testlibliterals "hydra.dev/hydra/test/lib/literals"
  testliblogic "hydra.dev/hydra/test/lib/logic"
  testlibmaps "hydra.dev/hydra/test/lib/maps"
  testlibmath "hydra.dev/hydra/test/lib/math"
  testlibmaybes "hydra.dev/hydra/test/lib/maybes"
  testlibpairs "hydra.dev/hydra/test/lib/pairs"
  testlibsets "hydra.dev/hydra/test/lib/sets"
  testlibstrings "hydra.dev/hydra/test/lib/strings"
  testreduction "hydra.dev/hydra/test/reduction"
  testrewriting "hydra.dev/hydra/test/rewriting"
  testserialization "hydra.dev/hydra/test/serialization"
  testsorting "hydra.dev/hydra/test/sorting"
  testsubstitution "hydra.dev/hydra/test/substitution"
  testunification "hydra.dev/hydra/test/unification"
  "hydra.dev/hydra/testing"
)

var AllTests = testing.TestGroup{Name: "common", Description: nil, Subgroups: []any{testlibchars.AllTests, testlibeithers.AllTests, testlibequality.AllTests, testliblists.AllTests, testlibliterals.AllTests, testliblogic.AllTests, testlibmaps.AllTests, testlibmath.AllTests, testlibmaybes.AllTests, testlibpairs.AllTests, testlibsets.AllTests, testlibstrings.AllTests, testannotations.AllTests, testcheckingall.AllTests, testetaExpansion.AllTests, testformatting.AllTests, testhoistingall.AllTests, testinferenceall.AllTests, testjsonparser.AllTests, testjsonroundtrip.AllTests, testjsonwriter.AllTests, testreduction.AllTests, testrewriting.AllTests, testserialization.AllTests, testsorting.AllTests, testsubstitution.AllTests, testunification.AllTests}, Cases: []any{}}
