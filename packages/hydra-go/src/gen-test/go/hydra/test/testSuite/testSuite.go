// Note: this is an automatically generated file. Do not edit.

package testSuite

import (
  "hydra/test/annotations",
  "hydra/test/checking/all",
  "hydra/test/etaExpansion",
  "hydra/test/formatting",
  "hydra/test/hoisting/all",
  "hydra/test/inference/all",
  "hydra/test/json/parser",
  "hydra/test/json/roundtrip",
  "hydra/test/json/writer",
  "hydra/test/lib/chars",
  "hydra/test/lib/eithers",
  "hydra/test/lib/equality",
  "hydra/test/lib/lists",
  "hydra/test/lib/literals",
  "hydra/test/lib/logic",
  "hydra/test/lib/maps",
  "hydra/test/lib/math",
  "hydra/test/lib/maybes",
  "hydra/test/lib/pairs",
  "hydra/test/lib/sets",
  "hydra/test/lib/strings",
  "hydra/test/reduction",
  "hydra/test/rewriting",
  "hydra/test/serialization",
  "hydra/test/sorting",
  "hydra/test/substitution",
  "hydra/test/unification",
  "hydra/testing")

var allTests = testing.TestGroup{Name: "common", Description: nil, Subgroups: []any{chars.AllTests, eithers.AllTests, equality.AllTests, lists.AllTests, literals.AllTests, logic.AllTests, maps.AllTests, math.AllTests, maybes.AllTests, pairs.AllTests, sets.AllTests, strings.AllTests, annotations.AllTests, all.AllTests, etaExpansion.AllTests, formatting.AllTests, all.AllTests, all.AllTests, parser.AllTests, roundtrip.AllTests, writer.AllTests, reduction.AllTests, rewriting.AllTests, serialization.AllTests, sorting.AllTests, substitution.AllTests, unification.AllTests}, Cases: []any{}}
