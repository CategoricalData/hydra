// Note: this is an automatically generated file. Do not edit.

package extractutil

import (
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/error"
  extractcore "hydra.dev/hydra/extract/core"
  "hydra.dev/hydra/graph"
  libeithers "hydra.dev/hydra/lib/eithers"
  libequality "hydra.dev/hydra/lib/equality"
  liblogic "hydra.dev/hydra/lib/logic"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/util"
)

func Comparison (cx context.Context, graph graph.Graph, term core.Term) any {
  return libeithers.Bind(extractcore.UnitVariant(cx, core.Name("hydra.util.Comparison"), graph, term)).(func(any) any)(func (fname core.Name) any {
    return liblogic.IfElse(libequality.Equal(func (v any) any {
      return v
    }(fname)).(func(any) any)("equalTo")).(func(any) any)([2]any{"right", util.ComparisonEqualTo{}}).(func(any) any)(liblogic.IfElse(libequality.Equal(func (v any) any {
      return v
    }(fname)).(func(any) any)("lessThan")).(func(any) any)([2]any{"right", util.ComparisonLessThan{}}).(func(any) any)(liblogic.IfElse(libequality.Equal(func (v any) any {
      return v
    }(fname)).(func(any) any)("greaterThan")).(func(any) any)([2]any{"right", util.ComparisonGreaterThan{}}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2("expected comparison but found ").(func(any) any)(func (v any) any {
      return v
    }(fname)).(string))}, Context: cx}})))
  })
}
