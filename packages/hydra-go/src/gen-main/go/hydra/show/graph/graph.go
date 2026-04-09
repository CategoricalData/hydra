// Note: this is an automatically generated file. Do not edit.

package showgraph

import (
  liblists "hydra.dev/hydra/lib/lists"
  libstrings "hydra.dev/hydra/lib/strings"
  showcore "hydra.dev/hydra/show/core"
)

func Graph (elements []any) string {
  return func () any {
    var elementStrs any = liblists.Map(showcore.Binding).(func(any) any)(elements)
    return libstrings.Cat([]any{"{", libstrings.Intercalate(", ").(func(any) any)(elementStrs), "}"})
  }().(string)
}
