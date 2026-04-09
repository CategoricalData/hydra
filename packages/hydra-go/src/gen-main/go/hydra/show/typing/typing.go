// Note: this is an automatically generated file. Do not edit.

package showtyping

import (
  "hydra.dev/hydra/core"
  liblists "hydra.dev/hydra/lib/lists"
  libmaps "hydra.dev/hydra/lib/maps"
  libpairs "hydra.dev/hydra/lib/pairs"
  libstrings "hydra.dev/hydra/lib/strings"
  showcore "hydra.dev/hydra/show/core"
  "hydra.dev/hydra/typing"
)

func TypeConstraint (tc typing.TypeConstraint) string {
  return func () any {
    var ltyp any = func (v any) any {
      return v.(typing.TypeConstraint).Left
    }(tc)
    var rtyp any = func (v any) any {
      return v.(typing.TypeConstraint).Right
    }(tc)
    return libstrings.Cat([]any{showcore.Type_(ltyp.(core.Type)), "\u2261", showcore.Type_(rtyp.(core.Type))})
  }().(string)
}

func TypeSubst (ts typing.TypeSubst) string {
  return func () any {
    var subst any = func (v any) any {
      return v
    }(ts)
    var pairs any = libmaps.ToList(subst)
    showPair := func (pair any) any {
      return func () any {
        var name any = libpairs.First(pair)
        var typ any = libpairs.Second(pair)
        return libstrings.Cat([]any{name, "\u21A6", showcore.Type_(typ.(core.Type))})
      }()
    }
    var pairStrs any = liblists.Map(showPair).(func(any) any)(pairs)
    return libstrings.Cat([]any{"{", libstrings.Intercalate(",").(func(any) any)(pairStrs), "}"})
  }().(string)
}
