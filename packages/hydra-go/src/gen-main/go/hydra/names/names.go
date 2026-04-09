// Note: this is an automatically generated file. Do not edit.

package names

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/formatting"
  libequality "hydra.dev/hydra/lib/equality"
  liblists "hydra.dev/hydra/lib/lists"
  liblogic "hydra.dev/hydra/lib/logic"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libsets "hydra.dev/hydra/lib/sets"
  libstrings "hydra.dev/hydra/lib/strings"
  hmodule "hydra.dev/hydra/module"
  "hydra.dev/hydra/util"
)

func CompactName (namespaces []any, name core.Name) string {
  return func () any {
    var qualName any = QualifyName(name)
    var mns any = qualName.(hmodule.QualifiedName).Namespace
    var local any = qualName.(hmodule.QualifiedName).Local
    return libmaybes.Maybe(func (v any) any {
      return v
    }(name)).(func(any) any)(func (ns hmodule.Namespace) any {
      return libmaybes.Maybe(local).(func(any) any)(func (pre string) any {
        return libstrings.Cat([]any{pre, ":", local})
      }).(func(any) any)(libmaps.Lookup(ns).(func(any) any)(namespaces))
    }).(func(any) any)(mns)
  }().(string)
}

func LocalNameOf (arg_ core.Name) string {
  return func (v any) any {
    return v.(hmodule.QualifiedName).Local
  }(QualifyName(arg_)).(string)
}

func NamespaceOf (arg_ core.Name) any {
  return func (v any) any {
    return v.(hmodule.QualifiedName).Namespace
  }(QualifyName(arg_))
}

func NamespaceToFilePath (caseConv util.CaseConvention, ext hmodule.FileExtension, ns hmodule.Namespace) string {
  return func () any {
    var parts any = liblists.Map(func (v1 string) any {
      return formatting.ConvertCase(util.CaseConventionCamel{}, caseConv, v1)
    }).(func(any) any)(libstrings.SplitOn(".").(func(any) any)(func (v any) any {
      return v
    }(ns)))
    return libstrings.Cat2(libstrings.Cat2(libstrings.Intercalate("/").(func(any) any)(parts)).(func(any) any)(".")).(func(any) any)(func (v any) any {
      return v
    }(ext))
  }().(string)
}

func Qname (ns hmodule.Namespace, name string) core.Name {
  return core.Name(libstrings.Cat([]any{func (v any) any {
    return v
  }(ns), ".", name}).(string))
}

func QualifyName (name core.Name) hmodule.QualifiedName {
  return func () any {
    var parts any = liblists.Reverse(libstrings.SplitOn(".").(func(any) any)(func (v any) any {
      return v
    }(name)))
    return liblogic.IfElse(libequality.Equal(1).(func(any) any)(liblists.Length(parts))).(func(any) any)(hmodule.QualifiedName{Namespace: nil, Local: func (v any) any {
      return v
    }(name).(string)}).(func(any) any)(hmodule.QualifiedName{Namespace: func () any {
      _v := hmodule.Namespace(libstrings.Intercalate(".").(func(any) any)(liblists.Reverse(liblists.Tail(parts))).(string))
      return &_v
    }(), Local: liblists.Head(parts).(string)})
  }().(hmodule.QualifiedName)
}

func UniqueLabel (visited []any, l string) string {
  return liblogic.IfElse(libsets.Member(l).(func(any) any)(visited)).(func(any) any)(UniqueLabel(visited, libstrings.Cat2(l).(func(any) any)("'").(string))).(func(any) any)(l).(string)
}

func UnqualifyName (qname hmodule.QualifiedName) core.Name {
  return func () any {
    var prefix any = libmaybes.Maybe("").(func(any) any)(func (n hmodule.Namespace) any {
      return libstrings.Cat2(func (v any) any {
        return v
      }(n)).(func(any) any)(".")
    }).(func(any) any)(func (v any) any {
      return v.(hmodule.QualifiedName).Namespace
    }(qname))
    return core.Name(libstrings.Cat2(prefix).(func(any) any)(func (v any) any {
      return v.(hmodule.QualifiedName).Local
    }(qname)).(string))
  }().(core.Name)
}
