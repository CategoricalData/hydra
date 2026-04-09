// Note: this is an automatically generated file. Do not edit.

package encodemodule

import (
  "hydra.dev/hydra/core"
  encodecore "hydra.dev/hydra/encode/core"
  liblists "hydra.dev/hydra/lib/lists"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libpairs "hydra.dev/hydra/lib/pairs"
  hmodule "hydra.dev/hydra/module"
)

func Definition (v1 hmodule.Definition) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case hmodule.DefinitionTerm:
      return func (y hmodule.TermDefinition) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.module.Definition"), Field: core.Field{Name: core.Name("term"), Term: TermDefinition(y)}}}
      }(v.Value)
      case hmodule.DefinitionType_:
      return func (y hmodule.TypeDefinition) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.module.Definition"), Field: core.Field{Name: core.Name("type"), Term: TypeDefinition(y)}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func FileExtension (x hmodule.FileExtension) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.module.FileExtension"), Body: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v
  }(x).(string)}}}}
}

func Module (x hmodule.Module) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.module.Module"), Fields: []any{core.Field{Name: core.Name("namespace"), Term: Namespace(func (v any) any {
    return v.(hmodule.Module).Namespace
  }(x).(hmodule.Namespace))}, core.Field{Name: core.Name("elements"), Term: core.TermList{Value: liblists.Map(encodecore.Binding).(func(any) any)(func (v any) any {
    return v.(hmodule.Module).Elements
  }(x)).([]any)}}, core.Field{Name: core.Name("termDependencies"), Term: core.TermList{Value: liblists.Map(Namespace).(func(any) any)(func (v any) any {
    return v.(hmodule.Module).TermDependencies
  }(x)).([]any)}}, core.Field{Name: core.Name("typeDependencies"), Term: core.TermList{Value: liblists.Map(Namespace).(func(any) any)(func (v any) any {
    return v.(hmodule.Module).TypeDependencies
  }(x)).([]any)}}, core.Field{Name: core.Name("description"), Term: core.TermMaybe{Value: libmaybes.Map(func (x2 string) any {
    return core.TermLiteral{Value: core.LiteralString_{Value: x2}}
  }).(func(any) any)(func (v any) any {
    return v.(hmodule.Module).Description
  }(x))}}}}}
}

func Namespace (x hmodule.Namespace) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.module.Namespace"), Body: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v
  }(x).(string)}}}}
}

func Namespaces[T0 any] (n func(T0) core.Term, x hmodule.Namespaces[T0]) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.module.Namespaces"), Fields: []any{core.Field{Name: core.Name("focus"), Term: core.TermPair{Value: libpairs.Bimap(Namespace).(func(any) any)(n).(func(any) any)(func (v any) any {
    return v.(hmodule.Namespaces[T0]).Focus
  }(x))}}, core.Field{Name: core.Name("mapping"), Term: core.TermMap_{Value: libmaps.Bimap(Namespace).(func(any) any)(n).(func(any) any)(func (v any) any {
    return v.(hmodule.Namespaces[T0]).Mapping
  }(x)).([]any)}}}}}
}

func QualifiedName (x hmodule.QualifiedName) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.module.QualifiedName"), Fields: []any{core.Field{Name: core.Name("namespace"), Term: core.TermMaybe{Value: libmaybes.Map(Namespace).(func(any) any)(func (v any) any {
    return v.(hmodule.QualifiedName).Namespace
  }(x))}}, core.Field{Name: core.Name("local"), Term: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v.(hmodule.QualifiedName).Local
  }(x).(string)}}}}}}
}

func TermDefinition (x hmodule.TermDefinition) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.module.TermDefinition"), Fields: []any{core.Field{Name: core.Name("name"), Term: encodecore.Name(func (v any) any {
    return v.(hmodule.TermDefinition).Name
  }(x).(core.Name))}, core.Field{Name: core.Name("term"), Term: encodecore.Term(func (v any) any {
    return v.(hmodule.TermDefinition).Term
  }(x).(core.Term))}, core.Field{Name: core.Name("type"), Term: encodecore.TypeScheme(func (v any) any {
    return v.(hmodule.TermDefinition).Type_
  }(x).(core.TypeScheme))}}}}
}

func TypeDefinition (x hmodule.TypeDefinition) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.module.TypeDefinition"), Fields: []any{core.Field{Name: core.Name("name"), Term: encodecore.Name(func (v any) any {
    return v.(hmodule.TypeDefinition).Name
  }(x).(core.Name))}, core.Field{Name: core.Name("type"), Term: encodecore.Type_(func (v any) any {
    return v.(hmodule.TypeDefinition).Type_
  }(x).(core.Type))}}}}
}
