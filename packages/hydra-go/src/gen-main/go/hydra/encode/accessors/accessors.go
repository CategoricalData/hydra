// Note: this is an automatically generated file. Do not edit.

package encodeaccessors

import (
  "hydra.dev/hydra/accessors"
  "hydra.dev/hydra/core"
  encodecore "hydra.dev/hydra/encode/core"
  liblists "hydra.dev/hydra/lib/lists"
)

func AccessorEdge (x accessors.AccessorEdge) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.accessors.AccessorEdge"), Fields: []any{core.Field{Name: core.Name("source"), Term: AccessorNode(func (v any) any {
    return v.(accessors.AccessorEdge).Source
  }(x).(accessors.AccessorNode))}, core.Field{Name: core.Name("path"), Term: AccessorPath(func (v any) any {
    return v.(accessors.AccessorEdge).Path
  }(x).(accessors.AccessorPath))}, core.Field{Name: core.Name("target"), Term: AccessorNode(func (v any) any {
    return v.(accessors.AccessorEdge).Target
  }(x).(accessors.AccessorNode))}}}}
}

func AccessorGraph (x accessors.AccessorGraph) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.accessors.AccessorGraph"), Fields: []any{core.Field{Name: core.Name("nodes"), Term: core.TermList{Value: liblists.Map(AccessorNode).(func(any) any)(func (v any) any {
    return v.(accessors.AccessorGraph).Nodes
  }(x)).([]any)}}, core.Field{Name: core.Name("edges"), Term: core.TermList{Value: liblists.Map(AccessorEdge).(func(any) any)(func (v any) any {
    return v.(accessors.AccessorGraph).Edges
  }(x)).([]any)}}}}}
}

func AccessorNode (x accessors.AccessorNode) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.accessors.AccessorNode"), Fields: []any{core.Field{Name: core.Name("name"), Term: encodecore.Name(func (v any) any {
    return v.(accessors.AccessorNode).Name
  }(x).(core.Name))}, core.Field{Name: core.Name("label"), Term: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v.(accessors.AccessorNode).Label
  }(x).(string)}}}, core.Field{Name: core.Name("id"), Term: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v.(accessors.AccessorNode).Id
  }(x).(string)}}}}}}
}

func AccessorPath (x accessors.AccessorPath) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.accessors.AccessorPath"), Body: core.TermList{Value: liblists.Map(TermAccessor).(func(any) any)(func (v any) any {
    return v
  }(x)).([]any)}}}
}

func TermAccessor (v1 accessors.TermAccessor) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case accessors.TermAccessorAnnotatedBody:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("annotatedBody"), Term: core.TermUnit{}}}}
      }(v)
      case accessors.TermAccessorApplicationFunction:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("applicationFunction"), Term: core.TermUnit{}}}}
      }(v)
      case accessors.TermAccessorApplicationArgument:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("applicationArgument"), Term: core.TermUnit{}}}}
      }(v)
      case accessors.TermAccessorLambdaBody:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("lambdaBody"), Term: core.TermUnit{}}}}
      }(v)
      case accessors.TermAccessorUnionCasesDefault:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("unionCasesDefault"), Term: core.TermUnit{}}}}
      }(v)
      case accessors.TermAccessorUnionCasesBranch:
      return func (y core.Name) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("unionCasesBranch"), Term: encodecore.Name(y)}}}
      }(v.Value)
      case accessors.TermAccessorLetBody:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("letBody"), Term: core.TermUnit{}}}}
      }(v)
      case accessors.TermAccessorLetBinding:
      return func (y core.Name) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("letBinding"), Term: encodecore.Name(y)}}}
      }(v.Value)
      case accessors.TermAccessorListElement:
      return func (y int32) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("listElement"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: y}}}}}}
      }(v.Value)
      case accessors.TermAccessorMapKey:
      return func (y int32) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("mapKey"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: y}}}}}}
      }(v.Value)
      case accessors.TermAccessorMapValue:
      return func (y int32) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("mapValue"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: y}}}}}}
      }(v.Value)
      case accessors.TermAccessorMaybeTerm:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("maybeTerm"), Term: core.TermUnit{}}}}
      }(v)
      case accessors.TermAccessorProductTerm:
      return func (y int32) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("productTerm"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: y}}}}}}
      }(v.Value)
      case accessors.TermAccessorRecordField:
      return func (y core.Name) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("recordField"), Term: encodecore.Name(y)}}}
      }(v.Value)
      case accessors.TermAccessorSetElement:
      return func (y int32) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("setElement"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: y}}}}}}
      }(v.Value)
      case accessors.TermAccessorSumTerm:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("sumTerm"), Term: core.TermUnit{}}}}
      }(v)
      case accessors.TermAccessorTypeLambdaBody:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("typeLambdaBody"), Term: core.TermUnit{}}}}
      }(v)
      case accessors.TermAccessorTypeApplicationTerm:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("typeApplicationTerm"), Term: core.TermUnit{}}}}
      }(v)
      case accessors.TermAccessorInjectionTerm:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("injectionTerm"), Term: core.TermUnit{}}}}
      }(v)
      case accessors.TermAccessorWrappedTerm:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.accessors.TermAccessor"), Field: core.Field{Name: core.Name("wrappedTerm"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}
