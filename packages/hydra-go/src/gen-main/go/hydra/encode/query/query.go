// Note: this is an automatically generated file. Do not edit.

package encodequery

import (
  "hydra.dev/hydra/core"
  encodecore "hydra.dev/hydra/encode/core"
  liblists "hydra.dev/hydra/lib/lists"
  libmaybes "hydra.dev/hydra/lib/maybes"
  "hydra.dev/hydra/query"
)

func ComparisonConstraint (v1 query.ComparisonConstraint) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case query.ComparisonConstraintEqual:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.ComparisonConstraint"), Field: core.Field{Name: core.Name("equal"), Term: core.TermUnit{}}}}
      }(v)
      case query.ComparisonConstraintNotEqual:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.ComparisonConstraint"), Field: core.Field{Name: core.Name("notEqual"), Term: core.TermUnit{}}}}
      }(v)
      case query.ComparisonConstraintLessThan:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.ComparisonConstraint"), Field: core.Field{Name: core.Name("lessThan"), Term: core.TermUnit{}}}}
      }(v)
      case query.ComparisonConstraintGreaterThan:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.ComparisonConstraint"), Field: core.Field{Name: core.Name("greaterThan"), Term: core.TermUnit{}}}}
      }(v)
      case query.ComparisonConstraintLessThanOrEqual:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.ComparisonConstraint"), Field: core.Field{Name: core.Name("lessThanOrEqual"), Term: core.TermUnit{}}}}
      }(v)
      case query.ComparisonConstraintGreaterThanOrEqual:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.ComparisonConstraint"), Field: core.Field{Name: core.Name("greaterThanOrEqual"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func Edge (x query.Edge) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.query.Edge"), Fields: []any{core.Field{Name: core.Name("type"), Term: encodecore.Name(func (v any) any {
    return v.(query.Edge).Type_
  }(x).(core.Name))}, core.Field{Name: core.Name("out"), Term: core.TermMaybe{Value: libmaybes.Map(encodecore.Name).(func(any) any)(func (v any) any {
    return v.(query.Edge).Out
  }(x))}}, core.Field{Name: core.Name("in"), Term: core.TermMaybe{Value: libmaybes.Map(encodecore.Name).(func(any) any)(func (v any) any {
    return v.(query.Edge).In
  }(x))}}}}}
}

func GraphPattern (x query.GraphPattern) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.query.GraphPattern"), Fields: []any{core.Field{Name: core.Name("graph"), Term: encodecore.Name(func (v any) any {
    return v.(query.GraphPattern).Graph
  }(x).(core.Name))}, core.Field{Name: core.Name("patterns"), Term: core.TermList{Value: liblists.Map(Pattern).(func(any) any)(func (v any) any {
    return v.(query.GraphPattern).Patterns
  }(x)).([]any)}}}}}
}

func Node (v1 query.Node_) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case query.Node_Term:
      return func (y core.Term) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.Node"), Field: core.Field{Name: core.Name("term"), Term: encodecore.Term(y)}}}
      }(v.Value)
      case query.Node_Variable:
      return func (y query.Variable) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.Node"), Field: core.Field{Name: core.Name("variable"), Term: Variable(y)}}}
      }(v.Value)
      case query.Node_Wildcard:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.Node"), Field: core.Field{Name: core.Name("wildcard"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func Path (v1 query.Path) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case query.PathStep:
      return func (y query.Step) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.Path"), Field: core.Field{Name: core.Name("step"), Term: Step(y)}}}
      }(v.Value)
      case query.PathRegex:
      return func (y query.RegexSequence) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.Path"), Field: core.Field{Name: core.Name("regex"), Term: RegexSequence(y)}}}
      }(v.Value)
      case query.PathInverse:
      return func (y query.Path) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.Path"), Field: core.Field{Name: core.Name("inverse"), Term: Path(y)}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func PathEquation (x query.PathEquation) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.query.PathEquation"), Fields: []any{core.Field{Name: core.Name("left"), Term: Path(func (v any) any {
    return v.(query.PathEquation).Left
  }(x).(query.Path))}, core.Field{Name: core.Name("right"), Term: Path(func (v any) any {
    return v.(query.PathEquation).Right
  }(x).(query.Path))}}}}
}

func Pattern (v1 query.Pattern) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case query.PatternTriple:
      return func (y query.TriplePattern) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.Pattern"), Field: core.Field{Name: core.Name("triple"), Term: TriplePattern(y)}}}
      }(v.Value)
      case query.PatternNegation:
      return func (y query.Pattern) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.Pattern"), Field: core.Field{Name: core.Name("negation"), Term: Pattern(y)}}}
      }(v.Value)
      case query.PatternConjunction:
      return func (y []any) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.Pattern"), Field: core.Field{Name: core.Name("conjunction"), Term: core.TermList{Value: liblists.Map(Pattern).(func(any) any)(y).([]any)}}}}
      }(v.Value)
      case query.PatternDisjunction:
      return func (y []any) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.Pattern"), Field: core.Field{Name: core.Name("disjunction"), Term: core.TermList{Value: liblists.Map(Pattern).(func(any) any)(y).([]any)}}}}
      }(v.Value)
      case query.PatternGraph:
      return func (y query.GraphPattern) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.Pattern"), Field: core.Field{Name: core.Name("graph"), Term: GraphPattern(y)}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func PatternImplication (x query.PatternImplication) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.query.PatternImplication"), Fields: []any{core.Field{Name: core.Name("antecedent"), Term: Pattern(func (v any) any {
    return v.(query.PatternImplication).Antecedent
  }(x).(query.Pattern))}, core.Field{Name: core.Name("consequent"), Term: Pattern(func (v any) any {
    return v.(query.PatternImplication).Consequent
  }(x).(query.Pattern))}}}}
}

func Query (x query.Query) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.query.Query"), Fields: []any{core.Field{Name: core.Name("variables"), Term: core.TermList{Value: liblists.Map(Variable).(func(any) any)(func (v any) any {
    return v.(query.Query).Variables
  }(x)).([]any)}}, core.Field{Name: core.Name("patterns"), Term: core.TermList{Value: liblists.Map(Pattern).(func(any) any)(func (v any) any {
    return v.(query.Query).Patterns
  }(x)).([]any)}}}}}
}

func Range_ (x query.Range) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.query.Range"), Fields: []any{core.Field{Name: core.Name("min"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: func (v any) any {
    return v.(query.Range).Min_
  }(x).(int32)}}}}, core.Field{Name: core.Name("max"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: func (v any) any {
    return v.(query.Range).Max_
  }(x).(int32)}}}}}}}
}

func RegexQuantifier (v1 query.RegexQuantifier) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case query.RegexQuantifierOne:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.RegexQuantifier"), Field: core.Field{Name: core.Name("one"), Term: core.TermUnit{}}}}
      }(v)
      case query.RegexQuantifierZeroOrOne:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.RegexQuantifier"), Field: core.Field{Name: core.Name("zeroOrOne"), Term: core.TermUnit{}}}}
      }(v)
      case query.RegexQuantifierZeroOrMore:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.RegexQuantifier"), Field: core.Field{Name: core.Name("zeroOrMore"), Term: core.TermUnit{}}}}
      }(v)
      case query.RegexQuantifierOneOrMore:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.RegexQuantifier"), Field: core.Field{Name: core.Name("oneOrMore"), Term: core.TermUnit{}}}}
      }(v)
      case query.RegexQuantifierExactly:
      return func (y int32) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.RegexQuantifier"), Field: core.Field{Name: core.Name("exactly"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: y}}}}}}
      }(v.Value)
      case query.RegexQuantifierAtLeast:
      return func (y int32) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.RegexQuantifier"), Field: core.Field{Name: core.Name("atLeast"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: y}}}}}}
      }(v.Value)
      case query.RegexQuantifierRange_:
      return func (y query.Range) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.RegexQuantifier"), Field: core.Field{Name: core.Name("range"), Term: Range_(y)}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func RegexSequence (x query.RegexSequence) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.query.RegexSequence"), Fields: []any{core.Field{Name: core.Name("path"), Term: Path(func (v any) any {
    return v.(query.RegexSequence).Path
  }(x).(query.Path))}, core.Field{Name: core.Name("quantifier"), Term: RegexQuantifier(func (v any) any {
    return v.(query.RegexSequence).Quantifier
  }(x).(query.RegexQuantifier))}}}}
}

func Step (v1 query.Step) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case query.StepEdge:
      return func (y query.Edge) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.Step"), Field: core.Field{Name: core.Name("edge"), Term: Edge(y)}}}
      }(v.Value)
      case query.StepProject:
      return func (y core.Projection) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.Step"), Field: core.Field{Name: core.Name("project"), Term: encodecore.Projection(y)}}}
      }(v.Value)
      case query.StepCompare:
      return func (y query.ComparisonConstraint) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.query.Step"), Field: core.Field{Name: core.Name("compare"), Term: ComparisonConstraint(y)}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func TriplePattern (x query.TriplePattern) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.query.TriplePattern"), Fields: []any{core.Field{Name: core.Name("subject"), Term: Node(func (v any) any {
    return v.(query.TriplePattern).Subject
  }(x).(query.Node_))}, core.Field{Name: core.Name("predicate"), Term: Path(func (v any) any {
    return v.(query.TriplePattern).Predicate
  }(x).(query.Path))}, core.Field{Name: core.Name("object"), Term: Node(func (v any) any {
    return v.(query.TriplePattern).Object
  }(x).(query.Node_))}}}}
}

func Variable (x query.Variable) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.query.Variable"), Body: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v
  }(x).(string)}}}}
}
