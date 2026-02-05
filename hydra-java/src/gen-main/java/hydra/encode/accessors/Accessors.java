// Note: this is an automatically generated file. Do not edit.

package hydra.encode.accessors;

/**
 * Term encoders for hydra.accessors
 */
public interface Accessors {
  static hydra.core.Term accessorEdge(hydra.accessors.AccessorEdge x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.accessors.AccessorEdge"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("source"), hydra.encode.accessors.Accessors.accessorNode(((x)).source)),
      new hydra.core.Field(new hydra.core.Name("path"), hydra.encode.accessors.Accessors.accessorPath(((x)).path)),
      new hydra.core.Field(new hydra.core.Name("target"), hydra.encode.accessors.Accessors.accessorNode(((x)).target)))));
  }
  
  static hydra.core.Term accessorGraph(hydra.accessors.AccessorGraph x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.accessors.AccessorGraph"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("nodes"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.encode.accessors.Accessors::accessorNode),
        ((x)).nodes))),
      new hydra.core.Field(new hydra.core.Name("edges"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.encode.accessors.Accessors::accessorEdge),
        ((x)).edges))))));
  }
  
  static hydra.core.Term accessorNode(hydra.accessors.AccessorNode x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.accessors.AccessorNode"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.core.Core.name(((x)).name)),
      new hydra.core.Field(new hydra.core.Name("label"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).label))),
      new hydra.core.Field(new hydra.core.Name("id"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).id))))));
  }
  
  static hydra.core.Term accessorPath(hydra.accessors.AccessorPath x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.accessors.AccessorPath"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
      (hydra.encode.accessors.Accessors::termAccessor),
      ((x)).value))));
  }
  
  static hydra.core.Term termAccessor(hydra.accessors.TermAccessor v1) {
    return ((v1)).accept(new hydra.accessors.TermAccessor.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.AnnotatedBody y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("annotatedBody"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.ApplicationFunction y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("applicationFunction"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.ApplicationArgument y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("applicationArgument"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.LambdaBody y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("lambdaBody"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.UnionCasesDefault y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("unionCasesDefault"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.UnionCasesBranch y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("unionCasesBranch"), hydra.encode.core.Core.name(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.LetBody y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("letBody"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.LetBinding y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("letBinding"), hydra.encode.core.Core.name(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.ListElement y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("listElement"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.MapKey y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("mapKey"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.MapValue y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("mapValue"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.MaybeTerm y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("maybeTerm"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.ProductTerm y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("productTerm"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.RecordField y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("recordField"), hydra.encode.core.Core.name(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.SetElement y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("setElement"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.SumTerm y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("sumTerm"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.TypeLambdaBody y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("typeLambdaBody"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.TypeApplicationTerm y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("typeApplicationTerm"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.InjectionTerm y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("injectionTerm"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.accessors.TermAccessor.WrappedTerm y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("wrappedTerm"), new hydra.core.Term.Unit())));
      }
    });
  }
}
