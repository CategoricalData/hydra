// Note: this is an automatically generated file. Do not edit.

package hydra.encode;

/**
 * Term encoders for hydra.paths
 */
public interface Paths {
  static hydra.core.Term subtermEdge(hydra.paths.SubtermEdge x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtermEdge"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("source"), hydra.encode.Paths.subtermNode((x).source)),
      new hydra.core.Field(new hydra.core.Name("path"), hydra.encode.Paths.subtermPath((x).path)),
      new hydra.core.Field(new hydra.core.Name("target"), hydra.encode.Paths.subtermNode((x).target)))));
  }

  static hydra.core.Term subtermGraph(hydra.paths.SubtermGraph x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtermGraph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("nodes"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.Paths::subtermNode,
        (x).nodes))),
      new hydra.core.Field(new hydra.core.Name("edges"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.Paths::subtermEdge,
        (x).edges))))));
  }

  static hydra.core.Term subtermNode(hydra.paths.SubtermNode x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtermNode"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)),
      new hydra.core.Field(new hydra.core.Name("label"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).label))),
      new hydra.core.Field(new hydra.core.Name("id"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).id))))));
  }

  static hydra.core.Term subtermPath(hydra.paths.SubtermPath x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.paths.SubtermPath"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
      hydra.encode.Paths::subtermStep,
      (x).value))));
  }

  static hydra.core.Term subtermStep(hydra.paths.SubtermStep v1) {
    return (v1).accept(new hydra.paths.SubtermStep.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.AnnotatedBody y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("annotatedBody"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.ApplicationFunction y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("applicationFunction"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.ApplicationArgument y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("applicationArgument"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.LambdaBody y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("lambdaBody"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.UnionCasesDefault y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("unionCasesDefault"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.UnionCasesBranch y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("unionCasesBranch"), hydra.encode.Core.name((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.LetBody y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("letBody"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.LetBinding y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("letBinding"), hydra.encode.Core.name((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.ListElement y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("listElement"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32((y).value))))));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.MapKey y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("mapKey"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32((y).value))))));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.MapValue y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("mapValue"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32((y).value))))));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.MaybeTerm y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("maybeTerm"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.ProductTerm y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("productTerm"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32((y).value))))));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.RecordField y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("recordField"), hydra.encode.Core.name((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.SetElement y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("setElement"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32((y).value))))));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.SumTerm y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("sumTerm"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.TypeLambdaBody y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("typeLambdaBody"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.TypeApplicationTerm y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("typeApplicationTerm"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.InjectionTerm y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("injectionTerm"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtermStep.WrappedTerm y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("wrappedTerm"), new hydra.core.Term.Unit())));
      }
    });
  }

  static hydra.core.Term subtypeEdge(hydra.paths.SubtypeEdge x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtypeEdge"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("source"), hydra.encode.Paths.subtypeNode((x).source)),
      new hydra.core.Field(new hydra.core.Name("path"), hydra.encode.Paths.subtypePath((x).path)),
      new hydra.core.Field(new hydra.core.Name("target"), hydra.encode.Paths.subtypeNode((x).target)))));
  }

  static hydra.core.Term subtypeGraph(hydra.paths.SubtypeGraph x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtypeGraph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("nodes"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.Paths::subtypeNode,
        (x).nodes))),
      new hydra.core.Field(new hydra.core.Name("edges"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.Paths::subtypeEdge,
        (x).edges))))));
  }

  static hydra.core.Term subtypeNode(hydra.paths.SubtypeNode x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtypeNode"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)),
      new hydra.core.Field(new hydra.core.Name("label"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).label))),
      new hydra.core.Field(new hydra.core.Name("id"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).id))))));
  }

  static hydra.core.Term subtypePath(hydra.paths.SubtypePath x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.paths.SubtypePath"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
      hydra.encode.Paths::subtypeStep,
      (x).value))));
  }

  static hydra.core.Term subtypeStep(hydra.paths.SubtypeStep v1) {
    return (v1).accept(new hydra.paths.SubtypeStep.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.AnnotatedBody y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("annotatedBody"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.ApplicationFunction y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("applicationFunction"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.ApplicationArgument y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("applicationArgument"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.EitherLeft y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("eitherLeft"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.EitherRight y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("eitherRight"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.ForallBody y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("forallBody"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.FunctionDomain y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("functionDomain"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.FunctionCodomain y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("functionCodomain"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.ListElement y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("listElement"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.MapKeys y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("mapKeys"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.MapValues y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("mapValues"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.MaybeElement y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("maybeElement"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.PairFirst y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("pairFirst"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.PairSecond y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("pairSecond"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.RecordField y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("recordField"), hydra.encode.Core.name((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.SetElement y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("setElement"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.UnionField y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("unionField"), hydra.encode.Core.name((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.paths.SubtypeStep.WrappedType y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("wrappedType"), new hydra.core.Term.Unit())));
      }
    });
  }
}
