// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Source-to-source automatic differentiation for Float64 terms.
 */
public interface Differentiation {
  static hydra.core.Term differentiateBinary(hydra.core.Name bfname, hydra.core.Term a, hydra.core.Term b, hydra.core.Term da, hydra.core.Term db) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Or.apply(
        hydra.lib.equality.Equal.apply(
          bfname,
          new hydra.core.Name("hydra.lib.math.add")),
        hydra.lib.equality.Equal.apply(
          bfname,
          new hydra.core.Name("hydra.lib.math.addFloat64"))),
      () -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), da)), db)),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.logic.Or.apply(
          hydra.lib.equality.Equal.apply(
            bfname,
            new hydra.core.Name("hydra.lib.math.sub")),
          hydra.lib.equality.Equal.apply(
            bfname,
            new hydra.core.Name("hydra.lib.math.subFloat64"))),
        () -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.subFloat64")), da)), db)),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.Or.apply(
            hydra.lib.equality.Equal.apply(
              bfname,
              new hydra.core.Name("hydra.lib.math.mul")),
            hydra.lib.equality.Equal.apply(
              bfname,
              new hydra.core.Name("hydra.lib.math.mulFloat64"))),
          () -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), a)), db)))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), b)), da)))),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              bfname,
              new hydra.core.Name("hydra.lib.math.pow")),
            () -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), a)), b)))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), db)), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), a)))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), b)), da)))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), a)), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))))))))),
            () -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                bfname,
                new hydra.core.Name("hydra.lib.math.atan2")),
              () -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.subFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), b)), da)))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), a)), db)))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), a)), a)))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), b)), b)))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))))),
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.equality.Equal.apply(
                  bfname,
                  new hydra.core.Name("hydra.lib.math.logBase")),
                () -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.subFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), a)))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), db)), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), b)), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), b)))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), da)), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), a)), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), a)))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), a)))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))))),
                () -> new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))))));
  }

  static hydra.core.Term differentiateFunction(hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return term;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return hydra.Differentiation.differentiateFunction((at).value.body);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Lambda l) {
        hydra.core.Term body = (l).value.body;
        hydra.core.Name paramName = (l).value.parameter;
        return new hydra.core.Term.Lambda(new hydra.core.Lambda(paramName, (l).value.domain, hydra.Differentiation.differentiateTerm(
          paramName,
          body)));
      }
    });
  }

  static hydra.core.Term differentiateTerm(hydra.core.Name dx, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable v) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (v).value,
            dx),
          () -> new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))),
          () -> new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Literal ignored) {
        return new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Application app) {
        hydra.core.Term arg = (app).value.argument;
        hydra.core.Term func = (app).value.function;
        return (func).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Term instance) {
            return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), hydra.Differentiation.differentiateTerm(
              dx,
              new hydra.core.Term.Application(new hydra.core.Application(func, arg))))), hydra.Differentiation.differentiateTerm(
              dx,
              arg)));
          }

          @Override
          public hydra.core.Term visit(hydra.core.Term.Variable fname) {
            return hydra.lib.maybes.Maybe.applyLazy(
              () -> hydra.Differentiation.differentiateTerm(
                dx,
                new hydra.core.Term.Application(new hydra.core.Application(func, arg))),
              (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (derivTerm -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Application(new hydra.core.Application(derivTerm, arg)))), hydra.Differentiation.differentiateTerm(
                dx,
                arg)))),
              hydra.Differentiation.primitiveDerivative((fname).value));
          }

          @Override
          public hydra.core.Term visit(hydra.core.Term.Application innerApp) {
            hydra.core.Term innerArg = (innerApp).value.argument;
            hydra.core.Term innerFunc = (innerApp).value.function;
            return (innerFunc).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public hydra.core.Term otherwise(hydra.core.Term instance) {
                return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), hydra.Differentiation.differentiateTerm(
                  dx,
                  new hydra.core.Term.Application(new hydra.core.Application(func, arg))))), hydra.Differentiation.differentiateTerm(
                  dx,
                  arg)));
              }

              @Override
              public hydra.core.Term visit(hydra.core.Term.Variable bfname) {
                return hydra.Differentiation.differentiateBinary(
                  (bfname).value,
                  innerArg,
                  arg,
                  hydra.Differentiation.differentiateTerm(
                    dx,
                    innerArg),
                  hydra.Differentiation.differentiateTerm(
                    dx,
                    arg));
              }
            });
          }
        });
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Lambda l) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (l).value.parameter,
            dx),
          () -> new hydra.core.Term.Lambda(new hydra.core.Lambda((l).value.parameter, (l).value.domain, new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))),
          () -> new hydra.core.Term.Lambda(new hydra.core.Lambda((l).value.parameter, (l).value.domain, hydra.Differentiation.differentiateTerm(
            dx,
            (l).value.body))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Cases ignored) {
        return new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Project ignored) {
        return new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Unwrap ignored) {
        return new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Let l) {
        return new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, hydra.Differentiation.differentiateTerm(
            dx,
            (b).term), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))),
          (l).value.bindings), hydra.Differentiation.differentiateTerm(
          dx,
          (l).value.body)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return hydra.Differentiation.differentiateTerm(
          dx,
          (at).value.body);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.List elems) {
        return new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v1 -> hydra.Differentiation.differentiateTerm(
            dx,
            v1)),
          (elems).value));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Pair p) {
        return new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(hydra.Differentiation.differentiateTerm(
          dx,
          hydra.lib.pairs.First.apply((p).value)), hydra.Differentiation.differentiateTerm(
          dx,
          hydra.lib.pairs.Second.apply((p).value))))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Record r) {
        return new hydra.core.Term.Record(new hydra.core.Record((r).value.typeName, hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (fld -> new hydra.core.Field((fld).name, hydra.Differentiation.differentiateTerm(
            dx,
            (fld).term))),
          (r).value.fields)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication ta) {
        return hydra.Differentiation.differentiateTerm(
          dx,
          (ta).value.body);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
        return hydra.Differentiation.differentiateTerm(
          dx,
          (tl).value.body);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Unit ignored) {
        return new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Set ignored) {
        return new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Map ignored) {
        return new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Either ignored) {
        return new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Maybe ignored) {
        return new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Inject ignored) {
        return new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Wrap ignored) {
        return new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)));
      }
    });
  }

  static hydra.core.Term gradient(hydra.core.Name typeName, java.util.List<hydra.core.Name> vars, hydra.core.Term term) {
    return new hydra.core.Term.Record(new hydra.core.Record(typeName, hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.core.Field>) (v -> new hydra.core.Field(v, hydra.Differentiation.differentiateTerm(
        v,
        term))),
      vars)));
  }

  static hydra.util.Maybe<hydra.core.Term> primitiveDerivative(hydra.core.Name name) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        name,
        new hydra.core.Name("hydra.lib.math.sin")),
      () -> hydra.util.Maybe.just(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos"))),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          name,
          new hydra.core.Name("hydra.lib.math.cos")),
        () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.negateFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Variable(new hydra.core.Name("_x"))))))))),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            name,
            new hydra.core.Name("hydra.lib.math.tan")),
          () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Variable(new hydra.core.Name("_x")))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-2.0)))))))),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              name,
              new hydra.core.Name("hydra.lib.math.exp")),
            () -> hydra.util.Maybe.just(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.exp"))),
            () -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                name,
                new hydra.core.Name("hydra.lib.math.log")),
              () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Variable(new hydra.core.Name("_x")))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0)))))))),
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.equality.Equal.apply(
                  name,
                  new hydra.core.Name("hydra.lib.math.sqrt")),
                () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sqrt")), new hydra.core.Term.Variable(new hydra.core.Name("_x")))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0)))))))))),
                () -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.equality.Equal.apply(
                    name,
                    new hydra.core.Name("hydra.lib.math.asin")),
                  () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sqrt")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.subFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Variable(new hydra.core.Name("_x")))), new hydra.core.Term.Variable(new hydra.core.Name("_x")))))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0)))))))),
                  () -> hydra.lib.logic.IfElse.lazy(
                    hydra.lib.equality.Equal.apply(
                      name,
                      new hydra.core.Name("hydra.lib.math.acos")),
                    () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.negateFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sqrt")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.subFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Variable(new hydra.core.Name("_x")))), new hydra.core.Term.Variable(new hydra.core.Name("_x")))))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0)))))))))),
                    () -> hydra.lib.logic.IfElse.lazy(
                      hydra.lib.equality.Equal.apply(
                        name,
                        new hydra.core.Name("hydra.lib.math.atan")),
                      () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Variable(new hydra.core.Name("_x")))), new hydra.core.Term.Variable(new hydra.core.Name("_x")))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0)))))))),
                      () -> hydra.lib.logic.IfElse.lazy(
                        hydra.lib.equality.Equal.apply(
                          name,
                          new hydra.core.Name("hydra.lib.math.sinh")),
                        () -> hydra.util.Maybe.just(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cosh"))),
                        () -> hydra.lib.logic.IfElse.lazy(
                          hydra.lib.equality.Equal.apply(
                            name,
                            new hydra.core.Name("hydra.lib.math.cosh")),
                          () -> hydra.util.Maybe.just(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sinh"))),
                          () -> hydra.lib.logic.IfElse.lazy(
                            hydra.lib.equality.Equal.apply(
                              name,
                              new hydra.core.Name("hydra.lib.math.tanh")),
                            () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.subFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.tanh")), new hydra.core.Term.Variable(new hydra.core.Name("_x")))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.tanh")), new hydra.core.Term.Variable(new hydra.core.Name("_x"))))))))))),
                            () -> hydra.lib.logic.IfElse.lazy(
                              hydra.lib.equality.Equal.apply(
                                name,
                                new hydra.core.Name("hydra.lib.math.asinh")),
                              () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sqrt")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Variable(new hydra.core.Name("_x")))), new hydra.core.Term.Variable(new hydra.core.Name("_x")))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0)))))))),
                              () -> hydra.lib.logic.IfElse.lazy(
                                hydra.lib.equality.Equal.apply(
                                  name,
                                  new hydra.core.Name("hydra.lib.math.acosh")),
                                () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sqrt")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.subFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Variable(new hydra.core.Name("_x")))), new hydra.core.Term.Variable(new hydra.core.Name("_x")))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0)))))))),
                                () -> hydra.lib.logic.IfElse.lazy(
                                  hydra.lib.equality.Equal.apply(
                                    name,
                                    new hydra.core.Name("hydra.lib.math.atanh")),
                                  () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.subFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Variable(new hydra.core.Name("_x")))), new hydra.core.Term.Variable(new hydra.core.Name("_x")))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0)))))))),
                                  () -> hydra.lib.logic.IfElse.lazy(
                                    hydra.lib.equality.Equal.apply(
                                      name,
                                      new hydra.core.Name("hydra.lib.math.negate")),
                                    () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0)))))),
                                    () -> hydra.lib.logic.IfElse.lazy(
                                      hydra.lib.equality.Equal.apply(
                                        name,
                                        new hydra.core.Name("hydra.lib.math.abs")),
                                      () -> hydra.util.Maybe.just(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.signum"))),
                                      () -> hydra.lib.logic.IfElse.lazy(
                                        hydra.lib.equality.Equal.apply(
                                          name,
                                          new hydra.core.Name("hydra.lib.math.ceiling")),
                                        () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))),
                                        () -> hydra.lib.logic.IfElse.lazy(
                                          hydra.lib.equality.Equal.apply(
                                            name,
                                            new hydra.core.Name("hydra.lib.math.floor")),
                                          () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))),
                                          () -> hydra.lib.logic.IfElse.lazy(
                                            hydra.lib.equality.Equal.apply(
                                              name,
                                              new hydra.core.Name("hydra.lib.math.round")),
                                            () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))),
                                            () -> hydra.lib.logic.IfElse.lazy(
                                              hydra.lib.equality.Equal.apply(
                                                name,
                                                new hydra.core.Name("hydra.lib.math.truncate")),
                                              () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))),
                                              () -> hydra.lib.logic.IfElse.lazy(
                                                hydra.lib.equality.Equal.apply(
                                                  name,
                                                  new hydra.core.Name("hydra.lib.math.signum")),
                                                () -> hydra.util.Maybe.just(new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))),
                                                () -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))))))))))))))))))))));
  }
}
