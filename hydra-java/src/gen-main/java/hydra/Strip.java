// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Annotation and type stripping and normalization
 */
public interface Strip {
  static hydra.core.Term deannotateAndDetypeTerm(hydra.core.Term t) {
    return (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return hydra.Strip.deannotateAndDetypeTerm((at).value.body);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
        return hydra.Strip.deannotateAndDetypeTerm((tt).value.body);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
        return hydra.Strip.deannotateAndDetypeTerm((ta).value.body);
      }
    });
  }

  static hydra.core.Term deannotateTerm(hydra.core.Term t) {
    return (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return hydra.Strip.deannotateTerm((at).value.body);
      }
    });
  }

  static hydra.core.Type deannotateType(hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return t;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated arg_) {
        return hydra.Strip.deannotateType((arg_).value.body);
      }
    });
  }

  static hydra.core.Type deannotateTypeParameters(hydra.core.Type t) {
    return hydra.Strip.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return t;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall lt) {
        return hydra.Strip.deannotateTypeParameters((lt).value.body);
      }
    });
  }

  static hydra.core.Type deannotateTypeRecursive(hydra.core.Type typ) {
    return hydra.Rewriting.rewriteType(
      p0 -> p1 -> hydra.Strip.<hydra.core.Type>deannotateTypeRecursive_strip(
        p0,
        p1),
      typ);
  }

  static <T0> hydra.core.Type deannotateTypeRecursive_strip(java.util.function.Function<T0, hydra.core.Type> recurse, T0 typ) {
    hydra.core.Type rewritten = (recurse).apply(typ);
    return (rewritten).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return rewritten;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return (at).value.body;
      }
    });
  }

  static hydra.core.TypeScheme deannotateTypeSchemeRecursive(hydra.core.TypeScheme ts) {
    hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>> constraints = (ts).constraints;
    hydra.core.Type typ = (ts).type;
    hydra.util.ConsList<hydra.core.Name> vars = (ts).variables;
    return new hydra.core.TypeScheme(vars, hydra.Strip.deannotateTypeRecursive(typ), constraints);
  }

  static hydra.core.Term detypeTerm(hydra.core.Term t) {
    return (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> ann = (at).value.annotation;
        hydra.core.Term subj = (at).value.body;
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.Strip.detypeTerm(subj), ann));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
        return hydra.Strip.deannotateAndDetypeTerm((tt).value.body);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
        return hydra.Strip.deannotateAndDetypeTerm((ta).value.body);
      }
    });
  }

  static hydra.core.Term removeTermAnnotations(hydra.core.Term term) {
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> remove = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term2 -> {
      hydra.core.Term rewritten = (recurse).apply(term2);
      return (term2).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return rewritten;
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Annotated at) {
          return (at).value.body;
        }
      });
    }));
    return hydra.Rewriting.rewriteTerm(
      remove,
      term);
  }

  static hydra.core.Type removeTypeAnnotations(hydra.core.Type typ) {
    return hydra.Rewriting.rewriteType(
      p0 -> p1 -> hydra.Strip.<hydra.core.Type>removeTypeAnnotations_remove(
        p0,
        p1),
      typ);
  }

  static hydra.core.Term removeTypeAnnotationsFromTerm(hydra.core.Term term) {
    return hydra.Rewriting.rewriteTerm(
      p0 -> p1 -> hydra.Strip.<hydra.core.Term>removeTypeAnnotationsFromTerm_strip(
        p0,
        p1),
      term);
  }

  static <T0> hydra.core.Term removeTypeAnnotationsFromTerm_strip(java.util.function.Function<T0, hydra.core.Term> recurse, T0 term) {
    hydra.core.Term rewritten = (recurse).apply(term);
    java.util.function.Function<hydra.core.Binding, hydra.core.Binding> stripBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, (b).term, (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())));
    return (rewritten).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return rewritten;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        return new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Map.apply(
          stripBinding,
          (lt).value.bindings), (lt).value.body));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
        return (tt).value.body;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
        return (ta).value.body;
      }
    });
  }

  static <T0> hydra.core.Type removeTypeAnnotations_remove(java.util.function.Function<T0, hydra.core.Type> recurse, T0 typ) {
    hydra.core.Type rewritten = (recurse).apply(typ);
    return (rewritten).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return rewritten;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return (at).value.body;
      }
    });
  }

  static hydra.core.Term removeTypesFromTerm(hydra.core.Term term) {
    return hydra.Rewriting.rewriteTerm(
      p0 -> p1 -> hydra.Strip.<hydra.core.Term>removeTypesFromTerm_strip(
        p0,
        p1),
      term);
  }

  static <T0> hydra.core.Term removeTypesFromTerm_strip(java.util.function.Function<T0, hydra.core.Term> recurse, T0 term) {
    hydra.core.Term rewritten = (recurse).apply(term);
    java.util.function.Function<hydra.core.Binding, hydra.core.Binding> stripBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, (b).term, (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())));
    return (rewritten).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return rewritten;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return new hydra.core.Term.Function((f).value);
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Elimination e) {
            return new hydra.core.Term.Function(new hydra.core.Function.Elimination((e).value));
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda l) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda((l).value.parameter, (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), (l).value.body)));
          }
        });
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        return new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Map.apply(
          stripBinding,
          (lt).value.bindings), (lt).value.body));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
        return (tt).value.body;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
        return (ta).value.body;
      }
    });
  }

  static hydra.core.Term stripTypeLambdas(hydra.core.Term t) {
    return (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> ann = (at).value.annotation;
        hydra.core.Term subj = (at).value.body;
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.Strip.stripTypeLambdas(subj), ann));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
        return hydra.Strip.stripTypeLambdas((ta).value.body);
      }
    });
  }
}
