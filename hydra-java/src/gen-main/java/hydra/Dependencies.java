// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Dependency extraction, binding sort, and let normalization
 */
public interface Dependencies {
  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.core.Binding>> definitionsWithDependencies(hydra.context.Context cx, hydra.graph.Graph graph, java.util.List<hydra.core.Binding> original) {
    java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Name>> depNames = (java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Name>>) (el -> hydra.lib.sets.ToList.apply(hydra.Dependencies.termDependencyNames(
      true,
      false,
      false,
      (el).term)));
    hydra.util.Lazy<java.util.List<hydra.core.Name>> allDepNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Nub.apply(hydra.lib.lists.Concat2.apply(
      hydra.lib.lists.Map.apply(
        projected -> projected.name,
        original),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        depNames,
        original)))));
    return hydra.lib.eithers.MapList.apply(
      (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Binding>>) (name -> hydra.Lexical.requireBinding(
        cx,
        graph,
        name)),
      allDepNames.get());
  }

  static hydra.core.Term flattenLetTerms(hydra.core.Term term) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>>>> flattenBodyLet = new java.util.concurrent.atomic.AtomicReference<>();
    flattenBodyLet.set((java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>>>) (bindings -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>>) (body -> (body).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>) ((hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>) (new hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>(hydra.lib.lists.Concat2.apply(
          (java.util.List<hydra.core.Binding>) (java.util.Collections.<hydra.core.Binding>emptyList()),
          bindings), body)));
      }

      @Override
      public hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term> visit(hydra.core.Term.Let innerLt) {
        java.util.List<hydra.core.Binding> innerBindings = (innerLt).value.bindings;
        hydra.core.Term innerBody = (innerLt).value.body;
        return flattenBodyLet.get().apply(hydra.lib.lists.Concat2.apply(
          bindings,
          innerBindings)).apply(innerBody);
      }
    }))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>>>> rewriteBinding = new java.util.concurrent.atomic.AtomicReference<>();
    rewriteBinding.set((java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>>>) (binding -> {
      hydra.core.Name key0 = (binding).name;
      hydra.util.Maybe<hydra.core.TypeScheme> t = (binding).type;
      hydra.core.Term val0 = (binding).term;
      return (val0).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>>) ((hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>>) (new hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>>(new hydra.core.Binding(key0, val0, t), (java.util.List<hydra.core.Binding>) (java.util.Collections.<hydra.core.Binding>emptyList()))));
        }

        @Override
        public hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>> visit(hydra.core.Term.Annotated at) {
          java.util.Map<hydra.core.Name, hydra.core.Term> ann = (at).value.annotation;
          hydra.core.Term val1 = (at).value.body;
          hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>> recursive = rewriteBinding.get().apply(new hydra.core.Binding(key0, val1, t));
          hydra.util.Lazy<java.util.List<hydra.core.Binding>> deps = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(recursive));
          hydra.util.Lazy<hydra.core.Binding> innerBinding = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(recursive));
          hydra.core.Term val2 = innerBinding.get().term;
          return (hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>>) ((hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>>) (new hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>>(new hydra.core.Binding(key0, new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(val2, ann)), t), deps.get())));
        }

        @Override
        public hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>> visit(hydra.core.Term.Let innerLet) {
          java.util.List<hydra.core.Binding> bindings1 = (innerLet).value.bindings;
          hydra.core.Term body1 = (innerLet).value.body;
          String prefix = hydra.lib.strings.Cat2.apply(
            (key0).value,
            "_");
          java.util.function.Function<hydra.core.Name, hydra.core.Name> qualify = (java.util.function.Function<hydra.core.Name, hydra.core.Name>) (n -> new hydra.core.Name(hydra.lib.strings.Cat2.apply(
            prefix,
            (n).value)));
          java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Name, hydra.core.Name>> toSubstPair = (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Name, hydra.core.Name>>) (b -> (hydra.util.Pair<hydra.core.Name, hydra.core.Name>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Name>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Name>((b).name, (qualify).apply((b).name)))));
          hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Name>> subst = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
            toSubstPair,
            bindings1)));
          java.util.function.Function<hydra.core.Term, hydra.core.Term> replaceVars = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v1 -> hydra.Variables.substituteVariables(
            subst.get(),
            v1));
          java.util.function.Function<hydra.core.Binding, hydra.core.Binding> newBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((qualify).apply((b).name), (replaceVars).apply((b).term), (b).type));
          hydra.core.Term newBody = (replaceVars).apply(body1);
          return (hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>>) ((hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>>) (new hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>>(new hydra.core.Binding(key0, newBody, t), hydra.lib.lists.Map.apply(
            newBinding,
            bindings1))));
        }
      });
    }));
    return hydra.Rewriting.rewriteTerm(
      (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v2 -> hydra.Dependencies.flattenLetTerms_flatten(
        flattenBodyLet.get(),
        rewriteBinding.get(),
        v1,
        v2))),
      term);
  }

  static <T0> hydra.core.Term flattenLetTerms_flatten(java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>>> flattenBodyLet, java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>>> rewriteBinding, java.util.function.Function<T0, hydra.core.Term> recurse, T0 term) {
    hydra.core.Term rewritten = (recurse).apply(term);
    return (rewritten).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return rewritten;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        java.util.List<hydra.core.Binding> bindings = (lt).value.bindings;
        hydra.core.Term body = (lt).value.body;
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> flattenedBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>>) (arg_ -> hydra.Dependencies.flattenLetTerms_forResult((rewriteBinding).apply(arg_))),
          bindings)));
        hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term> merged = (flattenBodyLet).apply(flattenedBindings.get()).apply(body);
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> newBindings = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(merged));
        hydra.util.Lazy<hydra.core.Term> newBody = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(merged));
        return new hydra.core.Term.Let(new hydra.core.Let(newBindings.get(), newBody.get()));
      }
    });
  }

  static <T1> java.util.List<T1> flattenLetTerms_forResult(hydra.util.Pair<T1, java.util.List<T1>> hr) {
    return hydra.lib.lists.Concat2.apply(
      hydra.lib.pairs.Second.apply(hr),
      hydra.lib.lists.Pure.apply(hydra.lib.pairs.First.apply(hr)));
  }

  static hydra.util.Either<String, hydra.core.Type> inlineType(java.util.Map<hydra.core.Name, hydra.core.Type> schema, hydra.core.Type typ) {
    return hydra.Rewriting.rewriteTypeM(
      (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Type>>, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Type>>>) (v1 -> (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Type>>) (v2 -> hydra.Dependencies.inlineType_f(
        (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Type>>>) (p0 -> p1 -> hydra.Dependencies.inlineType(
          p0,
          p1)),
        schema,
        v1,
        v2))),
      typ);
  }

  static <T0> hydra.util.Either<String, hydra.core.Type> inlineType_f(java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Type>>> hydra_dependencies_inlineType, java.util.Map<hydra.core.Name, hydra.core.Type> schema, java.util.function.Function<T0, hydra.util.Either<String, hydra.core.Type>> recurse, T0 typ) {
    java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Type>> afterRecurse = (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Type>>) (tr -> (tr).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<String, hydra.core.Type>right(tr);
      }

      @Override
      public hydra.util.Either<String, hydra.core.Type> visit(hydra.core.Type.Variable v) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.util.Either.<String, hydra.core.Type>left(hydra.lib.strings.Cat2.apply(
            "No such type in schema: ",
            (v).value.value)),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Type>>) (v1 -> (hydra_dependencies_inlineType).apply(schema).apply(v1)),
          hydra.lib.maps.Lookup.apply(
            (v).value,
            schema));
      }
    }));
    return hydra.lib.eithers.Bind.apply(
      (recurse).apply(typ),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Type>>) (tr -> (afterRecurse).apply(tr)));
  }

  static Boolean isLambda(hydra.core.Term term) {
    return hydra.Strip.deannotateTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Term.Function v1) {
        return (v1).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Function instance) {
            return false;
          }

          @Override
          public Boolean visit(hydra.core.Function.Lambda ignored) {
            return true;
          }
        });
      }

      @Override
      public Boolean visit(hydra.core.Term.Let lt) {
        return hydra.Dependencies.isLambda((lt).value.body);
      }
    });
  }

  static hydra.core.Term liftLambdaAboveLet(hydra.core.Term term0) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> rewrite = new java.util.concurrent.atomic.AtomicReference<>();
    rewrite.set((java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> {
      java.util.function.Function<hydra.core.Binding, hydra.core.Binding> rewriteBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, rewrite.get().apply(recurse).apply((b).term), (b).type));
      java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>> rewriteBindings = (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>>) (bs -> hydra.lib.lists.Map.apply(
        rewriteBinding,
        bs));
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>> digForLambdas = new java.util.concurrent.atomic.AtomicReference<>();
      digForLambdas.set((java.util.function.Function<hydra.core.Term, java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>) (original -> (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (cons -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term2 -> (term2).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return (recurse).apply(original);
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Annotated at) {
          return digForLambdas.get().apply(original).apply((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm((cons).apply(t), (at).value.annotation)))).apply((at).value.body);
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Function f) {
          return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.core.Term otherwise(hydra.core.Function instance) {
              return (recurse).apply(original);
            }

            @Override
            public hydra.core.Term visit(hydra.core.Function.Lambda l) {
              return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda((l).value.parameter, (l).value.domain, digForLambdas.get().apply((cons).apply((l).value.body)).apply((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> (cons).apply(t))).apply((l).value.body))));
            }
          });
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Let l) {
          return digForLambdas.get().apply(original).apply((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> (cons).apply(new hydra.core.Term.Let(new hydra.core.Let((rewriteBindings).apply((l).value.bindings), t))))).apply((l).value.body);
        }
      })))));
      return (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return (recurse).apply(term);
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Let l) {
          return digForLambdas.get().apply(term).apply((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Let(new hydra.core.Let((rewriteBindings).apply((l).value.bindings), t)))).apply((l).value.body);
        }
      });
    })));
    return hydra.Rewriting.rewriteTerm(
      rewrite.get(),
      term0);
  }

  static hydra.core.Let pruneLet(hydra.core.Let l) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Term>> bindingMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Name, hydra.core.Term>>) (b -> (hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>((b).name, (b).term)))),
      (l).bindings)));
    hydra.core.Name rootName = new hydra.core.Name("[[[root]]]");
    java.util.function.Function<hydra.core.Name, java.util.Set<hydra.core.Name>> adj = (java.util.function.Function<hydra.core.Name, java.util.Set<hydra.core.Name>>) (n -> hydra.lib.sets.Intersection.apply(
      hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply(bindingMap.get())),
      hydra.Variables.freeVariablesInTerm(hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          n,
          rootName),
        () -> (l).body,
        () -> hydra.lib.maybes.FromJust.apply(hydra.lib.maps.Lookup.apply(
          n,
          bindingMap.get()))))));
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> reachable = new hydra.util.Lazy<>(() -> hydra.Sorting.findReachableNodes(
      adj,
      rootName));
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> prunedBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.sets.Member.apply(
        (b).name,
        reachable.get())),
      (l).bindings));
    return new hydra.core.Let(prunedBindings.get(), (l).body);
  }

  static hydra.core.Type replaceTypedefs(java.util.Map<hydra.core.Name, hydra.core.TypeScheme> types, hydra.core.Type typ0) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>> rewrite = new java.util.concurrent.atomic.AtomicReference<>();
    rewrite.set((java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return (recurse).apply(typ);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(rewrite.get().apply(recurse).apply((at).value.body), (at).value.annotation));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Record ignored) {
        return typ;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Union ignored) {
        return typ;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable v) {
        java.util.function.Function<hydra.core.Type, hydra.core.Type> forMono = (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.core.Type otherwise(hydra.core.Type instance) {
            return rewrite.get().apply(recurse).apply(t);
          }

          @Override
          public hydra.core.Type visit(hydra.core.Type.Record ignored) {
            return typ;
          }

          @Override
          public hydra.core.Type visit(hydra.core.Type.Union ignored) {
            return typ;
          }

          @Override
          public hydra.core.Type visit(hydra.core.Type.Wrap ignored) {
            return typ;
          }
        }));
        java.util.function.Function<hydra.core.TypeScheme, hydra.core.Type> forTypeScheme = (java.util.function.Function<hydra.core.TypeScheme, hydra.core.Type>) (ts -> {
          hydra.core.Type t = (ts).type;
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply((ts).variables),
            () -> (forMono).apply(t),
            () -> typ);
        });
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> typ,
          (java.util.function.Function<hydra.core.TypeScheme, hydra.core.Type>) (ts -> (forTypeScheme).apply(ts)),
          hydra.lib.maps.Lookup.apply(
            (v).value,
            types));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Wrap ignored) {
        return typ;
      }
    }))));
    return hydra.Rewriting.rewriteType(
      rewrite.get(),
      typ0);
  }

  static hydra.core.Term simplifyTerm(hydra.core.Term term) {
    return hydra.Rewriting.rewriteTerm(
      (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v2 -> hydra.Dependencies.simplifyTerm_simplify(
        hydra.Dependencies::simplifyTerm,
        hydra.Strip::deannotateTerm,
        hydra.Variables::freeVariablesInTerm,
        (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>) (p0 -> p1 -> p2 -> hydra.Variables.substituteVariable(
          p0,
          p1,
          p2)),
        v1,
        v2))),
      term);
  }

  static <T0> T0 simplifyTerm_simplify(java.util.function.Function<hydra.core.Term, hydra.core.Term> hydra_dependencies_simplifyTerm, java.util.function.Function<hydra.core.Term, hydra.core.Term> hydra_strip_deannotateTerm, java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>> hydra_variables_freeVariablesInTerm, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> hydra_variables_substituteVariable, java.util.function.Function<hydra.core.Term, T0> recurse, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> forRhs = (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>) (rhs -> (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (var -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (body -> (hydra_strip_deannotateTerm).apply(rhs).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return term;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable v) {
        return (hydra_dependencies_simplifyTerm).apply((hydra_variables_substituteVariable).apply(var).apply((v).value).apply(body));
      }
    }))));
    java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>> forLhs = (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (lhs -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (rhs -> {
      java.util.function.Function<hydra.core.Function, hydra.core.Term> forFun = (java.util.function.Function<hydra.core.Function, hydra.core.Term>) (fun -> (fun).accept(new hydra.core.Function.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Function instance) {
          return term;
        }

        @Override
        public hydra.core.Term visit(hydra.core.Function.Lambda l) {
          hydra.core.Term body = (l).value.body;
          hydra.core.Name var = (l).value.parameter;
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.sets.Member.apply(
              var,
              (hydra_variables_freeVariablesInTerm).apply(body)),
            () -> (forRhs).apply(rhs).apply(var).apply(body),
            () -> (hydra_dependencies_simplifyTerm).apply(body));
        }
      }));
      return (hydra_strip_deannotateTerm).apply(lhs).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return term;
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Function fun) {
          return (forFun).apply((fun).value);
        }
      });
    }));
    java.util.function.Function<hydra.core.Term, hydra.core.Term> forTerm = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return term;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Application app) {
        hydra.core.Term lhs = (app).value.function;
        hydra.core.Term rhs = (app).value.argument;
        return (forLhs).apply(lhs).apply(rhs);
      }
    }));
    hydra.core.Term stripped = (hydra_strip_deannotateTerm).apply(term);
    return (recurse).apply((forTerm).apply(stripped));
  }

  static java.util.Set<hydra.core.Name> termDependencyNames(Boolean binds, Boolean withPrims, Boolean withNoms, hydra.core.Term term0) {
    java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>>> addNames = (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>>>) (names -> (java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>>) (term -> {
      java.util.function.Function<hydra.core.Name, java.util.Set<hydra.core.Name>> nominal = (java.util.function.Function<hydra.core.Name, java.util.Set<hydra.core.Name>>) (name -> hydra.lib.logic.IfElse.lazy(
        withNoms,
        () -> hydra.lib.sets.Insert.apply(
          name,
          names),
        () -> names));
      java.util.function.Function<hydra.core.Name, java.util.Set<hydra.core.Name>> prim = (java.util.function.Function<hydra.core.Name, java.util.Set<hydra.core.Name>>) (name -> hydra.lib.logic.IfElse.lazy(
        withPrims,
        () -> hydra.lib.sets.Insert.apply(
          name,
          names),
        () -> names));
      java.util.function.Function<hydra.core.Name, java.util.Set<hydra.core.Name>> var = (java.util.function.Function<hydra.core.Name, java.util.Set<hydra.core.Name>>) (name -> hydra.lib.logic.IfElse.lazy(
        binds,
        () -> hydra.lib.sets.Insert.apply(
          name,
          names),
        () -> names));
      return (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public java.util.Set<hydra.core.Name> otherwise(hydra.core.Term instance) {
          return names;
        }

        @Override
        public java.util.Set<hydra.core.Name> visit(hydra.core.Term.Function f) {
          return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public java.util.Set<hydra.core.Name> otherwise(hydra.core.Function instance) {
              return names;
            }

            @Override
            public java.util.Set<hydra.core.Name> visit(hydra.core.Function.Elimination e) {
              return (e).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
                @Override
                public java.util.Set<hydra.core.Name> visit(hydra.core.Elimination.Record proj) {
                  return (nominal).apply((proj).value.typeName);
                }

                @Override
                public java.util.Set<hydra.core.Name> visit(hydra.core.Elimination.Union caseStmt) {
                  return (nominal).apply((caseStmt).value.typeName);
                }

                @Override
                public java.util.Set<hydra.core.Name> visit(hydra.core.Elimination.Wrap name) {
                  return (nominal).apply((name).value);
                }
              });
            }
          });
        }

        @Override
        public java.util.Set<hydra.core.Name> visit(hydra.core.Term.Record record) {
          return (nominal).apply((record).value.typeName);
        }

        @Override
        public java.util.Set<hydra.core.Name> visit(hydra.core.Term.Union injection) {
          return (nominal).apply((injection).value.typeName);
        }

        @Override
        public java.util.Set<hydra.core.Name> visit(hydra.core.Term.Variable name) {
          return (var).apply((name).value);
        }

        @Override
        public java.util.Set<hydra.core.Name> visit(hydra.core.Term.Wrap wrappedTerm) {
          return (nominal).apply((wrappedTerm).value.typeName);
        }
      });
    }));
    return hydra.Rewriting.foldOverTerm(
      new hydra.coders.TraversalOrder.Pre(),
      addNames,
      (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      term0);
  }

  static java.util.Map<hydra.core.Name, hydra.core.Name> toShortNames(java.util.List<hydra.core.Name> original) {
    java.util.function.Function<java.util.Map<String, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.core.Name, java.util.Map<String, java.util.Set<hydra.core.Name>>>> addName = (java.util.function.Function<java.util.Map<String, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.core.Name, java.util.Map<String, java.util.Set<hydra.core.Name>>>>) (acc -> (java.util.function.Function<hydra.core.Name, java.util.Map<String, java.util.Set<hydra.core.Name>>>) (name -> {
      String local = hydra.Names.localNameOf(name);
      hydra.util.Lazy<java.util.Set<hydra.core.Name>> group = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
        () -> (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
        hydra.lib.maps.Lookup.apply(
          local,
          acc)));
      return hydra.lib.maps.Insert.apply(
        local,
        hydra.lib.sets.Insert.apply(
          name,
          group.get()),
        acc);
    }));
    java.util.function.Function<java.util.List<hydra.core.Name>, java.util.Map<String, java.util.Set<hydra.core.Name>>> groupNamesByLocal = (java.util.function.Function<java.util.List<hydra.core.Name>, java.util.Map<String, java.util.Set<hydra.core.Name>>>) (names -> hydra.lib.lists.Foldl.apply(
      addName,
      (java.util.Map<String, java.util.Set<hydra.core.Name>>) ((java.util.Map<String, java.util.Set<hydra.core.Name>>) (hydra.lib.maps.Empty.<String, java.util.Set<hydra.core.Name>>apply())),
      names));
    java.util.Map<String, java.util.Set<hydra.core.Name>> groups = (groupNamesByLocal).apply(original);
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      p0 -> hydra.Dependencies.<hydra.core.Name>toShortNames_renameGroup(p0),
      hydra.lib.maps.ToList.apply(groups))));
  }

  static <T0> java.util.Set<T0> toShortNames_names(hydra.util.Pair<String, java.util.Set<T0>> localNames) {
    return hydra.lib.pairs.Second.apply(localNames);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Name> toShortNames_rename(String local, T1 name, Integer i) {
    return (hydra.util.Pair<T1, hydra.core.Name>) ((hydra.util.Pair<T1, hydra.core.Name>) (new hydra.util.Pair<T1, hydra.core.Name>(name, new hydra.core.Name(hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Gt.apply(
        i,
        1),
      () -> hydra.lib.strings.Cat2.apply(
        local,
        hydra.lib.literals.ShowInt32.apply(i)),
      () -> local)))));
  }

  static <T0> java.util.List<hydra.util.Pair<T0, hydra.core.Name>> toShortNames_renameGroup(hydra.util.Pair<String, java.util.Set<T0>> localNames) {
    hydra.util.Lazy<String> local = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(localNames));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Integer, java.util.List<Integer>>> rangeFrom = new java.util.concurrent.atomic.AtomicReference<>();
    rangeFrom.set((java.util.function.Function<Integer, java.util.List<Integer>>) (start -> hydra.lib.lists.Cons.apply(
      start,
      rangeFrom.get().apply(hydra.lib.math.Add.apply(
        start,
        1)))));
    return hydra.lib.lists.ZipWith.apply(
      (java.util.function.Function<T0, java.util.function.Function<Integer, hydra.util.Pair<T0, hydra.core.Name>>>) (v1 -> (java.util.function.Function<Integer, hydra.util.Pair<T0, hydra.core.Name>>) (v2 -> hydra.Dependencies.<T0>toShortNames_rename(
        local.get(),
        v1,
        v2))),
      hydra.lib.sets.ToList.apply(hydra.Dependencies.<T0>toShortNames_names(localNames)),
      rangeFrom.get().apply(1));
  }

  static java.util.List<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>> topologicalSortBindingMap(java.util.Map<hydra.core.Name, hydra.core.Term> bindingMap) {
    hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>> bindings = new hydra.util.Lazy<>(() -> hydra.lib.maps.ToList.apply(bindingMap));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, Boolean>> hasTypeAnnotation = new java.util.concurrent.atomic.AtomicReference<>();
    hasTypeAnnotation.set((java.util.function.Function<hydra.core.Term, Boolean>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Term.Annotated at) {
        return hasTypeAnnotation.get().apply((at).value.body);
      }
    })));
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> keys = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Term>, hydra.core.Name>) ((java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Term>, hydra.core.Name>) (hydra.lib.pairs.First::apply)),
      bindings.get())));
    java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.Term>> toPair = (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.Term>>) (name -> (hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>(name, hydra.lib.maybes.FromMaybe.applyLazy(
      () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_("Impossible!")),
      hydra.lib.maps.Lookup.apply(
        name,
        bindingMap))))));
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<hydra.core.Name>, java.util.List<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>) (v1 -> hydra.lib.lists.Map.apply(
        toPair,
        v1)),
      hydra.Sorting.topologicalSortComponents(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Term>, hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>>) (v1 -> hydra.Dependencies.topologicalSortBindingMap_depsOf(
          hasTypeAnnotation.get(),
          hydra.Variables::freeVariablesInTerm,
          keys.get(),
          v1)),
        bindings.get())));
  }

  static <T0> hydra.util.Pair<T0, java.util.List<hydra.core.Name>> topologicalSortBindingMap_depsOf(java.util.function.Function<hydra.core.Term, Boolean> hasTypeAnnotation, java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>> hydra_variables_freeVariablesInTerm, java.util.Set<hydra.core.Name> keys, hydra.util.Pair<T0, hydra.core.Term> nameAndTerm) {
    hydra.util.Lazy<hydra.core.Term> term = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nameAndTerm));
    return (hydra.util.Pair<T0, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<T0, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<T0, java.util.List<hydra.core.Name>>(hydra.Dependencies.<T0>topologicalSortBindingMap_name(nameAndTerm), hydra.lib.logic.IfElse.lazy(
      (hasTypeAnnotation).apply(term.get()),
      () -> (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()),
      () -> hydra.lib.sets.ToList.apply(hydra.lib.sets.Intersection.apply(
        keys,
        (hydra_variables_freeVariablesInTerm).apply(term.get())))))));
  }

  static <T0> T0 topologicalSortBindingMap_name(hydra.util.Pair<T0, hydra.core.Term> nameAndTerm) {
    return hydra.lib.pairs.First.apply(nameAndTerm);
  }

  static hydra.util.Either<java.util.List<java.util.List<hydra.core.Name>>, java.util.List<hydra.core.Name>> topologicalSortBindings(java.util.List<hydra.core.Binding> els) {
    java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>> adjlist = (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>>) (e -> (hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>((e).name, hydra.lib.sets.ToList.apply(hydra.Dependencies.termDependencyNames(
      false,
      true,
      true,
      (e).term))))));
    return hydra.Sorting.topologicalSort(hydra.lib.lists.Map.apply(
      adjlist,
      els));
  }

  static java.util.List<java.util.List<hydra.packaging.TypeDefinition>> topologicalSortTypeDefinitions(java.util.List<hydra.packaging.TypeDefinition> defs) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.packaging.TypeDefinition>> nameToDef = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.TypeDefinition, hydra.util.Pair<hydra.core.Name, hydra.packaging.TypeDefinition>>) (d -> (hydra.util.Pair<hydra.core.Name, hydra.packaging.TypeDefinition>) ((hydra.util.Pair<hydra.core.Name, hydra.packaging.TypeDefinition>) (new hydra.util.Pair<hydra.core.Name, hydra.packaging.TypeDefinition>((d).name, d)))),
      defs)));
    java.util.function.Function<hydra.packaging.TypeDefinition, hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>> toPair = (java.util.function.Function<hydra.packaging.TypeDefinition, hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>>) (def -> (hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>((def).name, hydra.lib.sets.ToList.apply(hydra.Dependencies.typeDependencyNames(
      false,
      (def).type.type))))));
    hydra.util.Lazy<java.util.List<java.util.List<hydra.core.Name>>> sorted = new hydra.util.Lazy<>(() -> hydra.Sorting.topologicalSortComponents(hydra.lib.lists.Map.apply(
      toPair,
      defs)));
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<hydra.core.Name>, java.util.List<hydra.packaging.TypeDefinition>>) (names -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.packaging.TypeDefinition>>) (n -> hydra.lib.maps.Lookup.apply(
          n,
          nameToDef.get())),
        names))),
      sorted.get());
  }

  static java.util.Set<hydra.core.Name> typeDependencyNames(Boolean withSchema, hydra.core.Type typ) {
    return hydra.lib.logic.IfElse.lazy(
      withSchema,
      () -> hydra.lib.sets.Union.apply(
        hydra.Variables.freeVariablesInType(typ),
        hydra.Dependencies.typeNamesInType(typ)),
      () -> hydra.Variables.freeVariablesInType(typ));
  }

  static <T0> java.util.Set<T0> typeNamesInType(hydra.core.Type typ0) {
    return hydra.Rewriting.<java.util.Set<T0>>foldOverType(
      new hydra.coders.TraversalOrder.Pre(),
      p0 -> p1 -> hydra.Dependencies.<java.util.Set<T0>, hydra.core.Type>typeNamesInType_addNames(
        p0,
        p1),
      (java.util.Set<T0>) (hydra.lib.sets.Empty.<T0>apply()),
      typ0);
  }

  static <T1, T2> T1 typeNamesInType_addNames(T1 names, T2 typ) {
    return names;
  }
}
