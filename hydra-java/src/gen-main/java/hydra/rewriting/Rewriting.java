// Note: this is an automatically generated file. Do not edit.

package hydra.rewriting;

/**
 * Utilities for type and term rewriting and analysis.
 */
public interface Rewriting {
  static hydra.core.Term applyInsideTypeLambdasAndAnnotations(java.util.function.Function<hydra.core.Term, hydra.core.Term> f, hydra.core.Term term0) {
    return (term0).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (f).apply(term0);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.rewriting.Rewriting.applyInsideTypeLambdasAndAnnotations(
          f,
          ((at).value).body), ((at).value).annotation));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
        return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(((tl).value).parameter, hydra.rewriting.Rewriting.applyInsideTypeLambdasAndAnnotations(
          f,
          ((tl).value).body)));
      }
    });
  }
  
  static hydra.core.Term deannotateAndDetypeTerm(hydra.core.Term t) {
    return (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return hydra.rewriting.Rewriting.deannotateAndDetypeTerm(((at).value).body);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
        return hydra.rewriting.Rewriting.deannotateAndDetypeTerm(((tt).value).body);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
        return hydra.rewriting.Rewriting.deannotateAndDetypeTerm(((ta).value).body);
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
        return hydra.rewriting.Rewriting.deannotateTerm(((at).value).body);
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
        return hydra.rewriting.Rewriting.deannotateType(((arg_).value).body);
      }
    });
  }
  
  static hydra.core.Type deannotateTypeParameters(hydra.core.Type t) {
    return (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return t;
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall lt) {
        return hydra.rewriting.Rewriting.deannotateTypeParameters(((lt).value).body);
      }
    });
  }
  
  static hydra.core.Type deannotateTypeRecursive(hydra.core.Type typ) {
    return hydra.rewriting.Rewriting.rewriteType(
      p0 -> p1 -> hydra.rewriting.Rewriting.<hydra.core.Type>deannotateTypeRecursive_strip(
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
        return ((at).value).body;
      }
    });
  }
  
  static hydra.core.TypeScheme deannotateTypeSchemeRecursive(hydra.core.TypeScheme ts) {
    hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> constraints = (ts).constraints;
    hydra.core.Type typ = (ts).type;
    java.util.List<hydra.core.Name> vars = (ts).variables;
    return new hydra.core.TypeScheme(vars, hydra.rewriting.Rewriting.deannotateTypeRecursive(typ), constraints);
  }
  
  static hydra.core.Term detypeTerm(hydra.core.Term t) {
    return (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        java.util.Map<hydra.core.Name, hydra.core.Term> ann = ((at).value).annotation;
        hydra.core.Term subj = ((at).value).body;
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.rewriting.Rewriting.detypeTerm(subj), ann));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
        return hydra.rewriting.Rewriting.deannotateAndDetypeTerm(((tt).value).body);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
        return hydra.rewriting.Rewriting.deannotateAndDetypeTerm(((ta).value).body);
      }
    });
  }
  
  static hydra.core.Term flattenLetTerms(hydra.core.Term term) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>>>> flattenBodyLet = new java.util.concurrent.atomic.AtomicReference<>();
    flattenBodyLet.set((java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>>>) (bindings -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>>) (body -> (body).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>(hydra.lib.lists.Concat2.apply(
          (java.util.List<hydra.core.Binding>) (java.util.List.<hydra.core.Binding>of()),
          bindings), body)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term> visit(hydra.core.Term.Let innerLt) {
        java.util.List<hydra.core.Binding> innerBindings = ((innerLt).value).bindings;
        hydra.core.Term innerBody = ((innerLt).value).body;
        return ((flattenBodyLet.get()).apply(hydra.lib.lists.Concat2.apply(
          bindings,
          innerBindings))).apply(innerBody);
      }
    }))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Binding>>>> rewriteBinding = new java.util.concurrent.atomic.AtomicReference<>();
    rewriteBinding.set((java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Binding>>>) (binding -> {
      hydra.core.Name key0 = (binding).name;
      hydra.util.Maybe<hydra.core.TypeScheme> t = (binding).type;
      hydra.core.Term val0 = (binding).term;
      return (val0).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Binding>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Binding>>) ((hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Binding>>) (new hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Binding>>(new hydra.core.Binding(key0, val0, t), (java.util.List<hydra.core.Binding>) (java.util.List.<hydra.core.Binding>of()))));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Binding>> visit(hydra.core.Term.Annotated at) {
          java.util.Map<hydra.core.Name, hydra.core.Term> ann = ((at).value).annotation;
          hydra.core.Term val1 = ((at).value).body;
          hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Binding>> recursive = (rewriteBinding.get()).apply(new hydra.core.Binding(key0, val1, t));
          hydra.util.Lazy<java.util.List<hydra.core.Binding>> deps = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(recursive));
          hydra.util.Lazy<hydra.core.Binding> innerBinding = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(recursive));
          hydra.core.Term val2 = (innerBinding.get()).term;
          return (hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Binding>>) ((hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Binding>>) (new hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Binding>>(new hydra.core.Binding(key0, new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(val2, ann)), t), deps.get())));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Binding>> visit(hydra.core.Term.Let innerLet) {
          java.util.List<hydra.core.Binding> bindings1 = ((innerLet).value).bindings;
          hydra.core.Term body1 = ((innerLet).value).body;
          String prefix = hydra.lib.strings.Cat2.apply(
            (key0).value,
            "_");
          java.util.function.Function<hydra.core.Name, hydra.core.Name> qualify = (java.util.function.Function<hydra.core.Name, hydra.core.Name>) (n -> new hydra.core.Name(hydra.lib.strings.Cat2.apply(
            prefix,
            (n).value)));
          java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>> toSubstPair = (java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>>) (b -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Name>((b).name, (qualify).apply((b).name)))));
          hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Name>> subst = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
            toSubstPair,
            bindings1)));
          java.util.function.Function<hydra.core.Term, hydra.core.Term> replaceVars = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v1 -> hydra.rewriting.Rewriting.substituteVariables(
            subst.get(),
            v1));
          java.util.function.Function<hydra.core.Binding, hydra.core.Binding> newBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((qualify).apply((b).name), (replaceVars).apply((b).term), (b).type));
          hydra.core.Term newBody = (replaceVars).apply(body1);
          return (hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Binding>>) ((hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Binding>>) (new hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Binding>>(new hydra.core.Binding(key0, newBody, t), hydra.lib.lists.Map.apply(
            newBinding,
            bindings1))));
        }
      });
    }));
    return hydra.rewriting.Rewriting.rewriteTerm(
      (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v2 -> hydra.rewriting.Rewriting.flattenLetTerms_flatten(
        flattenBodyLet.get(),
        rewriteBinding.get(),
        v1,
        v2))),
      term);
  }
  
  static <T0, T1> hydra.core.Term flattenLetTerms_flatten(java.util.function.Function<java.util.List<T0>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>>> flattenBodyLet, java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>> rewriteBinding, java.util.function.Function<T1, hydra.core.Term> recurse, T1 term) {
    hydra.core.Term rewritten = (recurse).apply(term);
    return (rewritten).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return rewritten;
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        java.util.List<hydra.core.Binding> bindings = ((lt).value).bindings;
        hydra.core.Term body = ((lt).value).body;
        hydra.util.Lazy<java.util.List<T0>> flattenedBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, java.util.List<T0>>) (arg_ -> hydra.rewriting.Rewriting.<T0>flattenLetTerms_forResult((rewriteBinding).apply(arg_))),
          bindings)));
        hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term> merged = ((flattenBodyLet).apply(flattenedBindings.get())).apply(body);
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> newBindings = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(merged));
        hydra.util.Lazy<hydra.core.Term> newBody = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(merged));
        return new hydra.core.Term.Let(new hydra.core.Let(newBindings.get(), newBody.get()));
      }
    });
  }
  
  static <T0> java.util.List<T0> flattenLetTerms_forResult(hydra.util.Tuple.Tuple2<T0, java.util.List<T0>> hr) {
    return hydra.lib.lists.Concat2.apply(
      hydra.lib.pairs.Second.apply(hr),
      hydra.lib.lists.Pure.apply(hydra.lib.pairs.First.apply(hr)));
  }
  
  static <T0> T0 foldOverTerm(hydra.coders.TraversalOrder order, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T0>> fld, T0 b0, hydra.core.Term term) {
    return (order).accept(new hydra.coders.TraversalOrder.PartialVisitor<>() {
      @Override
      public T0 visit(hydra.coders.TraversalOrder.Pre ignored) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T0>>) (v1 -> (java.util.function.Function<hydra.core.Term, T0>) (v2 -> hydra.rewriting.Rewriting.<T0>foldOverTerm(
            order,
            fld,
            v1,
            v2))),
          ((fld).apply(b0)).apply(term),
          hydra.rewriting.Rewriting.subterms(term));
      }
      
      @Override
      public T0 visit(hydra.coders.TraversalOrder.Post ignored) {
        return ((fld).apply(hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T0>>) (v1 -> (java.util.function.Function<hydra.core.Term, T0>) (v2 -> hydra.rewriting.Rewriting.<T0>foldOverTerm(
            order,
            fld,
            v1,
            v2))),
          b0,
          hydra.rewriting.Rewriting.subterms(term)))).apply(term);
      }
    });
  }
  
  static <T0> T0 foldOverType(hydra.coders.TraversalOrder order, java.util.function.Function<T0, java.util.function.Function<hydra.core.Type, T0>> fld, T0 b0, hydra.core.Type typ) {
    return (order).accept(new hydra.coders.TraversalOrder.PartialVisitor<>() {
      @Override
      public T0 visit(hydra.coders.TraversalOrder.Pre ignored) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<T0, java.util.function.Function<hydra.core.Type, T0>>) (v1 -> (java.util.function.Function<hydra.core.Type, T0>) (v2 -> hydra.rewriting.Rewriting.<T0>foldOverType(
            order,
            fld,
            v1,
            v2))),
          ((fld).apply(b0)).apply(typ),
          hydra.rewriting.Rewriting.subtypes(typ));
      }
      
      @Override
      public T0 visit(hydra.coders.TraversalOrder.Post ignored) {
        return ((fld).apply(hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<T0, java.util.function.Function<hydra.core.Type, T0>>) (v1 -> (java.util.function.Function<hydra.core.Type, T0>) (v2 -> hydra.rewriting.Rewriting.<T0>foldOverType(
            order,
            fld,
            v1,
            v2))),
          b0,
          hydra.rewriting.Rewriting.subtypes(typ)))).apply(typ);
      }
    });
  }
  
  static java.util.Set<hydra.core.Name> freeTypeVariablesInTerm(hydra.core.Term term0) {
    java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>> tryType = (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>>) (tvars -> (java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>) (typ -> hydra.lib.sets.Difference.apply(
      hydra.rewriting.Rewriting.freeVariablesInType(typ),
      tvars)));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>>>> getAll = new java.util.concurrent.atomic.AtomicReference<>();
    getAll.set((java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>>>) (vars -> (java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>>) (term -> {
      java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>> recurse = (java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>>) (v1 -> ((getAll.get()).apply(vars)).apply(v1));
      hydra.util.Lazy<java.util.Set<hydra.core.Name>> dflt = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.freeTypeVariablesInTerm_allOf(hydra.lib.lists.Map.apply(
        recurse,
        hydra.rewriting.Rewriting.subterms(term))));
      return (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public java.util.Set<hydra.core.Name> otherwise(hydra.core.Term instance) {
          return dflt.get();
        }
        
        @Override
        public java.util.Set<hydra.core.Name> visit(hydra.core.Term.Function f) {
          return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public java.util.Set<hydra.core.Name> otherwise(hydra.core.Function instance) {
              return dflt.get();
            }
            
            @Override
            public java.util.Set<hydra.core.Name> visit(hydra.core.Function.Elimination e) {
              return dflt.get();
            }
            
            @Override
            public java.util.Set<hydra.core.Name> visit(hydra.core.Function.Lambda l) {
              hydra.util.Lazy<java.util.Set<hydra.core.Name>> domt = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.apply(
                (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                (java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>) (v1 -> ((tryType).apply(vars)).apply(v1)),
                ((l).value).domain));
              return hydra.lib.sets.Union.apply(
                domt.get(),
                (recurse).apply(((l).value).body));
            }
          });
        }
        
        @Override
        public java.util.Set<hydra.core.Name> visit(hydra.core.Term.Let l) {
          java.util.function.Function<hydra.core.Binding, java.util.Set<hydra.core.Name>> forBinding = (java.util.function.Function<hydra.core.Binding, java.util.Set<hydra.core.Name>>) (b -> {
            hydra.util.Lazy<java.util.Set<hydra.core.Name>> newVars = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.apply(
              vars,
              (java.util.function.Function<hydra.core.TypeScheme, java.util.Set<hydra.core.Name>>) (ts -> hydra.lib.sets.Union.apply(
                vars,
                hydra.lib.sets.FromList.apply((ts).variables))),
              (b).type));
            return hydra.lib.sets.Union.apply(
              ((getAll.get()).apply(newVars.get())).apply((b).term),
              hydra.lib.maybes.Maybe.apply(
                (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                (java.util.function.Function<hydra.core.TypeScheme, java.util.Set<hydra.core.Name>>) (ts -> ((tryType).apply(newVars.get())).apply((ts).type)),
                (b).type));
          });
          return hydra.lib.sets.Union.apply(
            hydra.rewriting.Rewriting.freeTypeVariablesInTerm_allOf(hydra.lib.lists.Map.apply(
              forBinding,
              ((l).value).bindings)),
            (recurse).apply(((l).value).body));
        }
        
        @Override
        public java.util.Set<hydra.core.Name> visit(hydra.core.Term.TypeApplication tt) {
          return hydra.lib.sets.Union.apply(
            ((tryType).apply(vars)).apply(((tt).value).type),
            (recurse).apply(((tt).value).body));
        }
        
        @Override
        public java.util.Set<hydra.core.Name> visit(hydra.core.Term.TypeLambda tl) {
          return hydra.lib.sets.Union.apply(
            ((tryType).apply(vars)).apply(new hydra.core.Type.Variable(((tl).value).parameter)),
            (recurse).apply(((tl).value).body));
        }
      });
    })));
    return ((getAll.get()).apply((java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()))).apply(term0);
  }
  
  static <T0> java.util.Set<T0> freeTypeVariablesInTerm_allOf(java.util.List<java.util.Set<T0>> sets) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Set<T0>, java.util.function.Function<java.util.Set<T0>, java.util.Set<T0>>>) ((java.util.function.Function<java.util.Set<T0>, java.util.function.Function<java.util.Set<T0>, java.util.Set<T0>>>) (p0 -> p1 -> hydra.lib.sets.Union.apply(
        p0,
        p1))),
      (java.util.Set<T0>) (hydra.lib.sets.Empty.<T0>apply()),
      sets);
  }
  
  static java.util.Set<hydra.core.Name> freeVariablesInTerm(hydra.core.Term term) {
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> dfltVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>>>) (s -> (java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>>) (t -> hydra.lib.sets.Union.apply(
        s,
        hydra.rewriting.Rewriting.freeVariablesInTerm(t)))),
      (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      hydra.rewriting.Rewriting.subterms(term)));
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public java.util.Set<hydra.core.Name> otherwise(hydra.core.Term instance) {
        return dfltVars.get();
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Term.Function v1) {
        return ((v1).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public java.util.Set<hydra.core.Name> otherwise(hydra.core.Function instance) {
            return dfltVars.get();
          }
          
          @Override
          public java.util.Set<hydra.core.Name> visit(hydra.core.Function.Lambda l) {
            return hydra.lib.sets.Delete.apply(
              ((l).value).parameter,
              hydra.rewriting.Rewriting.freeVariablesInTerm(((l).value).body));
          }
        });
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Term.Let l) {
        return hydra.lib.sets.Difference.apply(
          dfltVars.get(),
          hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
            projected -> projected.name,
            ((l).value).bindings)));
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Term.Variable v) {
        return hydra.lib.sets.Singleton.apply((v).value);
      }
    });
  }
  
  static java.util.Set<hydra.core.Name> freeVariablesInType(hydra.core.Type typ) {
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> dfltVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>>) (s -> (java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>) (t -> hydra.lib.sets.Union.apply(
        s,
        hydra.rewriting.Rewriting.freeVariablesInType(t)))),
      (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      hydra.rewriting.Rewriting.subtypes(typ)));
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.Set<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return dfltVars.get();
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Forall lt) {
        return hydra.lib.sets.Delete.apply(
          ((lt).value).parameter,
          hydra.rewriting.Rewriting.freeVariablesInType(((lt).value).body));
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Variable v) {
        return hydra.lib.sets.Singleton.apply((v).value);
      }
    });
  }
  
  static java.util.List<hydra.core.Name> freeVariablesInTypeOrdered(hydra.core.Type typ) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Name>>>> collectVars = new java.util.concurrent.atomic.AtomicReference<>();
    collectVars.set((java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Name>>>) (boundVars -> (java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Name>>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Name>>) (v1 -> ((collectVars.get()).apply(boundVars)).apply(v1)),
          hydra.rewriting.Rewriting.subtypes(t)));
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Variable v) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.sets.Member.apply(
            (v).value,
            boundVars),
          () -> (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()),
          () -> java.util.List.of((v).value));
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return ((collectVars.get()).apply(hydra.lib.sets.Insert.apply(
          ((ft).value).parameter,
          boundVars))).apply(((ft).value).body);
      }
    }))));
    return hydra.lib.lists.Nub.apply(((collectVars.get()).apply((java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()))).apply(typ));
  }
  
  static java.util.Set<hydra.core.Name> freeVariablesInTypeSchemeSimple(hydra.core.TypeScheme ts) {
    hydra.core.Type t = (ts).type;
    java.util.List<hydra.core.Name> vars = (ts).variables;
    return hydra.lib.sets.Difference.apply(
      hydra.rewriting.Rewriting.freeVariablesInTypeSimple(t),
      hydra.lib.sets.FromList.apply(vars));
  }
  
  static java.util.Set<hydra.core.Name> freeVariablesInTypeScheme(hydra.core.TypeScheme ts) {
    hydra.core.Type t = (ts).type;
    java.util.List<hydra.core.Name> vars = (ts).variables;
    return hydra.lib.sets.Difference.apply(
      hydra.rewriting.Rewriting.freeVariablesInType(t),
      hydra.lib.sets.FromList.apply(vars));
  }
  
  static java.util.Set<hydra.core.Name> freeVariablesInTypeSimple(hydra.core.Type typ) {
    java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>> helper = (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>>) (types2 -> (java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>) (typ2 -> (typ2).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.Set<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return types2;
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Variable v) {
        return hydra.lib.sets.Insert.apply(
          (v).value,
          types2);
      }
    })));
    return hydra.rewriting.Rewriting.foldOverType(
      new hydra.coders.TraversalOrder.Pre(),
      helper,
      (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      typ);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> inlineType(java.util.Map<hydra.core.Name, hydra.core.Type> schema, hydra.core.Type typ) {
    return hydra.rewriting.Rewriting.<T0>rewriteTypeM(
      (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>>) (v1 -> (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (v2 -> hydra.rewriting.Rewriting.<T0>inlineType_f(
        schema,
        v1,
        v2))),
      typ);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> inlineType_f(java.util.Map<hydra.core.Name, hydra.core.Type> schema, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>> recurse, hydra.core.Type typ) {
    return hydra.lib.flows.Bind.apply(
      (recurse).apply(typ),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (tr -> hydra.rewriting.Rewriting.<T0>inlineType_afterRecurse(
        schema,
        tr)));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> inlineType_afterRecurse(java.util.Map<hydra.core.Name, hydra.core.Type> schema, hydra.core.Type tr) {
    return (tr).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.lib.flows.Pure.apply(tr);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Variable v) {
        return hydra.lib.maybes.Maybe.apply(
          hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
            "No such type in schema: ",
            ((v).value).value)),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (v1 -> hydra.rewriting.Rewriting.<T0>inlineType(
            schema,
            v1)),
          hydra.lib.maps.Lookup.apply(
            (v).value,
            schema));
      }
    });
  }
  
  static Boolean isFreeVariableInTerm(hydra.core.Name v, hydra.core.Term term) {
    return hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
      v,
      hydra.rewriting.Rewriting.freeVariablesInTerm(term)));
  }
  
  static Boolean isLambda(hydra.core.Term term) {
    return (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Function v1) {
        return ((v1).value).accept(new hydra.core.Function.PartialVisitor<>() {
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
        return hydra.rewriting.Rewriting.isLambda(((lt).value).body);
      }
    });
  }
  
  static hydra.core.Term liftLambdaAboveLet(hydra.core.Term term0) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> rewrite = new java.util.concurrent.atomic.AtomicReference<>();
    rewrite.set((java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> {
      java.util.function.Function<hydra.core.Binding, hydra.core.Binding> rewriteBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, ((rewrite.get()).apply(recurse)).apply((b).term), (b).type));
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
          return (((digForLambdas.get()).apply(original)).apply((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm((cons).apply(t), ((at).value).annotation))))).apply(((at).value).body);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Function f) {
          return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.core.Term otherwise(hydra.core.Function instance) {
              return (recurse).apply(original);
            }
            
            @Override
            public hydra.core.Term visit(hydra.core.Function.Lambda l) {
              return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(((l).value).parameter, ((l).value).domain, (((digForLambdas.get()).apply((cons).apply(((l).value).body))).apply((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> (cons).apply(t)))).apply(((l).value).body))));
            }
          });
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Let l) {
          return (((digForLambdas.get()).apply(original)).apply((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> (cons).apply(new hydra.core.Term.Let(new hydra.core.Let((rewriteBindings).apply(((l).value).bindings), t)))))).apply(((l).value).body);
        }
      })))));
      return (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return (recurse).apply(term);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Let l) {
          return (((digForLambdas.get()).apply(term)).apply((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Let(new hydra.core.Let((rewriteBindings).apply(((l).value).bindings), t))))).apply(((l).value).body);
        }
      });
    })));
    return hydra.rewriting.Rewriting.rewriteTerm(
      rewrite.get(),
      term0);
  }
  
  static hydra.core.Type mapBeneathTypeAnnotations(java.util.function.Function<hydra.core.Type, hydra.core.Type> f, hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return (f).apply(t);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(hydra.rewriting.Rewriting.mapBeneathTypeAnnotations(
          f,
          ((at).value).body), ((at).value).annotation));
      }
    });
  }
  
  static hydra.core.Term normalizeTypeVariablesInTerm(hydra.core.Term term) {
    java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.Type>> substType = (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (subst -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> {
      java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>> rewrite = (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ2 -> (typ2).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.core.Type otherwise(hydra.core.Type instance) {
          return (recurse).apply(typ2);
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Variable v) {
          return new hydra.core.Type.Variable(hydra.rewriting.Rewriting.normalizeTypeVariablesInTerm_replaceName(
            subst,
            (v).value));
        }
      })));
      return hydra.rewriting.Rewriting.rewriteType(
        rewrite,
        typ);
    }));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> rewriteWithSubst = new java.util.concurrent.atomic.AtomicReference<>();
    rewriteWithSubst.set((java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (state -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term0 -> {
      hydra.util.Lazy<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>> sb = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(state));
      hydra.util.Lazy<java.util.Set<hydra.core.Name>> boundVars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(sb.get()));
      hydra.util.Lazy<Integer> next = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(state));
      hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Name>> subst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(sb.get()));
      java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> rewrite = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term2 -> (term2).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return (recurse).apply(term2);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Function v1) {
          return ((v1).value).accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.core.Term otherwise(hydra.core.Function instance) {
              return (recurse).apply(term2);
            }
            
            @Override
            public hydra.core.Term visit(hydra.core.Function.Elimination ignored) {
              return (recurse).apply(term2);
            }
            
            @Override
            public hydra.core.Term visit(hydra.core.Function.Lambda l) {
              hydra.util.Maybe<hydra.core.Type> domain = ((l).value).domain;
              return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(((l).value).parameter, hydra.lib.maybes.Map.apply(
                (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (v12 -> ((substType).apply(subst.get())).apply(v12)),
                domain), ((rewriteWithSubst.get()).apply((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>(subst.get(), boundVars.get()))), next.get()))))).apply(((l).value).body))));
            }
          });
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Let lt) {
          java.util.List<hydra.core.Binding> bindings0 = ((lt).value).bindings;
          java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>>>> step = new java.util.concurrent.atomic.AtomicReference<>();
          step.set((java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>>>) (acc -> (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>>) (bs -> {
            hydra.util.Lazy<hydra.core.Binding> b = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(bs));
            hydra.util.Lazy<hydra.core.Term> newVal = new hydra.util.Lazy<>(() -> ((rewriteWithSubst.get()).apply((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>(subst.get(), boundVars.get()))), next.get()))))).apply((b.get()).term));
            hydra.util.Lazy<java.util.List<hydra.core.Binding>> tl = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(bs));
            hydra.util.Lazy<java.util.List<hydra.core.Binding>> noType = new hydra.util.Lazy<>(() -> ((java.util.function.Supplier<java.util.List<hydra.core.Binding>>) (() -> {
              hydra.util.Lazy<hydra.core.Binding> b1 = new hydra.util.Lazy<>(() -> new hydra.core.Binding((b.get()).name, newVal.get(), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())));
              return ((step.get()).apply(hydra.lib.lists.Cons.apply(
                b1.get(),
                acc))).apply(tl.get());
            })).get());
            java.util.function.Function<hydra.core.TypeScheme, java.util.List<hydra.core.Binding>> withType = (java.util.function.Function<hydra.core.TypeScheme, java.util.List<hydra.core.Binding>>) (ts -> {
              java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Integer, java.util.function.Function<Integer, java.util.function.Function<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>>>> gen = new java.util.concurrent.atomic.AtomicReference<>();
              gen.set((java.util.function.Function<Integer, java.util.function.Function<Integer, java.util.function.Function<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>>>) (i -> (java.util.function.Function<Integer, java.util.function.Function<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>>) (rem -> (java.util.function.Function<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>) (acc2 -> {
                hydra.core.Name ti = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
                  "t",
                  hydra.lib.literals.ShowInt32.apply(hydra.lib.math.Add.apply(
                    next.get(),
                    i))));
                return hydra.lib.logic.IfElse.lazy(
                  hydra.lib.equality.Equal.apply(
                    rem,
                    0),
                  () -> hydra.lib.lists.Reverse.apply(acc2),
                  () -> (((gen.get()).apply(hydra.lib.math.Add.apply(
                    i,
                    1))).apply(hydra.lib.math.Sub.apply(
                    rem,
                    1))).apply(hydra.lib.lists.Cons.apply(
                    ti,
                    acc2)));
              }))));
              java.util.List<hydra.core.Name> vars = (ts).variables;
              hydra.util.Lazy<Integer> k = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(vars));
              hydra.util.Lazy<java.util.List<hydra.core.Name>> newVars = new hydra.util.Lazy<>(() -> (((gen.get()).apply(0)).apply(k.get())).apply((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of())));
              hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Name>> newSubst = new hydra.util.Lazy<>(() -> hydra.lib.maps.Union.apply(
                hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
                  vars,
                  newVars.get())),
                subst.get()));
              hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> oldConstraints = (ts).constraints;
              hydra.util.Lazy<hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>> newConstraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
                (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (v1 -> hydra.rewriting.Rewriting.normalizeTypeVariablesInTerm_renameConstraintKeys(
                  newSubst.get(),
                  v1)),
                oldConstraints));
              hydra.util.Lazy<java.util.Set<hydra.core.Name>> newBound = new hydra.util.Lazy<>(() -> hydra.lib.sets.Union.apply(
                boundVars.get(),
                hydra.lib.sets.FromList.apply(newVars.get())));
              hydra.util.Lazy<hydra.core.Term> newVal2 = new hydra.util.Lazy<>(() -> ((rewriteWithSubst.get()).apply((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>(newSubst.get(), newBound.get()))), hydra.lib.math.Add.apply(
                next.get(),
                k.get())))))).apply((b.get()).term));
              hydra.core.Type typ = (ts).type;
              hydra.core.Binding b1 = new hydra.core.Binding((b.get()).name, newVal2.get(), hydra.util.Maybe.just(new hydra.core.TypeScheme(newVars.get(), ((substType).apply(newSubst.get())).apply(typ), newConstraints.get())));
              return ((step.get()).apply(hydra.lib.lists.Cons.apply(
                b1,
                acc))).apply(tl.get());
            });
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply(bs),
              () -> hydra.lib.lists.Reverse.apply(acc),
              () -> hydra.lib.maybes.Maybe.apply(
                noType.get(),
                (java.util.function.Function<hydra.core.TypeScheme, java.util.List<hydra.core.Binding>>) (ts -> (withType).apply(ts)),
                (b.get()).type));
          })));
          hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindings1 = new hydra.util.Lazy<>(() -> ((step.get()).apply((java.util.List<hydra.core.Binding>) (java.util.List.<hydra.core.Binding>of()))).apply(bindings0));
          hydra.core.Term body0 = ((lt).value).body;
          return new hydra.core.Term.Let(new hydra.core.Let(bindings1.get(), ((rewriteWithSubst.get()).apply((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>(subst.get(), boundVars.get()))), next.get()))))).apply(body0)));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
          return new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(((rewriteWithSubst.get()).apply((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>(subst.get(), boundVars.get()))), next.get()))))).apply(((tt).value).body), ((substType).apply(subst.get())).apply(((tt).value).type)));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
          return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(hydra.rewriting.Rewriting.normalizeTypeVariablesInTerm_replaceName(
            subst.get(),
            ((ta).value).parameter), ((rewriteWithSubst.get()).apply((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>(subst.get(), boundVars.get()))), next.get()))))).apply(((ta).value).body)));
        }
      })));
      return hydra.rewriting.Rewriting.rewriteTerm(
        rewrite,
        term0);
    })));
    return ((rewriteWithSubst.get()).apply((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>, Integer>((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.Set<hydra.core.Name>>((java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())))), 0))))).apply(term);
  }
  
  static <T0> T0 normalizeTypeVariablesInTerm_replaceName(java.util.Map<T0, T0> subst, T0 v) {
    return hydra.lib.maybes.FromMaybe.apply(
      v,
      hydra.lib.maps.Lookup.apply(
        v,
        subst));
  }
  
  static <T0, T1> java.util.Map<T0, T1> normalizeTypeVariablesInTerm_renameConstraintKeys(java.util.Map<T0, T0> newSubst, java.util.Map<T0, T1> constraintMap) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, hydra.util.Tuple.Tuple2<T0, T1>>) (p -> {
        hydra.util.Lazy<T0> oldName = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
        hydra.util.Lazy<T0> newName = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.apply(
          oldName.get(),
          hydra.lib.maps.Lookup.apply(
            oldName.get(),
            newSubst)));
        return (hydra.util.Tuple.Tuple2<T0, T1>) ((hydra.util.Tuple.Tuple2<T0, T1>) (new hydra.util.Tuple.Tuple2<T0, T1>(newName.get(), hydra.rewriting.Rewriting.<T0, T1>normalizeTypeVariablesInTerm_meta(p))));
      }),
      hydra.lib.maps.ToList.apply(constraintMap)));
  }
  
  static <T0, T1> T1 normalizeTypeVariablesInTerm_meta(hydra.util.Tuple.Tuple2<T0, T1> p) {
    return hydra.lib.pairs.Second.apply(p);
  }
  
  static hydra.core.Let pruneLet(hydra.core.Let l) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Term>> bindingMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>) (b -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>((b).name, (b).term)))),
      (l).bindings)));
    hydra.core.Name rootName = new hydra.core.Name("[[[root]]]");
    java.util.function.Function<hydra.core.Name, java.util.Set<hydra.core.Name>> adj = (java.util.function.Function<hydra.core.Name, java.util.Set<hydra.core.Name>>) (n -> hydra.lib.sets.Intersection.apply(
      hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply(bindingMap.get())),
      hydra.rewriting.Rewriting.freeVariablesInTerm(hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          n,
          rootName),
        () -> (l).body,
        () -> hydra.lib.maybes.FromJust.apply(hydra.lib.maps.Lookup.apply(
          n,
          bindingMap.get()))))));
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> reachable = new hydra.util.Lazy<>(() -> hydra.sorting.Sorting.findReachableNodes(
      adj,
      rootName));
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> prunedBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.sets.Member.apply(
        (b).name,
        reachable.get())),
      (l).bindings));
    return new hydra.core.Let(prunedBindings.get(), (l).body);
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
          return ((at).value).body;
        }
      });
    }));
    return hydra.rewriting.Rewriting.rewriteTerm(
      remove,
      term);
  }
  
  static hydra.core.Type removeTypeAnnotations(hydra.core.Type typ) {
    return hydra.rewriting.Rewriting.rewriteType(
      p0 -> p1 -> hydra.rewriting.Rewriting.<hydra.core.Type>removeTypeAnnotations_remove(
        p0,
        p1),
      typ);
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
        return ((at).value).body;
      }
    });
  }
  
  static hydra.core.Term removeTypeAnnotationsFromTerm(hydra.core.Term term) {
    return hydra.rewriting.Rewriting.rewriteTerm(
      p0 -> p1 -> hydra.rewriting.Rewriting.<hydra.core.Term>removeTypeAnnotationsFromTerm_strip(
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
          ((lt).value).bindings), ((lt).value).body));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
        return ((tt).value).body;
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
        return ((ta).value).body;
      }
    });
  }
  
  static hydra.core.Term removeTypesFromTerm(hydra.core.Term term) {
    return hydra.rewriting.Rewriting.rewriteTerm(
      p0 -> p1 -> hydra.rewriting.Rewriting.<hydra.core.Term>removeTypesFromTerm_strip(
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
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
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
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(((l).value).parameter, (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), ((l).value).body)));
          }
        });
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        return new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Map.apply(
          stripBinding,
          ((lt).value).bindings), ((lt).value).body));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
        return ((tt).value).body;
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
        return ((ta).value).body;
      }
    });
  }
  
  static hydra.core.Term replaceFreeTermVariable(hydra.core.Name vold, hydra.core.Term tnew, hydra.core.Term term) {
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> rewrite = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (recurse).apply(t);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return (recurse).apply(t);
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda l) {
            hydra.core.Name v = ((l).value).parameter;
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                v,
                vold),
              () -> t,
              () -> (recurse).apply(t));
          }
        });
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable v) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (v).value,
            vold),
          () -> tnew,
          () -> new hydra.core.Term.Variable((v).value));
      }
    })));
    return hydra.rewriting.Rewriting.rewriteTerm(
      rewrite,
      term);
  }
  
  static hydra.core.Type replaceFreeTypeVariable(hydra.core.Name v, hydra.core.Type rep, hydra.core.Type typ) {
    java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>> mapExpr = (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return (recurse).apply(t);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            v,
            ((ft).value).parameter),
          () -> t,
          () -> new hydra.core.Type.Forall(new hydra.core.ForallType(((ft).value).parameter, (recurse).apply(((ft).value).body))));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable v_) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            v,
            (v_).value),
          () -> rep,
          () -> t);
      }
    })));
    return hydra.rewriting.Rewriting.rewriteType(
      mapExpr,
      typ);
  }
  
  static hydra.core.Type replaceTypedefs(java.util.Map<hydra.core.Name, hydra.core.TypeScheme> types2, hydra.core.Type typ0) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>> rewrite = new java.util.concurrent.atomic.AtomicReference<>();
    rewrite.set((java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> {
      hydra.core.Type dflt = (recurse).apply(typ);
      return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.core.Type otherwise(hydra.core.Type instance) {
          return dflt;
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Annotated at) {
          return new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(((rewrite.get()).apply(recurse)).apply(((at).value).body), ((at).value).annotation));
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
              return ((rewrite.get()).apply(recurse)).apply(t);
            }
            
            @Override
            public hydra.core.Type visit(hydra.core.Type.Record ignored) {
              return dflt;
            }
            
            @Override
            public hydra.core.Type visit(hydra.core.Type.Union ignored) {
              return dflt;
            }
            
            @Override
            public hydra.core.Type visit(hydra.core.Type.Wrap ignored) {
              return dflt;
            }
          }));
          java.util.function.Function<hydra.core.TypeScheme, hydra.core.Type> forTypeScheme = (java.util.function.Function<hydra.core.TypeScheme, hydra.core.Type>) (ts -> {
            hydra.core.Type t = (ts).type;
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply((ts).variables),
              () -> (forMono).apply(t),
              () -> dflt);
          });
          return hydra.lib.maybes.Maybe.apply(
            dflt,
            (java.util.function.Function<hydra.core.TypeScheme, hydra.core.Type>) (ts -> (forTypeScheme).apply(ts)),
            hydra.lib.maps.Lookup.apply(
              (v).value,
              types2));
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Wrap ignored) {
          return typ;
        }
      });
    })));
    return hydra.rewriting.Rewriting.rewriteType(
      rewrite.get(),
      typ0);
  }
  
  static <T0> hydra.util.Tuple.Tuple2<T0, hydra.core.Term> rewriteAndFoldTerm(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>> f, T0 term0, hydra.core.Term v1) {
    return hydra.rewriting.Rewriting.<T0>rewriteAndFoldTerm_recurse(
      f,
      term0,
      v1);
  }
  
  static <T0> hydra.util.Tuple.Tuple2<T0, hydra.core.Term> rewriteAndFoldTerm_fsub(java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>> recurse, T0 val0, hydra.core.Term term0) {
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> dflt = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_dflt(
      term0,
      val0));
    java.util.function.Function<T0, java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<T0, hydra.core.Field>>> forField = (java.util.function.Function<T0, java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<T0, hydra.core.Field>>>) (v1 -> (java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<T0, hydra.core.Field>>) (v2 -> hydra.rewriting.Rewriting.<T0, T0>rewriteAndFoldTerm_forField(
      recurse,
      v1,
      v2)));
    return (term0).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return dflt.get();
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Annotated at) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTerm_forSingle(
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(t, ((at).value).annotation))),
          val0,
          ((at).value).body);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Application a) {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rlhs = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_rlhs(
          (a).value,
          recurse,
          val0));
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rrhs = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_rrhs(
          (a).value,
          recurse,
          rlhs.get()));
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(rrhs.get()), new hydra.core.Term.Application(new hydra.core.Application(hydra.lib.pairs.Second.apply(rlhs.get()), hydra.lib.pairs.Second.apply(rrhs.get()))))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (l -> {
            hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rl = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_rl(
              l,
              recurse,
              val0));
            return (hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(rl.get()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(hydra.lib.pairs.Second.apply(rl.get()))))))));
          }),
          (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (r -> {
            hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rr = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_rr(
              r,
              recurse,
              val0));
            return (hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(rr.get()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(hydra.lib.pairs.Second.apply(rr.get()))))))));
          }),
          (e).value);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Function f) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTerm_forSingle(
          (java.util.function.Function<T0, java.util.function.Function<hydra.core.Function, hydra.util.Tuple.Tuple2<T0, hydra.core.Function>>>) (v1 -> (java.util.function.Function<hydra.core.Function, hydra.util.Tuple.Tuple2<T0, hydra.core.Function>>) (v2 -> hydra.rewriting.Rewriting.<T0>rewriteAndFoldTerm_forFunction(
            (java.util.function.Function<T0, java.util.function.Function<hydra.core.Elimination, hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>>>) (v12 -> (java.util.function.Function<hydra.core.Elimination, hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>>) (v22 -> hydra.rewriting.Rewriting.<T0>rewriteAndFoldTerm_forElimination(
              (java.util.function.Function<T0, java.util.function.Function<java.util.List<hydra.core.Field>, hydra.util.Tuple.Tuple2<T0, java.util.List<hydra.core.Field>>>>) (v13 -> (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.util.Tuple.Tuple2<T0, java.util.List<hydra.core.Field>>>) (v23 -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_forFields(
                forField,
                v13,
                v23))),
              recurse,
              v12,
              v22))),
            recurse,
            v1,
            v2))),
          (java.util.function.Function<hydra.core.Function, hydra.core.Term>) (f2 -> new hydra.core.Term.Function(f2)),
          val0,
          (f).value);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Let l) {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> renv = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_renv(
          (l).value,
          recurse,
          val0));
        return hydra.rewriting.Rewriting.rewriteAndFoldTerm_forMany(
          (java.util.function.Function<T0, java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<T0, hydra.core.Binding>>>) (v1 -> (java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<T0, hydra.core.Binding>>) (v2 -> hydra.rewriting.Rewriting.<T0, T0>rewriteAndFoldTerm_forBinding(
            recurse,
            v1,
            v2))),
          (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.core.Term>) (bins -> new hydra.core.Term.Let(new hydra.core.Let(bins, hydra.lib.pairs.Second.apply(renv.get())))),
          hydra.lib.pairs.First.apply(renv.get()),
          ((l).value).bindings);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.List els) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTerm_forMany(
          recurse,
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (x -> new hydra.core.Term.List(x)),
          val0,
          (els).value);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Map m) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTerm_forMany(
          (java.util.function.Function<T0, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>>) (v2 -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_forPair(
            recurse,
            v1,
            v2))),
          (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>, hydra.core.Term>) (pairs -> new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(pairs))),
          val0,
          hydra.lib.maps.ToList.apply((m).value));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Maybe.apply(
          dflt.get(),
          (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (t -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_forSingle(
            recurse,
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t1 -> new hydra.core.Term.Maybe(hydra.util.Maybe.just(t1))),
            val0,
            t)),
          (mt).value);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Pair p) {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rf = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_rf(
          (p).value,
          recurse,
          val0));
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rs = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_rs(
          (p).value,
          recurse,
          rf.get()));
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(rs.get()), new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(hydra.lib.pairs.Second.apply(rf.get()), hydra.lib.pairs.Second.apply(rs.get()))))))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Record r) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTerm_forMany(
          forField,
          (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.core.Term>) (fields -> new hydra.core.Term.Record(new hydra.core.Record(((r).value).typeName, fields))),
          val0,
          ((r).value).fields);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Set els) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTerm_forMany(
          recurse,
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (e -> new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(e))),
          val0,
          hydra.lib.sets.ToList.apply((els).value));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.TypeApplication ta) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTerm_forSingle(
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(t, ((ta).value).type))),
          val0,
          ((ta).value).body);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.TypeLambda tl) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTerm_forSingle(
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(((tl).value).parameter, t))),
          val0,
          ((tl).value).body);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Union inj) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTerm_forSingle(
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Union(new hydra.core.Injection(((inj).value).typeName, new hydra.core.Field((((inj).value).field).name, t)))),
          val0,
          (((inj).value).field).term);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Wrap wt) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTerm_forSingle(
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(((wt).value).typeName, t))),
          val0,
          ((wt).value).body);
      }
    });
  }
  
  static <T0> hydra.util.Tuple.Tuple2<T0, hydra.core.Term> rewriteAndFoldTerm_recurse(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>> f, T0 v1, hydra.core.Term v2) {
    return (((f).apply((java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (v22 -> hydra.rewriting.Rewriting.<T0>rewriteAndFoldTerm_fsub(
      (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (v13 -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (v23 -> hydra.rewriting.Rewriting.<T0>rewriteAndFoldTerm_recurse(
        f,
        v13,
        v23))),
      v12,
      v22))))).apply(v1)).apply(v2);
  }
  
  static <T0, T1, T2, T3, T4> hydra.util.Tuple.Tuple2<T2, T4> rewriteAndFoldTerm_forSingle(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T2, T3>>> rec, java.util.function.Function<T3, T4> cons, T0 val, T1 term) {
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<T2, T3>> r = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.<T0, T1, hydra.util.Tuple.Tuple2<T2, T3>>rewriteAndFoldTerm_r4(
      rec,
      term,
      val));
    return (hydra.util.Tuple.Tuple2<T2, T4>) ((hydra.util.Tuple.Tuple2<T2, T4>) (new hydra.util.Tuple.Tuple2<T2, T4>(hydra.lib.pairs.First.apply(r.get()), (cons).apply(hydra.lib.pairs.Second.apply(r.get())))));
  }
  
  static <T0, T1, T2, T3> hydra.util.Tuple.Tuple2<T0, T3> rewriteAndFoldTerm_forMany(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T0, T2>>> rec, java.util.function.Function<java.util.List<T2>, T3> cons, T0 val, java.util.List<T1> els) {
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, java.util.List<T2>>> rr = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.<T1, T0, T2>rewriteAndFoldTerm_rr2(
      els,
      rec,
      val));
    return (hydra.util.Tuple.Tuple2<T0, T3>) ((hydra.util.Tuple.Tuple2<T0, T3>) (new hydra.util.Tuple.Tuple2<T0, T3>(hydra.lib.pairs.First.apply(rr.get()), (cons).apply(hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(rr.get()))))));
  }
  
  static <T0, T1> hydra.util.Tuple.Tuple2<T1, hydra.core.Field> rewriteAndFoldTerm_forField(java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T1, hydra.core.Term>>> recurse, T0 val, hydra.core.Field field) {
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<T1, hydra.core.Term>> r = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_r3(
      field,
      recurse,
      val));
    return (hydra.util.Tuple.Tuple2<T1, hydra.core.Field>) ((hydra.util.Tuple.Tuple2<T1, hydra.core.Field>) (new hydra.util.Tuple.Tuple2<T1, hydra.core.Field>(hydra.lib.pairs.First.apply(r.get()), new hydra.core.Field((field).name, hydra.lib.pairs.Second.apply(r.get())))));
  }
  
  static <T0, T1, T2> hydra.util.Tuple.Tuple2<T0, java.util.List<T2>> rewriteAndFoldTerm_forFields(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T0, T2>>> forField, T0 v1, java.util.List<T1> v2) {
    return hydra.rewriting.Rewriting.<T0, T1, T2, java.util.List<T2>>rewriteAndFoldTerm_forMany(
      forField,
      (java.util.function.Function<java.util.List<T2>, java.util.List<T2>>) (x -> x),
      v1,
      v2);
  }
  
  static <T0, T1, T2> hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<T2, T2>> rewriteAndFoldTerm_forPair(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T0, T2>>> recurse, T0 val, hydra.util.Tuple.Tuple2<T1, T1> kv) {
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, T2>> rk = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.<T1, T1, T0, hydra.util.Tuple.Tuple2<T0, T2>>rewriteAndFoldTerm_rk(
      kv,
      recurse,
      val));
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, T2>> rv = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.<T1, T1, T0, hydra.util.Tuple.Tuple2<T0, T2>, T2>rewriteAndFoldTerm_rv(
      kv,
      recurse,
      rk.get()));
    return (hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<T2, T2>>) ((hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<T2, T2>>) (new hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<T2, T2>>(hydra.lib.pairs.First.apply(rv.get()), (hydra.util.Tuple.Tuple2<T2, T2>) ((hydra.util.Tuple.Tuple2<T2, T2>) (new hydra.util.Tuple.Tuple2<T2, T2>(hydra.lib.pairs.Second.apply(rk.get()), hydra.lib.pairs.Second.apply(rv.get())))))));
  }
  
  static <T0, T1> hydra.util.Tuple.Tuple2<T1, hydra.core.Binding> rewriteAndFoldTerm_forBinding(java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T1, hydra.core.Term>>> recurse, T0 val, hydra.core.Binding binding) {
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<T1, hydra.core.Term>> r = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_r2(
      binding,
      recurse,
      val));
    return (hydra.util.Tuple.Tuple2<T1, hydra.core.Binding>) ((hydra.util.Tuple.Tuple2<T1, hydra.core.Binding>) (new hydra.util.Tuple.Tuple2<T1, hydra.core.Binding>(hydra.lib.pairs.First.apply(r.get()), new hydra.core.Binding((binding).name, hydra.lib.pairs.Second.apply(r.get()), (binding).type))));
  }
  
  static <T0> hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination> rewriteAndFoldTerm_forElimination(java.util.function.Function<T0, java.util.function.Function<java.util.List<hydra.core.Field>, hydra.util.Tuple.Tuple2<T0, java.util.List<hydra.core.Field>>>> forFields, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>> recurse, T0 val, hydra.core.Elimination elm) {
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>> r = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.<T0>rewriteAndFoldTerm_r(
      elm,
      forFields,
      recurse,
      val));
    return (hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>(hydra.lib.pairs.First.apply(r.get()), hydra.lib.pairs.Second.apply(r.get()))));
  }
  
  static <T0> hydra.util.Tuple.Tuple2<T0, hydra.core.Function> rewriteAndFoldTerm_forFunction(java.util.function.Function<T0, java.util.function.Function<hydra.core.Elimination, hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>>> forElimination, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>> recurse, T0 val, hydra.core.Function fun) {
    return (fun).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Function> otherwise(hydra.core.Function instance) {
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Function>(val, fun)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Function> visit(hydra.core.Function.Elimination elm) {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>> re = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_re(
          (elm).value,
          forElimination,
          val));
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Function>(hydra.lib.pairs.First.apply(re.get()), new hydra.core.Function.Elimination(hydra.lib.pairs.Second.apply(re.get())))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Function> visit(hydra.core.Function.Lambda l) {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rl = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_rl2(
          (l).value,
          recurse,
          val));
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Function>(hydra.lib.pairs.First.apply(rl.get()), new hydra.core.Function.Lambda(new hydra.core.Lambda(((l).value).parameter, ((l).value).domain, hydra.lib.pairs.Second.apply(rl.get()))))));
      }
    });
  }
  
  static <T0, T1> hydra.util.Tuple.Tuple2<T1, T0> rewriteAndFoldTerm_dflt(T0 term0, T1 val0) {
    return (hydra.util.Tuple.Tuple2<T1, T0>) ((hydra.util.Tuple.Tuple2<T1, T0>) (new hydra.util.Tuple.Tuple2<T1, T0>(val0, term0)));
  }
  
  static <T0, T1> T1 rewriteAndFoldTerm_rlhs(hydra.core.Application a, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T1>> recurse, T0 val0) {
    return ((recurse).apply(val0)).apply((a).function);
  }
  
  static <T0, T1, T2> T1 rewriteAndFoldTerm_rrhs(hydra.core.Application a, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T1>> recurse, hydra.util.Tuple.Tuple2<T0, T2> rlhs) {
    return ((recurse).apply(hydra.lib.pairs.First.apply(rlhs))).apply((a).argument);
  }
  
  static <T0, T1, T2> T2 rewriteAndFoldTerm_rl(T0 l, java.util.function.Function<T1, java.util.function.Function<T0, T2>> recurse, T1 val0) {
    return ((recurse).apply(val0)).apply(l);
  }
  
  static <T0, T1, T2> T2 rewriteAndFoldTerm_rr(T0 r, java.util.function.Function<T1, java.util.function.Function<T0, T2>> recurse, T1 val0) {
    return ((recurse).apply(val0)).apply(r);
  }
  
  static <T0, T1> T1 rewriteAndFoldTerm_renv(hydra.core.Let l, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T1>> recurse, T0 val0) {
    return ((recurse).apply(val0)).apply((l).body);
  }
  
  static <T0, T1, T2, T3> T3 rewriteAndFoldTerm_rf(hydra.util.Tuple.Tuple2<T0, T1> p, java.util.function.Function<T2, java.util.function.Function<T0, T3>> recurse, T2 val0) {
    return ((recurse).apply(val0)).apply(hydra.lib.pairs.First.apply(p));
  }
  
  static <T0, T1, T2, T3, T4> T3 rewriteAndFoldTerm_rs(hydra.util.Tuple.Tuple2<T0, T1> p, java.util.function.Function<T2, java.util.function.Function<T1, T3>> recurse, hydra.util.Tuple.Tuple2<T2, T4> rf) {
    return ((recurse).apply(hydra.lib.pairs.First.apply(rf))).apply(hydra.lib.pairs.Second.apply(p));
  }
  
  static <T0, T1, T2> T2 rewriteAndFoldTerm_re(T0 elm, java.util.function.Function<T1, java.util.function.Function<T0, T2>> forElimination, T1 val) {
    return ((forElimination).apply(val)).apply(elm);
  }
  
  static <T0, T1> T1 rewriteAndFoldTerm_rl2(hydra.core.Lambda l, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T1>> recurse, T0 val) {
    return ((recurse).apply(val)).apply((l).body);
  }
  
  static <T0> hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination> rewriteAndFoldTerm_r(hydra.core.Elimination elm, java.util.function.Function<T0, java.util.function.Function<java.util.List<hydra.core.Field>, hydra.util.Tuple.Tuple2<T0, java.util.List<hydra.core.Field>>>> forFields, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>> recurse, T0 val) {
    return (elm).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination> otherwise(hydra.core.Elimination instance) {
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>(val, elm)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination> visit(hydra.core.Elimination.Union cs) {
        hydra.util.Lazy<hydra.util.Maybe<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>> rmd = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_rmd(
          (cs).value,
          recurse,
          val));
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, java.util.List<hydra.core.Field>>> rcases = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTerm_rcases(
          (cs).value,
          forFields,
          hydra.rewriting.Rewriting.rewriteAndFoldTerm_val1(
            rmd.get(),
            val)));
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>(hydra.lib.pairs.First.apply(rcases.get()), new hydra.core.Elimination.Union(new hydra.core.CaseStatement(((cs).value).typeName, hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>, hydra.core.Term>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>, hydra.core.Term>) (hydra.lib.pairs.Second::apply)),
          rmd.get()), hydra.lib.pairs.Second.apply(rcases.get()))))));
      }
    });
  }
  
  static <T0, T1> hydra.util.Maybe<T1> rewriteAndFoldTerm_rmd(hydra.core.CaseStatement cs, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T1>> recurse, T0 val) {
    return hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.core.Term, T1>) (v1 -> ((recurse).apply(val)).apply(v1)),
      (cs).default_);
  }
  
  static <T0, T1> T0 rewriteAndFoldTerm_val1(hydra.util.Maybe<hydra.util.Tuple.Tuple2<T0, T1>> rmd, T0 val) {
    return hydra.lib.maybes.Maybe.apply(
      val,
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, T0>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, T0>) (hydra.lib.pairs.First::apply)),
      rmd);
  }
  
  static <T0, T1> T1 rewriteAndFoldTerm_rcases(hydra.core.CaseStatement cs, java.util.function.Function<T0, java.util.function.Function<java.util.List<hydra.core.Field>, T1>> forFields, T0 val1) {
    return ((forFields).apply(val1)).apply((cs).cases);
  }
  
  static <T0, T1> T1 rewriteAndFoldTerm_r2(hydra.core.Binding binding, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T1>> recurse, T0 val) {
    return ((recurse).apply(val)).apply((binding).term);
  }
  
  static <T0, T1, T2, T3> T3 rewriteAndFoldTerm_rk(hydra.util.Tuple.Tuple2<T0, T1> kv, java.util.function.Function<T2, java.util.function.Function<T0, T3>> recurse, T2 val) {
    return ((recurse).apply(val)).apply(hydra.lib.pairs.First.apply(kv));
  }
  
  static <T0, T1, T2, T3, T4> T3 rewriteAndFoldTerm_rv(hydra.util.Tuple.Tuple2<T0, T1> kv, java.util.function.Function<T2, java.util.function.Function<T1, T3>> recurse, hydra.util.Tuple.Tuple2<T2, T4> rk) {
    return ((recurse).apply(hydra.lib.pairs.First.apply(rk))).apply(hydra.lib.pairs.Second.apply(kv));
  }
  
  static <T0, T1> T1 rewriteAndFoldTerm_r3(hydra.core.Field field, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T1>> recurse, T0 val) {
    return ((recurse).apply(val)).apply((field).term);
  }
  
  static <T0, T1, T2> hydra.util.Tuple.Tuple2<T1, java.util.List<T2>> rewriteAndFoldTerm_rr2(java.util.List<T0> els, java.util.function.Function<T1, java.util.function.Function<T0, hydra.util.Tuple.Tuple2<T1, T2>>> rec, T1 val) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>, java.util.function.Function<T0, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>>) (r -> (java.util.function.Function<T0, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>) (el -> {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T1, T2>> r2 = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.<T0, T1, java.util.List<T2>, hydra.util.Tuple.Tuple2<T1, T2>>rewriteAndFoldTerm_r22(
          el,
          r,
          rec));
        return (hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>) ((hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>) (new hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>(hydra.lib.pairs.First.apply(r2.get()), hydra.lib.lists.Cons.apply(
          hydra.lib.pairs.Second.apply(r2.get()),
          hydra.lib.pairs.Second.apply(r)))));
      })),
      (hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>) ((hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>) (new hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>(val, (java.util.List<T2>) (java.util.List.<T2>of())))),
      els);
  }
  
  static <T0, T1, T2, T3> T3 rewriteAndFoldTerm_r22(T0 el, hydra.util.Tuple.Tuple2<T1, T2> r, java.util.function.Function<T1, java.util.function.Function<T0, T3>> rec) {
    return ((rec).apply(hydra.lib.pairs.First.apply(r))).apply(el);
  }
  
  static <T0, T1, T2> T2 rewriteAndFoldTerm_r4(java.util.function.Function<T0, java.util.function.Function<T1, T2>> rec, T1 term, T0 val) {
    return ((rec).apply(val)).apply(term);
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rewriteAndFoldTermM(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>> f, T0 term0, hydra.core.Term v1) {
    return hydra.rewriting.Rewriting.<T0, T1>rewriteAndFoldTermM_recurse(
      f,
      term0,
      v1);
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rewriteAndFoldTermM_fsub(java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>> recurse, T0 val0, hydra.core.Term term0) {
    java.util.function.Function<T0, java.util.function.Function<hydra.core.Field, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Field>>>> forField = (java.util.function.Function<T0, java.util.function.Function<hydra.core.Field, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Field>>>>) (v1 -> (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Field>>>) (v2 -> hydra.rewriting.Rewriting.<T0, T1, T0>rewriteAndFoldTermM_forField(
      recurse,
      v1,
      v2)));
    return (term0).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> otherwise(hydra.core.Term instance) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTermM_dflt(
          term0,
          val0);
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> visit(hydra.core.Term.Annotated at) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTermM_forSingle(
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(t, ((at).value).annotation))),
          val0,
          ((at).value).body);
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> visit(hydra.core.Term.Application a) {
        return hydra.lib.flows.Bind.apply(
          ((recurse).apply(val0)).apply(((a).value).function),
          (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (rlhs -> hydra.lib.flows.Bind.apply(
            ((recurse).apply(hydra.lib.pairs.First.apply(rlhs))).apply(((a).value).argument),
            (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (rrhs -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(rrhs), new hydra.core.Term.Application(new hydra.core.Application(hydra.lib.pairs.Second.apply(rlhs), hydra.lib.pairs.Second.apply(rrhs)))))))))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (l -> hydra.lib.flows.Bind.apply(
            ((recurse).apply(val0)).apply(l),
            (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (rl -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(rl), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(hydra.lib.pairs.Second.apply(rl)))))))))))),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (r -> hydra.lib.flows.Bind.apply(
            ((recurse).apply(val0)).apply(r),
            (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (rr -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(rr), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(hydra.lib.pairs.Second.apply(rr)))))))))))),
          (e).value);
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> visit(hydra.core.Term.Function f) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTermM_forSingle(
          (java.util.function.Function<T0, java.util.function.Function<hydra.core.Function, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Function>>>>) (v1 -> (java.util.function.Function<hydra.core.Function, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Function>>>) (v2 -> hydra.rewriting.Rewriting.<T0, T1>rewriteAndFoldTermM_forFunction(
            (java.util.function.Function<T0, java.util.function.Function<hydra.core.Elimination, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>>>>) (v12 -> (java.util.function.Function<hydra.core.Elimination, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>>>) (v22 -> hydra.rewriting.Rewriting.<T0, T1>rewriteAndFoldTermM_forElimination(
              (java.util.function.Function<T0, java.util.function.Function<java.util.List<hydra.core.Field>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, java.util.List<hydra.core.Field>>>>>) (v13 -> (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, java.util.List<hydra.core.Field>>>>) (v23 -> hydra.rewriting.Rewriting.rewriteAndFoldTermM_forFields(
                forField,
                v13,
                v23))),
              recurse,
              v12,
              v22))),
            recurse,
            v1,
            v2))),
          (java.util.function.Function<hydra.core.Function, hydra.core.Term>) (f2 -> new hydra.core.Term.Function(f2)),
          val0,
          (f).value);
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> visit(hydra.core.Term.Let l) {
        return hydra.lib.flows.Bind.apply(
          ((recurse).apply(val0)).apply(((l).value).body),
          (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (renv -> hydra.rewriting.Rewriting.rewriteAndFoldTermM_forMany(
            (java.util.function.Function<T0, java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Binding>>>>) (v1 -> (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Binding>>>) (v2 -> hydra.rewriting.Rewriting.<T0, T1, T0>rewriteAndFoldTermM_forBinding(
              recurse,
              v1,
              v2))),
            (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.core.Term>) (bins -> new hydra.core.Term.Let(new hydra.core.Let(bins, hydra.lib.pairs.Second.apply(renv)))),
            hydra.lib.pairs.First.apply(renv),
            ((l).value).bindings)));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> visit(hydra.core.Term.List els) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTermM_forMany(
          recurse,
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (x -> new hydra.core.Term.List(x)),
          val0,
          (els).value);
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> visit(hydra.core.Term.Map m) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTermM_forMany(
          (java.util.function.Function<T0, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>>>>) (v1 -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>>>) (v2 -> hydra.rewriting.Rewriting.rewriteAndFoldTermM_forPair(
            recurse,
            v1,
            v2))),
          (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>, hydra.core.Term>) (pairs -> new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(pairs))),
          val0,
          hydra.lib.maps.ToList.apply((m).value));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Maybe.apply(
          hydra.rewriting.Rewriting.rewriteAndFoldTermM_dflt(
            term0,
            val0),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (t -> hydra.rewriting.Rewriting.rewriteAndFoldTermM_forSingle(
            recurse,
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t1 -> new hydra.core.Term.Maybe(hydra.util.Maybe.just(t1))),
            val0,
            t)),
          (mt).value);
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> visit(hydra.core.Term.Pair p) {
        return hydra.lib.flows.Bind.apply(
          ((recurse).apply(val0)).apply(hydra.lib.pairs.First.apply((p).value)),
          (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (rf -> hydra.lib.flows.Bind.apply(
            ((recurse).apply(hydra.lib.pairs.First.apply(rf))).apply(hydra.lib.pairs.Second.apply((p).value)),
            (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (rs -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(rs), new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(hydra.lib.pairs.Second.apply(rf), hydra.lib.pairs.Second.apply(rs)))))))))))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> visit(hydra.core.Term.Record r) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTermM_forMany(
          forField,
          (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.core.Term>) (fields -> new hydra.core.Term.Record(new hydra.core.Record(((r).value).typeName, fields))),
          val0,
          ((r).value).fields);
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> visit(hydra.core.Term.Set els) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTermM_forMany(
          recurse,
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (e -> new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(e))),
          val0,
          hydra.lib.sets.ToList.apply((els).value));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> visit(hydra.core.Term.TypeApplication ta) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTermM_forSingle(
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(t, ((ta).value).type))),
          val0,
          ((ta).value).body);
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> visit(hydra.core.Term.TypeLambda tl) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTermM_forSingle(
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(((tl).value).parameter, t))),
          val0,
          ((tl).value).body);
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> visit(hydra.core.Term.Union inj) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTermM_forSingle(
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Union(new hydra.core.Injection(((inj).value).typeName, new hydra.core.Field((((inj).value).field).name, t)))),
          val0,
          (((inj).value).field).term);
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> visit(hydra.core.Term.Wrap wt) {
        return hydra.rewriting.Rewriting.rewriteAndFoldTermM_forSingle(
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(((wt).value).typeName, t))),
          val0,
          ((wt).value).body);
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rewriteAndFoldTermM_recurse(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>> f, T0 v1, hydra.core.Term v2) {
    return (((f).apply((java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (v22 -> hydra.rewriting.Rewriting.<T0, T1>rewriteAndFoldTermM_fsub(
      (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>) (v13 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (v23 -> hydra.rewriting.Rewriting.<T0, T1>rewriteAndFoldTermM_recurse(
        f,
        v13,
        v23))),
      v12,
      v22))))).apply(v1)).apply(v2);
  }
  
  static <T0, T1, T2, T3, T4, T5> hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<T3, T5>> rewriteAndFoldTermM_forSingle(java.util.function.Function<T0, java.util.function.Function<T1, hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<T3, T4>>>> rec, java.util.function.Function<T4, T5> cons, T0 val, T1 term) {
    return hydra.lib.flows.Bind.apply(
      ((rec).apply(val)).apply(term),
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T3, T4>, hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<T3, T5>>>) (r -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T3, T5>) ((hydra.util.Tuple.Tuple2<T3, T5>) (new hydra.util.Tuple.Tuple2<T3, T5>(hydra.lib.pairs.First.apply(r), (cons).apply(hydra.lib.pairs.Second.apply(r))))))));
  }
  
  static <T0, T1, T2, T3, T4> hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<T0, T4>> rewriteAndFoldTermM_forMany(java.util.function.Function<T0, java.util.function.Function<T1, hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<T0, T3>>>> rec, java.util.function.Function<java.util.List<T3>, T4> cons, T0 val, java.util.List<T1> els) {
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.Foldl.apply(
        (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, java.util.List<T3>>, java.util.function.Function<T1, hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<T0, java.util.List<T3>>>>>) (r -> (java.util.function.Function<T1, hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<T0, java.util.List<T3>>>>) (el -> hydra.lib.flows.Bind.apply(
          ((rec).apply(hydra.lib.pairs.First.apply(r))).apply(el),
          (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T3>, hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<T0, java.util.List<T3>>>>) (r2 -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T0, java.util.List<T3>>) ((hydra.util.Tuple.Tuple2<T0, java.util.List<T3>>) (new hydra.util.Tuple.Tuple2<T0, java.util.List<T3>>(hydra.lib.pairs.First.apply(r2), hydra.lib.lists.Cons.apply(
            hydra.lib.pairs.Second.apply(r2),
            hydra.lib.pairs.Second.apply(r)))))))))),
        (hydra.util.Tuple.Tuple2<T0, java.util.List<T3>>) ((hydra.util.Tuple.Tuple2<T0, java.util.List<T3>>) (new hydra.util.Tuple.Tuple2<T0, java.util.List<T3>>(val, (java.util.List<T3>) (java.util.List.<T3>of())))),
        els),
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, java.util.List<T3>>, hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<T0, T4>>>) (rr -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T0, T4>) ((hydra.util.Tuple.Tuple2<T0, T4>) (new hydra.util.Tuple.Tuple2<T0, T4>(hydra.lib.pairs.First.apply(rr), (cons).apply(hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(rr)))))))));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T2, hydra.core.Field>> rewriteAndFoldTermM_forField(java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T2, hydra.core.Term>>>> recurse, T0 val, hydra.core.Field field) {
    return hydra.lib.flows.Bind.apply(
      ((recurse).apply(val)).apply((field).term),
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T2, hydra.core.Term>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T2, hydra.core.Field>>>) (r -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T2, hydra.core.Field>) ((hydra.util.Tuple.Tuple2<T2, hydra.core.Field>) (new hydra.util.Tuple.Tuple2<T2, hydra.core.Field>(hydra.lib.pairs.First.apply(r), new hydra.core.Field((field).name, hydra.lib.pairs.Second.apply(r))))))));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<T0, java.util.List<T3>>> rewriteAndFoldTermM_forFields(java.util.function.Function<T0, java.util.function.Function<T1, hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<T0, T3>>>> forField, T0 v1, java.util.List<T1> v2) {
    return hydra.rewriting.Rewriting.<T0, T1, T2, T3, java.util.List<T3>>rewriteAndFoldTermM_forMany(
      forField,
      (java.util.function.Function<java.util.List<T3>, java.util.List<T3>>) (x -> x),
      v1,
      v2);
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<T3, T3>>> rewriteAndFoldTermM_forPair(java.util.function.Function<T0, java.util.function.Function<T1, hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<T0, T3>>>> recurse, T0 val, hydra.util.Tuple.Tuple2<T1, T1> kv) {
    return hydra.lib.flows.Bind.apply(
      ((recurse).apply(val)).apply(hydra.lib.pairs.First.apply(kv)),
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T3>, hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<T3, T3>>>>) (rk -> hydra.lib.flows.Bind.apply(
        ((recurse).apply(hydra.lib.pairs.First.apply(rk))).apply(hydra.lib.pairs.Second.apply(kv)),
        (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T3>, hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<T3, T3>>>>) (rv -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<T3, T3>>) ((hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<T3, T3>>) (new hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<T3, T3>>(hydra.lib.pairs.First.apply(rv), (hydra.util.Tuple.Tuple2<T3, T3>) ((hydra.util.Tuple.Tuple2<T3, T3>) (new hydra.util.Tuple.Tuple2<T3, T3>(hydra.lib.pairs.Second.apply(rk), hydra.lib.pairs.Second.apply(rv))))))))))));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T2, hydra.core.Binding>> rewriteAndFoldTermM_forBinding(java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T2, hydra.core.Term>>>> recurse, T0 val, hydra.core.Binding binding) {
    return hydra.lib.flows.Bind.apply(
      ((recurse).apply(val)).apply((binding).term),
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T2, hydra.core.Term>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T2, hydra.core.Binding>>>) (r -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T2, hydra.core.Binding>) ((hydra.util.Tuple.Tuple2<T2, hydra.core.Binding>) (new hydra.util.Tuple.Tuple2<T2, hydra.core.Binding>(hydra.lib.pairs.First.apply(r), new hydra.core.Binding((binding).name, hydra.lib.pairs.Second.apply(r), (binding).type)))))));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>> rewriteAndFoldTermM_forElimination(java.util.function.Function<T0, java.util.function.Function<java.util.List<hydra.core.Field>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, java.util.List<hydra.core.Field>>>>> forFields, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>> recurse, T0 val, hydra.core.Elimination elm) {
    return hydra.lib.flows.Bind.apply(
      hydra.rewriting.Rewriting.<T0, T1>rewriteAndFoldTermM_rw(
        forFields,
        recurse,
        val,
        elm),
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>>>) (r -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>(hydra.lib.pairs.First.apply(r), hydra.lib.pairs.Second.apply(r)))))));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Function>> rewriteAndFoldTermM_forFunction(java.util.function.Function<T0, java.util.function.Function<hydra.core.Elimination, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>>>> forElimination, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>> recurse, T0 val, hydra.core.Function fun) {
    return (fun).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Function>> otherwise(hydra.core.Function instance) {
        return hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Function>(val, fun))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Function>> visit(hydra.core.Function.Elimination elm) {
        return hydra.lib.flows.Bind.apply(
          ((forElimination).apply(val)).apply((elm).value),
          (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Function>>>) (r -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Function>(hydra.lib.pairs.First.apply(r), new hydra.core.Function.Elimination(hydra.lib.pairs.Second.apply(r))))))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Function>> visit(hydra.core.Function.Lambda l) {
        return hydra.lib.flows.Bind.apply(
          ((recurse).apply(val)).apply(((l).value).body),
          (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Function>>>) (r -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Function>(hydra.lib.pairs.First.apply(r), new hydra.core.Function.Lambda(new hydra.core.Lambda(((l).value).parameter, ((l).value).domain, hydra.lib.pairs.Second.apply(r)))))))));
      }
    });
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<T1, T0>> rewriteAndFoldTermM_dflt(T0 term0, T1 val0) {
    return hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T1, T0>) ((hydra.util.Tuple.Tuple2<T1, T0>) (new hydra.util.Tuple.Tuple2<T1, T0>(val0, term0))));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>> rewriteAndFoldTermM_rw(java.util.function.Function<T0, java.util.function.Function<java.util.List<hydra.core.Field>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, java.util.List<hydra.core.Field>>>>> forFields, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>> recurse, T0 val, hydra.core.Elimination elm) {
    return (elm).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>> otherwise(hydra.core.Elimination instance) {
        return hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>(val, elm))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>> visit(hydra.core.Elimination.Union cs) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.maybes.Maybe.apply(
            hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (hydra.util.Maybe.<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>nothing())),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Maybe<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>) (def -> hydra.lib.flows.Map.apply(
              (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>, hydra.util.Maybe<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (hydra.lib.maybes.Pure::apply),
              ((recurse).apply(val)).apply(def))),
            ((cs).value).default_),
          (java.util.function.Function<hydra.util.Maybe<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>>>) (rmd -> hydra.lib.flows.Bind.apply(
            ((forFields).apply(hydra.rewriting.Rewriting.rewriteAndFoldTermM_val1(
              rmd,
              val))).apply(((cs).value).cases),
            (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, java.util.List<hydra.core.Field>>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>>>) (rcases -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>(hydra.lib.pairs.First.apply(rcases), new hydra.core.Elimination.Union(new hydra.core.CaseStatement(((cs).value).typeName, hydra.lib.maybes.Map.apply(
              (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>, hydra.core.Term>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>, hydra.core.Term>) (hydra.lib.pairs.Second::apply)),
              rmd), hydra.lib.pairs.Second.apply(rcases)))))))))));
      }
    });
  }
  
  static <T0, T1> T0 rewriteAndFoldTermM_val1(hydra.util.Maybe<hydra.util.Tuple.Tuple2<T0, T1>> rmd, T0 val) {
    return hydra.lib.maybes.Maybe.apply(
      val,
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, T0>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, T0>) (hydra.lib.pairs.First::apply)),
      rmd);
  }
  
  static <T0> hydra.util.Tuple.Tuple2<T0, hydra.core.Term> rewriteAndFoldTermWithPath(java.util.function.Function<java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>> f, T0 term0, hydra.core.Term v1) {
    return hydra.rewriting.Rewriting.<T0>rewriteAndFoldTermWithPath_recurse(
      f,
      (java.util.List<hydra.accessors.TermAccessor>) (java.util.List.<hydra.accessors.TermAccessor>of()),
      term0,
      v1);
  }
  
  static <T0> hydra.util.Tuple.Tuple2<T0, hydra.core.Term> rewriteAndFoldTermWithPath_fsub(java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>> recurse, java.util.List<hydra.accessors.TermAccessor> path, T0 val0, hydra.core.Term term0) {
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> dflt = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_dflt(
      term0,
      val0));
    return (term0).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return dflt.get();
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Annotated at) {
        return ((((((java.util.function.Function<java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>, java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>>>) (v1 -> (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>>) (v2 -> (java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>) (v3 -> (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (v4 -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (v5 -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_forSingleWithAccessor(
          path,
          v1,
          v2,
          v3,
          v4,
          v5))))))).apply(recurse)).apply((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(t, ((at).value).annotation))))).apply(new hydra.accessors.TermAccessor.AnnotatedBody())).apply(val0)).apply(((at).value).body);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Application a) {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rlhs = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_rlhs(
          (a).value,
          path,
          recurse,
          val0));
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rrhs = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_rrhs(
          (a).value,
          path,
          recurse,
          rlhs.get()));
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(rrhs.get()), new hydra.core.Term.Application(new hydra.core.Application(hydra.lib.pairs.Second.apply(rlhs.get()), hydra.lib.pairs.Second.apply(rrhs.get()))))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (l -> {
            hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rl = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_rl(
              l,
              path,
              recurse,
              val0));
            return (hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(rl.get()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(hydra.lib.pairs.Second.apply(rl.get()))))))));
          }),
          (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (r -> {
            hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rr = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_rr(
              path,
              r,
              recurse,
              val0));
            return (hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(rr.get()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(hydra.lib.pairs.Second.apply(rr.get()))))))));
          }),
          (e).value);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Function f) {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Function>> rf = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_rf(
          (f).value,
          (java.util.function.Function<T0, java.util.function.Function<hydra.core.Function, hydra.util.Tuple.Tuple2<T0, hydra.core.Function>>>) (v1 -> (java.util.function.Function<hydra.core.Function, hydra.util.Tuple.Tuple2<T0, hydra.core.Function>>) (v2 -> hydra.rewriting.Rewriting.<T0>rewriteAndFoldTermWithPath_forFunction(
            (java.util.function.Function<T0, java.util.function.Function<hydra.core.Elimination, hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>>>) (v12 -> (java.util.function.Function<hydra.core.Elimination, hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>>) (v22 -> hydra.rewriting.Rewriting.<T0>rewriteAndFoldTermWithPath_forElimination(
              path,
              recurse,
              v12,
              v22))),
            path,
            recurse,
            v1,
            v2))),
          val0));
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(rf.get()), new hydra.core.Term.Function(hydra.lib.pairs.Second.apply(rf.get())))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Let l) {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> renv = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_renv(
          (l).value,
          path,
          recurse,
          val0));
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, java.util.List<hydra.core.Binding>>> rbindings = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_rbindings(
          (java.util.function.Function<T0, java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<T0, hydra.core.Binding>>>) (v1 -> (java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<T0, hydra.core.Binding>>) (v2 -> hydra.rewriting.Rewriting.<T0, T0>rewriteAndFoldTermWithPath_forBindingWithAccessor(
            path,
            recurse,
            v1,
            v2))),
          (l).value,
          renv.get()));
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(rbindings.get()), new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(rbindings.get())), hydra.lib.pairs.Second.apply(renv.get()))))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.List els) {
        Integer idx = 0;
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T0, java.util.List<hydra.core.Term>>>> rr = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_rr2(
          (els).value,
          idx,
          path,
          recurse,
          val0));
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(rr.get())), new hydra.core.Term.List(hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(rr.get())))))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Map m) {
        Integer idx = 0;
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T0, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>>>> rr = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_rr3(
          idx,
          (m).value,
          path,
          recurse,
          val0));
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(rr.get())), new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(rr.get()))))))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Maybe.apply(
          dflt.get(),
          (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (t -> ((((((java.util.function.Function<java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>, java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>>>) (v1 -> (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>>) (v2 -> (java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>) (v3 -> (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (v4 -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (v5 -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_forSingleWithAccessor(
            path,
            v1,
            v2,
            v3,
            v4,
            v5))))))).apply(recurse)).apply((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t1 -> new hydra.core.Term.Maybe(hydra.util.Maybe.just(t1))))).apply(new hydra.accessors.TermAccessor.MaybeTerm())).apply(val0)).apply(t)),
          (mt).value);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Pair p) {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rf = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_rf2(
          (p).value,
          path,
          recurse,
          val0));
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rs = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_rs(
          (p).value,
          path,
          recurse,
          rf.get()));
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(rs.get()), new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(hydra.lib.pairs.Second.apply(rf.get()), hydra.lib.pairs.Second.apply(rs.get()))))))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Record r) {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, java.util.List<hydra.core.Term>>> rfields = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_rfields(
          path,
          (r).value,
          recurse,
          val0));
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(rfields.get()), new hydra.core.Term.Record(new hydra.core.Record(((r).value).typeName, hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>, hydra.core.Field>) (ft -> new hydra.core.Field(hydra.lib.pairs.First.apply(ft), hydra.lib.pairs.Second.apply(ft))),
          hydra.lib.lists.Zip.apply(
            hydra.lib.lists.Map.apply(
              projected -> projected.name,
              ((r).value).fields),
            hydra.lib.pairs.Second.apply(rfields.get()))))))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Set els) {
        Integer idx = 0;
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T0, java.util.List<hydra.core.Term>>>> rr = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_rr4(
          (els).value,
          idx,
          path,
          recurse,
          val0));
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(rr.get())), new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(rr.get()))))))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.TypeApplication ta) {
        return ((((((java.util.function.Function<java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>, java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>>>) (v1 -> (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>>) (v2 -> (java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>) (v3 -> (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (v4 -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (v5 -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_forSingleWithAccessor(
          path,
          v1,
          v2,
          v3,
          v4,
          v5))))))).apply(recurse)).apply((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(t, ((ta).value).type))))).apply(new hydra.accessors.TermAccessor.TypeApplicationTerm())).apply(val0)).apply(((ta).value).body);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.TypeLambda tl) {
        return ((((((java.util.function.Function<java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>, java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>>>) (v1 -> (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>>) (v2 -> (java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>) (v3 -> (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (v4 -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (v5 -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_forSingleWithAccessor(
          path,
          v1,
          v2,
          v3,
          v4,
          v5))))))).apply(recurse)).apply((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(((tl).value).parameter, t))))).apply(new hydra.accessors.TermAccessor.TypeLambdaBody())).apply(val0)).apply(((tl).value).body);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Union inj) {
        return ((((((java.util.function.Function<java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>, java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>>>) (v1 -> (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>>) (v2 -> (java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>) (v3 -> (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (v4 -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (v5 -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_forSingleWithAccessor(
          path,
          v1,
          v2,
          v3,
          v4,
          v5))))))).apply(recurse)).apply((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Union(new hydra.core.Injection(((inj).value).typeName, new hydra.core.Field((((inj).value).field).name, t)))))).apply(new hydra.accessors.TermAccessor.InjectionTerm())).apply(val0)).apply((((inj).value).field).term);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Term> visit(hydra.core.Term.Wrap wt) {
        return ((((((java.util.function.Function<java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>, java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>>>) (v1 -> (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>>) (v2 -> (java.util.function.Function<hydra.accessors.TermAccessor, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>) (v3 -> (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (v4 -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (v5 -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_forSingleWithAccessor(
          path,
          v1,
          v2,
          v3,
          v4,
          v5))))))).apply(recurse)).apply((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(((wt).value).typeName, t))))).apply(new hydra.accessors.TermAccessor.WrappedTerm())).apply(val0)).apply(((wt).value).body);
      }
    });
  }
  
  static <T0> hydra.util.Tuple.Tuple2<T0, hydra.core.Term> rewriteAndFoldTermWithPath_recurse(java.util.function.Function<java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>> f, java.util.List<hydra.accessors.TermAccessor> v1, T0 v2, hydra.core.Term v3) {
    return ((((f).apply((java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>) (v12 -> (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (v22 -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (v32 -> hydra.rewriting.Rewriting.<T0>rewriteAndFoldTermWithPath_fsub(
      (java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>) (v13 -> (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>) (v23 -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>) (v33 -> hydra.rewriting.Rewriting.<T0>rewriteAndFoldTermWithPath_recurse(
        f,
        v13,
        v23,
        v33)))),
      v12,
      v22,
      v32)))))).apply(v1)).apply(v2)).apply(v3);
  }
  
  static <T0, T1, T2, T3, T4, T5> hydra.util.Tuple.Tuple2<T3, T5> rewriteAndFoldTermWithPath_forSingleWithAccessor(java.util.List<T0> path, java.util.function.Function<java.util.List<T0>, java.util.function.Function<T1, java.util.function.Function<T2, hydra.util.Tuple.Tuple2<T3, T4>>>> rec, java.util.function.Function<T4, T5> cons, T0 accessor, T1 val, T2 term) {
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<T3, T4>> r = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.<T0, T1, T2, hydra.util.Tuple.Tuple2<T3, T4>>rewriteAndFoldTermWithPath_r5(
      accessor,
      path,
      rec,
      term,
      val));
    return (hydra.util.Tuple.Tuple2<T3, T5>) ((hydra.util.Tuple.Tuple2<T3, T5>) (new hydra.util.Tuple.Tuple2<T3, T5>(hydra.lib.pairs.First.apply(r.get()), (cons).apply(hydra.lib.pairs.Second.apply(r.get())))));
  }
  
  static <T0, T1, T2, T3, T4> hydra.util.Tuple.Tuple2<T1, T4> rewriteAndFoldTermWithPath_forManyWithAccessors(java.util.List<T0> path, java.util.function.Function<java.util.List<T0>, java.util.function.Function<T1, java.util.function.Function<T2, hydra.util.Tuple.Tuple2<T1, T3>>>> rec, java.util.function.Function<java.util.List<T3>, T4> cons, T1 val, java.util.List<hydra.util.Tuple.Tuple2<T0, T2>> accessorTermPairs) {
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<T1, java.util.List<T3>>> rr = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.<T0, T2, T1, T3>rewriteAndFoldTermWithPath_rr5(
      accessorTermPairs,
      path,
      rec,
      val));
    return (hydra.util.Tuple.Tuple2<T1, T4>) ((hydra.util.Tuple.Tuple2<T1, T4>) (new hydra.util.Tuple.Tuple2<T1, T4>(hydra.lib.pairs.First.apply(rr.get()), (cons).apply(hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(rr.get()))))));
  }
  
  static <T0, T1, T2> hydra.util.Tuple.Tuple2<T2, hydra.core.Field> rewriteAndFoldTermWithPath_forFieldWithAccessor(java.util.List<T0> path, java.util.function.Function<java.util.List<T0>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T2, hydra.core.Term>>>> recurse, java.util.function.Function<hydra.core.Name, T0> mkAccessor, T1 val, hydra.core.Field field) {
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<T2, hydra.core.Term>> r = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_r4(
      field,
      mkAccessor,
      path,
      recurse,
      val));
    return (hydra.util.Tuple.Tuple2<T2, hydra.core.Field>) ((hydra.util.Tuple.Tuple2<T2, hydra.core.Field>) (new hydra.util.Tuple.Tuple2<T2, hydra.core.Field>(hydra.lib.pairs.First.apply(r.get()), new hydra.core.Field((field).name, hydra.lib.pairs.Second.apply(r.get())))));
  }
  
  static <T0, T1, T2, T3, T4> java.util.function.Function<T2, java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<T0, T3>>, hydra.util.Tuple.Tuple2<T2, java.util.List<T4>>>> rewriteAndFoldTermWithPath_forFieldsWithAccessor(java.util.List<T0> path, java.util.function.Function<T1, java.util.function.Function<T2, java.util.function.Function<T3, hydra.util.Tuple.Tuple2<T2, T4>>>> forFieldWithAccessor, T1 mkAccessor) {
    return (((java.util.function.Function<java.util.function.Function<java.util.List<T0>, java.util.function.Function<T2, java.util.function.Function<T3, hydra.util.Tuple.Tuple2<T2, T4>>>>, java.util.function.Function<java.util.function.Function<java.util.List<T4>, java.util.List<T4>>, java.util.function.Function<T2, java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<T0, T3>>, hydra.util.Tuple.Tuple2<T2, java.util.List<T4>>>>>>) (v1 -> (java.util.function.Function<java.util.function.Function<java.util.List<T4>, java.util.List<T4>>, java.util.function.Function<T2, java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<T0, T3>>, hydra.util.Tuple.Tuple2<T2, java.util.List<T4>>>>>) (v2 -> (java.util.function.Function<T2, java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<T0, T3>>, hydra.util.Tuple.Tuple2<T2, java.util.List<T4>>>>) (v3 -> (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<T0, T3>>, hydra.util.Tuple.Tuple2<T2, java.util.List<T4>>>) (v4 -> hydra.rewriting.Rewriting.<T0, T2, T3, T4, java.util.List<T4>>rewriteAndFoldTermWithPath_forManyWithAccessors(
      path,
      v1,
      v2,
      v3,
      v4)))))).apply((java.util.function.Function<java.util.List<T0>, java.util.function.Function<T2, java.util.function.Function<T3, hydra.util.Tuple.Tuple2<T2, T4>>>>) (path1 -> (java.util.function.Function<T2, java.util.function.Function<T3, hydra.util.Tuple.Tuple2<T2, T4>>>) (val1 -> (java.util.function.Function<T3, hydra.util.Tuple.Tuple2<T2, T4>>) (field1 -> (((forFieldWithAccessor).apply(mkAccessor)).apply(val1)).apply(field1)))))).apply((java.util.function.Function<java.util.List<T4>, java.util.List<T4>>) (x -> x));
  }
  
  static <T0, T1, T2, T3> hydra.util.Tuple.Tuple2<T1, hydra.util.Tuple.Tuple2<T3, T3>> rewriteAndFoldTermWithPath_forPairWithAccessors(java.util.List<T0> path, java.util.function.Function<java.util.List<T0>, java.util.function.Function<T1, java.util.function.Function<T2, hydra.util.Tuple.Tuple2<T1, T3>>>> recurse, T0 keyAccessor, T0 valAccessor, T1 val, hydra.util.Tuple.Tuple2<T2, T2> kv) {
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<T1, T3>> rk = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.<T0, T2, T2, T1, hydra.util.Tuple.Tuple2<T1, T3>>rewriteAndFoldTermWithPath_rk2(
      keyAccessor,
      kv,
      path,
      recurse,
      val));
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<T1, T3>> rv = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.<T2, T2, T0, T1, hydra.util.Tuple.Tuple2<T1, T3>, T3>rewriteAndFoldTermWithPath_rv2(
      kv,
      path,
      recurse,
      rk.get(),
      valAccessor));
    return (hydra.util.Tuple.Tuple2<T1, hydra.util.Tuple.Tuple2<T3, T3>>) ((hydra.util.Tuple.Tuple2<T1, hydra.util.Tuple.Tuple2<T3, T3>>) (new hydra.util.Tuple.Tuple2<T1, hydra.util.Tuple.Tuple2<T3, T3>>(hydra.lib.pairs.First.apply(rv.get()), (hydra.util.Tuple.Tuple2<T3, T3>) ((hydra.util.Tuple.Tuple2<T3, T3>) (new hydra.util.Tuple.Tuple2<T3, T3>(hydra.lib.pairs.Second.apply(rk.get()), hydra.lib.pairs.Second.apply(rv.get())))))));
  }
  
  static <T0, T1> hydra.util.Tuple.Tuple2<T1, hydra.core.Binding> rewriteAndFoldTermWithPath_forBindingWithAccessor(java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T1, hydra.core.Term>>>> recurse, T0 val, hydra.core.Binding binding) {
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<T1, hydra.core.Term>> r = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_r3(
      binding,
      path,
      recurse,
      val));
    return (hydra.util.Tuple.Tuple2<T1, hydra.core.Binding>) ((hydra.util.Tuple.Tuple2<T1, hydra.core.Binding>) (new hydra.util.Tuple.Tuple2<T1, hydra.core.Binding>(hydra.lib.pairs.First.apply(r.get()), new hydra.core.Binding((binding).name, hydra.lib.pairs.Second.apply(r.get()), (binding).type))));
  }
  
  static <T0> hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination> rewriteAndFoldTermWithPath_forElimination(java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>> recurse, T0 val, hydra.core.Elimination elm) {
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>> r = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.<T0>rewriteAndFoldTermWithPath_r(
      elm,
      path,
      recurse,
      val));
    return (hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>(hydra.lib.pairs.First.apply(r.get()), hydra.lib.pairs.Second.apply(r.get()))));
  }
  
  static <T0> hydra.util.Tuple.Tuple2<T0, hydra.core.Function> rewriteAndFoldTermWithPath_forFunction(java.util.function.Function<T0, java.util.function.Function<hydra.core.Elimination, hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>>> forElimination, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>> recurse, T0 val, hydra.core.Function fun) {
    return (fun).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Function> otherwise(hydra.core.Function instance) {
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Function>(val, fun)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Function> visit(hydra.core.Function.Elimination elm) {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>> re = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_re(
          (elm).value,
          forElimination,
          val));
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Function>(hydra.lib.pairs.First.apply(re.get()), new hydra.core.Function.Elimination(hydra.lib.pairs.Second.apply(re.get())))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Function> visit(hydra.core.Function.Lambda l) {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>> rl = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_rl2(
          (l).value,
          path,
          recurse,
          val));
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Function>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Function>(hydra.lib.pairs.First.apply(rl.get()), new hydra.core.Function.Lambda(new hydra.core.Lambda(((l).value).parameter, ((l).value).domain, hydra.lib.pairs.Second.apply(rl.get()))))));
      }
    });
  }
  
  static <T0, T1> hydra.util.Tuple.Tuple2<T1, T0> rewriteAndFoldTermWithPath_dflt(T0 term0, T1 val0) {
    return (hydra.util.Tuple.Tuple2<T1, T0>) ((hydra.util.Tuple.Tuple2<T1, T0>) (new hydra.util.Tuple.Tuple2<T1, T0>(val0, term0)));
  }
  
  static <T0, T1> T1 rewriteAndFoldTermWithPath_rlhs(hydra.core.Application a, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T1>>> recurse, T0 val0) {
    return (((recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(new hydra.accessors.TermAccessor.ApplicationFunction())))).apply(val0)).apply((a).function);
  }
  
  static <T0, T1, T2> T1 rewriteAndFoldTermWithPath_rrhs(hydra.core.Application a, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T1>>> recurse, hydra.util.Tuple.Tuple2<T0, T2> rlhs) {
    return (((recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(new hydra.accessors.TermAccessor.ApplicationArgument())))).apply(hydra.lib.pairs.First.apply(rlhs))).apply((a).argument);
  }
  
  static <T0, T1, T2> T2 rewriteAndFoldTermWithPath_rl(T0 l, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T1, java.util.function.Function<T0, T2>>> recurse, T1 val0) {
    return (((recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(new hydra.accessors.TermAccessor.SumTerm())))).apply(val0)).apply(l);
  }
  
  static <T0, T1, T2> T2 rewriteAndFoldTermWithPath_rr(java.util.List<hydra.accessors.TermAccessor> path, T0 r, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T1, java.util.function.Function<T0, T2>>> recurse, T1 val0) {
    return (((recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(new hydra.accessors.TermAccessor.SumTerm())))).apply(val0)).apply(r);
  }
  
  static <T0, T1, T2> T2 rewriteAndFoldTermWithPath_rf(T0 f, java.util.function.Function<T1, java.util.function.Function<T0, T2>> forFunction, T1 val0) {
    return ((forFunction).apply(val0)).apply(f);
  }
  
  static <T0, T1> T1 rewriteAndFoldTermWithPath_renv(hydra.core.Let l, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T1>>> recurse, T0 val0) {
    return (((recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(new hydra.accessors.TermAccessor.LetBody())))).apply(val0)).apply((l).body);
  }
  
  static <T0, T1, T2> hydra.util.Tuple.Tuple2<T0, java.util.List<T1>> rewriteAndFoldTermWithPath_rbindings(java.util.function.Function<T0, java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<T0, T1>>> forBindingWithAccessor, hydra.core.Let l, hydra.util.Tuple.Tuple2<T0, T2> renv) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>, java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>>>) (r -> (java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>>) (binding -> {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, T1>> rb = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_rb(
          binding,
          forBindingWithAccessor,
          r));
        return (hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>) ((hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>) (new hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>(hydra.lib.pairs.First.apply(rb.get()), hydra.lib.lists.Cons.apply(
          hydra.lib.pairs.Second.apply(rb.get()),
          hydra.lib.pairs.Second.apply(r)))));
      })),
      (hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>) ((hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>) (new hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>(hydra.lib.pairs.First.apply(renv), (java.util.List<T1>) (java.util.List.<T1>of())))),
      (l).bindings);
  }
  
  static <T0, T1, T2, T3> T2 rewriteAndFoldTermWithPath_rb(T0 binding, java.util.function.Function<T1, java.util.function.Function<T0, T2>> forBindingWithAccessor, hydra.util.Tuple.Tuple2<T1, T3> r) {
    return ((forBindingWithAccessor).apply(hydra.lib.pairs.First.apply(r))).apply(binding);
  }
  
  static <T0, T1, T2> hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>> rewriteAndFoldTermWithPath_rr2(java.util.List<T0> els, Integer idx, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T1, java.util.function.Function<T0, hydra.util.Tuple.Tuple2<T1, T2>>>> recurse, T1 val0) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>, java.util.function.Function<T0, hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>>>) (r -> (java.util.function.Function<T0, hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>>) (el -> {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T1, T2>> r2 = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.<T0, T1, java.util.List<T2>, hydra.util.Tuple.Tuple2<T1, T2>>rewriteAndFoldTermWithPath_r2(
          el,
          path,
          r,
          recurse));
        return (hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>) ((hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>) (new hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>(hydra.lib.math.Add.apply(
          hydra.lib.pairs.First.apply(r),
          1), (hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>) ((hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>) (new hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>(hydra.lib.pairs.First.apply(r2.get()), hydra.lib.lists.Cons.apply(
          hydra.lib.pairs.Second.apply(r2.get()),
          hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(r)))))))));
      })),
      (hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>) ((hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>) (new hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>(idx, (hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>) ((hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>) (new hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>(val0, (java.util.List<T2>) (java.util.List.<T2>of()))))))),
      els);
  }
  
  static <T0, T1, T2, T3> T3 rewriteAndFoldTermWithPath_r2(T0 el, java.util.List<hydra.accessors.TermAccessor> path, hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, T2>> r, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T1, java.util.function.Function<T0, T3>>> recurse) {
    return (((recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(new hydra.accessors.TermAccessor.ListElement(hydra.lib.pairs.First.apply(r)))))).apply(hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(r)))).apply(el);
  }
  
  static <T0, T1, T2> hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>> rewriteAndFoldTermWithPath_rr3(Integer idx, java.util.Map<T0, T0> m, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T1, java.util.function.Function<T0, hydra.util.Tuple.Tuple2<T1, T2>>>> recurse, T1 val0) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>>, java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T0>, hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>>>>) (r -> (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T0>, hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>>>) (kv -> {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T1, T2>> rk = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.<T0, T0, T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>, hydra.util.Tuple.Tuple2<T1, T2>>rewriteAndFoldTermWithPath_rk(
          kv,
          path,
          r,
          recurse));
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T1, T2>> rv = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.<T0, T0, hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>, T1, hydra.util.Tuple.Tuple2<T1, T2>, T2>rewriteAndFoldTermWithPath_rv(
          kv,
          path,
          r,
          recurse,
          rk.get()));
        return (hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>>) ((hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>>) (new hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>>(hydra.lib.math.Add.apply(
          hydra.lib.pairs.First.apply(r),
          1), (hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>) ((hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>) (new hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>(hydra.lib.pairs.First.apply(rv.get()), hydra.lib.lists.Cons.apply(
          (hydra.util.Tuple.Tuple2<T2, T2>) ((hydra.util.Tuple.Tuple2<T2, T2>) (new hydra.util.Tuple.Tuple2<T2, T2>(hydra.lib.pairs.Second.apply(rk.get()), hydra.lib.pairs.Second.apply(rv.get())))),
          hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(r)))))))));
      })),
      (hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>>) ((hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>>) (new hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>>(idx, (hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>) ((hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>) (new hydra.util.Tuple.Tuple2<T1, java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>>(val0, (java.util.List<hydra.util.Tuple.Tuple2<T2, T2>>) (java.util.List.<hydra.util.Tuple.Tuple2<T2, T2>>of()))))))),
      hydra.lib.maps.ToList.apply(m));
  }
  
  static <T0, T1, T2, T3, T4> T4 rewriteAndFoldTermWithPath_rk(hydra.util.Tuple.Tuple2<T0, T1> kv, java.util.List<hydra.accessors.TermAccessor> path, hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T2, T3>> r, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T2, java.util.function.Function<T0, T4>>> recurse) {
    return (((recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(new hydra.accessors.TermAccessor.MapKey(hydra.lib.pairs.First.apply(r)))))).apply(hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(r)))).apply(hydra.lib.pairs.First.apply(kv));
  }
  
  static <T0, T1, T2, T3, T4, T5> T4 rewriteAndFoldTermWithPath_rv(hydra.util.Tuple.Tuple2<T0, T1> kv, java.util.List<hydra.accessors.TermAccessor> path, hydra.util.Tuple.Tuple2<Integer, T2> r, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T3, java.util.function.Function<T1, T4>>> recurse, hydra.util.Tuple.Tuple2<T3, T5> rk) {
    return (((recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(new hydra.accessors.TermAccessor.MapValue(hydra.lib.pairs.First.apply(r)))))).apply(hydra.lib.pairs.First.apply(rk))).apply(hydra.lib.pairs.Second.apply(kv));
  }
  
  static <T0, T1, T2, T3> T3 rewriteAndFoldTermWithPath_rf2(hydra.util.Tuple.Tuple2<T0, T1> p, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T2, java.util.function.Function<T0, T3>>> recurse, T2 val0) {
    return (((recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(new hydra.accessors.TermAccessor.ProductTerm(0))))).apply(val0)).apply(hydra.lib.pairs.First.apply(p));
  }
  
  static <T0, T1, T2, T3, T4> T3 rewriteAndFoldTermWithPath_rs(hydra.util.Tuple.Tuple2<T0, T1> p, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T2, java.util.function.Function<T1, T3>>> recurse, hydra.util.Tuple.Tuple2<T2, T4> rf) {
    return (((recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(new hydra.accessors.TermAccessor.ProductTerm(1))))).apply(hydra.lib.pairs.First.apply(rf))).apply(hydra.lib.pairs.Second.apply(p));
  }
  
  static <T0, T1> hydra.util.Tuple.Tuple2<T0, java.util.List<T1>> rewriteAndFoldTermWithPath_rfields(java.util.List<hydra.accessors.TermAccessor> path, hydra.core.Record r, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, T1>>>> recurse, T0 val0) {
    return (((((java.util.function.Function<java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, T1>>>>, java.util.function.Function<java.util.function.Function<java.util.List<T1>, java.util.List<T1>>, java.util.function.Function<T0, java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>, hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>>>>>) (v1 -> (java.util.function.Function<java.util.function.Function<java.util.List<T1>, java.util.List<T1>>, java.util.function.Function<T0, java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>, hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>>>>) (v2 -> (java.util.function.Function<T0, java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>, hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>>>) (v3 -> (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>, hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>>) (v4 -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_forManyWithAccessors(
      path,
      v1,
      v2,
      v3,
      v4)))))).apply(recurse)).apply((java.util.function.Function<java.util.List<T1>, java.util.List<T1>>) (x -> x))).apply(val0)).apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>) (f -> (hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.RecordField((f).name), (f).term)))),
      (r).fields));
  }
  
  static <T0, T1, T2> hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>> rewriteAndFoldTermWithPath_rr4(java.util.Set<T0> els, Integer idx, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T1, java.util.function.Function<T0, hydra.util.Tuple.Tuple2<T1, T2>>>> recurse, T1 val0) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>, java.util.function.Function<T0, hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>>>) (r -> (java.util.function.Function<T0, hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>>) (el -> {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T1, T2>> r2 = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.<T0, T1, java.util.List<T2>, hydra.util.Tuple.Tuple2<T1, T2>>rewriteAndFoldTermWithPath_r22(
          el,
          path,
          r,
          recurse));
        return (hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>) ((hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>) (new hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>(hydra.lib.math.Add.apply(
          hydra.lib.pairs.First.apply(r),
          1), (hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>) ((hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>) (new hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>(hydra.lib.pairs.First.apply(r2.get()), hydra.lib.lists.Cons.apply(
          hydra.lib.pairs.Second.apply(r2.get()),
          hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(r)))))))));
      })),
      (hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>) ((hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>) (new hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>>(idx, (hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>) ((hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>) (new hydra.util.Tuple.Tuple2<T1, java.util.List<T2>>(val0, (java.util.List<T2>) (java.util.List.<T2>of()))))))),
      hydra.lib.sets.ToList.apply(els));
  }
  
  static <T0, T1, T2, T3> T3 rewriteAndFoldTermWithPath_r22(T0 el, java.util.List<hydra.accessors.TermAccessor> path, hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T1, T2>> r, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T1, java.util.function.Function<T0, T3>>> recurse) {
    return (((recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(new hydra.accessors.TermAccessor.SetElement(hydra.lib.pairs.First.apply(r)))))).apply(hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(r)))).apply(el);
  }
  
  static <T0, T1, T2> T2 rewriteAndFoldTermWithPath_re(T0 elm, java.util.function.Function<T1, java.util.function.Function<T0, T2>> forElimination, T1 val) {
    return ((forElimination).apply(val)).apply(elm);
  }
  
  static <T0, T1> T1 rewriteAndFoldTermWithPath_rl2(hydra.core.Lambda l, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T1>>> recurse, T0 val) {
    return (((recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(new hydra.accessors.TermAccessor.LambdaBody())))).apply(val)).apply((l).body);
  }
  
  static <T0> hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination> rewriteAndFoldTermWithPath_r(hydra.core.Elimination elm, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>> recurse, T0 val) {
    return (elm).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination> otherwise(hydra.core.Elimination instance) {
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>(val, elm)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination> visit(hydra.core.Elimination.Union cs) {
        hydra.util.Lazy<hydra.util.Maybe<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>> rmd = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_rmd(
          (cs).value,
          path,
          recurse,
          val));
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T0, java.util.List<hydra.core.Term>>> rcases = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_rcases(
          path,
          (cs).value,
          recurse,
          hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_val1(
            rmd.get(),
            val)));
        return (hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Elimination>(hydra.lib.pairs.First.apply(rcases.get()), new hydra.core.Elimination.Union(new hydra.core.CaseStatement(((cs).value).typeName, hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>, hydra.core.Term>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Term>, hydra.core.Term>) (hydra.lib.pairs.Second::apply)),
          rmd.get()), hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>, hydra.core.Field>) (ft -> new hydra.core.Field(hydra.lib.pairs.First.apply(ft), hydra.lib.pairs.Second.apply(ft))),
          hydra.lib.lists.Zip.apply(
            hydra.lib.lists.Map.apply(
              projected -> projected.name,
              ((cs).value).cases),
            hydra.lib.pairs.Second.apply(rcases.get()))))))));
      }
    });
  }
  
  static <T0, T1> hydra.util.Maybe<T1> rewriteAndFoldTermWithPath_rmd(hydra.core.CaseStatement cs, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T1>>> recurse, T0 val) {
    return hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.core.Term, T1>) (def -> (((recurse).apply(hydra.lib.lists.Concat2.apply(
        path,
        java.util.List.of(new hydra.accessors.TermAccessor.UnionCasesDefault())))).apply(val)).apply(def)),
      (cs).default_);
  }
  
  static <T0, T1> T0 rewriteAndFoldTermWithPath_val1(hydra.util.Maybe<hydra.util.Tuple.Tuple2<T0, T1>> rmd, T0 val) {
    return hydra.lib.maybes.Maybe.apply(
      val,
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, T0>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, T0>) (hydra.lib.pairs.First::apply)),
      rmd);
  }
  
  static <T0, T1> hydra.util.Tuple.Tuple2<T0, java.util.List<T1>> rewriteAndFoldTermWithPath_rcases(java.util.List<hydra.accessors.TermAccessor> path, hydra.core.CaseStatement cs, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, T1>>>> recurse, T0 val1) {
    return (((((java.util.function.Function<java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, T1>>>>, java.util.function.Function<java.util.function.Function<java.util.List<T1>, java.util.List<T1>>, java.util.function.Function<T0, java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>, hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>>>>>) (v1 -> (java.util.function.Function<java.util.function.Function<java.util.List<T1>, java.util.List<T1>>, java.util.function.Function<T0, java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>, hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>>>>) (v2 -> (java.util.function.Function<T0, java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>, hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>>>) (v3 -> (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>, hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>>) (v4 -> hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath_forManyWithAccessors(
      path,
      v1,
      v2,
      v3,
      v4)))))).apply(recurse)).apply((java.util.function.Function<java.util.List<T1>, java.util.List<T1>>) (x -> x))).apply(val1)).apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>) (f -> (hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.UnionCasesBranch((f).name), (f).term)))),
      (cs).cases));
  }
  
  static <T0, T1> T1 rewriteAndFoldTermWithPath_r3(hydra.core.Binding binding, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T1>>> recurse, T0 val) {
    return (((recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(new hydra.accessors.TermAccessor.LetBinding((binding).name))))).apply(val)).apply((binding).term);
  }
  
  static <T0, T1, T2, T3, T4> T4 rewriteAndFoldTermWithPath_rk2(T0 keyAccessor, hydra.util.Tuple.Tuple2<T1, T2> kv, java.util.List<T0> path, java.util.function.Function<java.util.List<T0>, java.util.function.Function<T3, java.util.function.Function<T1, T4>>> recurse, T3 val) {
    return (((recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(keyAccessor)))).apply(val)).apply(hydra.lib.pairs.First.apply(kv));
  }
  
  static <T0, T1, T2, T3, T4, T5> T4 rewriteAndFoldTermWithPath_rv2(hydra.util.Tuple.Tuple2<T0, T1> kv, java.util.List<T2> path, java.util.function.Function<java.util.List<T2>, java.util.function.Function<T3, java.util.function.Function<T1, T4>>> recurse, hydra.util.Tuple.Tuple2<T3, T5> rk, T2 valAccessor) {
    return (((recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(valAccessor)))).apply(hydra.lib.pairs.First.apply(rk))).apply(hydra.lib.pairs.Second.apply(kv));
  }
  
  static <T0, T1, T2> T2 rewriteAndFoldTermWithPath_r4(hydra.core.Field field, java.util.function.Function<hydra.core.Name, T0> mkAccessor, java.util.List<T0> path, java.util.function.Function<java.util.List<T0>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, T2>>> recurse, T1 val) {
    return (((recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of((mkAccessor).apply((field).name))))).apply(val)).apply((field).term);
  }
  
  static <T0, T1, T2, T3> hydra.util.Tuple.Tuple2<T2, java.util.List<T3>> rewriteAndFoldTermWithPath_rr5(java.util.List<hydra.util.Tuple.Tuple2<T0, T1>> accessorTermPairs, java.util.List<T0> path, java.util.function.Function<java.util.List<T0>, java.util.function.Function<T2, java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T2, T3>>>> rec, T2 val) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T2, java.util.List<T3>>, java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, hydra.util.Tuple.Tuple2<T2, java.util.List<T3>>>>) (r -> (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, hydra.util.Tuple.Tuple2<T2, java.util.List<T3>>>) (atp -> {
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<T2, T3>> r2 = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.<T0, T1, T2, java.util.List<T3>, hydra.util.Tuple.Tuple2<T2, T3>>rewriteAndFoldTermWithPath_r23(
          atp,
          path,
          r,
          rec));
        return (hydra.util.Tuple.Tuple2<T2, java.util.List<T3>>) ((hydra.util.Tuple.Tuple2<T2, java.util.List<T3>>) (new hydra.util.Tuple.Tuple2<T2, java.util.List<T3>>(hydra.lib.pairs.First.apply(r2.get()), hydra.lib.lists.Cons.apply(
          hydra.lib.pairs.Second.apply(r2.get()),
          hydra.lib.pairs.Second.apply(r)))));
      })),
      (hydra.util.Tuple.Tuple2<T2, java.util.List<T3>>) ((hydra.util.Tuple.Tuple2<T2, java.util.List<T3>>) (new hydra.util.Tuple.Tuple2<T2, java.util.List<T3>>(val, (java.util.List<T3>) (java.util.List.<T3>of())))),
      accessorTermPairs);
  }
  
  static <T0, T1, T2, T3, T4> T4 rewriteAndFoldTermWithPath_r23(hydra.util.Tuple.Tuple2<T0, T1> atp, java.util.List<T0> path, hydra.util.Tuple.Tuple2<T2, T3> r, java.util.function.Function<java.util.List<T0>, java.util.function.Function<T2, java.util.function.Function<T1, T4>>> rec) {
    return (((rec).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(hydra.lib.pairs.First.apply(atp))))).apply(hydra.lib.pairs.First.apply(r))).apply(hydra.lib.pairs.Second.apply(atp));
  }
  
  static <T0, T1, T2, T3> T3 rewriteAndFoldTermWithPath_r5(T0 accessor, java.util.List<T0> path, java.util.function.Function<java.util.List<T0>, java.util.function.Function<T1, java.util.function.Function<T2, T3>>> rec, T2 term, T1 val) {
    return (((rec).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(accessor)))).apply(val)).apply(term);
  }
  
  static hydra.core.Term rewriteTerm(java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> f, hydra.core.Term term0) {
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> fsub = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse2 -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> {
      java.util.function.Function<hydra.core.Field, hydra.core.Field> forField = (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (f2 -> new hydra.core.Field((f2).name, (recurse2).apply((f2).term)));
      java.util.function.Function<hydra.core.Elimination, hydra.core.Elimination> forElimination = (java.util.function.Function<hydra.core.Elimination, hydra.core.Elimination>) (elm -> (elm).accept(new hydra.core.Elimination.PartialVisitor<>() {
        @Override
        public hydra.core.Elimination visit(hydra.core.Elimination.Record p) {
          return new hydra.core.Elimination.Record((p).value);
        }
        
        @Override
        public hydra.core.Elimination visit(hydra.core.Elimination.Union cs) {
          return new hydra.core.Elimination.Union(new hydra.core.CaseStatement(((cs).value).typeName, hydra.lib.maybes.Map.apply(
            recurse2,
            ((cs).value).default_), hydra.lib.lists.Map.apply(
            forField,
            ((cs).value).cases)));
        }
        
        @Override
        public hydra.core.Elimination visit(hydra.core.Elimination.Wrap name) {
          return new hydra.core.Elimination.Wrap((name).value);
        }
      }));
      java.util.function.Function<hydra.core.Function, hydra.core.Function> forFunction = (java.util.function.Function<hydra.core.Function, hydra.core.Function>) (fun -> (fun).accept(new hydra.core.Function.PartialVisitor<>() {
        @Override
        public hydra.core.Function visit(hydra.core.Function.Elimination elm) {
          return new hydra.core.Function.Elimination((forElimination).apply((elm).value));
        }
        
        @Override
        public hydra.core.Function visit(hydra.core.Function.Lambda l) {
          return new hydra.core.Function.Lambda(new hydra.core.Lambda(((l).value).parameter, ((l).value).domain, (recurse2).apply(((l).value).body)));
        }
        
        @Override
        public hydra.core.Function visit(hydra.core.Function.Primitive name) {
          return new hydra.core.Function.Primitive((name).value);
        }
      }));
      java.util.function.Function<hydra.core.Let, hydra.core.Let> forLet = (java.util.function.Function<hydra.core.Let, hydra.core.Let>) (lt -> {
        java.util.function.Function<hydra.core.Binding, hydra.core.Binding> mapBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, (recurse2).apply((b).term), (b).type));
        return new hydra.core.Let(hydra.lib.lists.Map.apply(
          mapBinding,
          (lt).bindings), (recurse2).apply((lt).body));
      });
      java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, java.util.Map<hydra.core.Term, hydra.core.Term>> forMap = (java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, java.util.Map<hydra.core.Term, hydra.core.Term>>) (m -> {
        java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>> forPair = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>) (p -> (hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>((recurse2).apply(hydra.lib.pairs.First.apply(p)), (recurse2).apply(hydra.lib.pairs.Second.apply(p))))));
        return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          forPair,
          hydra.lib.maps.ToList.apply(m)));
      });
      return (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term visit(hydra.core.Term.Annotated at) {
          return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm((recurse2).apply(((at).value).body), ((at).value).annotation));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Application a) {
          return new hydra.core.Term.Application(new hydra.core.Application((recurse2).apply(((a).value).function), (recurse2).apply(((a).value).argument)));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Either e) {
          return new hydra.core.Term.Either(hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (l -> (hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left((recurse2).apply(l))))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (r -> (hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right((recurse2).apply(r))))),
            (e).value));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Function fun) {
          return new hydra.core.Term.Function((forFunction).apply((fun).value));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Let lt) {
          return new hydra.core.Term.Let((forLet).apply((lt).value));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.List els) {
          return new hydra.core.Term.List(hydra.lib.lists.Map.apply(
            recurse2,
            (els).value));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Literal v) {
          return new hydra.core.Term.Literal((v).value);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Map m) {
          return new hydra.core.Term.Map((forMap).apply((m).value));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Maybe m) {
          return new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
            recurse2,
            (m).value));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Pair p) {
          return new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>((recurse2).apply(hydra.lib.pairs.First.apply((p).value)), (recurse2).apply(hydra.lib.pairs.Second.apply((p).value))))));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Record r) {
          return new hydra.core.Term.Record(new hydra.core.Record(((r).value).typeName, hydra.lib.lists.Map.apply(
            forField,
            ((r).value).fields)));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Set s) {
          return new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
            recurse2,
            hydra.lib.sets.ToList.apply((s).value))));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
          return new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm((recurse2).apply(((tt).value).body), ((tt).value).type));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
          return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(((ta).value).parameter, (recurse2).apply(((ta).value).body)));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Union i) {
          return new hydra.core.Term.Union(new hydra.core.Injection(((i).value).typeName, (forField).apply(((i).value).field)));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Unit ignored) {
          return new hydra.core.Term.Unit();
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Variable v) {
          return new hydra.core.Term.Variable((v).value);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Wrap wt) {
          return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(((wt).value).typeName, (recurse2).apply(((wt).value).body)));
        }
      });
    }));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, hydra.core.Term>> recurse = new java.util.concurrent.atomic.AtomicReference<>();
    recurse.set((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v1 -> ((f).apply((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v12 -> ((fsub).apply(recurse.get())).apply(v12)))).apply(v1)));
    return (recurse.get()).apply(term0);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> rewriteTermM(java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>> f, hydra.core.Term term0) {
    return hydra.rewriting.Rewriting.<T0>rewriteTermM_recurse(
      f,
      term0);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> rewriteTermM_fsub(java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>> recurse, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Field, hydra.compute.Flow<T0, hydra.core.Field>> forField = (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<T0, hydra.core.Field>>) (v1 -> hydra.rewriting.Rewriting.<T0>rewriteTermM_forField(
      recurse,
      v1));
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Annotated at) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(((at).value).body),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>) (ex -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(ex, ((at).value).annotation)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Application app) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(((app).value).function),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>) (lhs -> hydra.lib.flows.Bind.apply(
            (recurse).apply(((app).value).argument),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>) (rhs -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application(lhs, rhs)))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Either e) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Either<hydra.core.Term, hydra.core.Term>>>) (l -> hydra.lib.flows.Map.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (x -> (hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(x)))),
              (recurse).apply(l))),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Either<hydra.core.Term, hydra.core.Term>>>) (r -> hydra.lib.flows.Map.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (x -> (hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(x)))),
              (recurse).apply(r))),
            (e).value),
          (java.util.function.Function<hydra.util.Either<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<T0, hydra.core.Term>>) (re -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Either(re))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Function fun) {
        return hydra.lib.flows.Bind.apply(
          hydra.rewriting.Rewriting.<T0>rewriteTermM_forFun(
            (java.util.function.Function<hydra.core.Elimination, hydra.compute.Flow<T0, hydra.core.Function>>) (v1 -> hydra.rewriting.Rewriting.<T0>rewriteTermM_forElm(
              forField,
              recurse,
              v1)),
            recurse,
            (fun).value),
          (java.util.function.Function<hydra.core.Function, hydra.compute.Flow<T0, hydra.core.Term>>) (rfun -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Function(rfun))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Let lt) {
        java.util.List<hydra.core.Binding> bindings = ((lt).value).bindings;
        hydra.core.Term env = ((lt).value).body;
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<T0, hydra.core.Binding>>) (v1 -> hydra.rewriting.Rewriting.<T0>rewriteTermM_mapBinding(
              recurse,
              v1)),
            bindings),
          (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.compute.Flow<T0, hydra.core.Term>>) (rbindings -> hydra.lib.flows.Bind.apply(
            (recurse).apply(env),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>) (renv -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Let(new hydra.core.Let(rbindings, renv)))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.List els) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            recurse,
            (els).value),
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<T0, hydra.core.Term>>) (rels -> hydra.lib.flows.Pure.apply(new hydra.core.Term.List(rels))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Literal v) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Literal((v).value));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Map m) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>>) (v1 -> hydra.rewriting.Rewriting.rewriteTermM_forPair(
              recurse,
              v1)),
            hydra.lib.maps.ToList.apply((m).value)),
          (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>, hydra.compute.Flow<T0, hydra.core.Term>>) (pairs -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(pairs)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapMaybe.apply(
            recurse,
            (m).value),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.compute.Flow<T0, hydra.core.Term>>) (rm -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Maybe(rm))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Pair p) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(hydra.lib.pairs.First.apply((p).value)),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>) (rf -> hydra.lib.flows.Bind.apply(
            (recurse).apply(hydra.lib.pairs.Second.apply((p).value)),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>) (rs -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(rf, rs)))))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Record r) {
        java.util.List<hydra.core.Field> fields = ((r).value).fields;
        hydra.core.Name n = ((r).value).typeName;
        return hydra.lib.flows.Map.apply(
          (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.core.Term>) (rfields -> new hydra.core.Term.Record(new hydra.core.Record(n, rfields))),
          hydra.lib.flows.MapList.apply(
            forField,
            fields));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Set s) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            recurse,
            hydra.lib.sets.ToList.apply((s).value)),
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<T0, hydra.core.Term>>) (rlist -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(rlist)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.TypeApplication tt) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(((tt).value).body),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>) (t -> hydra.lib.flows.Pure.apply(new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(t, ((tt).value).type)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.TypeLambda tl) {
        hydra.core.Term body = ((tl).value).body;
        hydra.core.Name v = ((tl).value).parameter;
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(body),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>) (rbody -> hydra.lib.flows.Pure.apply(new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(v, rbody)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Union i) {
        hydra.core.Field field = ((i).value).field;
        hydra.core.Name n = ((i).value).typeName;
        return hydra.lib.flows.Map.apply(
          (java.util.function.Function<hydra.core.Field, hydra.core.Term>) (rfield -> new hydra.core.Term.Union(new hydra.core.Injection(n, rfield))),
          (forField).apply(field));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Unit ignored) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Unit());
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Variable v) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Variable((v).value));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Wrap wt) {
        hydra.core.Name name = ((wt).value).typeName;
        hydra.core.Term t = ((wt).value).body;
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(t),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>) (rt -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(name, rt)))));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> rewriteTermM_recurse(java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>> f, hydra.core.Term v1) {
    return ((f).apply((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>) (v12 -> hydra.rewriting.Rewriting.<T0>rewriteTermM_fsub(
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>) (v13 -> hydra.rewriting.Rewriting.<T0>rewriteTermM_recurse(
        f,
        v13)),
      v12)))).apply(v1);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Field> rewriteTermM_forField(java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>> recurse, hydra.core.Field field) {
    return hydra.lib.flows.Bind.apply(
      (recurse).apply((field).term),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Field>>) (t -> hydra.lib.flows.Pure.apply(new hydra.core.Field((field).name, t))));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T2, T2>> rewriteTermM_forPair(java.util.function.Function<T0, hydra.compute.Flow<T1, T2>> recurse, hydra.util.Tuple.Tuple2<T0, T0> kv) {
    return hydra.lib.flows.Bind.apply(
      (recurse).apply(hydra.lib.pairs.First.apply(kv)),
      (java.util.function.Function<T2, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T2, T2>>>) (k -> hydra.lib.flows.Bind.apply(
        (recurse).apply(hydra.lib.pairs.Second.apply(kv)),
        (java.util.function.Function<T2, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T2, T2>>>) (v -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T2, T2>) ((hydra.util.Tuple.Tuple2<T2, T2>) (new hydra.util.Tuple.Tuple2<T2, T2>(k, v))))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Binding> rewriteTermM_mapBinding(java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>> recurse, hydra.core.Binding b) {
    return hydra.lib.flows.Bind.apply(
      (recurse).apply((b).term),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Binding>>) (v -> hydra.lib.flows.Pure.apply(new hydra.core.Binding((b).name, v, (b).type))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Function> rewriteTermM_forElm(java.util.function.Function<hydra.core.Field, hydra.compute.Flow<T0, hydra.core.Field>> forField, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>> recurse, hydra.core.Elimination e) {
    return (e).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Function> visit(hydra.core.Elimination.Record p) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record((p).value)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Function> visit(hydra.core.Elimination.Union cs) {
        java.util.List<hydra.core.Field> cases = ((cs).value).cases;
        hydra.util.Maybe<hydra.core.Term> def = ((cs).value).default_;
        hydra.core.Name n = ((cs).value).typeName;
        return hydra.lib.flows.Bind.apply(
          hydra.lib.maybes.Maybe.apply(
            hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.Term>>>) (t -> hydra.lib.flows.Map.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maybes.Pure::apply),
              (recurse).apply(t))),
            def),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.compute.Flow<T0, hydra.core.Function>>) (rdef -> hydra.lib.flows.Map.apply(
            (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.core.Function>) (rcases -> new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(n, rdef, rcases)))),
            hydra.lib.flows.MapList.apply(
              forField,
              cases))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Function> visit(hydra.core.Elimination.Wrap name) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap((name).value)));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Function> rewriteTermM_forFun(java.util.function.Function<hydra.core.Elimination, hydra.compute.Flow<T0, hydra.core.Function>> forElm, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>> recurse, hydra.core.Function fun) {
    return (fun).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Function> visit(hydra.core.Function.Elimination e) {
        return (forElm).apply((e).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Function> visit(hydra.core.Function.Lambda l) {
        hydra.core.Term body = ((l).value).body;
        hydra.util.Maybe<hydra.core.Type> d = ((l).value).domain;
        hydra.core.Name v = ((l).value).parameter;
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(body),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Function>>) (rbody -> hydra.lib.flows.Pure.apply(new hydra.core.Function.Lambda(new hydra.core.Lambda(v, d, rbody)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Function> visit(hydra.core.Function.Primitive name) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Function.Primitive((name).value));
      }
    });
  }
  
  static <T0> hydra.core.Term rewriteTermWithContext(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.core.Term>>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> f, T0 cx0, hydra.core.Term term0) {
    return hydra.rewriting.Rewriting.<T0>rewriteTermWithContext_rewrite(
      f,
      cx0,
      term0);
  }
  
  static <T0> hydra.core.Term rewriteTermWithContext_forSubterms(java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.core.Term>> recurse0, T0 cx, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Term, hydra.core.Term> recurse = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v1 -> ((recurse0).apply(cx)).apply(v1));
    java.util.function.Function<hydra.core.Field, hydra.core.Field> forField = (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (field -> new hydra.core.Field((field).name, (recurse).apply((field).term)));
    java.util.function.Function<hydra.core.Elimination, hydra.core.Elimination> forElimination = (java.util.function.Function<hydra.core.Elimination, hydra.core.Elimination>) (elm -> (elm).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public hydra.core.Elimination visit(hydra.core.Elimination.Record p) {
        return new hydra.core.Elimination.Record((p).value);
      }
      
      @Override
      public hydra.core.Elimination visit(hydra.core.Elimination.Union cs) {
        return new hydra.core.Elimination.Union(new hydra.core.CaseStatement(((cs).value).typeName, hydra.lib.maybes.Map.apply(
          recurse,
          ((cs).value).default_), hydra.lib.lists.Map.apply(
          forField,
          ((cs).value).cases)));
      }
      
      @Override
      public hydra.core.Elimination visit(hydra.core.Elimination.Wrap name) {
        return new hydra.core.Elimination.Wrap((name).value);
      }
    }));
    java.util.function.Function<hydra.core.Function, hydra.core.Function> forFunction = (java.util.function.Function<hydra.core.Function, hydra.core.Function>) (fun -> (fun).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.core.Function visit(hydra.core.Function.Elimination elm) {
        return new hydra.core.Function.Elimination((forElimination).apply((elm).value));
      }
      
      @Override
      public hydra.core.Function visit(hydra.core.Function.Lambda l) {
        return new hydra.core.Function.Lambda(new hydra.core.Lambda(((l).value).parameter, ((l).value).domain, (recurse).apply(((l).value).body)));
      }
      
      @Override
      public hydra.core.Function visit(hydra.core.Function.Primitive name) {
        return new hydra.core.Function.Primitive((name).value);
      }
    }));
    java.util.function.Function<hydra.core.Let, hydra.core.Let> forLet = (java.util.function.Function<hydra.core.Let, hydra.core.Let>) (lt -> {
      java.util.function.Function<hydra.core.Binding, hydra.core.Binding> mapBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, (recurse).apply((b).term), (b).type));
      return new hydra.core.Let(hydra.lib.lists.Map.apply(
        mapBinding,
        (lt).bindings), (recurse).apply((lt).body));
    });
    java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, java.util.Map<hydra.core.Term, hydra.core.Term>> forMap = (java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, java.util.Map<hydra.core.Term, hydra.core.Term>>) (m -> {
      java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>> forPair = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>) (p -> (hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>((recurse).apply(hydra.lib.pairs.First.apply(p)), (recurse).apply(hydra.lib.pairs.Second.apply(p))))));
      return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        forPair,
        hydra.lib.maps.ToList.apply(m)));
    });
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm((recurse).apply(((at).value).body), ((at).value).annotation));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Application a) {
        return new hydra.core.Term.Application(new hydra.core.Application((recurse).apply(((a).value).function), (recurse).apply(((a).value).argument)));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Either e) {
        return new hydra.core.Term.Either(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (l -> (hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left((recurse).apply(l))))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (r -> (hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right((recurse).apply(r))))),
          (e).value));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Function fun) {
        return new hydra.core.Term.Function((forFunction).apply((fun).value));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        return new hydra.core.Term.Let((forLet).apply((lt).value));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.List els) {
        return new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          recurse,
          (els).value));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Literal v) {
        return new hydra.core.Term.Literal((v).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Map m) {
        return new hydra.core.Term.Map((forMap).apply((m).value));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Maybe m) {
        return new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
          recurse,
          (m).value));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Pair p) {
        return new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>((recurse).apply(hydra.lib.pairs.First.apply((p).value)), (recurse).apply(hydra.lib.pairs.Second.apply((p).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Record r) {
        return new hydra.core.Term.Record(new hydra.core.Record(((r).value).typeName, hydra.lib.lists.Map.apply(
          forField,
          ((r).value).fields)));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Set s) {
        return new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
          recurse,
          hydra.lib.sets.ToList.apply((s).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
        return new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm((recurse).apply(((tt).value).body), ((tt).value).type));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
        return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(((ta).value).parameter, (recurse).apply(((ta).value).body)));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Union i) {
        return new hydra.core.Term.Union(new hydra.core.Injection(((i).value).typeName, (forField).apply(((i).value).field)));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Unit ignored) {
        return new hydra.core.Term.Unit();
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable v) {
        return new hydra.core.Term.Variable((v).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Wrap wt) {
        return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(((wt).value).typeName, (recurse).apply(((wt).value).body)));
      }
    });
  }
  
  static <T0> hydra.core.Term rewriteTermWithContext_rewrite(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.core.Term>>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> f, T0 cx, hydra.core.Term term) {
    return (((f).apply((java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v2 -> hydra.rewriting.Rewriting.<T0>rewriteTermWithContext_forSubterms(
      (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v22 -> hydra.rewriting.Rewriting.<T0>rewriteTermWithContext_rewrite(
        f,
        v12,
        v22))),
      v1,
      v2))))).apply(cx)).apply(term);
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.core.Term> rewriteTermWithContextM(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>>> f, T0 cx0, hydra.core.Term term0) {
    return hydra.rewriting.Rewriting.<T0, T1>rewriteTermWithContextM_rewrite(
      f,
      cx0,
      term0);
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.core.Term> rewriteTermWithContextM_forSubterms(java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>> recurse0, T0 cx, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>> recurse = (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>) (v1 -> hydra.rewriting.Rewriting.rewriteTermWithContextM_recurse(
      cx,
      recurse0,
      v1));
    java.util.function.Function<hydra.core.Field, hydra.compute.Flow<T1, hydra.core.Field>> forField = (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<T1, hydra.core.Field>>) (v1 -> hydra.rewriting.Rewriting.<T1>rewriteTermWithContextM_forField(
      recurse,
      v1));
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Annotated at) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(((at).value).body),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>) (ex -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(ex, ((at).value).annotation)))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Application app) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(((app).value).function),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>) (lhs -> hydra.lib.flows.Bind.apply(
            (recurse).apply(((app).value).argument),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>) (rhs -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application(lhs, rhs)))))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Either e) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Either<hydra.core.Term, hydra.core.Term>>>) (l -> hydra.lib.flows.Map.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (x -> (hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(x)))),
              (recurse).apply(l))),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.util.Either<hydra.core.Term, hydra.core.Term>>>) (r -> hydra.lib.flows.Map.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (x -> (hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(x)))),
              (recurse).apply(r))),
            (e).value),
          (java.util.function.Function<hydra.util.Either<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<T1, hydra.core.Term>>) (re -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Either(re))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Function fun) {
        return hydra.lib.flows.Bind.apply(
          hydra.rewriting.Rewriting.<T1>rewriteTermWithContextM_forFunction(
            (java.util.function.Function<hydra.core.Elimination, hydra.compute.Flow<T1, hydra.core.Function>>) (v1 -> hydra.rewriting.Rewriting.<T1>rewriteTermWithContextM_forElimination(
              forField,
              recurse,
              v1)),
            recurse,
            (fun).value),
          (java.util.function.Function<hydra.core.Function, hydra.compute.Flow<T1, hydra.core.Term>>) (rfun -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Function(rfun))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Let lt) {
        java.util.List<hydra.core.Binding> bindings = ((lt).value).bindings;
        hydra.core.Term body = ((lt).value).body;
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<T1, hydra.core.Binding>>) (v1 -> hydra.rewriting.Rewriting.<T1>rewriteTermWithContextM_mapBinding(
              recurse,
              v1)),
            bindings),
          (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.compute.Flow<T1, hydra.core.Term>>) (rbindings -> hydra.lib.flows.Bind.apply(
            (recurse).apply(body),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>) (rbody -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Let(new hydra.core.Let(rbindings, rbody)))))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.List els) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            recurse,
            (els).value),
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<T1, hydra.core.Term>>) (rels -> hydra.lib.flows.Pure.apply(new hydra.core.Term.List(rels))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Literal v) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Literal((v).value));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Map m) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>>) (v1 -> hydra.rewriting.Rewriting.rewriteTermWithContextM_forPair(
              recurse,
              v1)),
            hydra.lib.maps.ToList.apply((m).value)),
          (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>, hydra.compute.Flow<T1, hydra.core.Term>>) (pairs -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(pairs)))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapMaybe.apply(
            recurse,
            (m).value),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.compute.Flow<T1, hydra.core.Term>>) (rm -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Maybe(rm))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Pair p) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(hydra.lib.pairs.First.apply((p).value)),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>) (rfirst -> hydra.lib.flows.Bind.apply(
            (recurse).apply(hydra.lib.pairs.Second.apply((p).value)),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>) (rsecond -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(rfirst, rsecond)))))))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Record r) {
        java.util.List<hydra.core.Field> fields = ((r).value).fields;
        hydra.core.Name n = ((r).value).typeName;
        return hydra.lib.flows.Map.apply(
          (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.core.Term>) (rfields -> new hydra.core.Term.Record(new hydra.core.Record(n, rfields))),
          hydra.lib.flows.MapList.apply(
            forField,
            fields));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Set s) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            recurse,
            hydra.lib.sets.ToList.apply((s).value)),
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<T1, hydra.core.Term>>) (rlist -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(rlist)))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.TypeApplication tt) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(((tt).value).body),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>) (t -> hydra.lib.flows.Pure.apply(new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(t, ((tt).value).type)))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.TypeLambda tl) {
        hydra.core.Term body = ((tl).value).body;
        hydra.core.Name v = ((tl).value).parameter;
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(body),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>) (rbody -> hydra.lib.flows.Pure.apply(new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(v, rbody)))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Union i) {
        hydra.core.Field field = ((i).value).field;
        hydra.core.Name n = ((i).value).typeName;
        return hydra.lib.flows.Map.apply(
          (java.util.function.Function<hydra.core.Field, hydra.core.Term>) (rfield -> new hydra.core.Term.Union(new hydra.core.Injection(n, rfield))),
          (forField).apply(field));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Unit ignored) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Unit());
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Variable v) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Variable((v).value));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Wrap wt) {
        hydra.core.Name name = ((wt).value).typeName;
        hydra.core.Term t = ((wt).value).body;
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(t),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>) (rt -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(name, rt)))));
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.core.Term> rewriteTermWithContextM_rewrite(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>>> f, T0 cx, hydra.core.Term term) {
    return (((f).apply((java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>) (v2 -> hydra.rewriting.Rewriting.<T0, T1>rewriteTermWithContextM_forSubterms(
      (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>) (v22 -> hydra.rewriting.Rewriting.<T0, T1>rewriteTermWithContextM_rewrite(
        f,
        v12,
        v22))),
      v1,
      v2))))).apply(cx)).apply(term);
  }
  
  static <T0, T1, T2> T2 rewriteTermWithContextM_recurse(T0 cx, java.util.function.Function<T0, java.util.function.Function<T1, T2>> recurse0, T1 v1) {
    return ((recurse0).apply(cx)).apply(v1);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Field> rewriteTermWithContextM_forField(java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>> recurse, hydra.core.Field field) {
    return hydra.lib.flows.Bind.apply(
      (recurse).apply((field).term),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Field>>) (t -> hydra.lib.flows.Pure.apply(new hydra.core.Field((field).name, t))));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T2, T2>> rewriteTermWithContextM_forPair(java.util.function.Function<T0, hydra.compute.Flow<T1, T2>> recurse, hydra.util.Tuple.Tuple2<T0, T0> kv) {
    return hydra.lib.flows.Bind.apply(
      (recurse).apply(hydra.lib.pairs.First.apply(kv)),
      (java.util.function.Function<T2, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T2, T2>>>) (k -> hydra.lib.flows.Bind.apply(
        (recurse).apply(hydra.lib.pairs.Second.apply(kv)),
        (java.util.function.Function<T2, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T2, T2>>>) (v -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T2, T2>) ((hydra.util.Tuple.Tuple2<T2, T2>) (new hydra.util.Tuple.Tuple2<T2, T2>(k, v))))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Function> rewriteTermWithContextM_forElimination(java.util.function.Function<hydra.core.Field, hydra.compute.Flow<T0, hydra.core.Field>> forField, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>> recurse, hydra.core.Elimination e) {
    return (e).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Function> visit(hydra.core.Elimination.Record p) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record((p).value)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Function> visit(hydra.core.Elimination.Union cs) {
        java.util.List<hydra.core.Field> cases = ((cs).value).cases;
        hydra.util.Maybe<hydra.core.Term> def = ((cs).value).default_;
        hydra.core.Name n = ((cs).value).typeName;
        return hydra.lib.flows.Bind.apply(
          hydra.lib.maybes.Maybe.apply(
            hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.Term>>>) (t -> hydra.lib.flows.Map.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maybes.Pure::apply),
              (recurse).apply(t))),
            def),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.compute.Flow<T0, hydra.core.Function>>) (rdef -> hydra.lib.flows.Map.apply(
            (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.core.Function>) (rcases -> new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(n, rdef, rcases)))),
            hydra.lib.flows.MapList.apply(
              forField,
              cases))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Function> visit(hydra.core.Elimination.Wrap name) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap((name).value)));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Function> rewriteTermWithContextM_forFunction(java.util.function.Function<hydra.core.Elimination, hydra.compute.Flow<T0, hydra.core.Function>> forElimination, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>> recurse, hydra.core.Function fun) {
    return (fun).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Function> visit(hydra.core.Function.Elimination e) {
        return (forElimination).apply((e).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Function> visit(hydra.core.Function.Lambda l) {
        hydra.core.Term body = ((l).value).body;
        hydra.util.Maybe<hydra.core.Type> d = ((l).value).domain;
        hydra.core.Name v = ((l).value).parameter;
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(body),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Function>>) (rbody -> hydra.lib.flows.Pure.apply(new hydra.core.Function.Lambda(new hydra.core.Lambda(v, d, rbody)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Function> visit(hydra.core.Function.Primitive name) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Function.Primitive((name).value));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Binding> rewriteTermWithContextM_mapBinding(java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>> recurse, hydra.core.Binding b) {
    return hydra.lib.flows.Bind.apply(
      (recurse).apply((b).term),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Binding>>) (v -> hydra.lib.flows.Pure.apply(new hydra.core.Binding((b).name, v, (b).type))));
  }
  
  static hydra.core.Type rewriteType(java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>> f, hydra.core.Type typ0) {
    java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>> fsub = (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse2 -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> {
      java.util.function.Function<hydra.core.FieldType, hydra.core.FieldType> forField = (java.util.function.Function<hydra.core.FieldType, hydra.core.FieldType>) (field -> new hydra.core.FieldType((field).name, (recurse2).apply((field).type)));
      return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.core.Type visit(hydra.core.Type.Annotated at) {
          return new hydra.core.Type.Annotated(new hydra.core.AnnotatedType((recurse2).apply(((at).value).body), ((at).value).annotation));
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Application app) {
          return new hydra.core.Type.Application(new hydra.core.ApplicationType((recurse2).apply(((app).value).function), (recurse2).apply(((app).value).argument)));
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Either et) {
          return new hydra.core.Type.Either(new hydra.core.EitherType((recurse2).apply(((et).value).left), (recurse2).apply(((et).value).right)));
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Pair pt) {
          return new hydra.core.Type.Pair(new hydra.core.PairType((recurse2).apply(((pt).value).first), (recurse2).apply(((pt).value).second)));
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Function fun) {
          return new hydra.core.Type.Function(new hydra.core.FunctionType((recurse2).apply(((fun).value).domain), (recurse2).apply(((fun).value).codomain)));
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Forall lt) {
          return new hydra.core.Type.Forall(new hydra.core.ForallType(((lt).value).parameter, (recurse2).apply(((lt).value).body)));
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.List t) {
          return new hydra.core.Type.List((recurse2).apply((t).value));
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Literal lt) {
          return new hydra.core.Type.Literal((lt).value);
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Map mt) {
          return new hydra.core.Type.Map(new hydra.core.MapType((recurse2).apply(((mt).value).keys), (recurse2).apply(((mt).value).values)));
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Maybe t) {
          return new hydra.core.Type.Maybe((recurse2).apply((t).value));
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Record rt) {
          return new hydra.core.Type.Record(new hydra.core.RowType(((rt).value).typeName, hydra.lib.lists.Map.apply(
            forField,
            ((rt).value).fields)));
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Set t) {
          return new hydra.core.Type.Set((recurse2).apply((t).value));
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Union rt) {
          return new hydra.core.Type.Union(new hydra.core.RowType(((rt).value).typeName, hydra.lib.lists.Map.apply(
            forField,
            ((rt).value).fields)));
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Unit ignored) {
          return new hydra.core.Type.Unit();
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Variable v) {
          return new hydra.core.Type.Variable((v).value);
        }
        
        @Override
        public hydra.core.Type visit(hydra.core.Type.Wrap wt) {
          return new hydra.core.Type.Wrap(new hydra.core.WrappedType(((wt).value).typeName, (recurse2).apply(((wt).value).body)));
        }
      });
    }));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, hydra.core.Type>> recurse = new java.util.concurrent.atomic.AtomicReference<>();
    recurse.set((java.util.function.Function<hydra.core.Type, hydra.core.Type>) (v1 -> ((f).apply((java.util.function.Function<hydra.core.Type, hydra.core.Type>) (v12 -> ((fsub).apply(recurse.get())).apply(v12)))).apply(v1)));
    return (recurse.get()).apply(typ0);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> rewriteTypeM(java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>> f, hydra.core.Type typ0) {
    return hydra.rewriting.Rewriting.<T0>rewriteTypeM_recurse(
      f,
      typ0);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> rewriteTypeM_fsub(java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>> recurse, hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Annotated at) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(((at).value).body),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (t -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(t, ((at).value).annotation)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Application at) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(((at).value).function),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (lhs -> hydra.lib.flows.Bind.apply(
            (recurse).apply(((at).value).argument),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (rhs -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Application(new hydra.core.ApplicationType(lhs, rhs)))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Either et) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(((et).value).left),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (left -> hydra.lib.flows.Bind.apply(
            (recurse).apply(((et).value).right),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (right -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Either(new hydra.core.EitherType(left, right)))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Pair pt) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(((pt).value).first),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (pairFirst -> hydra.lib.flows.Bind.apply(
            (recurse).apply(((pt).value).second),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (pairSecond -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Pair(new hydra.core.PairType(pairFirst, pairSecond)))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Function ft) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(((ft).value).domain),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (dom -> hydra.lib.flows.Bind.apply(
            (recurse).apply(((ft).value).codomain),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (cod -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod)))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Forall ft) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(((ft).value).body),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (b -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Forall(new hydra.core.ForallType(((ft).value).parameter, b)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.List t) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply((t).value),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (rt -> hydra.lib.flows.Pure.apply(new hydra.core.Type.List(rt))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Literal lt) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Type.Literal((lt).value));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Map mt) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(((mt).value).keys),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (kt -> hydra.lib.flows.Bind.apply(
            (recurse).apply(((mt).value).values),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (vt -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Map(new hydra.core.MapType(kt, vt)))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Maybe t) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply((t).value),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (rt -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Maybe(rt))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Record rt) {
        java.util.List<hydra.core.FieldType> fields = ((rt).value).fields;
        hydra.core.Name name = ((rt).value).typeName;
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<T0, hydra.core.FieldType>>) (v1 -> hydra.rewriting.Rewriting.<T0>rewriteTypeM_forField(
              recurse,
              v1)),
            fields),
          (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.compute.Flow<T0, hydra.core.Type>>) (rfields -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Record(new hydra.core.RowType(name, rfields)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Set t) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply((t).value),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (rt -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Set(rt))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Union rt) {
        java.util.List<hydra.core.FieldType> fields = ((rt).value).fields;
        hydra.core.Name name = ((rt).value).typeName;
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.core.FieldType, hydra.compute.Flow<T0, hydra.core.FieldType>>) (v1 -> hydra.rewriting.Rewriting.<T0>rewriteTypeM_forField2(
              recurse,
              v1)),
            fields),
          (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.compute.Flow<T0, hydra.core.Type>>) (rfields -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Union(new hydra.core.RowType(name, rfields)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Unit ignored) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Type.Unit());
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Variable v) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Type.Variable((v).value));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Wrap wt) {
        return hydra.lib.flows.Bind.apply(
          (recurse).apply(((wt).value).body),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (t -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Wrap(new hydra.core.WrappedType(((wt).value).typeName, t)))));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> rewriteTypeM_recurse(java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>> f, hydra.core.Type v1) {
    return ((f).apply((java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (v12 -> hydra.rewriting.Rewriting.<T0>rewriteTypeM_fsub(
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (v13 -> hydra.rewriting.Rewriting.<T0>rewriteTypeM_recurse(
        f,
        v13)),
      v12)))).apply(v1);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.FieldType> rewriteTypeM_forField(java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>> recurse, hydra.core.FieldType f) {
    return hydra.lib.flows.Bind.apply(
      (recurse).apply((f).type),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.FieldType>>) (t -> hydra.lib.flows.Pure.apply(new hydra.core.FieldType((f).name, t))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.FieldType> rewriteTypeM_forField2(java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>> recurse, hydra.core.FieldType f) {
    return hydra.lib.flows.Bind.apply(
      (recurse).apply((f).type),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.FieldType>>) (t -> hydra.lib.flows.Pure.apply(new hydra.core.FieldType((f).name, t))));
  }
  
  static hydra.core.Term simplifyTerm(hydra.core.Term term) {
    return hydra.rewriting.Rewriting.rewriteTerm(
      (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v2 -> hydra.rewriting.Rewriting.simplifyTerm_simplify(
        hydra.rewriting.Rewriting::deannotateTerm,
        hydra.rewriting.Rewriting::freeVariablesInTerm,
        hydra.rewriting.Rewriting::simplifyTerm,
        (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>) (p0 -> p1 -> p2 -> hydra.rewriting.Rewriting.substituteVariable(
          p0,
          p1,
          p2)),
        v1,
        v2))),
      term);
  }
  
  static <T0> T0 simplifyTerm_simplify(java.util.function.Function<hydra.core.Term, hydra.core.Term> hydra_rewriting_deannotateTerm2, java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>> hydra_rewriting_freeVariablesInTerm2, java.util.function.Function<hydra.core.Term, hydra.core.Term> hydra_rewriting_simplifyTerm2, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> hydra_rewriting_substituteVariable2, java.util.function.Function<hydra.core.Term, T0> recurse, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> forRhs = (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>) (rhs -> (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (var -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (body -> ((hydra_rewriting_deannotateTerm2).apply(rhs)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return term;
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable v) {
        return (hydra_rewriting_simplifyTerm2).apply((((hydra_rewriting_substituteVariable2).apply(var)).apply((v).value)).apply(body));
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
          hydra.core.Term body = ((l).value).body;
          hydra.core.Name var = ((l).value).parameter;
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.sets.Member.apply(
              var,
              (hydra_rewriting_freeVariablesInTerm2).apply(body)),
            () -> (((forRhs).apply(rhs)).apply(var)).apply(body),
            () -> (hydra_rewriting_simplifyTerm2).apply(body));
        }
      }));
      return ((hydra_rewriting_deannotateTerm2).apply(lhs)).accept(new hydra.core.Term.PartialVisitor<>() {
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
    java.util.function.Function<hydra.core.Term, hydra.core.Term> forTerm = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return term;
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Application app) {
        hydra.core.Term lhs = ((app).value).function;
        hydra.core.Term rhs = ((app).value).argument;
        return ((forLhs).apply(lhs)).apply(rhs);
      }
    }));
    hydra.core.Term stripped = (hydra_rewriting_deannotateTerm2).apply(term);
    return (recurse).apply((forTerm).apply(stripped));
  }
  
  static hydra.core.Type substituteTypeVariables(java.util.Map<hydra.core.Name, hydra.core.Name> subst, hydra.core.Type typ) {
    java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>> replace = (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ2 -> (typ2).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return (recurse).apply(typ2);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable n) {
        return new hydra.core.Type.Variable(hydra.lib.maybes.FromMaybe.apply(
          (n).value,
          hydra.lib.maps.Lookup.apply(
            (n).value,
            subst)));
      }
    })));
    return hydra.rewriting.Rewriting.rewriteType(
      replace,
      typ);
  }
  
  static hydra.core.Term substituteVariable(hydra.core.Name from, hydra.core.Name to, hydra.core.Term term) {
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> replace = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term2 -> (term2).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (recurse).apply(term2);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable x) {
        return new hydra.core.Term.Variable(hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (x).value,
            from),
          () -> to,
          () -> (x).value));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Function v1) {
        return ((v1).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return (recurse).apply(term2);
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda l) {
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                ((l).value).parameter,
                from),
              () -> term2,
              () -> (recurse).apply(term2));
          }
        });
      }
    })));
    return hydra.rewriting.Rewriting.rewriteTerm(
      replace,
      term);
  }
  
  static hydra.core.Term substituteVariables(java.util.Map<hydra.core.Name, hydra.core.Name> subst, hydra.core.Term term) {
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> replace = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term2 -> (term2).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (recurse).apply(term2);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable n) {
        return new hydra.core.Term.Variable(hydra.lib.maybes.FromMaybe.apply(
          (n).value,
          hydra.lib.maps.Lookup.apply(
            (n).value,
            subst)));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Function v1) {
        return ((v1).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return (recurse).apply(term2);
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda l) {
            return hydra.lib.maybes.Maybe.apply(
              (recurse).apply(term2),
              (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (ignored -> term2),
              hydra.lib.maps.Lookup.apply(
                ((l).value).parameter,
                subst));
          }
        });
      }
    })));
    return hydra.rewriting.Rewriting.rewriteTerm(
      replace,
      term);
  }
  
  static java.util.List<hydra.core.Term> subterms(hydra.core.Term v1) {
    return (v1).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Annotated at) {
        return java.util.List.of(((at).value).body);
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Application p) {
        return java.util.List.of(
          ((p).value).function,
          ((p).value).argument);
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (l -> java.util.List.of(l)),
          (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (r -> java.util.List.of(r)),
          (e).value);
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Function v12) {
        return ((v12).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public java.util.List<hydra.core.Term> otherwise(hydra.core.Function instance) {
            return (java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of());
          }
          
          @Override
          public java.util.List<hydra.core.Term> visit(hydra.core.Function.Elimination v13) {
            return ((v13).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public java.util.List<hydra.core.Term> otherwise(hydra.core.Elimination instance) {
                return (java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of());
              }
              
              @Override
              public java.util.List<hydra.core.Term> visit(hydra.core.Elimination.Union cs) {
                return hydra.lib.lists.Concat2.apply(
                  hydra.lib.maybes.Maybe.apply(
                    (java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()),
                    (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (t -> java.util.List.of(t)),
                    ((cs).value).default_),
                  hydra.lib.lists.Map.apply(
                    projected -> projected.term,
                    ((cs).value).cases));
              }
            });
          }
          
          @Override
          public java.util.List<hydra.core.Term> visit(hydra.core.Function.Lambda l) {
            return java.util.List.of(((l).value).body);
          }
        });
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Let lt) {
        return hydra.lib.lists.Cons.apply(
          ((lt).value).body,
          hydra.lib.lists.Map.apply(
            projected -> projected.term,
            ((lt).value).bindings));
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.List l) {
        return (l).value;
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Literal ignored) {
        return (java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of());
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Map m) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, java.util.List<hydra.core.Term>>) (p -> java.util.List.of(
            hydra.lib.pairs.First.apply(p),
            hydra.lib.pairs.Second.apply(p))),
          hydra.lib.maps.ToList.apply((m).value)));
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.maybes.Maybe.apply(
          (java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()),
          (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (t -> java.util.List.of(t)),
          (m).value);
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Pair p) {
        return java.util.List.of(
          hydra.lib.pairs.First.apply((p).value),
          hydra.lib.pairs.Second.apply((p).value));
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Record rt) {
        return hydra.lib.lists.Map.apply(
          projected -> projected.term,
          ((rt).value).fields);
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Set l) {
        return hydra.lib.sets.ToList.apply((l).value);
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.TypeApplication ta) {
        return java.util.List.of(((ta).value).body);
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.TypeLambda ta) {
        return java.util.List.of(((ta).value).body);
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Union ut) {
        return java.util.List.of((((ut).value).field).term);
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Unit ignored) {
        return (java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of());
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Variable ignored) {
        return (java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of());
      }
      
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Wrap n) {
        return java.util.List.of(((n).value).body);
      }
    });
  }
  
  static java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> subtermsWithAccessors(hydra.core.Term v1) {
    return (v1).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.Annotated at) {
        return java.util.List.of((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.AnnotatedBody(), ((at).value).body))));
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.Application p) {
        return java.util.List.of(
          (hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.ApplicationFunction(), ((p).value).function))),
          (hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.ApplicationArgument(), ((p).value).argument))));
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.Either e) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.Function v12) {
        return ((v12).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> otherwise(hydra.core.Function instance) {
            return (java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>of());
          }
          
          @Override
          public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Function.Elimination v13) {
            return ((v13).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> otherwise(hydra.core.Elimination instance) {
                return (java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>of());
              }
              
              @Override
              public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Elimination.Union cs) {
                return hydra.lib.lists.Concat2.apply(
                  hydra.lib.maybes.Maybe.apply(
                    (java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>of()),
                    (java.util.function.Function<hydra.core.Term, java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>>) (t -> java.util.List.of((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.UnionCasesDefault(), t))))),
                    ((cs).value).default_),
                  hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>) (f -> (hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.UnionCasesBranch((f).name), (f).term)))),
                    ((cs).value).cases));
              }
            });
          }
          
          @Override
          public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Function.Lambda l) {
            return java.util.List.of((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.LambdaBody(), ((l).value).body))));
          }
        });
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.Let lt) {
        return hydra.lib.lists.Cons.apply(
          (hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.LetBody(), ((lt).value).body))),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>) (b -> (hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.LetBinding((b).name), (b).term)))),
            ((lt).value).bindings));
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.List l) {
        return hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>) (e -> (hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.ListElement(0), e)))),
          (l).value);
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.Literal ignored) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.Map m) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>>) (p -> java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.MapKey(0), hydra.lib.pairs.First.apply(p)))),
            (hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.MapValue(0), hydra.lib.pairs.Second.apply(p)))))),
          hydra.lib.maps.ToList.apply((m).value)));
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.maybes.Maybe.apply(
          (java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>of()),
          (java.util.function.Function<hydra.core.Term, java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>>) (t -> java.util.List.of((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.MaybeTerm(), t))))),
          (m).value);
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.Pair p) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.Record rt) {
        return hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>) (f -> (hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.RecordField((f).name), (f).term)))),
          ((rt).value).fields);
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.Set s) {
        return hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>) (e -> (hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.ListElement(0), e)))),
          hydra.lib.sets.ToList.apply((s).value));
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.TypeApplication ta) {
        return java.util.List.of((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.TypeApplicationTerm(), ((ta).value).body))));
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.TypeLambda ta) {
        return java.util.List.of((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.TypeLambdaBody(), ((ta).value).body))));
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.Union ut) {
        return java.util.List.of((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.InjectionTerm(), (((ut).value).field).term))));
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.Unit ignored) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.Variable ignored) {
        return (java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>>of());
      }
      
      @Override
      public java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>> visit(hydra.core.Term.Wrap n) {
        return java.util.List.of((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.WrappedTerm(), ((n).value).body))));
      }
    });
  }
  
  static java.util.List<hydra.core.Type> subtypes(hydra.core.Type v1) {
    return (v1).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Annotated at) {
        return java.util.List.of(((at).value).body);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Application at) {
        return java.util.List.of(
          ((at).value).function,
          ((at).value).argument);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Either et) {
        return java.util.List.of(
          ((et).value).left,
          ((et).value).right);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Pair pt) {
        return java.util.List.of(
          ((pt).value).first,
          ((pt).value).second);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Function ft) {
        return java.util.List.of(
          ((ft).value).domain,
          ((ft).value).codomain);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Forall lt) {
        return java.util.List.of(((lt).value).body);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.List lt) {
        return java.util.List.of((lt).value);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Literal ignored) {
        return (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of());
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Map mt) {
        return java.util.List.of(
          ((mt).value).keys,
          ((mt).value).values);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Maybe ot) {
        return java.util.List.of((ot).value);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Record rt) {
        return hydra.lib.lists.Map.apply(
          projected -> projected.type,
          ((rt).value).fields);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Set st) {
        return java.util.List.of((st).value);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Union rt) {
        return hydra.lib.lists.Map.apply(
          projected -> projected.type,
          ((rt).value).fields);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Unit ignored) {
        return (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of());
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Variable ignored) {
        return (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of());
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Wrap nt) {
        return java.util.List.of(((nt).value).body);
      }
    });
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
          return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public java.util.Set<hydra.core.Name> otherwise(hydra.core.Function instance) {
              return names;
            }
            
            @Override
            public java.util.Set<hydra.core.Name> visit(hydra.core.Function.Primitive name) {
              return (prim).apply((name).value);
            }
            
            @Override
            public java.util.Set<hydra.core.Name> visit(hydra.core.Function.Elimination e) {
              return ((e).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
                @Override
                public java.util.Set<hydra.core.Name> visit(hydra.core.Elimination.Record proj) {
                  return (nominal).apply(((proj).value).typeName);
                }
                
                @Override
                public java.util.Set<hydra.core.Name> visit(hydra.core.Elimination.Union caseStmt) {
                  return (nominal).apply(((caseStmt).value).typeName);
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
          return (nominal).apply(((record).value).typeName);
        }
        
        @Override
        public java.util.Set<hydra.core.Name> visit(hydra.core.Term.Union injection) {
          return (nominal).apply(((injection).value).typeName);
        }
        
        @Override
        public java.util.Set<hydra.core.Name> visit(hydra.core.Term.Variable name) {
          return (var).apply((name).value);
        }
        
        @Override
        public java.util.Set<hydra.core.Name> visit(hydra.core.Term.Wrap wrappedTerm) {
          return (nominal).apply(((wrappedTerm).value).typeName);
        }
      });
    }));
    return hydra.rewriting.Rewriting.foldOverTerm(
      new hydra.coders.TraversalOrder.Pre(),
      addNames,
      (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      term0);
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.Name> toShortNames(java.util.List<hydra.core.Name> original) {
    java.util.function.Function<java.util.Map<String, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.core.Name, java.util.Map<String, java.util.Set<hydra.core.Name>>>> addName = (java.util.function.Function<java.util.Map<String, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.core.Name, java.util.Map<String, java.util.Set<hydra.core.Name>>>>) (acc -> (java.util.function.Function<hydra.core.Name, java.util.Map<String, java.util.Set<hydra.core.Name>>>) (name -> {
      String local = hydra.names.Names.localNameOf(name);
      hydra.util.Lazy<java.util.Set<hydra.core.Name>> group = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.apply(
        (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
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
      p0 -> hydra.rewriting.Rewriting.<hydra.core.Name>toShortNames_renameGroup(p0),
      hydra.lib.maps.ToList.apply(groups))));
  }
  
  static <T0> java.util.List<hydra.util.Tuple.Tuple2<T0, hydra.core.Name>> toShortNames_renameGroup(hydra.util.Tuple.Tuple2<String, java.util.Set<T0>> localNames) {
    hydra.util.Lazy<String> local = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(localNames));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Integer, java.util.List<Integer>>> rangeFrom = new java.util.concurrent.atomic.AtomicReference<>();
    rangeFrom.set((java.util.function.Function<Integer, java.util.List<Integer>>) (start -> hydra.lib.lists.Cons.apply(
      start,
      (rangeFrom.get()).apply(hydra.lib.math.Add.apply(
        start,
        1)))));
    return hydra.lib.lists.ZipWith.apply(
      (java.util.function.Function<T0, java.util.function.Function<Integer, hydra.util.Tuple.Tuple2<T0, hydra.core.Name>>>) (v1 -> (java.util.function.Function<Integer, hydra.util.Tuple.Tuple2<T0, hydra.core.Name>>) (v2 -> hydra.rewriting.Rewriting.<T0>toShortNames_rename(
        local.get(),
        v1,
        v2))),
      hydra.lib.sets.ToList.apply(hydra.rewriting.Rewriting.<String, java.util.Set<T0>>toShortNames_names(localNames)),
      (rangeFrom.get()).apply(1));
  }
  
  static <T0, T1> T1 toShortNames_names(hydra.util.Tuple.Tuple2<T0, T1> localNames) {
    return hydra.lib.pairs.Second.apply(localNames);
  }
  
  static <T0> hydra.util.Tuple.Tuple2<T0, hydra.core.Name> toShortNames_rename(String local, T0 name, Integer i) {
    return (hydra.util.Tuple.Tuple2<T0, hydra.core.Name>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Name>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Name>(name, new hydra.core.Name(hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Gt.apply(
        i,
        1),
      () -> hydra.lib.strings.Cat2.apply(
        local,
        hydra.lib.literals.ShowInt32.apply(i)),
      () -> local)))));
  }
  
  static java.util.List<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>> topologicalSortBindingMap(java.util.Map<hydra.core.Name, hydra.core.Term> bindingMap) {
    hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>> bindings = new hydra.util.Lazy<>(() -> hydra.lib.maps.ToList.apply(bindingMap));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, Boolean>> hasTypeAnnotation = new java.util.concurrent.atomic.AtomicReference<>();
    hasTypeAnnotation.set((java.util.function.Function<hydra.core.Term, Boolean>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Annotated at) {
        return (hasTypeAnnotation.get()).apply(((at).value).body);
      }
    })));
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> keys = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>, hydra.core.Name>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>, hydra.core.Name>) (hydra.lib.pairs.First::apply)),
      bindings.get())));
    java.util.function.Function<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>> toPair = (java.util.function.Function<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>) (name -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>(name, hydra.lib.maybes.FromMaybe.apply(
      new hydra.core.Term.Literal(new hydra.core.Literal.String_("Impossible!")),
      hydra.lib.maps.Lookup.apply(
        name,
        bindingMap))))));
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<hydra.core.Name>, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>) (v1 -> hydra.lib.lists.Map.apply(
        toPair,
        v1)),
      hydra.sorting.Sorting.topologicalSortComponents(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>>) (v1 -> hydra.rewriting.Rewriting.topologicalSortBindingMap_depsOf(
          hasTypeAnnotation.get(),
          hydra.rewriting.Rewriting::freeVariablesInTerm,
          keys.get(),
          v1)),
        bindings.get())));
  }
  
  static <T0, T1, T2> hydra.util.Tuple.Tuple2<T2, java.util.List<T1>> topologicalSortBindingMap_depsOf(java.util.function.Function<T0, Boolean> hasTypeAnnotation, java.util.function.Function<T0, java.util.Set<T1>> hydra_rewriting_freeVariablesInTerm2, java.util.Set<T1> keys, hydra.util.Tuple.Tuple2<T2, T0> nameAndTerm) {
    hydra.util.Lazy<T0> term = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nameAndTerm));
    return (hydra.util.Tuple.Tuple2<T2, java.util.List<T1>>) ((hydra.util.Tuple.Tuple2<T2, java.util.List<T1>>) (new hydra.util.Tuple.Tuple2<T2, java.util.List<T1>>(hydra.rewriting.Rewriting.<T2, T0>topologicalSortBindingMap_name(nameAndTerm), hydra.lib.logic.IfElse.lazy(
      (hasTypeAnnotation).apply(term.get()),
      () -> (java.util.List<T1>) (java.util.List.<T1>of()),
      () -> hydra.lib.sets.ToList.apply(hydra.lib.sets.Intersection.apply(
        keys,
        (hydra_rewriting_freeVariablesInTerm2).apply(term.get())))))));
  }
  
  static <T0, T1> T0 topologicalSortBindingMap_name(hydra.util.Tuple.Tuple2<T0, T1> nameAndTerm) {
    return hydra.lib.pairs.First.apply(nameAndTerm);
  }
  
  static hydra.util.Either<java.util.List<java.util.List<hydra.core.Name>>, java.util.List<hydra.core.Name>> topologicalSortBindings(java.util.List<hydra.core.Binding> els) {
    java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>> adjlist = (java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>>) (e -> (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>((e).name, hydra.lib.sets.ToList.apply(hydra.rewriting.Rewriting.termDependencyNames(
      false,
      true,
      true,
      (e).term))))));
    return hydra.sorting.Sorting.topologicalSort(hydra.lib.lists.Map.apply(
      adjlist,
      els));
  }
  
  static java.util.Set<hydra.core.Name> typeDependencyNames(Boolean withSchema, hydra.core.Type typ) {
    return hydra.lib.logic.IfElse.lazy(
      withSchema,
      () -> hydra.lib.sets.Union.apply(
        hydra.rewriting.Rewriting.freeVariablesInType(typ),
        hydra.rewriting.Rewriting.typeNamesInType(typ)),
      () -> hydra.rewriting.Rewriting.freeVariablesInType(typ));
  }
  
  static java.util.Set<hydra.core.Name> typeNamesInType(hydra.core.Type typ0) {
    java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>> addNames = (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>>) (names -> (java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>) (typ -> (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.Set<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return names;
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Record rowType) {
        hydra.core.Name tname = ((rowType).value).typeName;
        return hydra.lib.sets.Insert.apply(
          tname,
          names);
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Union rowType) {
        hydra.core.Name tname = ((rowType).value).typeName;
        return hydra.lib.sets.Insert.apply(
          tname,
          names);
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Wrap wrappedType) {
        hydra.core.Name tname = ((wrappedType).value).typeName;
        return hydra.lib.sets.Insert.apply(
          tname,
          names);
      }
    })));
    return hydra.rewriting.Rewriting.foldOverType(
      new hydra.coders.TraversalOrder.Pre(),
      addNames,
      (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      typ0);
  }
  
  static hydra.core.Term unshadowVariables(hydra.core.Term term0) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.function.Function<java.util.Map<hydra.core.Name, Integer>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term>>>, java.util.function.Function<java.util.Map<hydra.core.Name, Integer>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term>>>>> rewrite = new java.util.concurrent.atomic.AtomicReference<>();
    rewrite.set((java.util.function.Function<java.util.function.Function<java.util.Map<hydra.core.Name, Integer>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term>>>, java.util.function.Function<java.util.Map<hydra.core.Name, Integer>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term>>>>) (recurse -> (java.util.function.Function<java.util.Map<hydra.core.Name, Integer>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term>>>) (m -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term>>) (term -> {
      hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term> dflt = ((recurse).apply(m)).apply(term);
      return (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term> otherwise(hydra.core.Term instance) {
          return dflt;
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term> visit(hydra.core.Term.Function f) {
          return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term> otherwise(hydra.core.Function instance) {
              return dflt;
            }
            
            @Override
            public hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term> visit(hydra.core.Function.Lambda l) {
              hydra.core.Term body = ((l).value).body;
              hydra.util.Maybe<hydra.core.Type> domain = ((l).value).domain;
              hydra.core.Name v = ((l).value).parameter;
              return (hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term>(m, hydra.lib.maybes.Maybe.apply(
                new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(v, domain, hydra.lib.pairs.Second.apply((((rewrite.get()).apply(recurse)).apply(hydra.lib.maps.Insert.apply(
                  v,
                  1,
                  m))).apply(body))))),
                (java.util.function.Function<Integer, hydra.core.Term>) (i -> {
                  Integer i2 = hydra.lib.math.Add.apply(
                    i,
                    1);
                  hydra.util.Lazy<java.util.Map<hydra.core.Name, Integer>> m2 = new hydra.util.Lazy<>(() -> hydra.lib.maps.Insert.apply(
                    v,
                    i2,
                    m));
                  hydra.core.Name v2 = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
                    (v).value,
                    hydra.lib.literals.ShowInt32.apply(i2)));
                  return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(v2, domain, hydra.lib.pairs.Second.apply((((rewrite.get()).apply(recurse)).apply(m2.get())).apply(body)))));
                }),
                hydra.lib.maps.Lookup.apply(
                  v,
                  m)))));
            }
          });
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term> visit(hydra.core.Term.Let lt) {
          hydra.util.Lazy<java.util.Map<hydra.core.Name, Integer>> m2 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<java.util.Map<hydra.core.Name, Integer>, java.util.function.Function<hydra.core.Binding, java.util.Map<hydra.core.Name, Integer>>>) (acc -> (java.util.function.Function<hydra.core.Binding, java.util.Map<hydra.core.Name, Integer>>) (b -> hydra.lib.maps.Insert.apply(
              (b).name,
              1,
              acc))),
            m,
            ((lt).value).bindings));
          return ((recurse).apply(m2.get())).apply(term);
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term> visit(hydra.core.Term.Variable v) {
          return (hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, Integer>, hydra.core.Term>(m, new hydra.core.Term.Variable(hydra.lib.maybes.Maybe.apply(
            (v).value,
            (java.util.function.Function<Integer, hydra.core.Name>) (i -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                i,
                1),
              () -> (v).value,
              () -> new hydra.core.Name(hydra.lib.strings.Cat2.apply(
                ((v).value).value,
                hydra.lib.literals.ShowInt32.apply(i))))),
            hydra.lib.maps.Lookup.apply(
              (v).value,
              m))))));
        }
      });
    }))));
    return hydra.lib.pairs.Second.apply(hydra.rewriting.Rewriting.rewriteAndFoldTerm(
      rewrite.get(),
      (java.util.Map<hydra.core.Name, Integer>) ((java.util.Map<hydra.core.Name, Integer>) (hydra.lib.maps.Empty.<hydra.core.Name, Integer>apply())),
      term0));
  }
}
