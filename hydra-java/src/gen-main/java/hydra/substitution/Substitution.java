// Note: this is an automatically generated file. Do not edit.

package hydra.substitution;

/**
 * Variable substitution in type and term expressions.
 */
public interface Substitution {
  static hydra.typing.TypeSubst composeTypeSubst(hydra.typing.TypeSubst s1, hydra.typing.TypeSubst s2) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.maps.Null.apply((s1).value),
      () -> s2,
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.maps.Null.apply((s2).value),
        () -> s1,
        () -> hydra.substitution.Substitution.composeTypeSubstNonEmpty(
          s1,
          s2)));
  }
  
  static hydra.typing.TypeSubst composeTypeSubstNonEmpty(hydra.typing.TypeSubst s1, hydra.typing.TypeSubst s2) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Type>> withExtra = new hydra.util.Lazy<>(() -> hydra.lib.maps.FilterWithKey.apply(
      (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, Boolean>>) (v1 -> (java.util.function.Function<hydra.core.Type, Boolean>) (v2 -> hydra.substitution.Substitution.composeTypeSubstNonEmpty_isExtra(
        s1,
        v1,
        v2))),
      (s2).value));
    return new hydra.typing.TypeSubst(hydra.lib.maps.Union.apply(
      withExtra.get(),
      hydra.lib.maps.Map.apply(
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (v1 -> hydra.substitution.Substitution.substInType(
          s2,
          v1)),
        (s1).value)));
  }
  
  static <T0> Boolean composeTypeSubstNonEmpty_isExtra(hydra.typing.TypeSubst s1, hydra.core.Name k, T0 v) {
    return hydra.lib.maybes.IsNothing.apply(hydra.lib.maps.Lookup.apply(
      k,
      (s1).value));
  }
  
  static hydra.typing.TypeSubst composeTypeSubstList(java.util.List<hydra.typing.TypeSubst> v1) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>>) (p0 -> p1 -> hydra.substitution.Substitution.composeTypeSubst(
        p0,
        p1)),
      hydra.substitution.Substitution.idTypeSubst(),
      v1);
  }
  
  static hydra.typing.TypeSubst idTypeSubst() {
    return new hydra.typing.TypeSubst((java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())));
  }
  
  static hydra.typing.TypeSubst singletonTypeSubst(hydra.core.Name v, hydra.core.Type t) {
    return new hydra.typing.TypeSubst(hydra.lib.maps.Singleton.apply(
      v,
      t));
  }
  
  static hydra.core.Binding substituteInBinding(hydra.typing.TermSubst subst, hydra.core.Binding b) {
    return new hydra.core.Binding((b).name, hydra.substitution.Substitution.substituteInTerm(
      subst,
      (b).term), (b).type);
  }
  
  static hydra.typing.TypeConstraint substituteInConstraint(hydra.typing.TypeSubst subst, hydra.typing.TypeConstraint c) {
    return new hydra.typing.TypeConstraint(hydra.substitution.Substitution.substInType(
      subst,
      (c).left), hydra.substitution.Substitution.substInType(
      subst,
      (c).right), (c).comment);
  }
  
  static java.util.List<hydra.typing.TypeConstraint> substituteInConstraints(hydra.typing.TypeSubst subst, java.util.List<hydra.typing.TypeConstraint> cs) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.typing.TypeConstraint, hydra.typing.TypeConstraint>) (v1 -> hydra.substitution.Substitution.substituteInConstraint(
        subst,
        v1)),
      cs);
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> substInClassConstraints(hydra.typing.TypeSubst subst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> constraints) {
    java.util.Map<hydra.core.Name, hydra.core.Type> substMap = (subst).value;
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeVariableMetadata>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (acc -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeVariableMetadata>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (pair -> {
        hydra.util.Lazy<hydra.core.TypeVariableMetadata> metadata = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
        hydra.util.Lazy<hydra.core.Name> varName = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
        return hydra.lib.maybes.Maybe.apply(
          hydra.substitution.Substitution.substInClassConstraints_insertOrMerge(
            varName.get(),
            metadata.get(),
            acc),
          (java.util.function.Function<hydra.core.Type, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (targetType -> {
            hydra.util.Lazy<java.util.List<hydra.core.Name>> freeVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply(hydra.rewriting.Rewriting.freeVariablesInType(targetType)));
            return hydra.lib.lists.Foldl.apply(
              (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, java.util.function.Function<hydra.core.Name, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (acc2 -> (java.util.function.Function<hydra.core.Name, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (freeVar -> hydra.substitution.Substitution.substInClassConstraints_insertOrMerge(
                freeVar,
                metadata.get(),
                acc2))),
              acc,
              freeVars.get());
          }),
          hydra.lib.maps.Lookup.apply(
            varName.get(),
            substMap));
      })),
      (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
      hydra.lib.maps.ToList.apply(constraints));
  }
  
  static <T0> java.util.Map<T0, hydra.core.TypeVariableMetadata> substInClassConstraints_insertOrMerge(T0 varName, hydra.core.TypeVariableMetadata metadata, java.util.Map<T0, hydra.core.TypeVariableMetadata> acc) {
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.maps.Insert.apply(
        varName,
        metadata,
        acc),
      (java.util.function.Function<hydra.core.TypeVariableMetadata, java.util.Map<T0, hydra.core.TypeVariableMetadata>>) (existing -> {
        hydra.util.Lazy<hydra.core.TypeVariableMetadata> merged = new hydra.util.Lazy<>(() -> new hydra.core.TypeVariableMetadata(hydra.lib.sets.Union.apply(
          (existing).classes,
          (metadata).classes)));
        return hydra.lib.maps.Insert.apply(
          varName,
          merged.get(),
          acc);
      }),
      hydra.lib.maps.Lookup.apply(
        varName,
        acc));
  }
  
  static hydra.typing.InferenceContext substInContext(hydra.typing.TypeSubst subst, hydra.typing.InferenceContext cx) {
    java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> newClassConstraints = hydra.substitution.Substitution.substInClassConstraints(
      subst,
      (cx).classConstraints);
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> newDataTypes = new hydra.util.Lazy<>(() -> hydra.lib.maps.Map.apply(
      (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (v1 -> hydra.substitution.Substitution.substInTypeScheme(
        subst,
        v1)),
      (cx).dataTypes));
    return new hydra.typing.InferenceContext((cx).schemaTypes, (cx).primitiveTypes, newDataTypes.get(), newClassConstraints, (cx).debug);
  }
  
  static hydra.core.Term substituteInTerm(hydra.typing.TermSubst subst, hydra.core.Term term0) {
    java.util.Map<hydra.core.Name, hydra.core.Term> s = (subst).value;
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> rewrite = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> {
      java.util.function.Function<hydra.core.Lambda, hydra.core.Term> withLambda = (java.util.function.Function<hydra.core.Lambda, hydra.core.Term>) (l -> {
        hydra.core.Name v = (l).parameter;
        hydra.util.Lazy<hydra.typing.TermSubst> subst2 = new hydra.util.Lazy<>(() -> new hydra.typing.TermSubst(hydra.lib.maps.Delete.apply(
          v,
          s)));
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(v, (l).domain, hydra.substitution.Substitution.substituteInTerm(
          subst2.get(),
          (l).body))));
      });
      java.util.function.Function<hydra.core.Let, hydra.core.Term> withLet = (java.util.function.Function<hydra.core.Let, hydra.core.Term>) (lt -> {
        java.util.List<hydra.core.Binding> bindings = (lt).bindings;
        hydra.util.Lazy<java.util.Set<hydra.core.Name>> names = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
          projected -> projected.name,
          bindings)));
        hydra.util.Lazy<hydra.typing.TermSubst> subst2 = new hydra.util.Lazy<>(() -> new hydra.typing.TermSubst(hydra.lib.maps.FilterWithKey.apply(
          (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, Boolean>>) (k -> (java.util.function.Function<hydra.core.Term, Boolean>) (v -> hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
            k,
            names.get())))),
          s)));
        java.util.function.Function<hydra.core.Binding, hydra.core.Binding> rewriteBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, hydra.substitution.Substitution.substituteInTerm(
          subst2.get(),
          (b).term), (b).type));
        return new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Map.apply(
          rewriteBinding,
          bindings), hydra.substitution.Substitution.substituteInTerm(
          subst2.get(),
          (lt).body)));
      });
      return (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return (recurse).apply(term);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Function fun) {
          return ((fun).value).accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.core.Term otherwise(hydra.core.Function instance) {
              return (recurse).apply(term);
            }
            
            @Override
            public hydra.core.Term visit(hydra.core.Function.Lambda l) {
              return (withLambda).apply((l).value);
            }
          });
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Let l) {
          return (withLet).apply((l).value);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Variable name) {
          return hydra.lib.maybes.Maybe.apply(
            (recurse).apply(term),
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (sterm -> sterm),
            hydra.lib.maps.Lookup.apply(
              (name).value,
              s));
        }
      });
    }));
    return hydra.rewriting.Rewriting.rewriteTerm(
      rewrite,
      term0);
  }
  
  static hydra.core.Type substInType(hydra.typing.TypeSubst subst, hydra.core.Type typ0) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.maps.Null.apply((subst).value),
      () -> typ0,
      () -> hydra.substitution.Substitution.substInTypeNonEmpty(
        subst,
        typ0));
  }
  
  static hydra.core.Type substInTypeNonEmpty(hydra.typing.TypeSubst subst, hydra.core.Type typ0) {
    java.util.function.Function<hydra.core.Name, hydra.typing.TypeSubst> removeVar = (java.util.function.Function<hydra.core.Name, hydra.typing.TypeSubst>) (v -> new hydra.typing.TypeSubst(hydra.lib.maps.Delete.apply(
      v,
      (subst).value)));
    java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>> rewrite = (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return (recurse).apply(typ);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall lt) {
        return hydra.lib.maybes.Maybe.apply(
          (recurse).apply(typ),
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (styp -> new hydra.core.Type.Forall(new hydra.core.ForallType(((lt).value).parameter, hydra.substitution.Substitution.substInType(
            (removeVar).apply(((lt).value).parameter),
            ((lt).value).body)))),
          hydra.lib.maps.Lookup.apply(
            ((lt).value).parameter,
            (subst).value));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable v) {
        return hydra.lib.maybes.Maybe.apply(
          typ,
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (styp -> styp),
          hydra.lib.maps.Lookup.apply(
            (v).value,
            (subst).value));
      }
    })));
    return hydra.rewriting.Rewriting.rewriteType(
      rewrite,
      typ0);
  }
  
  static hydra.core.TypeScheme substInTypeScheme(hydra.typing.TypeSubst subst, hydra.core.TypeScheme ts) {
    return new hydra.core.TypeScheme((ts).variables, hydra.substitution.Substitution.substInType(
      subst,
      (ts).type), hydra.lib.maybes.Map.apply(
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (v1 -> hydra.substitution.Substitution.substInClassConstraints(
        subst,
        v1)),
      (ts).constraints));
  }
  
  static hydra.core.Term substTypesInTerm(hydra.typing.TypeSubst subst, hydra.core.Term term0) {
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> rewrite = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> {
      hydra.core.Term dflt = (recurse).apply(term);
      java.util.function.Function<hydra.core.Lambda, hydra.core.Term> forLambda = (java.util.function.Function<hydra.core.Lambda, hydra.core.Term>) (l -> new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda((l).parameter, hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (v1 -> hydra.substitution.Substitution.substInType(
          subst,
          v1)),
        (l).domain), hydra.substitution.Substitution.substTypesInTerm(
        subst,
        (l).body)))));
      java.util.function.Function<hydra.core.Function, hydra.core.Term> forFunction = (java.util.function.Function<hydra.core.Function, hydra.core.Term>) (f -> (f).accept(new hydra.core.Function.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Function instance) {
          return dflt;
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Function.Elimination e) {
          return dflt;
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Function.Lambda l) {
          return (forLambda).apply((l).value);
        }
      }));
      java.util.function.Function<hydra.core.Let, hydra.core.Term> forLet = (java.util.function.Function<hydra.core.Let, hydra.core.Term>) (l -> {
        java.util.function.Function<hydra.core.Binding, hydra.core.Binding> rewriteBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, hydra.substitution.Substitution.substTypesInTerm(
          subst,
          (b).term), hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (v1 -> hydra.substitution.Substitution.substInTypeScheme(
            subst,
            v1)),
          (b).type)));
        return new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Map.apply(
          rewriteBinding,
          (l).bindings), hydra.substitution.Substitution.substTypesInTerm(
          subst,
          (l).body)));
      });
      java.util.function.Function<hydra.core.TypeApplicationTerm, hydra.core.Term> forTypeApplication = (java.util.function.Function<hydra.core.TypeApplicationTerm, hydra.core.Term>) (tt -> new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(hydra.substitution.Substitution.substTypesInTerm(
        subst,
        (tt).body), hydra.substitution.Substitution.substInType(
        subst,
        (tt).type))));
      java.util.function.Function<hydra.core.TypeLambda, hydra.core.Term> forTypeLambda = (java.util.function.Function<hydra.core.TypeLambda, hydra.core.Term>) (ta -> {
        hydra.core.Name param = (ta).parameter;
        hydra.util.Lazy<hydra.typing.TypeSubst> subst2 = new hydra.util.Lazy<>(() -> new hydra.typing.TypeSubst(hydra.lib.maps.Delete.apply(
          param,
          (subst).value)));
        return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(param, hydra.substitution.Substitution.substTypesInTerm(
          subst2.get(),
          (ta).body)));
      });
      return (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return dflt;
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Function f) {
          return (forFunction).apply((f).value);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Let l) {
          return (forLet).apply((l).value);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeApplication ta) {
          return (forTypeApplication).apply((ta).value);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
          return (forTypeLambda).apply((tl).value);
        }
      });
    }));
    return hydra.rewriting.Rewriting.rewriteTerm(
      rewrite,
      term0);
  }
}
