// Note: this is an automatically generated file. Do not edit.

package hydra.checking;

/**
 * Type checking and type reconstruction (type-of) for the results of Hydra unification and inference
 */
public interface Checking {
  static <T0> Boolean allEqual(java.util.List<T0> els) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((els)),
      true,
      hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<Boolean, java.util.function.Function<T0, Boolean>>) (b -> (java.util.function.Function<T0, Boolean>) (t -> hydra.lib.logic.And.apply(
          (b),
          hydra.lib.equality.Equal.apply(
            (t),
            hydra.lib.lists.Head.apply((els)))))),
        true,
        hydra.lib.lists.Tail.apply((els))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> applyTypeArgumentsToType(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.Type t) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((typeArgs)),
      hydra.lib.flows.Bind.apply(
        hydra.checking.Checking.checkTypeVariables(
          (tx),
          (t)),
        (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Type>>) (ignored -> hydra.lib.flows.Pure.apply((t)))),
      hydra.checking.Checking.<T0>applyTypeArgumentsToType_nonnull(
        (hydra.show.core.Core::type),
        (java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (p0 -> p1 -> hydra.substitution.Substitution.substInType(
          (p0),
          (p1))),
        (t),
        (tx),
        (typeArgs)));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> applyTypeArgumentsToType_nonnull(java.util.function.Function<hydra.core.Type, String> hydra_show_core_type2, java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.core.Type, hydra.core.Type>> hydra_substitution_substInType2, hydra.core.Type t, hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs) {
    return ((t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
          "not a forall type: ",
          ((hydra_show_core_type2)).apply((t)),
          ". Trying to apply ",
          hydra.lib.literals.ShowInt32.apply(hydra.lib.lists.Length.apply((typeArgs))),
          " type args: ",
          hydra.formatting.Formatting.showList(
            (hydra_show_core_type2),
            (typeArgs)),
          ". Context has vars: {",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              wrapped -> ((wrapped)).value,
              hydra.lib.maps.Keys.apply(((tx)).types))),
          "}")));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Forall ft) {
        hydra.core.Type tbody = (((ft)).value).body;
        hydra.core.Name v = (((ft)).value).parameter;
        return hydra.checking.Checking.<T0>applyTypeArgumentsToType(
          (tx),
          hydra.lib.lists.Tail.apply((typeArgs)),
          (((hydra_substitution_substInType2)).apply(new hydra.typing.TypeSubst(hydra.lib.maps.Singleton.apply(
            (v),
            hydra.lib.lists.Head.apply((typeArgs)))))).apply((tbody)));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, Boolean> checkForUnboundTypeVariables(hydra.typing.InferenceContext cx, hydra.core.Term term0) {
    java.util.Set<hydra.core.Name> svars = hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply(((cx)).schemaTypes));
    return (((((java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.List<String>, java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, Boolean>>>>>) (v1 -> (java.util.function.Function<java.util.List<String>, java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, Boolean>>>>) (v2 -> (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, Boolean>>>) (v3 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, Boolean>>) (v4 -> hydra.checking.Checking.<T0>checkForUnboundTypeVariables_checkRecursive(
      (hydra.rewriting.Rewriting::freeVariablesInType),
      (hydra.rewriting.Rewriting::subterms),
      (hydra.show.core.Core::term),
      (hydra.show.core.Core::type),
      (hydra.show.core.Core::typeScheme),
      (svars),
      (v1),
      (v2),
      (v3),
      (v4))))))).apply((java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()))).apply(java.util.List.of("top level"))).apply((hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing()))).apply((term0));
  }
  
  static <T0> hydra.compute.Flow<T0, Boolean> checkForUnboundTypeVariables_checkRecursive(java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>> hydra_rewriting_freeVariablesInType2, java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>> hydra_rewriting_subterms2, java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, java.util.function.Function<hydra.core.Type, String> hydra_show_core_type2, java.util.function.Function<hydra.core.TypeScheme, String> hydra_show_core_typeScheme2, java.util.Set<hydra.core.Name> svars, java.util.Set<hydra.core.Name> vars, java.util.List<String> trace, hydra.util.Maybe<hydra.core.Binding> lbinding, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, Boolean> otherwise(hydra.core.Term instance) {
        return hydra.checking.Checking.checkForUnboundTypeVariables_dflt(
          (hydra_rewriting_subterms2),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, Boolean>>) (v1 -> (hydra.checking.Checking.<T0>checkForUnboundTypeVariables_recurse(
            (hydra_rewriting_freeVariablesInType2),
            (hydra_rewriting_subterms2),
            (hydra_show_core_term2),
            (hydra_show_core_type2),
            (hydra_show_core_typeScheme2),
            (svars),
            (lbinding),
            (trace),
            (vars))).apply((v1))),
          (term));
      }
      
      @Override
      public hydra.compute.Flow<T0, Boolean> visit(hydra.core.Term.Function f) {
        return (((f)).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, Boolean> otherwise(hydra.core.Function instance) {
            return hydra.checking.Checking.checkForUnboundTypeVariables_dflt(
              (hydra_rewriting_subterms2),
              (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, Boolean>>) (v1 -> (hydra.checking.Checking.<T0>checkForUnboundTypeVariables_recurse(
                (hydra_rewriting_freeVariablesInType2),
                (hydra_rewriting_subterms2),
                (hydra_show_core_term2),
                (hydra_show_core_type2),
                (hydra_show_core_typeScheme2),
                (svars),
                (lbinding),
                (trace),
                (vars))).apply((v1))),
              (term));
          }
          
          @Override
          public hydra.compute.Flow<T0, Boolean> visit(hydra.core.Function.Elimination e) {
            return hydra.checking.Checking.checkForUnboundTypeVariables_dflt(
              (hydra_rewriting_subterms2),
              (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, Boolean>>) (v1 -> (hydra.checking.Checking.<T0>checkForUnboundTypeVariables_recurse(
                (hydra_rewriting_freeVariablesInType2),
                (hydra_rewriting_subterms2),
                (hydra_show_core_term2),
                (hydra_show_core_type2),
                (hydra_show_core_typeScheme2),
                (svars),
                (lbinding),
                (trace),
                (vars))).apply((v1))),
              (term));
          }
          
          @Override
          public hydra.compute.Flow<T0, Boolean> visit(hydra.core.Function.Lambda l) {
            return hydra.lib.flows.Bind.apply(
              ((java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.compute.Flow<T0, Boolean>>) (v1 -> hydra.checking.Checking.checkForUnboundTypeVariables_checkOptional(
                (hydra_rewriting_freeVariablesInType2),
                (hydra_show_core_term2),
                (hydra_show_core_type2),
                (hydra_show_core_typeScheme2),
                (lbinding),
                (svars),
                (trace),
                (vars),
                (v1)))).apply((((l)).value).domain),
              (java.util.function.Function<Boolean, hydra.compute.Flow<T0, Boolean>>) (ignored -> (hydra.checking.Checking.<T0>checkForUnboundTypeVariables_recurse(
                (hydra_rewriting_freeVariablesInType2),
                (hydra_rewriting_subterms2),
                (hydra_show_core_term2),
                (hydra_show_core_type2),
                (hydra_show_core_typeScheme2),
                (svars),
                (lbinding),
                (trace),
                (vars))).apply((((l)).value).body)));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, Boolean> visit(hydra.core.Term.Let l) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<T0, Boolean>>) (v1 -> hydra.checking.Checking.<T0>checkForUnboundTypeVariables_forBinding(
              (hydra_rewriting_freeVariablesInType2),
              (hydra_rewriting_subterms2),
              (hydra_show_core_term2),
              (hydra_show_core_type2),
              (hydra_show_core_typeScheme2),
              (svars),
              (trace),
              (vars),
              (v1))),
            (((l)).value).bindings),
          (java.util.function.Function<java.util.List<Boolean>, hydra.compute.Flow<T0, Boolean>>) (ignored -> (hydra.checking.Checking.<T0>checkForUnboundTypeVariables_recurse(
            (hydra_rewriting_freeVariablesInType2),
            (hydra_rewriting_subterms2),
            (hydra_show_core_term2),
            (hydra_show_core_type2),
            (hydra_show_core_typeScheme2),
            (svars),
            (lbinding),
            (trace),
            (vars))).apply((((l)).value).body)));
      }
      
      @Override
      public hydra.compute.Flow<T0, Boolean> visit(hydra.core.Term.TypeApplication tt) {
        return hydra.lib.flows.Bind.apply(
          ((java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, Boolean>>) (v1 -> hydra.checking.Checking.checkForUnboundTypeVariables_check(
            (hydra_rewriting_freeVariablesInType2),
            (hydra_show_core_term2),
            (hydra_show_core_type2),
            (hydra_show_core_typeScheme2),
            (lbinding),
            (svars),
            (trace),
            (vars),
            (v1)))).apply((((tt)).value).type),
          (java.util.function.Function<Boolean, hydra.compute.Flow<T0, Boolean>>) (ignored -> (hydra.checking.Checking.<T0>checkForUnboundTypeVariables_recurse(
            (hydra_rewriting_freeVariablesInType2),
            (hydra_rewriting_subterms2),
            (hydra_show_core_term2),
            (hydra_show_core_type2),
            (hydra_show_core_typeScheme2),
            (svars),
            (lbinding),
            (trace),
            (vars))).apply((((tt)).value).body)));
      }
      
      @Override
      public hydra.compute.Flow<T0, Boolean> visit(hydra.core.Term.TypeLambda tl) {
        return hydra.lib.flows.Bind.apply(
          ((java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, Boolean>>) (v1 -> hydra.checking.Checking.checkForUnboundTypeVariables_check(
            (hydra_rewriting_freeVariablesInType2),
            (hydra_show_core_term2),
            (hydra_show_core_type2),
            (hydra_show_core_typeScheme2),
            (lbinding),
            (svars),
            (trace),
            (vars),
            (v1)))).apply(new hydra.core.Type.Variable((((tl)).value).parameter)),
          (java.util.function.Function<Boolean, hydra.compute.Flow<T0, Boolean>>) (ignored -> (hydra.checking.Checking.<T0>checkForUnboundTypeVariables_recurse(
            (hydra_rewriting_freeVariablesInType2),
            (hydra_rewriting_subterms2),
            (hydra_show_core_term2),
            (hydra_show_core_type2),
            (hydra_show_core_typeScheme2),
            (svars),
            (lbinding),
            (trace),
            (vars))).apply((((tl)).value).body)));
      }
    });
  }
  
  static <T0> java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, Boolean>> checkForUnboundTypeVariables_recurse(java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>> hydra_rewriting_freeVariablesInType2, java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>> hydra_rewriting_subterms2, java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, java.util.function.Function<hydra.core.Type, String> hydra_show_core_type2, java.util.function.Function<hydra.core.TypeScheme, String> hydra_show_core_typeScheme2, java.util.Set<hydra.core.Name> svars, hydra.util.Maybe<hydra.core.Binding> lbinding, java.util.List<String> trace, java.util.Set<hydra.core.Name> vars) {
    return ((((java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.List<String>, java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, Boolean>>>>>) (v1 -> (java.util.function.Function<java.util.List<String>, java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, Boolean>>>>) (v2 -> (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, Boolean>>>) (v3 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, Boolean>>) (v4 -> hydra.checking.Checking.<T0>checkForUnboundTypeVariables_checkRecursive(
      (hydra_rewriting_freeVariablesInType2),
      (hydra_rewriting_subterms2),
      (hydra_show_core_term2),
      (hydra_show_core_type2),
      (hydra_show_core_typeScheme2),
      (svars),
      (v1),
      (v2),
      (v3),
      (v4))))))).apply((vars))).apply((trace))).apply((lbinding));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T2, Boolean> checkForUnboundTypeVariables_dflt(java.util.function.Function<T0, java.util.List<T1>> hydra_rewriting_subterms2, java.util.function.Function<T1, hydra.compute.Flow<T2, T3>> recurse, T0 term) {
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapList.apply(
        (recurse),
        ((hydra_rewriting_subterms2)).apply((term))),
      (java.util.function.Function<java.util.List<T3>, hydra.compute.Flow<T2, Boolean>>) (ignored -> hydra.lib.flows.Pure.apply(true)));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, Boolean> checkForUnboundTypeVariables_check(java.util.function.Function<T0, java.util.Set<hydra.core.Name>> hydra_rewriting_freeVariablesInType2, java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, java.util.function.Function<T0, String> hydra_show_core_type2, java.util.function.Function<hydra.core.TypeScheme, String> hydra_show_core_typeScheme2, hydra.util.Maybe<hydra.core.Binding> lbinding, java.util.Set<hydra.core.Name> svars, java.util.List<String> trace, java.util.Set<hydra.core.Name> vars, T0 typ) {
    java.util.Set<hydra.core.Name> freevars = ((hydra_rewriting_freeVariablesInType2)).apply((typ));
    java.util.Set<hydra.core.Name> badvars = hydra.lib.sets.Difference.apply(
      hydra.lib.sets.Difference.apply(
        (freevars),
        (vars)),
      (svars));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.sets.Null.apply((badvars)),
      hydra.lib.flows.Pure.apply(true),
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                hydra.lib.strings.Cat2.apply(
                  "unbound type variables: {",
                  hydra.lib.strings.Intercalate.apply(
                    ", ",
                    hydra.lib.lists.Map.apply(
                      wrapped -> ((wrapped)).value,
                      hydra.lib.sets.ToList.apply((badvars))))),
                "} in type "),
              ((hydra_show_core_type2)).apply((typ))),
            " at path: "),
          hydra.lib.strings.Intercalate.apply(
            " >> ",
            hydra.lib.lists.Reverse.apply((trace)))),
        hydra.lib.maybes.Maybe.apply(
          "none",
          (java.util.function.Function<hydra.core.Binding, String>) (binding -> hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                ". bound term = ",
                ((hydra_show_core_term2)).apply(((binding)).term)),
              ". bound type = "),
            hydra.lib.maybes.Maybe.apply(
              "none",
              (hydra_show_core_typeScheme2),
              ((binding)).type))),
          (lbinding)))));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, Boolean> checkForUnboundTypeVariables_checkOptional(java.util.function.Function<T0, java.util.Set<hydra.core.Name>> hydra_rewriting_freeVariablesInType2, java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, java.util.function.Function<T0, String> hydra_show_core_type2, java.util.function.Function<hydra.core.TypeScheme, String> hydra_show_core_typeScheme2, hydra.util.Maybe<hydra.core.Binding> lbinding, java.util.Set<hydra.core.Name> svars, java.util.List<String> trace, java.util.Set<hydra.core.Name> vars, hydra.util.Maybe<T0> m) {
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapMaybe.apply(
        (java.util.function.Function<T0, hydra.compute.Flow<T1, Boolean>>) (v1 -> hydra.checking.Checking.<T0, T1>checkForUnboundTypeVariables_check(
          (hydra_rewriting_freeVariablesInType2),
          (hydra_show_core_term2),
          (hydra_show_core_type2),
          (hydra_show_core_typeScheme2),
          (lbinding),
          (svars),
          (trace),
          (vars),
          (v1))),
        (m)),
      (java.util.function.Function<hydra.util.Maybe<Boolean>, hydra.compute.Flow<T1, Boolean>>) (ignored -> hydra.lib.flows.Pure.apply(true)));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, Boolean> checkForUnboundTypeVariables_checkOptionalList(java.util.function.Function<T0, java.util.Set<hydra.core.Name>> hydra_rewriting_freeVariablesInType2, java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, java.util.function.Function<T0, String> hydra_show_core_type2, java.util.function.Function<hydra.core.TypeScheme, String> hydra_show_core_typeScheme2, hydra.util.Maybe<hydra.core.Binding> lbinding, java.util.Set<hydra.core.Name> svars, java.util.List<String> trace, java.util.Set<hydra.core.Name> vars, hydra.util.Maybe<java.util.List<T0>> ml) {
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapMaybe.apply(
        (java.util.function.Function<java.util.List<T0>, hydra.compute.Flow<T1, java.util.List<Boolean>>>) (l -> hydra.lib.flows.MapList.apply(
          (java.util.function.Function<T0, hydra.compute.Flow<T1, Boolean>>) (v1 -> hydra.checking.Checking.<T0, T1>checkForUnboundTypeVariables_check(
            (hydra_rewriting_freeVariablesInType2),
            (hydra_show_core_term2),
            (hydra_show_core_type2),
            (hydra_show_core_typeScheme2),
            (lbinding),
            (svars),
            (trace),
            (vars),
            (v1))),
          (l))),
        (ml)),
      (java.util.function.Function<hydra.util.Maybe<java.util.List<Boolean>>, hydra.compute.Flow<T1, Boolean>>) (ignored -> hydra.lib.flows.Pure.apply(true)));
  }
  
  static <T0> hydra.compute.Flow<T0, Boolean> checkForUnboundTypeVariables_forBinding(java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>> hydra_rewriting_freeVariablesInType2, java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>> hydra_rewriting_subterms2, java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, java.util.function.Function<hydra.core.Type, String> hydra_show_core_type2, java.util.function.Function<hydra.core.TypeScheme, String> hydra_show_core_typeScheme2, java.util.Set<hydra.core.Name> svars, java.util.List<String> trace, java.util.Set<hydra.core.Name> vars, hydra.core.Binding b) {
    hydra.core.Term bterm = ((b)).term;
    java.util.List<String> newTrace = hydra.lib.lists.Cons.apply(
      (((b)).name).value,
      (trace));
    java.util.Set<hydra.core.Name> newVars = hydra.lib.maybes.Maybe.apply(
      (vars),
      (java.util.function.Function<hydra.core.TypeScheme, java.util.Set<hydra.core.Name>>) (ts -> hydra.lib.sets.Union.apply(
        (vars),
        hydra.lib.sets.FromList.apply(((ts)).variables))),
      ((b)).type);
    return (((((java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.List<String>, java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, Boolean>>>>>) (v1 -> (java.util.function.Function<java.util.List<String>, java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, Boolean>>>>) (v2 -> (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, Boolean>>>) (v3 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, Boolean>>) (v4 -> hydra.checking.Checking.<T0>checkForUnboundTypeVariables_checkRecursive(
      (hydra_rewriting_freeVariablesInType2),
      (hydra_rewriting_subterms2),
      (hydra_show_core_term2),
      (hydra_show_core_type2),
      (hydra_show_core_typeScheme2),
      (svars),
      (v1),
      (v2),
      (v3),
      (v4))))))).apply((newVars))).apply((newTrace))).apply(hydra.util.Maybe.just((b)))).apply((bterm));
  }
  
  static <T0> hydra.compute.Flow<T0, Boolean> checkNominalApplication(hydra.typing.TypeContext tx, hydra.core.Name tname, java.util.List<hydra.core.Type> typeArgs) {
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>requireSchemaType(
        ((tx)).inferenceContext,
        (tname)),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, Boolean>>) (schemaType -> {
        Integer argslen = hydra.lib.lists.Length.apply((typeArgs));
        hydra.core.Type body = ((schemaType)).type;
        java.util.List<hydra.core.Name> vars = ((schemaType)).variables;
        Integer varslen = hydra.lib.lists.Length.apply((vars));
        return hydra.lib.logic.IfElse.apply(
          hydra.lib.equality.Equal.apply(
            (varslen),
            (argslen)),
          hydra.lib.flows.Pure.apply(true),
          hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                hydra.lib.strings.Cat2.apply(
                  hydra.lib.strings.Cat2.apply(
                    hydra.lib.strings.Cat2.apply(
                      hydra.lib.strings.Cat2.apply(
                        hydra.lib.strings.Cat2.apply(
                          "nominal type ",
                          ((tname)).value),
                        " applied to the wrong number of type arguments: "),
                      "(expected "),
                    hydra.lib.literals.ShowInt32.apply((varslen))),
                  " arguments, got "),
                hydra.lib.literals.ShowInt32.apply((argslen))),
              "): "),
            hydra.formatting.Formatting.showList(
              (hydra.show.core.Core::type),
              (typeArgs)))));
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> checkSameType(hydra.typing.TypeContext tx, String desc, java.util.List<hydra.core.Type> types) {
    return hydra.lib.logic.IfElse.apply(
      hydra.checking.Checking.typesAllEffectivelyEqual(
        (tx),
        (types)),
      hydra.lib.flows.Pure.apply(hydra.lib.lists.Head.apply((types))),
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
        "unequal types ",
        hydra.formatting.Formatting.showList(
          (hydra.show.core.Core::type),
          (types)),
        " in ",
        (desc)))));
  }
  
  static <T0> hydra.compute.Flow<T0, Boolean> checkType(hydra.typing.TypeContext tx, hydra.core.Term term, hydra.core.Type typ) {
    hydra.typing.InferenceContext cx = ((tx)).inferenceContext;
    java.util.Set<hydra.core.Name> vars = ((tx)).typeVariables;
    return hydra.lib.logic.IfElse.apply(
      (hydra.constants.Constants.debugInference),
      hydra.lib.flows.Bind.apply(
        hydra.checking.Checking.<T0>typeOf(
          (tx),
          (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
          (term)),
        (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, Boolean>>) (t0 -> hydra.lib.logic.IfElse.apply(
          hydra.checking.Checking.typesEffectivelyEqual(
            (tx),
            (t0),
            (typ)),
          hydra.lib.flows.Pure.apply(true),
          hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
            "type checking failed: expected ",
            hydra.show.core.Core.type((typ)),
            " but found ",
            hydra.show.core.Core.type((t0)))))))),
      hydra.lib.flows.Pure.apply(true));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.TypeSubst> checkTypeSubst(hydra.typing.InferenceContext cx, hydra.typing.TypeSubst subst) {
    java.util.function.Function<hydra.core.TypeScheme, Boolean> isNominal = (java.util.function.Function<hydra.core.TypeScheme, Boolean>) (ts -> (hydra.rewriting.Rewriting.deannotateType(((ts)).type)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Record ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Union ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Wrap ignored) {
        return true;
      }
    }));
    java.util.Map<hydra.core.Name, hydra.core.Type> s = ((subst)).value;
    java.util.Set<hydra.core.Name> vars = hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply((s)));
    java.util.Set<hydra.core.Name> suspectVars = hydra.lib.sets.Intersection.apply(
      (vars),
      hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply(((cx)).schemaTypes)));
    java.util.Set<hydra.core.Name> badVars = hydra.lib.sets.FromList.apply(hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.maybes.Maybe.apply(
        false,
        (isNominal),
        hydra.lexical.Lexical.dereferenceSchemaType(
          (v),
          ((cx)).schemaTypes))),
      hydra.lib.sets.ToList.apply((suspectVars))));
    java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>> badPairs = hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>, Boolean>) (p -> hydra.lib.sets.Member.apply(
        hydra.lib.pairs.First.apply((p)),
        (badVars))),
      hydra.lib.maps.ToList.apply((s)));
    java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>, String> printPair = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        (hydra.lib.pairs.First.apply((p))).value,
        " --> "),
      hydra.show.core.Core.type(hydra.lib.pairs.Second.apply((p)))));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.sets.Null.apply((badVars)),
      hydra.lib.flows.Pure.apply((subst)),
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "Schema type(s) incorrectly unified: {",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (printPair),
              (badPairs)))),
        "}")));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T2, Boolean> checkTypeVariables(T0 _tx, T1 _typ) {
    return hydra.lib.flows.Pure.apply(true);
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.Type> toFContext(hydra.typing.InferenceContext cx) {
    return hydra.lib.maps.Map.apply(
      (hydra.schemas.Schemas::typeSchemeToFType),
      ((cx)).dataTypes);
  }
  
  static Boolean typeListsEffectivelyEqual(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> tlist1, java.util.List<hydra.core.Type> tlist2) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply((tlist1)),
        hydra.lib.lists.Length.apply((tlist2))),
      hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<Boolean, java.util.function.Function<Boolean, Boolean>>) (p0 -> p1 -> hydra.lib.logic.And.apply(
          (p0),
          (p1))),
        true,
        hydra.lib.lists.ZipWith.apply(
          (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, Boolean>>) (v1 -> (java.util.function.Function<hydra.core.Type, Boolean>) (v2 -> hydra.checking.Checking.typesEffectivelyEqual(
            (tx),
            (v1),
            (v2)))),
          (tlist1),
          (tlist2))),
      false);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOf(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.Term term) {
    return hydra.monads.Monads.withTrace(
      "typeOf",
      hydra.checking.Checking.<T0>typeOf_check(
        (hydra.reflect.Reflect::termVariant),
        (hydra.show.meta.Meta::termVariant),
        (term),
        (tx),
        (typeArgs)));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOf_check(java.util.function.Function<hydra.core.Term, hydra.variants.TermVariant> hydra_reflect_termVariant2, java.util.function.Function<hydra.variants.TermVariant, String> hydra_show_meta_termVariant2, hydra.core.Term term, hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> otherwise(hydra.core.Term instance) {
        return hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
          "unsupported term variant in typeOf: ",
          ((hydra_show_meta_termVariant2)).apply(((hydra_reflect_termVariant2)).apply((term))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.Annotated v1) {
        return hydra.checking.Checking.<T0>typeOfAnnotatedTerm(
          (tx),
          (typeArgs),
          ((v1)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.Application v1) {
        return hydra.checking.Checking.<T0>typeOfApplication(
          (tx),
          (typeArgs),
          ((v1)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.Either v1) {
        return hydra.checking.Checking.<T0>typeOfEither(
          (tx),
          (typeArgs),
          ((v1)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.Function f) {
        return (((f)).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Function.Elimination elm) {
            return (((elm)).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Elimination.Record v1) {
                return hydra.checking.Checking.<T0>typeOfProjection(
                  (tx),
                  (typeArgs),
                  ((v1)).value);
              }
              
              @Override
              public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Elimination.Union v1) {
                return hydra.checking.Checking.<T0>typeOfCaseStatement(
                  (tx),
                  (typeArgs),
                  ((v1)).value);
              }
              
              @Override
              public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Elimination.Wrap v1) {
                return hydra.checking.Checking.<T0>typeOfUnwrap(
                  (tx),
                  (typeArgs),
                  ((v1)).value);
              }
            });
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Function.Lambda v1) {
            return hydra.checking.Checking.<T0>typeOfLambda(
              (tx),
              (typeArgs),
              ((v1)).value);
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Function.Primitive v1) {
            return hydra.checking.Checking.<T0>typeOfPrimitive(
              (tx),
              (typeArgs),
              ((v1)).value);
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.Let v1) {
        return hydra.checking.Checking.<T0>typeOfLet(
          (tx),
          (typeArgs),
          ((v1)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.List v1) {
        return hydra.checking.Checking.<T0>typeOfList(
          (tx),
          (typeArgs),
          ((v1)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.Literal v1) {
        return hydra.checking.Checking.<T0>typeOfLiteral(
          (tx),
          (typeArgs),
          ((v1)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.Map v1) {
        return hydra.checking.Checking.<T0>typeOfMap(
          (tx),
          (typeArgs),
          ((v1)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.Maybe v1) {
        return hydra.checking.Checking.<T0>typeOfMaybe(
          (tx),
          (typeArgs),
          ((v1)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.Pair v1) {
        return hydra.checking.Checking.<T0>typeOfPair(
          (tx),
          (typeArgs),
          ((v1)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.Record v1) {
        return hydra.checking.Checking.<T0>typeOfRecord(
          (tx),
          (typeArgs),
          ((v1)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.Set v1) {
        return hydra.checking.Checking.<T0>typeOfSet(
          (tx),
          (typeArgs),
          ((v1)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.TypeApplication v1) {
        return hydra.checking.Checking.<T0>typeOfTypeApplication(
          (tx),
          (typeArgs),
          ((v1)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.TypeLambda v1) {
        return hydra.checking.Checking.<T0>typeOfTypeLambda(
          (tx),
          (typeArgs),
          ((v1)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.Union v1) {
        return hydra.checking.Checking.<T0>typeOfInjection(
          (tx),
          (typeArgs),
          ((v1)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.Unit ignored) {
        return hydra.checking.Checking.<T0>typeOfUnit(
          (tx),
          (typeArgs));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.Variable v1) {
        return hydra.checking.Checking.<T0>typeOfVariable(
          (tx),
          (typeArgs),
          ((v1)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Term.Wrap v1) {
        return hydra.checking.Checking.<T0>typeOfWrappedTerm(
          (tx),
          (typeArgs),
          ((v1)).value);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfAnnotatedTerm(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.AnnotatedTerm at) {
    return hydra.checking.Checking.<T0>typeOf(
      (tx),
      (typeArgs),
      ((at)).body);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfApplication(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.Application app) {
    hydra.core.Term arg = ((app)).argument;
    hydra.core.Term fun = ((app)).function;
    return hydra.lib.flows.Bind.apply(
      hydra.checking.Checking.<T0>typeOf(
        (tx),
        (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
        (fun)),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (tfun -> hydra.lib.flows.Bind.apply(
        hydra.checking.Checking.<T0>typeOf(
          (tx),
          (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
          (arg)),
        (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (targ -> hydra.lib.flows.Bind.apply(
          (((java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>>) (v1 -> (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (v2 -> hydra.checking.Checking.typeOfApplication_tryType(
            (fun),
            (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, Boolean>>>) (p0 -> p1 -> p2 -> hydra.checking.Checking.typesEffectivelyEqual(
              (p0),
              (p1),
              (p2))),
            (hydra.show.core.Core::term),
            (hydra.show.core.Core::type),
            (tx),
            (v1),
            (v2))))).apply((tfun))).apply((targ)),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (t -> hydra.checking.Checking.<T0>applyTypeArgumentsToType(
            (tx),
            (typeArgs),
            (t))))))));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.core.Type> typeOfApplication_tryType(T0 fun, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, Boolean>>> hydra_checking_typesEffectivelyEqual2, java.util.function.Function<T0, String> hydra_show_core_term2, java.util.function.Function<hydra.core.Type, String> hydra_show_core_type2, hydra.typing.TypeContext tx, hydra.core.Type tfun, hydra.core.Type targ) {
    return ((tfun)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T1, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
          "left hand side of application (",
          ((hydra_show_core_term2)).apply((fun)),
          ") is not function-typed (",
          ((hydra_show_core_type2)).apply((tfun)),
          ")",
          ". types: ",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.List.of(
                (hydra.lib.pairs.First.apply((p))).value,
                ": ",
                ((hydra_show_core_type2)).apply(hydra.lib.pairs.Second.apply((p)))))),
              hydra.lib.maps.ToList.apply(((tx)).types))))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Type> visit(hydra.core.Type.Forall ft) {
        return (((java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T1, hydra.core.Type>>>) (v1 -> (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T1, hydra.core.Type>>) (v2 -> hydra.checking.Checking.<T0, T1>typeOfApplication_tryType(
          (fun),
          (hydra_checking_typesEffectivelyEqual2),
          (hydra_show_core_term2),
          (hydra_show_core_type2),
          (tx),
          (v1),
          (v2))))).apply((((ft)).value).body)).apply((targ));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Type> visit(hydra.core.Type.Function ft) {
        hydra.core.Type cod = (((ft)).value).codomain;
        hydra.core.Type dom = (((ft)).value).domain;
        return hydra.lib.logic.IfElse.apply(
          ((((hydra_checking_typesEffectivelyEqual2)).apply((tx))).apply((dom))).apply((targ)),
          hydra.lib.flows.Pure.apply((cod)),
          hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
            "in application, expected ",
            ((hydra_show_core_type2)).apply((dom)),
            " but found ",
            ((hydra_show_core_type2)).apply((targ))))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Type> visit(hydra.core.Type.Variable v) {
        return hydra.lib.flows.Map.apply(
          (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable((x))),
          hydra.schemas.Schemas.<T1>freshName());
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfCaseStatement(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.CaseStatement cs) {
    java.util.List<hydra.core.Field> cases = ((cs)).cases;
    java.util.List<hydra.core.Term> cterms = hydra.lib.lists.Map.apply(
      projected -> projected.term,
      (cases));
    hydra.util.Maybe<hydra.core.Term> dflt = ((cs)).default_;
    hydra.core.Name tname = ((cs)).typeName;
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapMaybe.apply(
        (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Type>>) (e -> hydra.checking.Checking.<T0>typeOf(
          (tx),
          (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
          (e))),
        (dflt)),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.compute.Flow<T0, hydra.core.Type>>) (tdflt -> hydra.lib.flows.Bind.apply(
        hydra.lib.flows.MapList.apply(
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Type>>) (e -> hydra.checking.Checking.<T0>typeOf(
            (tx),
            (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
            (e))),
          (cterms)),
        (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.compute.Flow<T0, hydra.core.Type>>) (tcterms -> hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (t -> hydra.lib.flows.Map.apply(
              projected -> projected.codomain,
              hydra.extract.core.Core.<T0>functionType((t)))),
            (tcterms)),
          (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.compute.Flow<T0, hydra.core.Type>>) (fcods -> {
            java.util.List<hydra.core.Type> cods = hydra.lib.maybes.Cat.apply(hydra.lib.lists.Cons.apply(
              (tdflt),
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) ((hydra.lib.maybes.Pure::apply)),
                (fcods))));
            return hydra.lib.flows.Bind.apply(
              hydra.checking.Checking.<T0>checkSameType(
                (tx),
                "case branches",
                (cods)),
              (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (cod -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.schemas.Schemas.nominalApplication(
                (tname),
                (typeArgs)), (cod))))));
          }))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfEither(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.util.Either<hydra.core.Term, hydra.core.Term> et) {
    return hydra.lib.flows.Bind.apply(
      hydra.checking.Checking.typeOfEither_checkLength((typeArgs)),
      (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Type>>) (ignored -> hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Type>>) (leftTerm -> hydra.lib.flows.Bind.apply(
          hydra.checking.Checking.<T0>typeOf(
            (tx),
            (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
            (leftTerm)),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (leftType -> hydra.lib.flows.Bind.apply(
            hydra.checking.Checking.checkTypeVariables(
              (tx),
              (leftType)),
            (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Type>>) (_2 -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Either(new hydra.core.EitherType((leftType), hydra.lib.lists.At.apply(
              1,
              (typeArgs)))))))))),
        (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Type>>) (rightTerm -> hydra.lib.flows.Bind.apply(
          hydra.checking.Checking.<T0>typeOf(
            (tx),
            (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
            (rightTerm)),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (rightType -> hydra.lib.flows.Bind.apply(
            hydra.checking.Checking.checkTypeVariables(
              (tx),
              (rightType)),
            (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Type>>) (_2 -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Either(new hydra.core.EitherType(hydra.lib.lists.At.apply(
              0,
              (typeArgs)), (rightType))))))))),
        (et))));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, Boolean> typeOfEither_checkLength(java.util.List<T0> typeArgs) {
    Integer n = hydra.lib.lists.Length.apply((typeArgs));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.equality.Equal.apply(
        (n),
        2),
      hydra.lib.flows.Pure.apply(true),
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
        "either type requires 2 type arguments, got ",
        hydra.lib.literals.ShowInt32.apply((n)))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfInjection(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.Injection injection) {
    hydra.core.Field field = ((injection)).field;
    hydra.core.Name fname = ((field)).name;
    hydra.core.Term fterm = ((field)).term;
    hydra.core.Name tname = ((injection)).typeName;
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>requireSchemaType(
        ((tx)).inferenceContext,
        (tname)),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.core.Type>>) (schemaType -> {
        hydra.core.Type sbody = ((schemaType)).type;
        java.util.List<hydra.core.Name> svars = ((schemaType)).variables;
        return hydra.lib.flows.Bind.apply(
          hydra.extract.core.Core.<T0>unionType(
            (tname),
            (sbody)),
          (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.compute.Flow<T0, hydra.core.Type>>) (sfields -> hydra.lib.flows.Bind.apply(
            hydra.schemas.Schemas.<T0>findFieldType(
              (fname),
              (sfields)),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (ftyp -> hydra.lib.flows.Pure.apply(hydra.schemas.Schemas.nominalApplication(
              (tname),
              (typeArgs)))))));
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfLambda(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.Lambda l) {
    hydra.core.Term body = ((l)).body;
    hydra.util.Maybe<hydra.core.Type> mdom = ((l)).domain;
    hydra.core.Name v = ((l)).parameter;
    return hydra.lib.flows.Bind.apply(
      hydra.lib.maybes.Maybe.apply(
        hydra.lib.flows.Fail.apply("untyped lambda"),
        (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (dom -> hydra.lib.flows.Bind.apply(
          hydra.checking.Checking.checkTypeVariables(
            (tx),
            (dom)),
          (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Type>>) (ignored -> {
            java.util.Map<hydra.core.Name, hydra.core.Type> types2 = hydra.lib.maps.Insert.apply(
              (v),
              (dom),
              ((tx)).types);
            return hydra.lib.flows.Bind.apply(
              hydra.checking.Checking.<T0>typeOf(
                new hydra.typing.TypeContext((types2), ((tx)).metadata, ((tx)).typeVariables, ((tx)).lambdaVariables, ((tx)).letVariables, ((tx)).inferenceContext),
                (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
                (body)),
              (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (cod -> hydra.lib.flows.Bind.apply(
                hydra.checking.Checking.checkTypeVariables(
                  (tx),
                  (cod)),
                (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Type>>) (_2 -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Function(new hydra.core.FunctionType((dom), (cod))))))));
          }))),
        (mdom)),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (tbody -> hydra.checking.Checking.<T0>applyTypeArgumentsToType(
        (tx),
        (typeArgs),
        (tbody))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfLet(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.Let letTerm) {
    java.util.List<hydra.core.Binding> bs = ((letTerm)).bindings;
    java.util.List<hydra.core.Name> bnames = hydra.lib.lists.Map.apply(
      projected -> projected.name,
      (bs));
    hydra.core.Term body = ((letTerm)).body;
    java.util.List<hydra.core.Term> bterms = hydra.lib.lists.Map.apply(
      projected -> projected.term,
      (bs));
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapList.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<T0, hydra.core.Type>>) (v1 -> hydra.checking.Checking.typeOfLet_bindingType(
          (hydra.schemas.Schemas::typeSchemeToFType),
          (hydra.show.core.Core::binding),
          (v1))),
        (bs)),
      (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.compute.Flow<T0, hydra.core.Type>>) (btypes -> {
        hydra.typing.TypeContext tx2 = new hydra.typing.TypeContext(hydra.lib.maps.Union.apply(
          hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
            (bnames),
            (btypes))),
          ((tx)).types), ((tx)).metadata, ((tx)).typeVariables, ((tx)).lambdaVariables, ((tx)).letVariables, ((tx)).inferenceContext);
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Type>>) (v1 -> hydra.checking.Checking.<T0>typeOf(
              (tx2),
              (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
              (v1))),
            (bterms)),
          (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.compute.Flow<T0, hydra.core.Type>>) (typeofs -> hydra.lib.flows.Bind.apply(
            hydra.lib.logic.IfElse.apply(
              hydra.checking.Checking.typeListsEffectivelyEqual(
                (tx),
                (typeofs),
                (btypes)),
              hydra.checking.Checking.<T0>typeOf(
                (tx2),
                (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
                (body)),
              hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
                "binding types disagree: ",
                hydra.formatting.Formatting.showList(
                  (hydra.show.core.Core::type),
                  (btypes)),
                " and ",
                hydra.formatting.Formatting.showList(
                  (hydra.show.core.Core::type),
                  (typeofs)),
                " from terms: ",
                hydra.formatting.Formatting.showList(
                  (hydra.show.core.Core::term),
                  (bterms)))))),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (t -> hydra.checking.Checking.<T0>applyTypeArgumentsToType(
              (tx),
              (typeArgs),
              (t))))));
      }));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, T0> typeOfLet_bindingType(java.util.function.Function<hydra.core.TypeScheme, T0> hydra_schemas_typeSchemeToFType2, java.util.function.Function<hydra.core.Binding, String> hydra_show_core_binding2, hydra.core.Binding b) {
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
        "untyped let binding: ",
        ((hydra_show_core_binding2)).apply((b))))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T1, T0>>) (ts -> hydra.lib.flows.Pure.apply(((hydra_schemas_typeSchemeToFType2)).apply((ts)))),
      ((b)).type);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfList(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, java.util.List<hydra.core.Term> els) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((els)),
      hydra.lib.logic.IfElse.apply(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply((typeArgs)),
          1),
        hydra.lib.flows.Pure.apply(new hydra.core.Type.List(hydra.lib.lists.Head.apply((typeArgs)))),
        hydra.lib.flows.Fail.apply("list type applied to more or less than one argument")),
      hydra.lib.flows.Bind.apply(
        hydra.lib.flows.MapList.apply(
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Type>>) (v1 -> hydra.checking.Checking.<T0>typeOf(
            (tx),
            (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
            (v1))),
          (els)),
        (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.compute.Flow<T0, hydra.core.Type>>) (eltypes -> hydra.lib.flows.Bind.apply(
          hydra.checking.Checking.<T0>checkSameType(
            (tx),
            "list elements",
            (eltypes)),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (unifiedType -> hydra.lib.flows.Bind.apply(
            hydra.checking.Checking.checkTypeVariables(
              (tx),
              (unifiedType)),
            (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Type>>) (ignored -> hydra.lib.flows.Pure.apply(new hydra.core.Type.List((unifiedType))))))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfLiteral(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.Literal lit) {
    hydra.core.Type t = new hydra.core.Type.Literal(hydra.reflect.Reflect.literalType((lit)));
    return hydra.checking.Checking.<T0>applyTypeArgumentsToType(
      (tx),
      (typeArgs),
      (t));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfMap(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, java.util.Map<hydra.core.Term, hydra.core.Term> m) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.maps.Null.apply((m)),
      hydra.lib.logic.IfElse.apply(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply((typeArgs)),
          2),
        hydra.lib.flows.Pure.apply(new hydra.core.Type.Map(new hydra.core.MapType(hydra.lib.lists.At.apply(
          0,
          (typeArgs)), hydra.lib.lists.At.apply(
          1,
          (typeArgs))))),
        hydra.lib.flows.Fail.apply("map type applied to more or less than two arguments")),
      hydra.checking.Checking.<T0>typeOfMap_nonnull(
        (m),
        (tx),
        (typeArgs)));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfMap_nonnull(java.util.Map<hydra.core.Term, hydra.core.Term> m, hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs) {
    java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>> pairs = hydra.lib.maps.ToList.apply((m));
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.Bind.apply(
        hydra.lib.flows.MapList.apply(
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Type>>) (v1 -> hydra.checking.Checking.<T0>typeOf(
            (tx),
            (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
            (v1))),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.core.Term>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.core.Term>) ((hydra.lib.pairs.First::apply))),
            (pairs))),
        (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.compute.Flow<T0, hydra.core.Type>>) (v1 -> hydra.checking.Checking.<T0>checkSameType(
          (tx),
          "map keys",
          (v1)))),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (kt -> hydra.lib.flows.Bind.apply(
        hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Type>>) (v1 -> hydra.checking.Checking.<T0>typeOf(
              (tx),
              (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
              (v1))),
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.core.Term>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.core.Term>) ((hydra.lib.pairs.Second::apply))),
              (pairs))),
          (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.compute.Flow<T0, hydra.core.Type>>) (v1 -> hydra.checking.Checking.<T0>checkSameType(
            (tx),
            "map values",
            (v1)))),
        (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (vt -> hydra.lib.flows.Bind.apply(
          hydra.checking.Checking.checkTypeVariables(
            (tx),
            (kt)),
          (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Type>>) (ignored -> hydra.lib.flows.Bind.apply(
            hydra.checking.Checking.checkTypeVariables(
              (tx),
              (vt)),
            (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Type>>) (_2 -> hydra.checking.Checking.<T0>applyTypeArgumentsToType(
              (tx),
              (typeArgs),
              new hydra.core.Type.Map(new hydra.core.MapType((kt), (vt))))))))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfMaybe(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.util.Maybe<hydra.core.Term> mt) {
    return hydra.lib.maybes.Maybe.apply(
      hydra.checking.Checking.<T0>typeOfMaybe_forNothing((typeArgs)),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Type>>) (v1 -> hydra.checking.Checking.<T0>typeOfMaybe_forJust(
        (tx),
        (typeArgs),
        (v1))),
      (mt));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfMaybe_forNothing(java.util.List<hydra.core.Type> typeArgs) {
    Integer n = hydra.lib.lists.Length.apply((typeArgs));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.equality.Equal.apply(
        (n),
        1),
      hydra.lib.flows.Pure.apply(new hydra.core.Type.Maybe(hydra.lib.lists.Head.apply((typeArgs)))),
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "optional type applied to ",
          hydra.lib.literals.ShowInt32.apply((n))),
        " argument(s). Expected 1.")));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfMaybe_forJust(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.Term term) {
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.Bind.apply(
        hydra.checking.Checking.<T0>typeOf(
          (tx),
          (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
          (term)),
        (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (termType -> hydra.lib.flows.Bind.apply(
          hydra.checking.Checking.checkTypeVariables(
            (tx),
            (termType)),
          (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Type>>) (ignored -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Maybe((termType))))))),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (t -> hydra.checking.Checking.<T0>applyTypeArgumentsToType(
        (tx),
        (typeArgs),
        (t))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfPair(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term> p) {
    return hydra.lib.flows.Bind.apply(
      hydra.checking.Checking.typeOfPair_checkLength((typeArgs)),
      (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Type>>) (ignored -> {
        hydra.core.Term pairFst = hydra.lib.pairs.First.apply((p));
        hydra.core.Term pairSnd = hydra.lib.pairs.Second.apply((p));
        return hydra.lib.flows.Bind.apply(
          hydra.checking.Checking.<T0>typeOf(
            (tx),
            (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
            (pairFst)),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (firstType -> hydra.lib.flows.Bind.apply(
            hydra.checking.Checking.checkTypeVariables(
              (tx),
              (firstType)),
            (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Type>>) (_2 -> hydra.lib.flows.Bind.apply(
              hydra.checking.Checking.<T0>typeOf(
                (tx),
                (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
                (pairSnd)),
              (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (secondType -> hydra.lib.flows.Bind.apply(
                hydra.checking.Checking.checkTypeVariables(
                  (tx),
                  (secondType)),
                (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Type>>) (_3 -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Pair(new hydra.core.PairType((firstType), (secondType))))))))))));
      }));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, Boolean> typeOfPair_checkLength(java.util.List<T0> typeArgs) {
    Integer n = hydra.lib.lists.Length.apply((typeArgs));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.equality.Equal.apply(
        (n),
        2),
      hydra.lib.flows.Pure.apply(true),
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
        "pair type requires 2 type arguments, got ",
        hydra.lib.literals.ShowInt32.apply((n)))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfPrimitive(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.Name name) {
    return hydra.lib.flows.Bind.apply(
      hydra.lib.maybes.Maybe.apply(
        hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
          "no such primitive: ",
          ((name)).value))),
        p0 -> hydra.schemas.Schemas.<T0>instantiateTypeScheme((p0)),
        hydra.lib.maps.Lookup.apply(
          (name),
          (((tx)).inferenceContext).primitiveTypes)),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.core.Type>>) (ts -> {
        hydra.core.Type t = hydra.schemas.Schemas.typeSchemeToFType((ts));
        return hydra.checking.Checking.<T0>applyTypeArgumentsToType(
          (tx),
          (typeArgs),
          (t));
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfProjection(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.Projection p) {
    hydra.core.Name fname = ((p)).field;
    hydra.core.Name tname = ((p)).typeName;
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>requireSchemaType(
        ((tx)).inferenceContext,
        (tname)),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.core.Type>>) (schemaType -> {
        hydra.core.Type sbody = ((schemaType)).type;
        java.util.List<hydra.core.Name> svars = ((schemaType)).variables;
        return hydra.lib.flows.Bind.apply(
          hydra.extract.core.Core.<T0>recordType(
            (tname),
            (sbody)),
          (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.compute.Flow<T0, hydra.core.Type>>) (sfields -> hydra.lib.flows.Bind.apply(
            hydra.schemas.Schemas.<T0>findFieldType(
              (fname),
              (sfields)),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (ftyp -> {
              hydra.typing.TypeSubst subst = new hydra.typing.TypeSubst(hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
                (svars),
                (typeArgs))));
              hydra.core.Type sftyp = hydra.substitution.Substitution.substInType(
                (subst),
                (ftyp));
              return hydra.lib.flows.Pure.apply(new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.schemas.Schemas.nominalApplication(
                (tname),
                (typeArgs)), (sftyp))));
            }))));
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfRecord(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.Record record) {
    java.util.List<hydra.core.Field> fields = ((record)).fields;
    hydra.core.Name tname = ((record)).typeName;
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapList.apply(
        (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Type>>) (v1 -> hydra.checking.Checking.<T0>typeOf(
          (tx),
          (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
          (v1))),
        hydra.lib.lists.Map.apply(
          projected -> projected.term,
          (fields))),
      (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.compute.Flow<T0, hydra.core.Type>>) (ftypes -> hydra.lib.flows.Bind.apply(
        hydra.lib.flows.MapList.apply(
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, Boolean>>) (v1 -> hydra.checking.Checking.checkTypeVariables(
            (tx),
            (v1))),
          (ftypes)),
        (java.util.function.Function<java.util.List<Boolean>, hydra.compute.Flow<T0, hydra.core.Type>>) (ignored -> hydra.lib.flows.Pure.apply(hydra.schemas.Schemas.nominalApplication(
          (tname),
          (typeArgs)))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfSet(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, java.util.Set<hydra.core.Term> els) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.sets.Null.apply((els)),
      hydra.lib.logic.IfElse.apply(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply((typeArgs)),
          1),
        hydra.lib.flows.Pure.apply(new hydra.core.Type.Set(hydra.lib.lists.Head.apply((typeArgs)))),
        hydra.lib.flows.Fail.apply("set type applied to more or less than one argument")),
      hydra.lib.flows.Bind.apply(
        hydra.lib.flows.MapList.apply(
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Type>>) (v1 -> hydra.checking.Checking.<T0>typeOf(
            (tx),
            (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
            (v1))),
          hydra.lib.sets.ToList.apply((els))),
        (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.compute.Flow<T0, hydra.core.Type>>) (eltypes -> hydra.lib.flows.Bind.apply(
          hydra.checking.Checking.<T0>checkSameType(
            (tx),
            "set elements",
            (eltypes)),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (unifiedType -> hydra.lib.flows.Bind.apply(
            hydra.checking.Checking.checkTypeVariables(
              (tx),
              (unifiedType)),
            (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Type>>) (ignored -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Set((unifiedType))))))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfTypeApplication(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.TypeApplicationTerm tyapp) {
    hydra.core.Term body = ((tyapp)).body;
    hydra.core.Type t = ((tyapp)).type;
    return hydra.checking.Checking.<T0>typeOf(
      (tx),
      hydra.lib.lists.Cons.apply(
        (t),
        (typeArgs)),
      (body));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfTypeLambda(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.TypeLambda tl) {
    hydra.core.Term body = ((tl)).body;
    hydra.core.Name v = ((tl)).parameter;
    java.util.Set<hydra.core.Name> vars = ((tx)).typeVariables;
    hydra.typing.TypeContext tx2 = new hydra.typing.TypeContext(((tx)).types, ((tx)).metadata, hydra.lib.sets.Insert.apply(
      (v),
      (vars)), ((tx)).lambdaVariables, ((tx)).letVariables, ((tx)).inferenceContext);
    return hydra.lib.flows.Bind.apply(
      hydra.checking.Checking.<T0>typeOf(
        (tx2),
        (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
        (body)),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (t1 -> hydra.lib.flows.Bind.apply(
        hydra.checking.Checking.checkTypeVariables(
          (tx2),
          (t1)),
        (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Type>>) (ignored -> hydra.checking.Checking.<T0>applyTypeArgumentsToType(
          (tx),
          (typeArgs),
          new hydra.core.Type.Forall(new hydra.core.ForallType((v), (t1))))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfUnit(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs) {
    return hydra.checking.Checking.<T0>applyTypeArgumentsToType(
      (tx),
      (typeArgs),
      new hydra.core.Type.Unit(true));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfUnwrap(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.Name tname) {
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>requireSchemaType(
        ((tx)).inferenceContext,
        (tname)),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.core.Type>>) (schemaType -> {
        hydra.core.Type sbody = ((schemaType)).type;
        java.util.List<hydra.core.Name> svars = ((schemaType)).variables;
        return hydra.lib.flows.Bind.apply(
          hydra.extract.core.Core.<T0>wrappedType(
            (tname),
            (sbody)),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (wrapped -> {
            hydra.typing.TypeSubst subst = new hydra.typing.TypeSubst(hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
              (svars),
              (typeArgs))));
            hydra.core.Type swrapped = hydra.substitution.Substitution.substInType(
              (subst),
              (wrapped));
            return hydra.lib.flows.Pure.apply(new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.schemas.Schemas.nominalApplication(
              (tname),
              (typeArgs)), (swrapped))));
          }));
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfVariable(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.Name name) {
    hydra.util.Maybe<hydra.core.Type> rawType = hydra.lib.maps.Lookup.apply(
      (name),
      ((tx)).types);
    return hydra.lib.flows.Bind.apply(
      hydra.lib.maybes.Maybe.apply(
        hydra.checking.Checking.typeOfVariable_failMsg(
          (name),
          (tx)),
        (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (t -> hydra.lib.logic.IfElse.apply(
          hydra.lib.lists.Null.apply((typeArgs)),
          hydra.schemas.Schemas.<T0>instantiateType((t)),
          hydra.lib.flows.Pure.apply((t)))),
        (rawType)),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (t -> hydra.checking.Checking.<T0>applyTypeArgumentsToType(
        (tx),
        (typeArgs),
        (t))));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, T1> typeOfVariable_failMsg(hydra.core.Name name, hydra.typing.TypeContext tx) {
    return hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
      "unbound variable: ",
      ((name)).value,
      ". Variables: {",
      hydra.lib.strings.Intercalate.apply(
        ", ",
        hydra.lib.lists.Map.apply(
          wrapped -> ((wrapped)).value,
          hydra.lib.maps.Keys.apply(((tx)).types))),
      "}")));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> typeOfWrappedTerm(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> typeArgs, hydra.core.WrappedTerm wt) {
    hydra.core.Term body = ((wt)).body;
    hydra.core.Name tname = ((wt)).typeName;
    return hydra.lib.flows.Bind.apply(
      hydra.checking.Checking.<T0>typeOf(
        (tx),
        (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
        (body)),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (btype -> hydra.lib.flows.Bind.apply(
        hydra.checking.Checking.checkTypeVariables(
          (tx),
          (btype)),
        (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Type>>) (ignored -> hydra.lib.flows.Pure.apply(hydra.schemas.Schemas.nominalApplication(
          (tname),
          (typeArgs)))))));
  }
  
  static Boolean containsInScopeTypeVars(hydra.typing.TypeContext tx, hydra.core.Type t) {
    java.util.Set<hydra.core.Name> freeVars = hydra.rewriting.Rewriting.freeVariablesInTypeSimple((t));
    java.util.Set<hydra.core.Name> vars = ((tx)).typeVariables;
    return hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.lib.sets.Intersection.apply(
      (vars),
      (freeVars))));
  }
  
  static hydra.core.Type normalizeTypeFreeVars(hydra.core.Type typ) {
    java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.Map<hydra.core.Name, hydra.core.Name>>> collectVars = (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, java.util.Map<hydra.core.Name, hydra.core.Name>>>) (acc -> (java.util.function.Function<hydra.core.Type, java.util.Map<hydra.core.Name, hydra.core.Name>>) (t -> ((t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (acc);
      }
      
      @Override
      public java.util.Map<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Variable v) {
        return hydra.lib.logic.IfElse.apply(
          hydra.lib.maps.Member.apply(
            ((v)).value,
            (acc)),
          (acc),
          hydra.lib.maps.Insert.apply(
            ((v)).value,
            new hydra.core.Name(hydra.lib.strings.Cat2.apply(
              "_tv",
              hydra.lib.literals.ShowInt32.apply(hydra.lib.maps.Size.apply((acc))))),
            (acc)));
      }
    })));
    java.util.Map<hydra.core.Name, hydra.core.Name> subst = hydra.rewriting.Rewriting.foldOverType(
      new hydra.coders.TraversalOrder.Pre(true),
      (collectVars),
      (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
      (typ));
    return hydra.rewriting.Rewriting.substituteTypeVariables(
      (subst),
      (typ));
  }
  
  static Boolean typesAllEffectivelyEqual(hydra.typing.TypeContext tx, java.util.List<hydra.core.Type> tlist) {
    java.util.Map<hydra.core.Name, hydra.core.TypeScheme> types = (((tx)).inferenceContext).schemaTypes;
    java.util.function.Function<hydra.core.Type, Boolean> containsFreeVar = (java.util.function.Function<hydra.core.Type, Boolean>) (t -> {
      java.util.Set<hydra.core.Name> allVars = hydra.rewriting.Rewriting.freeVariablesInTypeSimple((t));
      java.util.Set<hydra.core.Name> schemaNames = hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply((types)));
      return hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.lib.sets.Difference.apply(
        (allVars),
        (schemaNames))));
    });
    Boolean anyContainsFreeVar = hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Type, Boolean>>) (acc -> (java.util.function.Function<hydra.core.Type, Boolean>) (t -> hydra.lib.logic.Or.apply(
        (acc),
        ((containsFreeVar)).apply((t))))),
      false,
      (tlist));
    return hydra.lib.logic.IfElse.apply(
      (anyContainsFreeVar),
      true,
      hydra.lib.logic.IfElse.apply(
        hydra.checking.Checking.allEqual(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> hydra.checking.Checking.normalizeTypeFreeVars((t))),
          (tlist))),
        true,
        hydra.checking.Checking.allEqual(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> hydra.checking.Checking.normalizeTypeFreeVars(hydra.rewriting.Rewriting.deannotateTypeRecursive(hydra.rewriting.Rewriting.replaceTypedefs(
            (types),
            (t))))),
          (tlist)))));
  }
  
  static Boolean typesEffectivelyEqual(hydra.typing.TypeContext tx, hydra.core.Type t1, hydra.core.Type t2) {
    return hydra.lib.logic.Or.apply(
      hydra.checking.Checking.containsInScopeTypeVars(
        (tx),
        (t1)),
      hydra.lib.logic.Or.apply(
        hydra.checking.Checking.containsInScopeTypeVars(
          (tx),
          (t2)),
        hydra.checking.Checking.typesAllEffectivelyEqual(
          (tx),
          java.util.List.of(
            hydra.schemas.Schemas.fullyStripAndNormalizeType((t1)),
            hydra.schemas.Schemas.fullyStripAndNormalizeType((t2))))));
  }
}
