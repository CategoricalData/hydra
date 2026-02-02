// Note: this is an automatically generated file. Do not edit.

package hydra.unification;

/**
 * Utilities for type unification.
 */
public interface Unification {
  static <T0> hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> joinTypes(hydra.core.Type left, hydra.core.Type right, String comment) {
    java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>> joinOne = (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>>) (l -> (java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>) (r -> new hydra.typing.TypeConstraint((l), (r), hydra.lib.strings.Cat2.apply(
      "join types; ",
      (comment)))));
    hydra.core.Type sleft = hydra.rewriting.Rewriting.deannotateType((left));
    hydra.core.Type sright = hydra.rewriting.Rewriting.deannotateType((right));
    return ((sleft)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
        return hydra.unification.Unification.joinTypes_cannotUnify(
          (hydra.show.core.Core::type),
          (sleft),
          (sright));
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Application l) {
        return ((sright)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              (hydra.show.core.Core::type),
              (sleft),
              (sright));
          }
          
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Application r) {
            return hydra.lib.flows.Pure.apply(java.util.List.of(
              (((joinOne)).apply((((l)).value).function)).apply((((r)).value).function),
              (((joinOne)).apply((((l)).value).argument)).apply((((r)).value).argument)));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Either l) {
        return ((sright)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              (hydra.show.core.Core::type),
              (sleft),
              (sright));
          }
          
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Either r) {
            return hydra.lib.flows.Pure.apply(java.util.List.of(
              (((joinOne)).apply((((l)).value).left)).apply((((r)).value).left),
              (((joinOne)).apply((((l)).value).right)).apply((((r)).value).right)));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Function l) {
        return ((sright)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              (hydra.show.core.Core::type),
              (sleft),
              (sright));
          }
          
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Function r) {
            return hydra.lib.flows.Pure.apply(java.util.List.of(
              (((joinOne)).apply((((l)).value).domain)).apply((((r)).value).domain),
              (((joinOne)).apply((((l)).value).codomain)).apply((((r)).value).codomain)));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.List l) {
        return ((sright)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              (hydra.show.core.Core::type),
              (sleft),
              (sright));
          }
          
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.List r) {
            return hydra.lib.flows.Pure.apply(java.util.List.of((((joinOne)).apply(((l)).value)).apply(((r)).value)));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Literal ignored) {
        return hydra.unification.Unification.joinTypes_assertEqual(
          (hydra.show.core.Core::type),
          (sleft),
          (sright));
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Map l) {
        return ((sright)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              (hydra.show.core.Core::type),
              (sleft),
              (sright));
          }
          
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Map r) {
            return hydra.lib.flows.Pure.apply(java.util.List.of(
              (((joinOne)).apply((((l)).value).keys)).apply((((r)).value).keys),
              (((joinOne)).apply((((l)).value).values)).apply((((r)).value).values)));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Maybe l) {
        return ((sright)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              (hydra.show.core.Core::type),
              (sleft),
              (sright));
          }
          
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Maybe r) {
            return hydra.lib.flows.Pure.apply(java.util.List.of((((joinOne)).apply(((l)).value)).apply(((r)).value)));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Pair l) {
        return ((sright)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              (hydra.show.core.Core::type),
              (sleft),
              (sright));
          }
          
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Pair r) {
            return hydra.lib.flows.Pure.apply(java.util.List.of(
              (((joinOne)).apply((((l)).value).first)).apply((((r)).value).first),
              (((joinOne)).apply((((l)).value).second)).apply((((r)).value).second)));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Record l) {
        return ((sright)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              (hydra.show.core.Core::type),
              (sleft),
              (sright));
          }
          
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Record r) {
            return (((java.util.function.Function<hydra.core.RowType, java.util.function.Function<hydra.core.RowType, hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>>>>) (v1 -> (java.util.function.Function<hydra.core.RowType, hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>>>) (v2 -> hydra.unification.Unification.joinTypes_joinRowTypes(
              (hydra.show.core.Core::type),
              (sleft),
              (sright),
              (joinOne),
              (v1),
              (v2))))).apply(((l)).value)).apply(((r)).value);
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Set l) {
        return ((sright)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              (hydra.show.core.Core::type),
              (sleft),
              (sright));
          }
          
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Set r) {
            return hydra.lib.flows.Pure.apply(java.util.List.of((((joinOne)).apply(((l)).value)).apply(((r)).value)));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Union l) {
        return ((sright)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              (hydra.show.core.Core::type),
              (sleft),
              (sright));
          }
          
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Union r) {
            return (((java.util.function.Function<hydra.core.RowType, java.util.function.Function<hydra.core.RowType, hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>>>>) (v1 -> (java.util.function.Function<hydra.core.RowType, hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>>>) (v2 -> hydra.unification.Unification.joinTypes_joinRowTypes(
              (hydra.show.core.Core::type),
              (sleft),
              (sright),
              (joinOne),
              (v1),
              (v2))))).apply(((l)).value)).apply(((r)).value);
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Unit ignored) {
        return ((sright)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              (hydra.show.core.Core::type),
              (sleft),
              (sright));
          }
          
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Unit _2) {
            return hydra.lib.flows.Pure.apply((java.util.List<hydra.typing.TypeConstraint>) (java.util.List.<hydra.typing.TypeConstraint>of()));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Wrap l) {
        return ((sright)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              (hydra.show.core.Core::type),
              (sleft),
              (sright));
          }
          
          @Override
          public hydra.compute.Flow<T0, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Wrap r) {
            return hydra.lib.logic.IfElse.apply(
              hydra.lib.equality.Equal.apply(
                ((((l)).value).typeName).value,
                ((((r)).value).typeName).value),
              hydra.lib.flows.Pure.apply(java.util.List.of((((joinOne)).apply((((l)).value).body)).apply((((r)).value).body))),
              hydra.unification.Unification.joinTypes_cannotUnify(
                (hydra.show.core.Core::type),
                (sleft),
                (sright)));
          }
        });
      }
    });
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T1, T2> joinTypes_cannotUnify(java.util.function.Function<T0, String> hydra_show_core_type2, T0 sleft, T0 sright) {
    return hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "cannot unify ",
          ((hydra_show_core_type2)).apply((sleft))),
        " with "),
      ((hydra_show_core_type2)).apply((sright))));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T1, java.util.List<T2>> joinTypes_assertEqual(java.util.function.Function<T0, String> hydra_show_core_type2, T0 sleft, T0 sright) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.equality.Equal.apply(
        (sleft),
        (sright)),
      hydra.lib.flows.Pure.apply((java.util.List<T2>) (java.util.List.<T2>of())),
      hydra.unification.Unification.<T0, T1, java.util.List<T2>>joinTypes_cannotUnify(
        (hydra_show_core_type2),
        (sleft),
        (sright)));
  }
  
  static <T0, T1, T2, T3, T4> hydra.compute.Flow<T4, java.util.List<T3>> joinTypes_joinList(java.util.function.Function<T0, String> hydra_show_core_type2, T0 sleft, T0 sright, java.util.function.Function<T1, java.util.function.Function<T2, T3>> joinOne, java.util.List<T1> lefts, java.util.List<T2> rights) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply((lefts)),
        hydra.lib.lists.Length.apply((rights))),
      hydra.lib.flows.Pure.apply(hydra.lib.lists.ZipWith.apply(
        (joinOne),
        (lefts),
        (rights))),
      hydra.unification.Unification.<T0, T4, java.util.List<T3>>joinTypes_cannotUnify(
        (hydra_show_core_type2),
        (sleft),
        (sright)));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T2, java.util.List<T1>> joinTypes_joinRowTypes(java.util.function.Function<T0, String> hydra_show_core_type2, T0 sleft, T0 sright, java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, T1>> joinOne, hydra.core.RowType left, hydra.core.RowType right) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.logic.And.apply(
        hydra.lib.equality.Equal.apply(
          (((left)).typeName).value,
          (((right)).typeName).value),
        hydra.lib.logic.And.apply(
          hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply(hydra.lib.lists.Map.apply(
              projected -> projected.name,
              ((left)).fields)),
            hydra.lib.lists.Length.apply(hydra.lib.lists.Map.apply(
              projected -> projected.name,
              ((right)).fields))),
          hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<Boolean, java.util.function.Function<Boolean, Boolean>>) (p0 -> p1 -> hydra.lib.logic.And.apply(
              (p0),
              (p1))),
            true,
            hydra.lib.lists.ZipWith.apply(
              (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Name, Boolean>>) (left2 -> (java.util.function.Function<hydra.core.Name, Boolean>) (right2 -> hydra.lib.equality.Equal.apply(
                ((left2)).value,
                ((right2)).value))),
              hydra.lib.lists.Map.apply(
                projected -> projected.name,
                ((left)).fields),
              hydra.lib.lists.Map.apply(
                projected -> projected.name,
                ((right)).fields))))),
      (((java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.List<hydra.core.Type>, hydra.compute.Flow<T2, java.util.List<T1>>>>) (v1 -> (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.compute.Flow<T2, java.util.List<T1>>>) (v2 -> hydra.unification.Unification.joinTypes_joinList(
        (hydra_show_core_type2),
        (sleft),
        (sright),
        (joinOne),
        (v1),
        (v2))))).apply(hydra.lib.lists.Map.apply(
        projected -> projected.type,
        ((left)).fields))).apply(hydra.lib.lists.Map.apply(
        projected -> projected.type,
        ((right)).fields)),
      hydra.unification.Unification.<T0, T2, java.util.List<T1>>joinTypes_cannotUnify(
        (hydra_show_core_type2),
        (sleft),
        (sright)));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.TypeSubst> unifyTypeConstraints(java.util.Map<hydra.core.Name, T0> schemaTypes, java.util.List<hydra.typing.TypeConstraint> constraints) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((constraints)),
      hydra.lib.flows.Pure.apply((hydra.substitution.Substitution.idTypeSubst)),
      hydra.unification.Unification.<T0, T1>unifyTypeConstraints_withConstraint(
        (hydra.rewriting.Rewriting::deannotateType),
        (hydra.show.core.Core::type),
        (java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>>) (p0 -> p1 -> hydra.substitution.Substitution.composeTypeSubst(
          (p0),
          (p1))),
        (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.typing.TypeSubst>>) (p0 -> p1 -> hydra.substitution.Substitution.singletonTypeSubst(
          (p0),
          (p1))),
        (java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, java.util.List<hydra.typing.TypeConstraint>>>) (p0 -> p1 -> hydra.substitution.Substitution.substituteInConstraints(
          (p0),
          (p1))),
        (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, Boolean>>) (p0 -> p1 -> hydra.unification.Unification.variableOccursInType(
          (p0),
          (p1))),
        (schemaTypes),
        hydra.lib.lists.Head.apply((constraints)),
        hydra.lib.lists.Tail.apply((constraints))));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.TypeSubst> unifyTypeConstraints_withConstraint(java.util.function.Function<hydra.core.Type, hydra.core.Type> hydra_rewriting_deannotateType2, java.util.function.Function<hydra.core.Type, String> hydra_show_core_type2, java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>> hydra_substitution_composeTypeSubst2, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.typing.TypeSubst>> hydra_substitution_singletonTypeSubst2, java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, java.util.List<hydra.typing.TypeConstraint>>> hydra_substitution_substituteInConstraints2, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, Boolean>> hydra_unification_variableOccursInType2, java.util.Map<hydra.core.Name, T0> schemaTypes, hydra.typing.TypeConstraint c, java.util.List<hydra.typing.TypeConstraint> rest) {
    String comment = ((c)).comment;
    hydra.core.Type sleft = ((hydra_rewriting_deannotateType2)).apply(((c)).left);
    hydra.core.Type sright = ((hydra_rewriting_deannotateType2)).apply(((c)).right);
    return ((sleft)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T1, hydra.typing.TypeSubst> otherwise(hydra.core.Type instance) {
        return hydra.unification.Unification.unifyTypeConstraints_dflt(
          hydra.unification.Unification.<T0, T1>unifyTypeConstraints_noVars(
            (comment),
            (rest),
            (schemaTypes),
            (sleft),
            (sright)),
          (sleft),
          (sright),
          (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T1, hydra.typing.TypeSubst>>>) (v1 -> (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T1, hydra.typing.TypeSubst>>) (v2 -> hydra.unification.Unification.unifyTypeConstraints_tryBinding(
            (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T1, hydra.typing.TypeSubst>>>) (v12 -> (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T1, hydra.typing.TypeSubst>>) (v22 -> hydra.unification.Unification.<T0, T1>unifyTypeConstraints_bind(
              (hydra_substitution_composeTypeSubst2),
              (hydra_substitution_singletonTypeSubst2),
              (hydra_substitution_substituteInConstraints2),
              (rest),
              (schemaTypes),
              (v12),
              (v22)))),
            (comment),
            (hydra_show_core_type2),
            (hydra_unification_variableOccursInType2),
            (v1),
            (v2)))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.typing.TypeSubst> visit(hydra.core.Type.Variable name) {
        return ((sright)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T1, hydra.typing.TypeSubst> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.unifyTypeConstraints_tryBinding(
              (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T1, hydra.typing.TypeSubst>>>) (v1 -> (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T1, hydra.typing.TypeSubst>>) (v2 -> hydra.unification.Unification.<T0, T1>unifyTypeConstraints_bind(
                (hydra_substitution_composeTypeSubst2),
                (hydra_substitution_singletonTypeSubst2),
                (hydra_substitution_substituteInConstraints2),
                (rest),
                (schemaTypes),
                (v1),
                (v2)))),
              (comment),
              (hydra_show_core_type2),
              (hydra_unification_variableOccursInType2),
              ((name)).value,
              (sright));
          }
          
          @Override
          public hydra.compute.Flow<T1, hydra.typing.TypeSubst> visit(hydra.core.Type.Variable name2) {
            return hydra.lib.logic.IfElse.apply(
              hydra.lib.equality.Equal.apply(
                (((name)).value).value,
                (((name2)).value).value),
              hydra.unification.Unification.<T0, T1>unifyTypeConstraints(
                (schemaTypes),
                (rest)),
              hydra.lib.logic.IfElse.apply(
                hydra.lib.maybes.IsJust.apply(hydra.lib.maps.Lookup.apply(
                  ((name)).value,
                  (schemaTypes))),
                hydra.lib.logic.IfElse.apply(
                  hydra.lib.maybes.IsJust.apply(hydra.lib.maps.Lookup.apply(
                    ((name2)).value,
                    (schemaTypes))),
                  hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
                    hydra.lib.strings.Cat2.apply(
                      hydra.lib.strings.Cat2.apply(
                        hydra.lib.strings.Cat2.apply(
                          hydra.lib.strings.Cat2.apply(
                            hydra.lib.strings.Cat2.apply(
                              "Attempted to unify schema names ",
                              (((name)).value).value),
                            " and "),
                          (((name2)).value).value),
                        " ("),
                      (comment)),
                    ")")),
                  hydra.unification.Unification.<T0, T1>unifyTypeConstraints_bind(
                    (hydra_substitution_composeTypeSubst2),
                    (hydra_substitution_singletonTypeSubst2),
                    (hydra_substitution_substituteInConstraints2),
                    (rest),
                    (schemaTypes),
                    ((name2)).value,
                    (sleft))),
                hydra.unification.Unification.<T0, T1>unifyTypeConstraints_bind(
                  (hydra_substitution_composeTypeSubst2),
                  (hydra_substitution_singletonTypeSubst2),
                  (hydra_substitution_substituteInConstraints2),
                  (rest),
                  (schemaTypes),
                  ((name)).value,
                  (sright))));
          }
        });
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.TypeSubst> unifyTypeConstraints_bind(java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>> hydra_substitution_composeTypeSubst2, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.typing.TypeSubst>> hydra_substitution_singletonTypeSubst2, java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, java.util.List<hydra.typing.TypeConstraint>>> hydra_substitution_substituteInConstraints2, java.util.List<hydra.typing.TypeConstraint> rest, java.util.Map<hydra.core.Name, T0> schemaTypes, hydra.core.Name v, hydra.core.Type t) {
    hydra.typing.TypeSubst subst = (((hydra_substitution_singletonTypeSubst2)).apply((v))).apply((t));
    java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst> withResult = (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>) (s -> (((hydra_substitution_composeTypeSubst2)).apply((subst))).apply((s)));
    return hydra.lib.flows.Map.apply(
      (withResult),
      hydra.unification.Unification.<T0, T1>unifyTypeConstraints(
        (schemaTypes),
        (((hydra_substitution_substituteInConstraints2)).apply((subst))).apply((rest))));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T1, T2> unifyTypeConstraints_tryBinding(java.util.function.Function<hydra.core.Name, java.util.function.Function<T0, hydra.compute.Flow<T1, T2>>> bind, String comment, java.util.function.Function<T0, String> hydra_show_core_type2, java.util.function.Function<hydra.core.Name, java.util.function.Function<T0, Boolean>> hydra_unification_variableOccursInType2, hydra.core.Name v, T0 t) {
    return hydra.lib.logic.IfElse.apply(
      (((hydra_unification_variableOccursInType2)).apply((v))).apply((t)),
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                hydra.lib.strings.Cat2.apply(
                  "Variable ",
                  ((v)).value),
                " appears free in type "),
              ((hydra_show_core_type2)).apply((t))),
            " ("),
          (comment)),
        ")")),
      (((bind)).apply((v))).apply((t)));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.TypeSubst> unifyTypeConstraints_noVars(String comment, java.util.List<hydra.typing.TypeConstraint> rest, java.util.Map<hydra.core.Name, T0> schemaTypes, hydra.core.Type sleft, hydra.core.Type sright) {
    return hydra.lib.flows.Bind.apply(
      hydra.unification.Unification.<T1>joinTypes(
        (sleft),
        (sright),
        (comment)),
      (java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, hydra.compute.Flow<T1, hydra.typing.TypeSubst>>) (v1 -> hydra.unification.Unification.<T0, T1>unifyTypeConstraints_withConstraints(
        (rest),
        (schemaTypes),
        (v1))));
  }
  
  static <T0, T1> T0 unifyTypeConstraints_dflt(T0 noVars, T1 sleft, hydra.core.Type sright, java.util.function.Function<hydra.core.Name, java.util.function.Function<T1, T0>> tryBinding) {
    return ((sright)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public T0 otherwise(hydra.core.Type instance) {
        return (noVars);
      }
      
      @Override
      public T0 visit(hydra.core.Type.Variable name) {
        return (((tryBinding)).apply(((name)).value)).apply((sleft));
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.TypeSubst> unifyTypeConstraints_withConstraints(java.util.List<hydra.typing.TypeConstraint> rest, java.util.Map<hydra.core.Name, T0> schemaTypes, java.util.List<hydra.typing.TypeConstraint> constraints2) {
    return hydra.unification.Unification.<T0, T1>unifyTypeConstraints(
      (schemaTypes),
      hydra.lib.lists.Concat2.apply(
        (constraints2),
        (rest)));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.TypeSubst> unifyTypeLists(java.util.Map<hydra.core.Name, T0> schemaTypes, java.util.List<hydra.core.Type> l, java.util.List<hydra.core.Type> r, String comment) {
    java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>> toConstraint = (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>>) (l2 -> (java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>) (r2 -> new hydra.typing.TypeConstraint((l2), (r2), (comment))));
    return hydra.unification.Unification.<T0, T1>unifyTypeConstraints(
      (schemaTypes),
      hydra.lib.lists.ZipWith.apply(
        (toConstraint),
        (l),
        (r)));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.TypeSubst> unifyTypes(java.util.Map<hydra.core.Name, T0> schemaTypes, hydra.core.Type l, hydra.core.Type r, String comment) {
    return hydra.unification.Unification.<T0, T1>unifyTypeConstraints(
      (schemaTypes),
      java.util.List.of(new hydra.typing.TypeConstraint((l), (r), (comment))));
  }
  
  static Boolean variableOccursInType(hydra.core.Name var, hydra.core.Type typ0) {
    java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Type, Boolean>> tryType = (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Type, Boolean>>) (b -> (java.util.function.Function<hydra.core.Type, Boolean>) (typ -> ((typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return (b);
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Variable v) {
        return hydra.lib.logic.Or.apply(
          (b),
          hydra.lib.equality.Equal.apply(
            (((v)).value).value,
            ((var)).value));
      }
    })));
    return hydra.rewriting.Rewriting.foldOverType(
      new hydra.coders.TraversalOrder.Pre(true),
      (tryType),
      false,
      (typ0));
  }
}
