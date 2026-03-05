// Note: this is an automatically generated file. Do not edit.

package hydra.unification;

/**
 * Utilities for type unification.
 */
public interface Unification {
  static hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> joinTypes(hydra.context.Context cx, hydra.core.Type left, hydra.core.Type right, String comment) {
    java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>> joinOne = (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>>) (l -> (java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>) (r -> new hydra.typing.TypeConstraint(l, r, hydra.lib.strings.Cat2.apply(
      "join types; ",
      comment))));
    hydra.core.Type sleft = hydra.rewriting.Rewriting.deannotateType(left);
    hydra.core.Type sright = hydra.rewriting.Rewriting.deannotateType(right);
    java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.List<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>>> joinList = (java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.List<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>>>) (lefts -> (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>>) (rights -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply(lefts),
        hydra.lib.lists.Length.apply(rights)),
      () -> (hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>right(hydra.lib.lists.ZipWith.apply(
        joinOne,
        lefts,
        rights)))),
      () -> hydra.unification.Unification.joinTypes_cannotUnify(
        cx,
        hydra.show.core.Core::type,
        sleft,
        sright))));
    java.util.function.Function<hydra.core.RowType, java.util.function.Function<hydra.core.RowType, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>>> joinRowTypes = (java.util.function.Function<hydra.core.RowType, java.util.function.Function<hydra.core.RowType, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>>>) (left2 -> (java.util.function.Function<hydra.core.RowType, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>>) (right2 -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        hydra.lib.equality.Equal.apply(
          ((left2).typeName).value,
          ((right2).typeName).value),
        hydra.lib.logic.And.apply(
          hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply(hydra.lib.lists.Map.apply(
              projected -> projected.name,
              (left2).fields)),
            hydra.lib.lists.Length.apply(hydra.lib.lists.Map.apply(
              projected -> projected.name,
              (right2).fields))),
          hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<Boolean, java.util.function.Function<Boolean, Boolean>>) (p0 -> p1 -> hydra.lib.logic.And.apply(
              p0,
              p1)),
            true,
            hydra.lib.lists.ZipWith.apply(
              (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Name, Boolean>>) (left3 -> (java.util.function.Function<hydra.core.Name, Boolean>) (right3 -> hydra.lib.equality.Equal.apply(
                (left3).value,
                (right3).value))),
              hydra.lib.lists.Map.apply(
                projected -> projected.name,
                (left2).fields),
              hydra.lib.lists.Map.apply(
                projected -> projected.name,
                (right2).fields))))),
      () -> ((joinList).apply(hydra.lib.lists.Map.apply(
        projected -> projected.type,
        (left2).fields))).apply(hydra.lib.lists.Map.apply(
        projected -> projected.type,
        (right2).fields)),
      () -> hydra.unification.Unification.joinTypes_cannotUnify(
        cx,
        hydra.show.core.Core::type,
        sleft,
        sright))));
    return (sleft).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
        return hydra.unification.Unification.joinTypes_cannotUnify(
          cx,
          hydra.show.core.Core::type,
          sleft,
          sright);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Application l) {
        return (sright).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              cx,
              hydra.show.core.Core::type,
              sleft,
              sright);
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Application r) {
            return (hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>right(java.util.List.of(
              ((joinOne).apply(((l).value).function)).apply(((r).value).function),
              ((joinOne).apply(((l).value).argument)).apply(((r).value).argument)))));
          }
        });
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Either l) {
        return (sright).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              cx,
              hydra.show.core.Core::type,
              sleft,
              sright);
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Either r) {
            return (hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>right(java.util.List.of(
              ((joinOne).apply(((l).value).left)).apply(((r).value).left),
              ((joinOne).apply(((l).value).right)).apply(((r).value).right)))));
          }
        });
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Function l) {
        return (sright).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              cx,
              hydra.show.core.Core::type,
              sleft,
              sright);
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Function r) {
            return (hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>right(java.util.List.of(
              ((joinOne).apply(((l).value).domain)).apply(((r).value).domain),
              ((joinOne).apply(((l).value).codomain)).apply(((r).value).codomain)))));
          }
        });
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.List l) {
        return (sright).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              cx,
              hydra.show.core.Core::type,
              sleft,
              sright);
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.List r) {
            return (hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>right(java.util.List.of(((joinOne).apply((l).value)).apply((r).value)))));
          }
        });
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Literal ignored) {
        return hydra.unification.Unification.joinTypes_assertEqual(
          cx,
          hydra.show.core.Core::type,
          sleft,
          sright);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Map l) {
        return (sright).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              cx,
              hydra.show.core.Core::type,
              sleft,
              sright);
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Map r) {
            return (hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>right(java.util.List.of(
              ((joinOne).apply(((l).value).keys)).apply(((r).value).keys),
              ((joinOne).apply(((l).value).values)).apply(((r).value).values)))));
          }
        });
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Maybe l) {
        return (sright).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              cx,
              hydra.show.core.Core::type,
              sleft,
              sright);
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Maybe r) {
            return (hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>right(java.util.List.of(((joinOne).apply((l).value)).apply((r).value)))));
          }
        });
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Pair l) {
        return (sright).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              cx,
              hydra.show.core.Core::type,
              sleft,
              sright);
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Pair r) {
            return (hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>right(java.util.List.of(
              ((joinOne).apply(((l).value).first)).apply(((r).value).first),
              ((joinOne).apply(((l).value).second)).apply(((r).value).second)))));
          }
        });
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Record l) {
        return (sright).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              cx,
              hydra.show.core.Core::type,
              sleft,
              sright);
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Record r) {
            return ((joinRowTypes).apply((l).value)).apply((r).value);
          }
        });
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Set l) {
        return (sright).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              cx,
              hydra.show.core.Core::type,
              sleft,
              sright);
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Set r) {
            return (hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>right(java.util.List.of(((joinOne).apply((l).value)).apply((r).value)))));
          }
        });
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Union l) {
        return (sright).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              cx,
              hydra.show.core.Core::type,
              sleft,
              sright);
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Union r) {
            return ((joinRowTypes).apply((l).value)).apply((r).value);
          }
        });
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Unit ignored) {
        return (sright).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              cx,
              hydra.show.core.Core::type,
              sleft,
              sright);
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Unit _2) {
            return (hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>right((java.util.List<hydra.typing.TypeConstraint>) (java.util.List.<hydra.typing.TypeConstraint>of()))));
          }
        });
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Wrap l) {
        return (sright).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> otherwise(hydra.core.Type instance) {
            return hydra.unification.Unification.joinTypes_cannotUnify(
              cx,
              hydra.show.core.Core::type,
              sleft,
              sright);
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>> visit(hydra.core.Type.Wrap r) {
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                (((l).value).typeName).value,
                (((r).value).typeName).value),
              () -> (hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<hydra.typing.TypeConstraint>>right(java.util.List.of(((joinOne).apply(((l).value).body)).apply(((r).value).body))))),
              () -> hydra.unification.Unification.joinTypes_cannotUnify(
                cx,
                hydra.show.core.Core::type,
                sleft,
                sright));
          }
        });
      }
    });
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, T0> joinTypes_cannotUnify(hydra.context.Context cx, java.util.function.Function<hydra.core.Type, String> hydra_show_core_type2, hydra.core.Type sleft, hydra.core.Type sright) {
    return (hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, T0>) ((hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, T0>) (hydra.util.Either.<hydra.context.InContext<hydra.error.UnificationError>, T0>left((hydra.context.InContext<hydra.error.UnificationError>) (new hydra.context.InContext<hydra.error.UnificationError>(new hydra.error.UnificationError(sleft, sright, hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "cannot unify ",
          (hydra_show_core_type2).apply(sleft)),
        " with "),
      (hydra_show_core_type2).apply(sright))), cx)))));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<T0>> joinTypes_assertEqual(hydra.context.Context cx, java.util.function.Function<hydra.core.Type, String> hydra_show_core_type2, hydra.core.Type sleft, hydra.core.Type sright) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        sleft,
        sright),
      () -> (hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<T0>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<T0>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.UnificationError>, java.util.List<T0>>right((java.util.List<T0>) (java.util.List.<T0>of())))),
      () -> hydra.unification.Unification.<java.util.List<T0>>joinTypes_cannotUnify(
        cx,
        hydra_show_core_type2,
        sleft,
        sright));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst> unifyTypeConstraints(hydra.context.Context cx, java.util.Map<hydra.core.Name, T0> schemaTypes, java.util.List<hydra.typing.TypeConstraint> constraints) {
    java.util.function.Function<hydra.typing.TypeConstraint, java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>>> withConstraint = (java.util.function.Function<hydra.typing.TypeConstraint, java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>>>) (c -> (java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>>) (rest -> {
      java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>>> bind = (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>>>) (v -> (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>>) (t -> {
        hydra.typing.TypeSubst subst = hydra.substitution.Substitution.singletonTypeSubst(
          v,
          t);
        java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst> withResult = (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>) (s -> hydra.substitution.Substitution.composeTypeSubst(
          subst,
          s));
        return hydra.lib.eithers.Map.apply(
          withResult,
          hydra.unification.Unification.<T0>unifyTypeConstraints(
            cx,
            schemaTypes,
            hydra.substitution.Substitution.substituteInConstraints(
              subst,
              rest)));
      }));
      String comment = (c).comment;
      hydra.core.Type sleft = hydra.rewriting.Rewriting.deannotateType((c).left);
      hydra.core.Type sright = hydra.rewriting.Rewriting.deannotateType((c).right);
      java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>> withConstraints = (java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>>) (constraints2 -> hydra.unification.Unification.<T0>unifyTypeConstraints(
        cx,
        schemaTypes,
        hydra.lib.lists.Concat2.apply(
          constraints2,
          rest)));
      hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>> noVars = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Bind.apply(
        hydra.unification.Unification.joinTypes(
          cx,
          sleft,
          sright,
          comment),
        withConstraints));
      java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>>> tryBinding = (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>>>) (v -> (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>>) (t -> hydra.lib.logic.IfElse.lazy(
        hydra.unification.Unification.variableOccursInType(
          v,
          t),
        () -> (hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>) ((hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>) (hydra.util.Either.<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>left((hydra.context.InContext<hydra.error.UnificationError>) (new hydra.context.InContext<hydra.error.UnificationError>(new hydra.error.UnificationError(sleft, sright, hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                hydra.lib.strings.Cat2.apply(
                  hydra.lib.strings.Cat2.apply(
                    "Variable ",
                    (v).value),
                  " appears free in type "),
                hydra.show.core.Core.type(t)),
              " ("),
            comment),
          ")")), cx))))),
        () -> ((bind).apply(v)).apply(t))));
      hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst> dflt = (sright).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst> otherwise(hydra.core.Type instance) {
          return noVars.get();
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst> visit(hydra.core.Type.Variable name) {
          return ((tryBinding).apply((name).value)).apply(sleft);
        }
      });
      return (sleft).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst> otherwise(hydra.core.Type instance) {
          return dflt;
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst> visit(hydra.core.Type.Variable name) {
          return (sright).accept(new hydra.core.Type.PartialVisitor<>() {
            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst> otherwise(hydra.core.Type instance) {
              return ((tryBinding).apply((name).value)).apply(sright);
            }
            
            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst> visit(hydra.core.Type.Variable name2) {
              return hydra.lib.logic.IfElse.lazy(
                hydra.lib.equality.Equal.apply(
                  ((name).value).value,
                  ((name2).value).value),
                () -> hydra.unification.Unification.<T0>unifyTypeConstraints(
                  cx,
                  schemaTypes,
                  rest),
                () -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.maybes.IsJust.apply(hydra.lib.maps.Lookup.apply(
                    (name).value,
                    schemaTypes)),
                  () -> hydra.lib.logic.IfElse.lazy(
                    hydra.lib.maybes.IsJust.apply(hydra.lib.maps.Lookup.apply(
                      (name2).value,
                      schemaTypes)),
                    () -> (hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>) ((hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>) (hydra.util.Either.<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>left((hydra.context.InContext<hydra.error.UnificationError>) (new hydra.context.InContext<hydra.error.UnificationError>(new hydra.error.UnificationError(sleft, sright, hydra.lib.strings.Cat2.apply(
                      hydra.lib.strings.Cat2.apply(
                        hydra.lib.strings.Cat2.apply(
                          hydra.lib.strings.Cat2.apply(
                            hydra.lib.strings.Cat2.apply(
                              hydra.lib.strings.Cat2.apply(
                                "Attempted to unify schema names ",
                                ((name).value).value),
                              " and "),
                            ((name2).value).value),
                          " ("),
                        comment),
                      ")")), cx))))),
                    () -> ((bind).apply((name2).value)).apply(sleft)),
                  () -> ((bind).apply((name).value)).apply(sright)));
            }
          });
        }
      });
    }));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(constraints),
      () -> (hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>) ((hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>) (hydra.util.Either.<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst>right(hydra.substitution.Substitution.idTypeSubst()))),
      () -> ((withConstraint).apply(hydra.lib.lists.Head.apply(constraints))).apply(hydra.lib.lists.Tail.apply(constraints)));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst> unifyTypeLists(hydra.context.Context cx, java.util.Map<hydra.core.Name, T0> schemaTypes, java.util.List<hydra.core.Type> l, java.util.List<hydra.core.Type> r, String comment) {
    java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>> toConstraint = (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>>) (l2 -> (java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>) (r2 -> new hydra.typing.TypeConstraint(l2, r2, comment)));
    return hydra.unification.Unification.<T0>unifyTypeConstraints(
      cx,
      schemaTypes,
      hydra.lib.lists.ZipWith.apply(
        toConstraint,
        l,
        r));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.UnificationError>, hydra.typing.TypeSubst> unifyTypes(hydra.context.Context cx, java.util.Map<hydra.core.Name, T0> schemaTypes, hydra.core.Type l, hydra.core.Type r, String comment) {
    return hydra.unification.Unification.<T0>unifyTypeConstraints(
      cx,
      schemaTypes,
      java.util.List.of(new hydra.typing.TypeConstraint(l, r, comment)));
  }
  
  static Boolean variableOccursInType(hydra.core.Name var, hydra.core.Type typ0) {
    java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Type, Boolean>> tryType = (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Type, Boolean>>) (b -> (java.util.function.Function<hydra.core.Type, Boolean>) (typ -> (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return b;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Variable v) {
        return hydra.lib.logic.Or.apply(
          b,
          hydra.lib.equality.Equal.apply(
            ((v).value).value,
            (var).value));
      }
    })));
    return hydra.rewriting.Rewriting.foldOverType(
      new hydra.coders.TraversalOrder.Pre(),
      tryType,
      false,
      typ0);
  }
}
