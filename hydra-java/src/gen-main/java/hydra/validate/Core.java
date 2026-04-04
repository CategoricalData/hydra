// Note: this is an automatically generated file. Do not edit.

package hydra.validate;

/**
 * Validation functions for core terms and types
 */
public interface Core {
  static hydra.util.Maybe<hydra.error.core.InvalidTermError> checkDuplicateBindings(hydra.paths.SubtermPath path, java.util.List<hydra.core.Binding> bindings) {
    hydra.util.Lazy<java.util.List<hydra.core.Name>> names = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      projected -> projected.name,
      bindings));
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.Name>> dup = new hydra.util.Lazy<>(() -> hydra.validate.Core.findDuplicate(names.get()));
    return hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.error.core.InvalidTermError>) (name -> new hydra.error.core.InvalidTermError.DuplicateBinding(new hydra.error.core.DuplicateBindingError(path, name))),
      dup.get());
  }

  static <T0> hydra.util.Maybe<T0> checkDuplicateFieldTypes(java.util.List<hydra.core.FieldType> fields, java.util.function.Function<hydra.core.Name, hydra.util.Maybe<T0>> mkError) {
    hydra.util.Lazy<java.util.List<hydra.core.Name>> names = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      projected -> projected.name,
      fields));
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.Name>> dup = new hydra.util.Lazy<>(() -> hydra.validate.Core.findDuplicateFieldType(names.get()));
    return hydra.lib.maybes.Cases.applyLazy(
      dup.get(),
      () -> (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),
      (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<T0>>) (name -> (mkError).apply(name)));
  }

  static hydra.util.Maybe<hydra.error.core.InvalidTermError> checkDuplicateFields(hydra.paths.SubtermPath path, java.util.List<hydra.core.Name> names) {
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.Name>> dup = new hydra.util.Lazy<>(() -> hydra.validate.Core.findDuplicate(names));
    return hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.error.core.InvalidTermError>) (name -> new hydra.error.core.InvalidTermError.DuplicateField(new hydra.error.core.DuplicateFieldError(path, name))),
      dup.get());
  }

  static hydra.util.Maybe<hydra.error.core.InvalidTermError> checkShadowing(hydra.paths.SubtermPath path, hydra.graph.Graph cx, java.util.List<hydra.core.Name> names) {
    hydra.util.Lazy<hydra.util.Maybe<hydra.error.core.InvalidTermError>> result = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Maybe<hydra.error.core.InvalidTermError>, java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.error.core.InvalidTermError>>>) (acc -> (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (name -> hydra.lib.maybes.Cases.applyLazy(
        acc,
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.Or.apply(
            hydra.lib.maybes.IsJust.apply(hydra.lib.maps.Lookup.apply(
              name,
              (cx).boundTerms)),
            hydra.lib.sets.Member.apply(
              name,
              (cx).lambdaVariables)),
          () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.TermVariableShadowing(new hydra.error.core.TermVariableShadowingError(path, name))),
          () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())),
        (java.util.function.Function<hydra.error.core.InvalidTermError, hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (ignored -> acc)))),
      (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()),
      names));
    return result.get();
  }

  static hydra.util.Maybe<hydra.error.core.InvalidTermError> checkTerm(Boolean typed, hydra.paths.SubtermPath path, hydra.graph.Graph cx, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Term instance) {
        return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Annotated ann) {
        java.util.Map<hydra.core.Name, hydra.core.Term> annMap = (ann).value.annotation;
        hydra.core.Term body = (ann).value.body;
        return hydra.validate.Core.firstError(java.util.Arrays.asList(
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.maps.Null.apply(annMap),
            () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.EmptyTermAnnotation(new hydra.error.core.EmptyTermAnnotationError(path))),
            () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())),
          (body).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Term instance) {
              return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
            }

            @Override
            public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Annotated ignored) {
              return hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.NestedTermAnnotation(new hydra.error.core.NestedTermAnnotationError(path)));
            }
          })));
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Application app) {
        hydra.core.Term arg = (app).value.argument;
        hydra.core.Term fun = (app).value.function;
        return hydra.validate.Core.firstError(java.util.Arrays.asList(
          (fun).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Term instance) {
              return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
            }

            @Override
            public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Function f) {
              return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
                @Override
                public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Function instance) {
                  return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
                }

                @Override
                public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Function.Primitive primName) {
                  return hydra.lib.logic.IfElse.lazy(
                    hydra.lib.equality.Equal.apply(
                      (primName).value.value,
                      "hydra.lib.logic.ifElse"),
                    () -> (arg).accept(new hydra.core.Term.PartialVisitor<>() {
                      @Override
                      public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Term instance) {
                        return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
                      }

                      @Override
                      public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Literal lit) {
                        return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                          @Override
                          public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Literal instance) {
                            return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
                          }

                          @Override
                          public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Literal.Boolean_ boolVal) {
                            return hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.ConstantCondition(new hydra.error.core.ConstantConditionError(path, (boolVal).value)));
                          }
                        });
                      }
                    }),
                    () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()));
                }
              });
            }
          }),
          (fun).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Term instance) {
              return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
            }

            @Override
            public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Variable funName) {
              return (arg).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Term instance) {
                  return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
                }

                @Override
                public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Variable argName) {
                  return hydra.lib.logic.IfElse.lazy(
                    hydra.lib.equality.Equal.apply(
                      (funName).value,
                      (argName).value),
                    () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.SelfApplication(new hydra.error.core.SelfApplicationError(path, (funName).value))),
                    () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()));
                }
              });
            }
          }),
          (fun).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Term instance) {
              return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
            }

            @Override
            public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Function f) {
              return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
                @Override
                public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Function instance) {
                  return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
                }

                @Override
                public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Function.Lambda lam) {
                  hydra.core.Term body = (lam).value.body;
                  hydra.core.Name param = (lam).value.parameter;
                  return (body).accept(new hydra.core.Term.PartialVisitor<>() {
                    @Override
                    public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Term instance) {
                      return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
                    }

                    @Override
                    public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Variable bodyVar) {
                      return hydra.lib.logic.IfElse.lazy(
                        hydra.lib.equality.Equal.apply(
                          param,
                          (bodyVar).value),
                        () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.UnnecessaryIdentityApplication(new hydra.error.core.UnnecessaryIdentityApplicationError(path))),
                        () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()));
                    }
                  });
                }
              });
            }
          }),
          (fun).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Term instance) {
              return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
            }

            @Override
            public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Function f) {
              return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
                @Override
                public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Function instance) {
                  return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
                }

                @Override
                public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Function.Elimination elim) {
                  return (elim).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
                    @Override
                    public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Elimination instance) {
                      return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
                    }

                    @Override
                    public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Elimination.Wrap unwrapName) {
                      return (arg).accept(new hydra.core.Term.PartialVisitor<>() {
                        @Override
                        public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Term instance) {
                          return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
                        }

                        @Override
                        public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Wrap wt) {
                          hydra.core.Name wrapName = (wt).value.typeName;
                          return hydra.lib.logic.IfElse.lazy(
                            hydra.lib.equality.Equal.apply(
                              (unwrapName).value,
                              wrapName),
                            () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.RedundantWrapUnwrap(new hydra.error.core.RedundantWrapUnwrapError(path, (unwrapName).value))),
                            () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()));
                        }
                      });
                    }
                  });
                }
              });
            }
          })));
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Record rec) {
        java.util.List<hydra.core.Field> flds = (rec).value.fields;
        hydra.core.Name tname = (rec).value.typeName;
        return hydra.validate.Core.firstError(java.util.Arrays.asList(
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              (tname).value,
              ""),
            () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.EmptyTypeNameInTerm(new hydra.error.core.EmptyTypeNameInTermError(path))),
            () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())),
          hydra.validate.Core.checkDuplicateFields(
            path,
            hydra.lib.lists.Map.apply(
              projected -> projected.name,
              flds))));
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Let lt) {
        java.util.List<hydra.core.Binding> bindings = (lt).value.bindings;
        hydra.util.Lazy<java.util.List<hydra.core.Name>> names = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          projected -> projected.name,
          bindings));
        return hydra.validate.Core.firstError(java.util.Arrays.asList(
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply(bindings),
            () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.EmptyLetBindings(new hydra.error.core.EmptyLetBindingsError(path))),
            () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())),
          hydra.validate.Core.checkDuplicateBindings(
            path,
            bindings),
          (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()),
          hydra.validate.Core.firstError(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (bname -> hydra.lib.logic.IfElse.lazy(
              hydra.validate.Core.isValidName(bname),
              () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()),
              () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.InvalidLetBindingName(new hydra.error.core.InvalidLetBindingNameError(path, bname))))),
            names.get())),
          hydra.lib.logic.IfElse.lazy(
            typed,
            () -> hydra.validate.Core.firstError(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (b -> hydra.lib.maybes.Cases.applyLazy(
                (b).type,
                () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()),
                (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (ts -> hydra.validate.Core.checkUndefinedTypeVariablesInTypeScheme(
                  path,
                  cx,
                  ts,
                  (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (uvName -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.UndefinedTypeVariableInBindingType(new hydra.error.core.UndefinedTypeVariableInBindingTypeError(path, uvName)))))))),
              bindings)),
            () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()))));
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Union inj) {
        hydra.core.Name tname = (inj).value.typeName;
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (tname).value,
            ""),
          () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.EmptyTypeNameInTerm(new hydra.error.core.EmptyTypeNameInTermError(path))),
          () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()));
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Function fun) {
        return (fun).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Function instance) {
            return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
          }

          @Override
          public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Function.Lambda lam) {
            hydra.core.Name paramName = (lam).value.parameter;
            return hydra.validate.Core.firstError(java.util.Arrays.asList(
              hydra.lib.logic.IfElse.lazy(
                hydra.lib.maybes.IsJust.apply(hydra.lib.maps.Lookup.apply(
                  paramName,
                  (cx).boundTerms)),
                () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.TermVariableShadowing(new hydra.error.core.TermVariableShadowingError(path, paramName))),
                () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())),
              hydra.lib.logic.IfElse.lazy(
                hydra.validate.Core.isValidName(paramName),
                () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()),
                () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.InvalidLambdaParameterName(new hydra.error.core.InvalidLambdaParameterNameError(path, paramName)))),
              hydra.lib.logic.IfElse.lazy(
                typed,
                () -> hydra.lib.maybes.Cases.applyLazy(
                  (lam).value.domain,
                  () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()),
                  (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (dom -> hydra.validate.Core.checkUndefinedTypeVariablesInType(
                    path,
                    cx,
                    dom,
                    (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (uvName -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.UndefinedTypeVariableInLambdaDomain(new hydra.error.core.UndefinedTypeVariableInLambdaDomainError(path, uvName))))))),
                () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()))));
          }

          @Override
          public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Function.Primitive primName) {
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.maybes.IsJust.apply(hydra.lib.maps.Lookup.apply(
                (primName).value,
                (cx).primitives)),
              () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()),
              () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.UnknownPrimitiveName(new hydra.error.core.UnknownPrimitiveNameError(path, (primName).value))));
          }

          @Override
          public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Function.Elimination elim) {
            return (elim).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Elimination instance) {
                return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
              }

              @Override
              public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Elimination.Record proj) {
                hydra.core.Name tname = (proj).value.typeName;
                return hydra.lib.logic.IfElse.lazy(
                  hydra.lib.equality.Equal.apply(
                    (tname).value,
                    ""),
                  () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.EmptyTypeNameInTerm(new hydra.error.core.EmptyTypeNameInTermError(path))),
                  () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()));
              }

              @Override
              public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Elimination.Union cs) {
                java.util.List<hydra.core.Field> csCases = (cs).value.cases;
                hydra.util.Maybe<hydra.core.Term> csDefault = (cs).value.default_;
                hydra.core.Name tname = (cs).value.typeName;
                return hydra.validate.Core.firstError(java.util.Arrays.asList(
                  hydra.lib.logic.IfElse.lazy(
                    hydra.lib.equality.Equal.apply(
                      (tname).value,
                      ""),
                    () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.EmptyTypeNameInTerm(new hydra.error.core.EmptyTypeNameInTermError(path))),
                    () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())),
                  hydra.lib.logic.IfElse.lazy(
                    hydra.lib.logic.And.apply(
                      hydra.lib.lists.Null.apply(csCases),
                      hydra.lib.maybes.IsNothing.apply(csDefault)),
                    () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.EmptyCaseStatement(new hydra.error.core.EmptyCaseStatementError(path, tname))),
                    () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())),
                  hydra.validate.Core.checkDuplicateFields(
                    path,
                    hydra.lib.lists.Map.apply(
                      projected -> projected.name,
                      csCases))));
              }
            });
          }
        });
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.TypeApplication ta) {
        return hydra.lib.logic.IfElse.lazy(
          typed,
          () -> hydra.validate.Core.checkUndefinedTypeVariablesInType(
            path,
            cx,
            (ta).value.type,
            (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (uvName -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.UndefinedTypeVariableInTypeApplication(new hydra.error.core.UndefinedTypeVariableInTypeApplicationError(path, uvName))))),
          () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()));
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.TypeLambda tl) {
        hydra.core.Name tvName = (tl).value.parameter;
        return hydra.validate.Core.firstError(java.util.Arrays.asList(
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.sets.Member.apply(
              tvName,
              hydra.lib.sets.Delete.apply(
                tvName,
                (cx).typeVariables)),
            () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.TypeVariableShadowingInTypeLambda(new hydra.error.core.TypeVariableShadowingInTypeLambdaError(path, tvName))),
            () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())),
          hydra.lib.logic.IfElse.lazy(
            hydra.validate.Core.isValidName(tvName),
            () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()),
            () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.InvalidTypeLambdaParameterName(new hydra.error.core.InvalidTypeLambdaParameterNameError(path, tvName))))));
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Variable varName) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.Or.apply(
            hydra.lib.maybes.IsJust.apply(hydra.lib.maps.Lookup.apply(
              (varName).value,
              (cx).boundTerms)),
            hydra.lib.logic.Or.apply(
              hydra.lib.sets.Member.apply(
                (varName).value,
                (cx).lambdaVariables),
              hydra.lib.maybes.IsJust.apply(hydra.lib.maps.Lookup.apply(
                (varName).value,
                (cx).primitives)))),
          () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()),
          () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.UndefinedTermVariable(new hydra.error.core.UndefinedTermVariableError(path, (varName).value))));
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Wrap wt) {
        hydra.core.Name tname = (wt).value.typeName;
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (tname).value,
            ""),
          () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTermError.EmptyTypeNameInTerm(new hydra.error.core.EmptyTypeNameInTermError(path))),
          () -> (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()));
      }
    });
  }

  static <T0, T1> hydra.util.Maybe<T1> checkUndefinedTypeVariablesInType(T0 path, hydra.graph.Graph cx, hydra.core.Type typ, java.util.function.Function<hydra.core.Name, hydra.util.Maybe<T1>> mkError) {
    java.util.Set<hydra.core.Name> freeVars = hydra.Variables.freeVariablesInType(typ);
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> undefined = new hydra.util.Lazy<>(() -> hydra.lib.sets.Difference.apply(
      freeVars,
      (cx).typeVariables));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Null.apply(undefined.get()),
      () -> (hydra.util.Maybe<T1>) (hydra.util.Maybe.<T1>nothing()),
      () -> ((java.util.function.Supplier<hydra.util.Maybe<T1>>) (() -> {
        hydra.util.Lazy<hydra.core.Name> firstUndefined = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(hydra.lib.sets.ToList.apply(undefined.get())));
        return (mkError).apply(firstUndefined.get());
      })).get());
  }

  static <T0, T1> hydra.util.Maybe<T1> checkUndefinedTypeVariablesInTypeScheme(T0 path, hydra.graph.Graph cx, hydra.core.TypeScheme ts, java.util.function.Function<hydra.core.Name, hydra.util.Maybe<T1>> mkError) {
    java.util.Set<hydra.core.Name> freeVars = hydra.Variables.freeVariablesInTypeScheme(ts);
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> undefined = new hydra.util.Lazy<>(() -> hydra.lib.sets.Difference.apply(
      freeVars,
      (cx).typeVariables));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Null.apply(undefined.get()),
      () -> (hydra.util.Maybe<T1>) (hydra.util.Maybe.<T1>nothing()),
      () -> ((java.util.function.Supplier<hydra.util.Maybe<T1>>) (() -> {
        hydra.util.Lazy<hydra.core.Name> firstUndefined = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(hydra.lib.sets.ToList.apply(undefined.get())));
        return (mkError).apply(firstUndefined.get());
      })).get());
  }

  static hydra.util.Maybe<hydra.error.core.InvalidTypeError> checkVoid(hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTypeError> otherwise(hydra.core.Type instance) {
        return (hydra.util.Maybe<hydra.error.core.InvalidTypeError>) (hydra.util.Maybe.<hydra.error.core.InvalidTypeError>nothing());
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Void_ ignored) {
        return hydra.util.Maybe.just(new hydra.error.core.InvalidTypeError.VoidInNonBottomPosition(new hydra.error.core.VoidInNonBottomPositionError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList())))));
      }
    });
  }

  static <T0> hydra.util.Maybe<T0> findDuplicate(java.util.List<T0> names) {
    return hydra.lib.pairs.Second.apply(hydra.validate.Core.<T0>findDuplicate_result(names));
  }

  static <T0> hydra.util.Maybe<T0> findDuplicateFieldType(java.util.List<T0> names) {
    return hydra.lib.pairs.Second.apply(hydra.validate.Core.<T0>findDuplicateFieldType_result(names));
  }

  static <T0> hydra.util.Maybe<T0> findDuplicateFieldType_dup(hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>> acc) {
    return hydra.lib.pairs.Second.apply(acc);
  }

  static <T0> hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>> findDuplicateFieldType_result(java.util.List<T0> names) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>, java.util.function.Function<T0, hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>>>) (acc -> (java.util.function.Function<T0, hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>>) (name -> {
        hydra.util.Lazy<java.util.Set<T0>> seen = new hydra.util.Lazy<>(() -> hydra.validate.Core.<T0>findDuplicateFieldType_seen(acc));
        return hydra.lib.maybes.Cases.applyLazy(
          hydra.validate.Core.<T0>findDuplicateFieldType_dup(acc),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.sets.Member.apply(
              name,
              seen.get()),
            () -> (hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>) ((hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>) (new hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>(seen.get(), hydra.util.Maybe.just(name)))),
            () -> (hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>) ((hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>) (new hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>(hydra.lib.sets.Insert.apply(
              name,
              seen.get()), (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()))))),
          (java.util.function.Function<T0, hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>>) (ignored -> acc));
      })),
      (hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>) ((hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>) (new hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>((java.util.Set<T0>) (hydra.lib.sets.Empty.<T0>apply()), (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing())))),
      names);
  }

  static <T0> java.util.Set<T0> findDuplicateFieldType_seen(hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>> acc) {
    return hydra.lib.pairs.First.apply(acc);
  }

  static <T0> hydra.util.Maybe<T0> findDuplicate_dup(hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>> acc) {
    return hydra.lib.pairs.Second.apply(acc);
  }

  static <T0> hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>> findDuplicate_result(java.util.List<T0> names) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>, java.util.function.Function<T0, hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>>>) (acc -> (java.util.function.Function<T0, hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>>) (name -> {
        hydra.util.Lazy<java.util.Set<T0>> seen = new hydra.util.Lazy<>(() -> hydra.validate.Core.<T0>findDuplicate_seen(acc));
        return hydra.lib.maybes.Cases.applyLazy(
          hydra.validate.Core.<T0>findDuplicate_dup(acc),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.sets.Member.apply(
              name,
              seen.get()),
            () -> (hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>) ((hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>) (new hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>(seen.get(), hydra.util.Maybe.just(name)))),
            () -> (hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>) ((hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>) (new hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>(hydra.lib.sets.Insert.apply(
              name,
              seen.get()), (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()))))),
          (java.util.function.Function<T0, hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>>) (ignored -> acc));
      })),
      (hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>) ((hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>) (new hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>>((java.util.Set<T0>) (hydra.lib.sets.Empty.<T0>apply()), (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing())))),
      names);
  }

  static <T0> java.util.Set<T0> findDuplicate_seen(hydra.util.Pair<java.util.Set<T0>, hydra.util.Maybe<T0>> acc) {
    return hydra.lib.pairs.First.apply(acc);
  }

  static <T0> hydra.util.Maybe<T0> firstError(java.util.List<hydra.util.Maybe<T0>> checks) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Maybe<T0>, java.util.function.Function<hydra.util.Maybe<T0>, hydra.util.Maybe<T0>>>) (acc -> (java.util.function.Function<hydra.util.Maybe<T0>, hydra.util.Maybe<T0>>) (check -> hydra.lib.maybes.Cases.applyLazy(
        acc,
        () -> check,
        (java.util.function.Function<T0, hydra.util.Maybe<T0>>) (ignored -> acc)))),
      (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),
      checks);
  }

  static <T0> hydra.util.Maybe<T0> firstTypeError(java.util.List<hydra.util.Maybe<T0>> checks) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Maybe<T0>, java.util.function.Function<hydra.util.Maybe<T0>, hydra.util.Maybe<T0>>>) (acc -> (java.util.function.Function<hydra.util.Maybe<T0>, hydra.util.Maybe<T0>>) (check -> hydra.lib.maybes.Cases.applyLazy(
        acc,
        () -> check,
        (java.util.function.Function<T0, hydra.util.Maybe<T0>>) (ignored -> acc)))),
      (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),
      checks);
  }

  static Boolean isValidName(hydra.core.Name name) {
    return hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
      (name).value,
      ""));
  }

  static hydra.util.Maybe<hydra.error.core.InvalidTermError> term(Boolean typed, hydra.graph.Graph g, hydra.core.Term t) {
    return hydra.Rewriting.foldTermWithGraphAndPath(
      (java.util.function.Function<java.util.function.Function<hydra.util.Maybe<hydra.error.core.InvalidTermError>, java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.error.core.InvalidTermError>>>, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.util.Maybe<hydra.error.core.InvalidTermError>, java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.error.core.InvalidTermError>>>>>>) (recurse -> (java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.util.Maybe<hydra.error.core.InvalidTermError>, java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.error.core.InvalidTermError>>>>>) (path -> (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.util.Maybe<hydra.error.core.InvalidTermError>, java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.error.core.InvalidTermError>>>>) (cx -> (java.util.function.Function<hydra.util.Maybe<hydra.error.core.InvalidTermError>, java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.error.core.InvalidTermError>>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (trm -> hydra.lib.maybes.Cases.applyLazy(
        acc,
        () -> ((java.util.function.Supplier<hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (() -> {
          hydra.util.Maybe<hydra.error.core.InvalidTermError> checkResult = hydra.validate.Core.checkTerm(
            typed,
            new hydra.paths.SubtermPath(path),
            cx,
            trm);
          return hydra.lib.maybes.Cases.applyLazy(
            checkResult,
            () -> (recurse).apply((hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())).apply(trm),
            (java.util.function.Function<hydra.error.core.InvalidTermError, hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (err -> hydra.util.Maybe.just(err)));
        })).get(),
        (java.util.function.Function<hydra.error.core.InvalidTermError, hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (ignored -> acc))))))),
      g,
      (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()),
      t);
  }

  static hydra.util.Maybe<hydra.error.core.InvalidTypeError> type(java.util.Set<hydra.core.Name> boundVars, hydra.core.Type typ) {
    hydra.util.Maybe<hydra.error.core.InvalidTypeError> checkResult = hydra.validate.Core.validateTypeNode(
      boundVars,
      typ);
    return hydra.lib.maybes.Cases.applyLazy(
      checkResult,
      () -> (typ).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<hydra.error.core.InvalidTypeError> otherwise(hydra.core.Type instance) {
          return (hydra.util.Maybe<hydra.error.core.InvalidTypeError>) (hydra.util.Maybe.<hydra.error.core.InvalidTypeError>nothing());
        }

        @Override
        public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Forall ft) {
          hydra.util.Lazy<java.util.Set<hydra.core.Name>> newBound = new hydra.util.Lazy<>(() -> hydra.lib.sets.Insert.apply(
            (ft).value.parameter,
            boundVars));
          return hydra.validate.Core.type(
            newBound.get(),
            (ft).value.body);
        }

        @Override
        public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Annotated ann) {
          return hydra.validate.Core.type(
            boundVars,
            (ann).value.body);
        }

        @Override
        public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Application at) {
          return hydra.validate.Core.firstTypeError(java.util.Arrays.asList(
            hydra.validate.Core.type(
              boundVars,
              (at).value.function),
            hydra.validate.Core.type(
              boundVars,
              (at).value.argument)));
        }

        @Override
        public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Either et) {
          return hydra.validate.Core.firstTypeError(java.util.Arrays.asList(
            hydra.validate.Core.type(
              boundVars,
              (et).value.left),
            hydra.validate.Core.type(
              boundVars,
              (et).value.right)));
        }

        @Override
        public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Function ft) {
          return hydra.validate.Core.firstTypeError(java.util.Arrays.asList(
            hydra.validate.Core.type(
              boundVars,
              (ft).value.domain),
            hydra.validate.Core.type(
              boundVars,
              (ft).value.codomain)));
        }

        @Override
        public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.List lt) {
          return hydra.validate.Core.type(
            boundVars,
            (lt).value);
        }

        @Override
        public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Map mt) {
          return hydra.validate.Core.firstTypeError(java.util.Arrays.asList(
            hydra.validate.Core.type(
              boundVars,
              (mt).value.keys),
            hydra.validate.Core.type(
              boundVars,
              (mt).value.values)));
        }

        @Override
        public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Maybe mt) {
          return hydra.validate.Core.type(
            boundVars,
            (mt).value);
        }

        @Override
        public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Pair pt) {
          return hydra.validate.Core.firstTypeError(java.util.Arrays.asList(
            hydra.validate.Core.type(
              boundVars,
              (pt).value.first),
            hydra.validate.Core.type(
              boundVars,
              (pt).value.second)));
        }

        @Override
        public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Record fields) {
          return hydra.validate.Core.firstTypeError(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.FieldType, hydra.util.Maybe<hydra.error.core.InvalidTypeError>>) (f -> hydra.validate.Core.type(
              boundVars,
              (f).type)),
            (fields).value));
        }

        @Override
        public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Set st) {
          return hydra.validate.Core.type(
            boundVars,
            (st).value);
        }

        @Override
        public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Union fields) {
          return hydra.validate.Core.firstTypeError(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.FieldType, hydra.util.Maybe<hydra.error.core.InvalidTypeError>>) (f -> hydra.validate.Core.type(
              boundVars,
              (f).type)),
            (fields).value));
        }

        @Override
        public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Wrap wt) {
          return hydra.validate.Core.type(
            boundVars,
            (wt).value);
        }
      }),
      (java.util.function.Function<hydra.error.core.InvalidTypeError, hydra.util.Maybe<hydra.error.core.InvalidTypeError>>) (err -> hydra.util.Maybe.just(err)));
  }

  static hydra.util.Maybe<hydra.error.core.InvalidTypeError> validateTypeNode(java.util.Set<hydra.core.Name> boundVars, hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTypeError> otherwise(hydra.core.Type instance) {
        return (hydra.util.Maybe<hydra.error.core.InvalidTypeError>) (hydra.util.Maybe.<hydra.error.core.InvalidTypeError>nothing());
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Annotated ann) {
        java.util.Map<hydra.core.Name, hydra.core.Term> annMap = (ann).value.annotation;
        hydra.core.Type body = (ann).value.body;
        return hydra.validate.Core.firstTypeError(java.util.Arrays.asList(
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.maps.Null.apply(annMap),
            () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTypeError.EmptyTypeAnnotation(new hydra.error.core.EmptyTypeAnnotationError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList()))))),
            () -> (hydra.util.Maybe<hydra.error.core.InvalidTypeError>) (hydra.util.Maybe.<hydra.error.core.InvalidTypeError>nothing())),
          (body).accept(new hydra.core.Type.PartialVisitor<>() {
            @Override
            public hydra.util.Maybe<hydra.error.core.InvalidTypeError> otherwise(hydra.core.Type instance) {
              return (hydra.util.Maybe<hydra.error.core.InvalidTypeError>) (hydra.util.Maybe.<hydra.error.core.InvalidTypeError>nothing());
            }

            @Override
            public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Annotated ignored) {
              return hydra.util.Maybe.just(new hydra.error.core.InvalidTypeError.NestedTypeAnnotation(new hydra.error.core.NestedTypeAnnotationError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList())))));
            }
          })));
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Either et) {
        return hydra.validate.Core.firstTypeError(java.util.Arrays.asList(
          hydra.validate.Core.checkVoid((et).value.left),
          hydra.validate.Core.checkVoid((et).value.right)));
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Forall ft) {
        hydra.core.Name paramName = (ft).value.parameter;
        return hydra.validate.Core.firstTypeError(java.util.Arrays.asList(
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.sets.Member.apply(
              paramName,
              boundVars),
            () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTypeError.TypeVariableShadowingInForall(new hydra.error.core.TypeVariableShadowingInForallError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList())), paramName))),
            () -> (hydra.util.Maybe<hydra.error.core.InvalidTypeError>) (hydra.util.Maybe.<hydra.error.core.InvalidTypeError>nothing())),
          hydra.lib.logic.IfElse.lazy(
            hydra.validate.Core.isValidName(paramName),
            () -> (hydra.util.Maybe<hydra.error.core.InvalidTypeError>) (hydra.util.Maybe.<hydra.error.core.InvalidTypeError>nothing()),
            () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTypeError.InvalidForallParameterName(new hydra.error.core.InvalidForallParameterNameError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList())), paramName))))));
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Function ft) {
        return hydra.validate.Core.checkVoid((ft).value.codomain);
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.List lt) {
        return hydra.validate.Core.checkVoid((lt).value);
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Map mt) {
        hydra.core.Type keyType = (mt).value.keys;
        return hydra.validate.Core.firstTypeError(java.util.Arrays.asList(
          (keyType).accept(new hydra.core.Type.PartialVisitor<>() {
            @Override
            public hydra.util.Maybe<hydra.error.core.InvalidTypeError> otherwise(hydra.core.Type instance) {
              return (hydra.util.Maybe<hydra.error.core.InvalidTypeError>) (hydra.util.Maybe.<hydra.error.core.InvalidTypeError>nothing());
            }

            @Override
            public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Function ignored) {
              return hydra.util.Maybe.just(new hydra.error.core.InvalidTypeError.NonComparableMapKeyType(new hydra.error.core.NonComparableMapKeyTypeError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList())), keyType)));
            }
          }),
          hydra.validate.Core.checkVoid(keyType),
          hydra.validate.Core.checkVoid((mt).value.values)));
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Pair pt) {
        return hydra.validate.Core.firstTypeError(java.util.Arrays.asList(
          hydra.validate.Core.checkVoid((pt).value.first),
          hydra.validate.Core.checkVoid((pt).value.second)));
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Record fields) {
        return hydra.validate.Core.firstTypeError(java.util.Arrays.asList(
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply((fields).value),
            () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTypeError.EmptyRecordType(new hydra.error.core.EmptyRecordTypeError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList()))))),
            () -> (hydra.util.Maybe<hydra.error.core.InvalidTypeError>) (hydra.util.Maybe.<hydra.error.core.InvalidTypeError>nothing())),
          hydra.validate.Core.checkDuplicateFieldTypes(
            (fields).value,
            (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.error.core.InvalidTypeError>>) (dupName -> hydra.util.Maybe.just(new hydra.error.core.InvalidTypeError.DuplicateRecordTypeFieldNames(new hydra.error.core.DuplicateRecordTypeFieldNamesError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList())), dupName))))),
          hydra.validate.Core.firstTypeError(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.FieldType, hydra.util.Maybe<hydra.error.core.InvalidTypeError>>) (f -> hydra.validate.Core.checkVoid((f).type)),
            (fields).value))));
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Set elemType) {
        return hydra.validate.Core.firstTypeError(java.util.Arrays.asList(
          (elemType).value.accept(new hydra.core.Type.PartialVisitor<>() {
            @Override
            public hydra.util.Maybe<hydra.error.core.InvalidTypeError> otherwise(hydra.core.Type instance) {
              return (hydra.util.Maybe<hydra.error.core.InvalidTypeError>) (hydra.util.Maybe.<hydra.error.core.InvalidTypeError>nothing());
            }

            @Override
            public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Function ignored) {
              return hydra.util.Maybe.just(new hydra.error.core.InvalidTypeError.NonComparableSetElementType(new hydra.error.core.NonComparableSetElementTypeError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList())), (elemType).value)));
            }
          }),
          hydra.validate.Core.checkVoid((elemType).value)));
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Union fields) {
        return hydra.validate.Core.firstTypeError(java.util.Arrays.asList(
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply((fields).value),
            () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTypeError.EmptyUnionType(new hydra.error.core.EmptyUnionTypeError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList()))))),
            () -> (hydra.util.Maybe<hydra.error.core.InvalidTypeError>) (hydra.util.Maybe.<hydra.error.core.InvalidTypeError>nothing())),
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              hydra.lib.lists.Length.apply((fields).value),
              1),
            () -> ((java.util.function.Supplier<hydra.util.Maybe<hydra.error.core.InvalidTypeError>>) (() -> {
              hydra.util.Lazy<hydra.core.FieldType> singleField = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply((fields).value));
              return hydra.util.Maybe.just(new hydra.error.core.InvalidTypeError.SingleVariantUnion(new hydra.error.core.SingleVariantUnionError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList())), singleField.get().name)));
            })).get(),
            () -> (hydra.util.Maybe<hydra.error.core.InvalidTypeError>) (hydra.util.Maybe.<hydra.error.core.InvalidTypeError>nothing())),
          hydra.validate.Core.checkDuplicateFieldTypes(
            (fields).value,
            (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.error.core.InvalidTypeError>>) (dupName -> hydra.util.Maybe.just(new hydra.error.core.InvalidTypeError.DuplicateUnionTypeFieldNames(new hydra.error.core.DuplicateUnionTypeFieldNamesError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList())), dupName))))),
          hydra.validate.Core.firstTypeError(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.FieldType, hydra.util.Maybe<hydra.error.core.InvalidTypeError>>) (f -> hydra.validate.Core.checkVoid((f).type)),
            (fields).value))));
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTypeError> visit(hydra.core.Type.Variable varName) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.sets.Member.apply(
            (varName).value,
            boundVars),
          () -> (hydra.util.Maybe<hydra.error.core.InvalidTypeError>) (hydra.util.Maybe.<hydra.error.core.InvalidTypeError>nothing()),
          () -> hydra.util.Maybe.just(new hydra.error.core.InvalidTypeError.UndefinedTypeVariable(new hydra.error.core.UndefinedTypeVariableError(new hydra.paths.SubtermPath((java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList())), (varName).value))));
      }
    });
  }
}
