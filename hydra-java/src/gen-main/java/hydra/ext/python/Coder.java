// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python;

/**
 * Python code generator: converts Hydra modules to Python source code
 */
public interface Coder {
  static <T0> hydra.util.Either<T0, hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>> analyzePythonFunction(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Term term) {
    return hydra.CoderUtils.analyzeFunctionTermWith(
      cx,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (p0 -> p1 -> hydra.ext.python.Coder.pythonBindingMetadata(
        p0,
        p1)),
      hydra.ext.python.Coder::pythonEnvironmentGetGraph,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, hydra.ext.python.environment.PythonEnvironment>>) (p0 -> p1 -> hydra.ext.python.Coder.pythonEnvironmentSetGraph(
        p0,
        p1)),
      env,
      term);
  }

  static hydra.ext.python.syntax.ClosedPattern classVariantPatternUnit(hydra.ext.python.syntax.Name pyVariantName) {
    return new hydra.ext.python.syntax.ClosedPattern.Class_(new hydra.ext.python.syntax.ClassPattern(new hydra.ext.python.syntax.NameOrAttribute(java.util.Arrays.asList(pyVariantName)), (hydra.util.Maybe<hydra.ext.python.syntax.PositionalPatterns>) (hydra.util.Maybe.<hydra.ext.python.syntax.PositionalPatterns>nothing()), (hydra.util.Maybe<hydra.ext.python.syntax.KeywordPatterns>) (hydra.util.Maybe.<hydra.ext.python.syntax.KeywordPatterns>nothing())));
  }

  static hydra.ext.python.syntax.ClosedPattern classVariantPatternWithCapture(hydra.ext.python.environment.PythonEnvironment env, hydra.ext.python.syntax.Name pyVariantName, hydra.core.Name varName) {
    hydra.ext.python.syntax.ClosedPattern capturePattern = new hydra.ext.python.syntax.ClosedPattern.Capture(new hydra.ext.python.syntax.CapturePattern(new hydra.ext.python.syntax.PatternCaptureTarget(hydra.ext.python.Names.encodeName(
      false,
      new hydra.util.CaseConvention.LowerSnake(),
      env,
      varName))));
    hydra.ext.python.syntax.KeywordPattern keywordPattern = new hydra.ext.python.syntax.KeywordPattern(new hydra.ext.python.syntax.Name("value"), new hydra.ext.python.syntax.Pattern.Or(new hydra.ext.python.syntax.OrPattern(java.util.Arrays.asList(capturePattern))));
    hydra.ext.python.syntax.NameOrAttribute pyVarNameAttr = new hydra.ext.python.syntax.NameOrAttribute(java.util.Arrays.asList(pyVariantName));
    return new hydra.ext.python.syntax.ClosedPattern.Class_(new hydra.ext.python.syntax.ClassPattern(pyVarNameAttr, (hydra.util.Maybe<hydra.ext.python.syntax.PositionalPatterns>) (hydra.util.Maybe.<hydra.ext.python.syntax.PositionalPatterns>nothing()), hydra.util.Maybe.just(new hydra.ext.python.syntax.KeywordPatterns(java.util.Arrays.asList(keywordPattern)))));
  }

  static java.util.Set<hydra.core.Name> collectTypeVariables(java.util.Set<hydra.core.Name> initial, hydra.core.Type typ) {
    return hydra.Strip.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.Set<hydra.core.Name> otherwise(hydra.core.Type instance) {
        java.util.Set<hydra.core.Name> freeVars = hydra.Variables.freeVariablesInType(typ);
        java.util.function.Function<hydra.core.Name, Boolean> isTypeVar = (java.util.function.Function<hydra.core.Name, Boolean>) (n -> hydra.ext.python.Coder.isTypeVariableName(n));
        hydra.util.Lazy<java.util.List<hydra.core.Name>> filteredList = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
          isTypeVar,
          hydra.lib.sets.ToList.apply(freeVars)));
        return hydra.lib.sets.Union.apply(
          initial,
          hydra.lib.sets.FromList.apply(filteredList.get()));
      }

      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        hydra.core.Type body = (ft).value.body;
        hydra.core.Name v = (ft).value.parameter;
        return hydra.ext.python.Coder.collectTypeVariables(
          hydra.lib.sets.Insert.apply(
            v,
            initial),
          body);
      }
    });
  }

  static <T0> hydra.util.Maybe<T0> condImportSymbol(T0 name, Boolean flag) {
    return hydra.lib.logic.IfElse.lazy(
      flag,
      () -> hydra.util.Maybe.just(name),
      () -> (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()));
  }

  static hydra.ext.python.syntax.NamedExpression dataclassDecorator() {
    return new hydra.ext.python.syntax.NamedExpression.Simple(hydra.ext.python.Utils.pyPrimaryToPyExpression(hydra.ext.python.Utils.primaryWithRhs(
      new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("dataclass"))),
      new hydra.ext.python.syntax.PrimaryRhs.Call(new hydra.ext.python.syntax.Args((java.util.List<hydra.ext.python.syntax.PosArg>) (java.util.Collections.<hydra.ext.python.syntax.PosArg>emptyList()), java.util.Arrays.asList(new hydra.ext.python.syntax.KwargOrStarred.Kwarg(new hydra.ext.python.syntax.Kwarg(new hydra.ext.python.syntax.Name("frozen"), hydra.ext.python.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.True())))), (java.util.List<hydra.ext.python.syntax.KwargOrDoubleStarred>) (java.util.Collections.<hydra.ext.python.syntax.KwargOrDoubleStarred>emptyList()))))));
  }

  static hydra.ext.python.syntax.Name deconflictVariantName(Boolean isQualified, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name unionName, hydra.core.Name fname, hydra.graph.Graph g) {
    hydra.core.Name candidateHydraName = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
      (unionName).value,
      hydra.Formatting.capitalize((fname).value)));
    hydra.util.Lazy<Boolean> termCollision = new hydra.util.Lazy<>(() -> hydra.lib.maps.Member.apply(
      candidateHydraName,
      (g).boundTerms));
    hydra.util.Lazy<Boolean> typeCollision = new hydra.util.Lazy<>(() -> hydra.lib.maps.Member.apply(
      candidateHydraName,
      (g).schemaTypes));
    Boolean collision = hydra.lib.logic.Or.apply(
      termCollision.get(),
      typeCollision.get());
    return hydra.lib.logic.IfElse.lazy(
      collision,
      () -> new hydra.ext.python.syntax.Name(hydra.lib.strings.Cat2.apply(
        hydra.ext.python.Names.variantName(
          isQualified,
          env,
          unionName,
          fname).value,
        "_")),
      () -> hydra.ext.python.Names.variantName(
        isQualified,
        env,
        unionName,
        fname));
  }

  static java.util.List<hydra.core.Field> deduplicateCaseVariables(java.util.List<hydra.core.Field> cases_) {
    java.util.function.Function<hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>, java.util.function.Function<hydra.core.Field, hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>>> rewriteCase = (java.util.function.Function<hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>, java.util.function.Function<hydra.core.Field, hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>>>) (state -> (java.util.function.Function<hydra.core.Field, hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>>) (field -> {
      hydra.util.Lazy<java.util.Map<hydra.core.Name, Integer>> countByName = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(state));
      hydra.util.Lazy<java.util.List<hydra.core.Field>> done = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(state));
      hydra.core.Name fname = (field).name;
      hydra.core.Term fterm = (field).term;
      return hydra.Strip.deannotateAndDetypeTerm(fterm).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) ((hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) (new hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>(countByName.get(), hydra.lib.lists.Cons.apply(
            field,
            done.get()))));
        }

        @Override
        public hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>> visit(hydra.core.Term.Function f) {
          return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>> otherwise(hydra.core.Function instance) {
              return (hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) ((hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) (new hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>(countByName.get(), hydra.lib.lists.Cons.apply(
                field,
                done.get()))));
            }

            @Override
            public hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>> visit(hydra.core.Function.Lambda lam) {
              hydra.core.Term body = (lam).value.body;
              hydra.util.Maybe<hydra.core.Type> mdom = (lam).value.domain;
              hydra.core.Name v = (lam).value.parameter;
              return hydra.lib.maybes.Maybe.applyLazy(
                () -> (hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) ((hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) (new hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>(hydra.lib.maps.Insert.apply(
                  v,
                  1,
                  countByName.get()), hydra.lib.lists.Cons.apply(
                  field,
                  done.get())))),
                (java.util.function.Function<Integer, hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>>) (count -> {
                  Integer count2 = hydra.lib.math.Add.apply(
                    count,
                    1);
                  hydra.core.Name v2 = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
                    (v).value,
                    hydra.lib.literals.ShowInt32.apply(count2)));
                  hydra.core.Term newBody = hydra.Reduction.alphaConvert(
                    v,
                    v2,
                    body);
                  hydra.core.Lambda newLam = new hydra.core.Lambda(v2, mdom, newBody);
                  hydra.core.Term newTerm = new hydra.core.Term.Function(new hydra.core.Function.Lambda(newLam));
                  hydra.core.Field newField = new hydra.core.Field(fname, newTerm);
                  return (hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) ((hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) (new hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>(hydra.lib.maps.Insert.apply(
                    v,
                    count2,
                    countByName.get()), hydra.lib.lists.Cons.apply(
                    newField,
                    done.get()))));
                }),
                hydra.lib.maps.Lookup.apply(
                  v,
                  countByName.get()));
            }
          });
        }
      });
    }));
    hydra.util.Lazy<hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>> result = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      rewriteCase,
      (hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) ((hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>) (new hydra.util.Pair<java.util.Map<hydra.core.Name, Integer>, java.util.List<hydra.core.Field>>((java.util.Map<hydra.core.Name, Integer>) ((java.util.Map<hydra.core.Name, Integer>) (hydra.lib.maps.Empty.<hydra.core.Name, Integer>apply())), (java.util.List<hydra.core.Field>) (java.util.Collections.<hydra.core.Field>emptyList())))),
      cases_));
    return hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(result.get()));
  }

  static hydra.ext.python.environment.PythonModuleMetadata digForWrap(Boolean isTermAnnot, hydra.ext.python.environment.PythonModuleMetadata meta, hydra.core.Type typ) {
    return hydra.Strip.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.ext.python.environment.PythonModuleMetadata otherwise(hydra.core.Type instance) {
        return meta;
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Type.Forall ft) {
        return hydra.ext.python.Coder.digForWrap(
          isTermAnnot,
          meta,
          (ft).value.body);
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Type.Wrap ignored) {
        return hydra.lib.logic.IfElse.lazy(
          isTermAnnot,
          () -> meta,
          () -> hydra.ext.python.Coder.setMetaUsesNode(
            meta,
            true));
      }
    });
  }

  static hydra.core.Term eliminateUnitVar(hydra.core.Name v, hydra.core.Term term0) {
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Binding, hydra.core.Binding>> rewriteBinding = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Binding, hydra.core.Binding>>) (rewrite -> (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (bnd -> new hydra.core.Binding((bnd).name, (rewrite).apply((bnd).term), (bnd).type)));
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Field, hydra.core.Field>> rewriteField = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Field, hydra.core.Field>>) (rewrite -> (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (fld -> new hydra.core.Field((fld).name, (rewrite).apply((fld).term))));
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> rewrite = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> hydra.Strip.deannotateAndDetypeTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return term;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable n) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (n).value,
            v),
          () -> new hydra.core.Term.Unit(),
          () -> term);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm((recurse).apply((at).value.body), (at).value.annotation));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Application app) {
        return new hydra.core.Term.Application(new hydra.core.Application((recurse).apply((app).value.function), (recurse).apply((app).value.argument)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return term;
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda lam) {
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                (lam).value.parameter,
                v),
              () -> term,
              () -> new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda((lam).value.parameter, (lam).value.domain, (recurse).apply((lam).value.body)))));
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Elimination e) {
            return (e).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public hydra.core.Term otherwise(hydra.core.Elimination instance) {
                return term;
              }

              @Override
              public hydra.core.Term visit(hydra.core.Elimination.Union cs) {
                return new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement((cs).value.typeName, hydra.lib.maybes.Map.apply(
                  recurse,
                  (cs).value.default_), hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (v1 -> (rewriteField).apply(recurse).apply(v1)),
                  (cs).value.cases)))));
              }
            });
          }
        });
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        return new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (v1 -> (rewriteBinding).apply(recurse).apply(v1)),
          (lt).value.bindings), (recurse).apply((lt).value.body)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.List ts) {
        return new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          recurse,
          (ts).value));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Map m) {
        return new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) (kv -> (hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>((recurse).apply(hydra.lib.pairs.First.apply(kv)), (recurse).apply(hydra.lib.pairs.Second.apply(kv)))))),
          hydra.lib.maps.ToList.apply((m).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Record rec) {
        return new hydra.core.Term.Record(new hydra.core.Record((rec).value.typeName, hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (v1 -> (rewriteField).apply(recurse).apply(v1)),
          (rec).value.fields)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Set s) {
        return new hydra.core.Term.Set(hydra.lib.sets.Map.apply(
          recurse,
          (s).value));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Union inj) {
        return new hydra.core.Term.Union(new hydra.core.Injection((inj).value.typeName, (rewriteField).apply(recurse).apply((inj).value.field)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Maybe mt) {
        return new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
          recurse,
          (mt).value));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Pair p) {
        return new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>((recurse).apply(hydra.lib.pairs.First.apply((p).value)), (recurse).apply(hydra.lib.pairs.Second.apply((p).value))))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Wrap wt) {
        return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm((wt).value.typeName, (recurse).apply((wt).value.body)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Either e) {
        return new hydra.core.Term.Either(hydra.lib.eithers.Bimap.apply(
          recurse,
          recurse,
          (e).value));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication ta) {
        return new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm((recurse).apply((ta).value.body), (ta).value.type));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
        return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda((tl).value.parameter, (recurse).apply((tl).value.body)));
      }
    })));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, hydra.core.Term>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> (rewrite).apply(go.get()).apply(term)));
    return go.get().apply(term0);
  }

  static hydra.ext.python.environment.PythonModuleMetadata emptyMetadata(hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> ns) {
    return new hydra.ext.python.environment.PythonModuleMetadata(ns, (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> encodeApplication(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Application app) {
    hydra.core.Term term = new hydra.core.Term.Application(app);
    hydra.util.Lazy<hydra.util.Pair<hydra.core.Term, java.util.List<hydra.core.Term>>> gathered = new hydra.util.Lazy<>(() -> hydra.CoderUtils.gatherArgs(
      term,
      (java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())));
    hydra.util.Lazy<java.util.List<hydra.core.Term>> args = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(gathered.get()));
    hydra.util.Lazy<hydra.core.Term> fun = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(gathered.get()));
    hydra.graph.Graph g = hydra.ext.python.Coder.pythonEnvironmentGetGraph(env);
    Integer knownArity = hydra.ext.python.Coder.termArityWithPrimitives(
      g,
      fun.get());
    hydra.util.Lazy<Integer> arity = new hydra.util.Lazy<>(() -> hydra.lib.math.Max.apply(
      knownArity,
      hydra.lib.lists.Length.apply(args.get())));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (t -> hydra.ext.python.Coder.encodeTermInline(
          cx,
          env,
          false,
          t)),
        args.get()),
      (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (pargs -> {
        hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> hargs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Take.apply(
          arity.get(),
          pargs));
        hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> rargs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Drop.apply(
          arity.get(),
          pargs));
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.python.Coder.encodeApplicationInner(
            cx,
            env,
            fun.get(),
            hargs.get(),
            rargs.get()),
          (java.util.function.Function<hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (result -> {
            hydra.util.Lazy<hydra.ext.python.syntax.Expression> lhs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
            hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> remainingRargs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result));
            hydra.util.Lazy<hydra.ext.python.syntax.Expression> pyapp = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
              (java.util.function.Function<hydra.ext.python.syntax.Expression, java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>>) (t -> (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>) (a -> hydra.ext.python.Utils.functionCall(
                hydra.ext.python.Utils.pyExpressionToPyPrimary(t),
                java.util.Arrays.asList(a)))),
              lhs.get(),
              remainingRargs.get()));
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(pyapp.get());
          }));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> encodeApplicationInner(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Term fun, java.util.List<hydra.ext.python.syntax.Expression> hargs, java.util.List<hydra.ext.python.syntax.Expression> rargs) {
    hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>> defaultCase = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Bind.apply(
      hydra.ext.python.Coder.encodeTermInline(
        cx,
        env,
        false,
        fun),
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (pfun -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>right((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(hydra.ext.python.Utils.functionCall(
        hydra.ext.python.Utils.pyExpressionToPyPrimary(pfun),
        hargs), rargs)))))));
    hydra.util.Lazy<hydra.ext.python.syntax.Expression> firstArg = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(hargs));
    hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> restArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(hargs));
    java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression> withRest = (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>) (e -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(restArgs.get()),
      () -> e,
      () -> hydra.ext.python.Utils.functionCall(
        hydra.ext.python.Utils.pyExpressionToPyPrimary(e),
        restArgs.get())));
    return hydra.Strip.deannotateAndDetypeTerm(fun).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> otherwise(hydra.core.Term instance) {
        return defaultCase.get();
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> otherwise(hydra.core.Function instance) {
            return defaultCase.get();
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> visit(hydra.core.Function.Elimination elm) {
            return (elm).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> otherwise(hydra.core.Elimination instance) {
                return defaultCase.get();
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> visit(hydra.core.Elimination.Record proj) {
                hydra.core.Name fname = (proj).value.field;
                hydra.ext.python.syntax.Expression fieldExpr = hydra.ext.python.Utils.projectFromExpression(
                  firstArg.get(),
                  hydra.ext.python.Names.encodeFieldName(
                    env,
                    fname));
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>right((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>((withRest).apply(fieldExpr), rargs))));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> visit(hydra.core.Elimination.Union cs) {
                return hydra.lib.eithers.Bind.apply(
                  hydra.ext.python.Coder.encodeUnionEliminationInline(
                    cx,
                    env,
                    (cs).value,
                    firstArg.get()),
                  (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (inlineExpr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>right((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>((withRest).apply(inlineExpr), rargs))))));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> visit(hydra.core.Elimination.Wrap ignored) {
                hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> allArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                  restArgs.get(),
                  rargs));
                hydra.ext.python.syntax.Expression valueExpr = hydra.ext.python.Utils.projectFromExpression(
                  firstArg.get(),
                  new hydra.ext.python.syntax.Name("value"));
                return hydra.lib.logic.IfElse.lazy(
                  hydra.lib.lists.Null.apply(allArgs.get()),
                  () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>right((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(valueExpr, (java.util.List<hydra.ext.python.syntax.Expression>) (java.util.Collections.<hydra.ext.python.syntax.Expression>emptyList()))))),
                  () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>right((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(hydra.ext.python.Utils.functionCall(
                    hydra.ext.python.Utils.pyExpressionToPyPrimary(valueExpr),
                    allArgs.get()), (java.util.List<hydra.ext.python.syntax.Expression>) (java.util.Collections.<hydra.ext.python.syntax.Expression>emptyList()))))));
              }
            });
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> visit(hydra.core.Function.Primitive name) {
            java.util.List<hydra.ext.python.syntax.Expression> wrappedArgs = hydra.ext.python.Coder.wrapLazyArguments(
              (name).value,
              hargs);
            return hydra.lib.eithers.Bind.apply(
              hydra.ext.python.Coder.encodeVariable(
                cx,
                env,
                (name).value,
                wrappedArgs),
              (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>right((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(expr, rargs))))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> visit(hydra.core.Function.Lambda ignored) {
            return hydra.lib.eithers.Bind.apply(
              hydra.ext.python.Coder.encodeTermInline(
                cx,
                env,
                false,
                fun),
              (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (pfun -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>right((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(hydra.ext.python.Utils.functionCall(
                hydra.ext.python.Utils.pyExpressionToPyPrimary(pfun),
                hargs), rargs))))));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>> visit(hydra.core.Term.Variable name) {
        hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> allArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
          hargs,
          rargs));
        hydra.graph.Graph g = hydra.ext.python.Coder.pythonEnvironmentGetGraph(env);
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.lib.eithers.Bind.apply(
            hydra.ext.python.Coder.encodeVariable(
              cx,
              env,
              (name).value,
              hargs),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>right((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(expr, rargs)))))),
          (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (el -> hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.lib.eithers.Bind.apply(
              hydra.ext.python.Coder.encodeVariable(
                cx,
                env,
                (name).value,
                hargs),
              (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>right((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(expr, rargs)))))),
            (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (ts -> {
              Integer elArity = hydra.Arity.typeSchemeArity(ts);
              hydra.util.Lazy<Integer> consumeCount = new hydra.util.Lazy<>(() -> hydra.lib.math.Min.apply(
                elArity,
                hydra.lib.lists.Length.apply(allArgs.get())));
              hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> consumedArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Take.apply(
                consumeCount.get(),
                allArgs.get()));
              hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> remainingArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Drop.apply(
                consumeCount.get(),
                allArgs.get()));
              return hydra.lib.logic.IfElse.lazy(
                hydra.lib.lists.Null.apply(consumedArgs.get()),
                () -> hydra.lib.eithers.Bind.apply(
                  hydra.ext.python.Coder.encodeVariable(
                    cx,
                    env,
                    (name).value,
                    (java.util.List<hydra.ext.python.syntax.Expression>) (java.util.Collections.<hydra.ext.python.syntax.Expression>emptyList())),
                  (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>right((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(expr, rargs)))))),
                () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>>right((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) ((hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>) (new hydra.util.Pair<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Expression>>(hydra.ext.python.Utils.functionCall(
                  hydra.ext.python.Utils.pyNameToPyPrimary(hydra.ext.python.Names.encodeName(
                    true,
                    new hydra.util.CaseConvention.LowerSnake(),
                    env,
                    (name).value)),
                  consumedArgs.get()), remainingArgs.get())))));
            }),
            (el).type)),
          hydra.Lexical.lookupElement(
            g,
            (name).value));
      }
    });
  }

  static <T0> hydra.util.Either<T0, hydra.ext.python.syntax.Expression> encodeApplicationType(hydra.ext.python.environment.PythonEnvironment env, hydra.core.ApplicationType at) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, java.util.function.Function<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>>>> gatherParams = new java.util.concurrent.atomic.AtomicReference<>();
    gatherParams.set((java.util.function.Function<hydra.core.Type, java.util.function.Function<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>>>) (t -> (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>>) (ps -> hydra.Strip.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Application appT) {
        return gatherParams.get().apply((appT).value.function).apply(hydra.lib.lists.Cons.apply(
          (appT).value.argument,
          ps));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Annotated ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Function ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Forall ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.List ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Literal ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Map ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Maybe ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Either ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Pair ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Record ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Set ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Union ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Unit ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Variable ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Void_ ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>> visit(hydra.core.Type.Wrap ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>(t, ps)));
      }
    }))));
    hydra.util.Lazy<hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Type>>> bodyAndArgs = new hydra.util.Lazy<>(() -> gatherParams.get().apply(new hydra.core.Type.Application(at)).apply((java.util.List<hydra.core.Type>) (java.util.Collections.<hydra.core.Type>emptyList())));
    hydra.util.Lazy<java.util.List<hydra.core.Type>> args = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bodyAndArgs.get()));
    hydra.util.Lazy<hydra.core.Type> body = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bodyAndArgs.get()));
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.python.Coder.<T0>encodeType(
        env,
        body.get()),
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (pyBody -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (v1 -> hydra.ext.python.Coder.<T0>encodeType(
            env,
            v1)),
          args.get()),
        (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Expression>, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (pyArgs -> hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.primaryAndParams(
          hydra.ext.python.Utils.pyExpressionToPyPrimary(pyBody),
          pyArgs))))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement> encodeBindingAs(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Binding binding) {
    hydra.core.Name name1 = (binding).name;
    hydra.ext.python.syntax.Name fname = hydra.ext.python.Names.encodeName(
      true,
      new hydra.util.CaseConvention.LowerSnake(),
      env,
      name1);
    hydra.util.Maybe<hydra.core.TypeScheme> mts = (binding).type;
    hydra.core.Term term1 = (binding).term;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (() -> {
        hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Term> gathered = hydra.ext.python.Coder.gatherLambdas(term1);
        return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (() -> {
          hydra.util.Lazy<java.util.List<hydra.core.Name>> lambdaParams = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(gathered));
          return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (() -> {
            hydra.util.Lazy<hydra.core.Term> innerBody = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(gathered));
            return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (() -> {
              hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>> mcsa = hydra.ext.python.Coder.isCaseStatementApplication(innerBody.get());
              return hydra.lib.maybes.Maybe.applyLazy(
                () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (() -> {
                  hydra.util.Maybe<hydra.core.CaseStatement> mcs = hydra.ext.python.Coder.extractCaseElimination(term1);
                  return hydra.lib.maybes.Maybe.applyLazy(
                    () -> hydra.lib.eithers.Map.apply(
                      (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.ext.python.syntax.Statement>) (stmts -> hydra.lib.lists.Head.apply(stmts)),
                      hydra.ext.python.Coder.encodeTermMultiline(
                        cx,
                        env,
                        term1)),
                    (java.util.function.Function<hydra.core.CaseStatement, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (cs -> {
                      java.util.List<hydra.core.Field> cases_ = (cs).cases;
                      hydra.util.Maybe<hydra.core.Term> dflt = (cs).default_;
                      hydra.core.Name tname = (cs).typeName;
                      return hydra.lib.eithers.Bind.apply(
                        hydra.Resolution.requireUnionType(
                          cx,
                          hydra.ext.python.Coder.pythonEnvironmentGetGraph(env),
                          tname),
                        (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (rt -> {
                          hydra.util.Lazy<hydra.ext.python.syntax.Param> innerParam = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Param(new hydra.ext.python.syntax.Name("x"), (hydra.util.Maybe<hydra.ext.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.ext.python.syntax.Annotation>nothing())));
                          Boolean isEnum = hydra.Predicates.isEnumRowType(rt);
                          hydra.util.Lazy<Boolean> isFull = new hydra.util.Lazy<>(() -> hydra.ext.python.Coder.isCasesFull(
                            rt,
                            cases_));
                          hydra.util.Lazy<hydra.ext.python.syntax.ParamNoDefault> param = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.ParamNoDefault(innerParam.get(), (hydra.util.Maybe<hydra.ext.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.TypeComment>nothing())));
                          hydra.util.Lazy<hydra.ext.python.syntax.Parameters> params = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Parameters.ParamNoDefault(new hydra.ext.python.syntax.ParamNoDefaultParameters(java.util.Arrays.asList(param.get()), (java.util.List<hydra.ext.python.syntax.ParamWithDefault>) (java.util.Collections.<hydra.ext.python.syntax.ParamWithDefault>emptyList()), (hydra.util.Maybe<hydra.ext.python.syntax.StarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.StarEtc>nothing()))));
                          return hydra.lib.eithers.Bind.apply(
                            hydra.lib.eithers.MapList.apply(
                              (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.CaseBlock>>) (v1 -> hydra.ext.python.Coder.encodeCaseBlock(
                                cx,
                                env,
                                tname,
                                rt,
                                isEnum,
                                (java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>>) (e -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (t -> hydra.ext.python.Coder.encodeTermMultiline(
                                  cx,
                                  e,
                                  t))),
                                v1)),
                              cases_),
                            (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (pyCases -> hydra.lib.eithers.Bind.apply(
                              hydra.ext.python.Coder.encodeDefaultCaseBlock(
                                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (t -> hydra.ext.python.Coder.encodeTermInline(
                                  cx,
                                  env,
                                  false,
                                  t)),
                                isFull.get(),
                                dflt,
                                tname),
                              (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (pyDflt -> {
                                hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.CaseBlock>> allCases = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                                  pyCases,
                                  pyDflt));
                                hydra.ext.python.syntax.SubjectExpression subj = new hydra.ext.python.syntax.SubjectExpression.Simple(new hydra.ext.python.syntax.NamedExpression.Simple(hydra.ext.python.Utils.pyNameToPyExpression(new hydra.ext.python.syntax.Name("x"))));
                                hydra.ext.python.syntax.Statement matchStmt = new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Match(new hydra.ext.python.syntax.MatchStatement(subj, allCases.get())));
                                hydra.util.Lazy<hydra.ext.python.syntax.Block> body = new hydra.util.Lazy<>(() -> hydra.ext.python.Utils.indentedBlock(
                                  (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
                                  java.util.Arrays.asList(java.util.Arrays.asList(matchStmt))));
                                hydra.util.Lazy<hydra.ext.python.syntax.FunctionDefRaw> funcDefRaw = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.FunctionDefRaw(false, fname, (java.util.List<hydra.ext.python.syntax.TypeParameter>) (java.util.Collections.<hydra.ext.python.syntax.TypeParameter>emptyList()), hydra.util.Maybe.just(params.get()), (hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing()), (hydra.util.Maybe<hydra.ext.python.syntax.FuncTypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.FuncTypeComment>nothing()), body.get()));
                                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>right(new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Function(new hydra.ext.python.syntax.FunctionDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), funcDefRaw.get()))));
                              }))));
                        }));
                    }),
                    mcs);
                })).get(),
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (csa -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.lists.Null.apply(lambdaParams.get()),
                  () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (() -> {
                    hydra.util.Maybe<hydra.core.CaseStatement> mcs = hydra.ext.python.Coder.extractCaseElimination(term1);
                    return hydra.lib.maybes.Maybe.applyLazy(
                      () -> hydra.lib.eithers.Map.apply(
                        (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.ext.python.syntax.Statement>) (stmts -> hydra.lib.lists.Head.apply(stmts)),
                        hydra.ext.python.Coder.encodeTermMultiline(
                          cx,
                          env,
                          term1)),
                      (java.util.function.Function<hydra.core.CaseStatement, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (cs -> {
                        java.util.List<hydra.core.Field> cases_ = (cs).cases;
                        hydra.util.Maybe<hydra.core.Term> dflt = (cs).default_;
                        hydra.core.Name tname = (cs).typeName;
                        return hydra.lib.eithers.Bind.apply(
                          hydra.Resolution.requireUnionType(
                            cx,
                            hydra.ext.python.Coder.pythonEnvironmentGetGraph(env),
                            tname),
                          (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (rt -> {
                            hydra.util.Lazy<hydra.ext.python.syntax.Param> innerParam = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Param(new hydra.ext.python.syntax.Name("x"), (hydra.util.Maybe<hydra.ext.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.ext.python.syntax.Annotation>nothing())));
                            Boolean isEnum = hydra.Predicates.isEnumRowType(rt);
                            hydra.util.Lazy<Boolean> isFull = new hydra.util.Lazy<>(() -> hydra.ext.python.Coder.isCasesFull(
                              rt,
                              cases_));
                            hydra.util.Lazy<hydra.ext.python.syntax.ParamNoDefault> param = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.ParamNoDefault(innerParam.get(), (hydra.util.Maybe<hydra.ext.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.TypeComment>nothing())));
                            hydra.util.Lazy<hydra.ext.python.syntax.Parameters> params = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Parameters.ParamNoDefault(new hydra.ext.python.syntax.ParamNoDefaultParameters(java.util.Arrays.asList(param.get()), (java.util.List<hydra.ext.python.syntax.ParamWithDefault>) (java.util.Collections.<hydra.ext.python.syntax.ParamWithDefault>emptyList()), (hydra.util.Maybe<hydra.ext.python.syntax.StarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.StarEtc>nothing()))));
                            return hydra.lib.eithers.Bind.apply(
                              hydra.lib.eithers.MapList.apply(
                                (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.CaseBlock>>) (v1 -> hydra.ext.python.Coder.encodeCaseBlock(
                                  cx,
                                  env,
                                  tname,
                                  rt,
                                  isEnum,
                                  (java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>>) (e -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (t -> hydra.ext.python.Coder.encodeTermMultiline(
                                    cx,
                                    e,
                                    t))),
                                  v1)),
                                cases_),
                              (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (pyCases -> hydra.lib.eithers.Bind.apply(
                                hydra.ext.python.Coder.encodeDefaultCaseBlock(
                                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (t -> hydra.ext.python.Coder.encodeTermInline(
                                    cx,
                                    env,
                                    false,
                                    t)),
                                  isFull.get(),
                                  dflt,
                                  tname),
                                (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (pyDflt -> {
                                  hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.CaseBlock>> allCases = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                                    pyCases,
                                    pyDflt));
                                  hydra.ext.python.syntax.SubjectExpression subj = new hydra.ext.python.syntax.SubjectExpression.Simple(new hydra.ext.python.syntax.NamedExpression.Simple(hydra.ext.python.Utils.pyNameToPyExpression(new hydra.ext.python.syntax.Name("x"))));
                                  hydra.ext.python.syntax.Statement matchStmt = new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Match(new hydra.ext.python.syntax.MatchStatement(subj, allCases.get())));
                                  hydra.util.Lazy<hydra.ext.python.syntax.Block> body = new hydra.util.Lazy<>(() -> hydra.ext.python.Utils.indentedBlock(
                                    (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
                                    java.util.Arrays.asList(java.util.Arrays.asList(matchStmt))));
                                  hydra.util.Lazy<hydra.ext.python.syntax.FunctionDefRaw> funcDefRaw = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.FunctionDefRaw(false, fname, (java.util.List<hydra.ext.python.syntax.TypeParameter>) (java.util.Collections.<hydra.ext.python.syntax.TypeParameter>emptyList()), hydra.util.Maybe.just(params.get()), (hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing()), (hydra.util.Maybe<hydra.ext.python.syntax.FuncTypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.FuncTypeComment>nothing()), body.get()));
                                  return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>right(new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Function(new hydra.ext.python.syntax.FunctionDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), funcDefRaw.get()))));
                                }))));
                          }));
                      }),
                      mcs);
                  })).get(),
                  () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (() -> {
                    hydra.util.Lazy<hydra.core.Name> tname = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(csa));
                    return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (() -> {
                      hydra.util.Lazy<hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>> rest1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(csa));
                      return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (() -> {
                        hydra.util.Lazy<hydra.util.Maybe<hydra.core.Term>> dflt = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(rest1.get()));
                        return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (() -> {
                          hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>> rest2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(rest1.get()));
                          return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (() -> {
                            hydra.util.Lazy<java.util.List<hydra.core.Field>> cases_ = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(rest2.get()));
                            return hydra.lib.eithers.Bind.apply(
                              hydra.Resolution.requireUnionType(
                                cx,
                                hydra.ext.python.Coder.pythonEnvironmentGetGraph(env),
                                tname.get()),
                              (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (rt -> {
                                hydra.util.Lazy<java.util.List<hydra.core.Name>> capturedVarNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Init.apply(lambdaParams.get()));
                                hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.ParamNoDefault>> capturedParams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                                  (java.util.function.Function<hydra.core.Name, hydra.ext.python.syntax.ParamNoDefault>) (n -> new hydra.ext.python.syntax.ParamNoDefault(new hydra.ext.python.syntax.Param(hydra.ext.python.Names.encodeName(
                                    false,
                                    new hydra.util.CaseConvention.LowerSnake(),
                                    env,
                                    n), (hydra.util.Maybe<hydra.ext.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.ext.python.syntax.Annotation>nothing())), (hydra.util.Maybe<hydra.ext.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.TypeComment>nothing()))),
                                  capturedVarNames.get()));
                                hydra.util.Lazy<hydra.core.Name> matchLambdaParam = new hydra.util.Lazy<>(() -> hydra.lib.lists.Last.apply(lambdaParams.get()));
                                hydra.ext.python.syntax.Name matchArgName = hydra.ext.python.Names.encodeName(
                                  false,
                                  new hydra.util.CaseConvention.LowerSnake(),
                                  env,
                                  matchLambdaParam.get());
                                hydra.util.Lazy<hydra.ext.python.syntax.ParamNoDefault> matchParam = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.ParamNoDefault(new hydra.ext.python.syntax.Param(matchArgName, (hydra.util.Maybe<hydra.ext.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.ext.python.syntax.Annotation>nothing())), (hydra.util.Maybe<hydra.ext.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.TypeComment>nothing())));
                                hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.ParamNoDefault>> allParams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                                  capturedParams.get(),
                                  java.util.Arrays.asList(matchParam.get())));
                                hydra.ext.python.environment.PythonEnvironment envWithParams = hydra.ext.python.Coder.extendEnvWithLambdaParams(
                                  env,
                                  term1);
                                Boolean isEnum = hydra.Predicates.isEnumRowType(rt);
                                hydra.util.Lazy<Boolean> isFull = new hydra.util.Lazy<>(() -> hydra.ext.python.Coder.isCasesFull(
                                  rt,
                                  cases_.get()));
                                hydra.util.Lazy<hydra.ext.python.syntax.Parameters> params = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Parameters.ParamNoDefault(new hydra.ext.python.syntax.ParamNoDefaultParameters(allParams.get(), (java.util.List<hydra.ext.python.syntax.ParamWithDefault>) (java.util.Collections.<hydra.ext.python.syntax.ParamWithDefault>emptyList()), (hydra.util.Maybe<hydra.ext.python.syntax.StarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.StarEtc>nothing()))));
                                return hydra.lib.eithers.Bind.apply(
                                  hydra.lib.eithers.MapList.apply(
                                    (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.CaseBlock>>) (v1 -> hydra.ext.python.Coder.encodeCaseBlock(
                                      cx,
                                      envWithParams,
                                      tname.get(),
                                      rt,
                                      isEnum,
                                      (java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>>) (e -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (t -> hydra.ext.python.Coder.encodeTermMultiline(
                                        cx,
                                        e,
                                        t))),
                                      v1)),
                                    cases_.get()),
                                  (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (pyCases -> hydra.lib.eithers.Bind.apply(
                                    hydra.ext.python.Coder.encodeDefaultCaseBlock(
                                      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (t -> hydra.ext.python.Coder.encodeTermInline(
                                        cx,
                                        envWithParams,
                                        false,
                                        t)),
                                      isFull.get(),
                                      dflt.get(),
                                      tname.get()),
                                    (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (pyDflt -> {
                                      hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.CaseBlock>> allCases = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                                        pyCases,
                                        pyDflt));
                                      hydra.ext.python.syntax.SubjectExpression subj = new hydra.ext.python.syntax.SubjectExpression.Simple(new hydra.ext.python.syntax.NamedExpression.Simple(hydra.ext.python.Utils.pyNameToPyExpression(matchArgName)));
                                      hydra.ext.python.syntax.Statement matchStmt = new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Match(new hydra.ext.python.syntax.MatchStatement(subj, allCases.get())));
                                      hydra.util.Lazy<hydra.ext.python.syntax.Block> body = new hydra.util.Lazy<>(() -> hydra.ext.python.Utils.indentedBlock(
                                        (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
                                        java.util.Arrays.asList(java.util.Arrays.asList(matchStmt))));
                                      hydra.util.Lazy<hydra.ext.python.syntax.FunctionDefRaw> funcDefRaw = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.FunctionDefRaw(false, fname, (java.util.List<hydra.ext.python.syntax.TypeParameter>) (java.util.Collections.<hydra.ext.python.syntax.TypeParameter>emptyList()), hydra.util.Maybe.just(params.get()), (hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing()), (hydra.util.Maybe<hydra.ext.python.syntax.FuncTypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.FuncTypeComment>nothing()), body.get()));
                                      return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>right(new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Function(new hydra.ext.python.syntax.FunctionDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), funcDefRaw.get()))));
                                    }))));
                              }));
                          })).get();
                        })).get();
                      })).get();
                    })).get();
                  })).get())),
                mcsa);
            })).get();
          })).get();
        })).get();
      })).get(),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (ts -> hydra.lib.eithers.Bind.apply(
        hydra.Annotations.getTermDescription(
          cx,
          hydra.ext.python.Coder.pythonEnvironmentGetGraph(env),
          term1),
        (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (comment -> {
          hydra.util.Lazy<hydra.util.Maybe<String>> normComment = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
            hydra.CoderUtils::normalizeComment,
            comment));
          return hydra.ext.python.Coder.encodeTermAssignment(
            cx,
            env,
            name1,
            term1,
            ts,
            normComment.get());
        }))),
      mts);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.NamedExpression> encodeBindingAsAssignment(hydra.context.Context cx, Boolean allowThunking, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Binding binding) {
    hydra.util.Maybe<hydra.core.TypeScheme> mts = (binding).type;
    hydra.core.Name name = (binding).name;
    hydra.ext.python.syntax.Name pyName = hydra.ext.python.Names.encodeName(
      false,
      new hydra.util.CaseConvention.LowerSnake(),
      env,
      name);
    hydra.core.Term term = (binding).term;
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.python.Coder.encodeTermInline(
        cx,
        env,
        false,
        term),
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.NamedExpression>>) (pbody -> {
        hydra.graph.Graph tc = (env).graph;
        Boolean isComplexVar = hydra.CoderUtils.isComplexVariable(
          tc,
          name);
        Boolean isTrivial = hydra.CoderUtils.isTrivialTerm(term);
        Boolean termIsComplex = hydra.CoderUtils.isComplexTerm(
          tc,
          term);
        hydra.util.Lazy<Boolean> needsThunk = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          isTrivial,
          () -> false,
          () -> hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.lib.logic.And.apply(
              allowThunking,
              hydra.lib.logic.Or.apply(
                isComplexVar,
                termIsComplex)),
            (java.util.function.Function<hydra.core.TypeScheme, Boolean>) (ts -> hydra.lib.logic.And.apply(
              allowThunking,
              hydra.lib.logic.And.apply(
                hydra.lib.equality.Equal.apply(
                  hydra.Arity.typeSchemeArity(ts),
                  0),
                hydra.lib.logic.Or.apply(
                  isComplexVar,
                  termIsComplex)))),
            mts)));
        hydra.util.Lazy<hydra.ext.python.syntax.Expression> pterm = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          needsThunk.get(),
          () -> hydra.ext.python.Coder.makeThunk(pbody),
          () -> pbody));
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.NamedExpression>right(new hydra.ext.python.syntax.NamedExpression.Assignment(new hydra.ext.python.syntax.AssignmentExpression(pyName, pterm.get())));
      }));
  }

  static <T0, T1, T2, T3> hydra.util.Either<T2, java.util.List<T3>> encodeBindingsAsDefs(T0 env, java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Either<T2, T3>>> encodeBinding, java.util.List<T1> bindings) {
    return hydra.lib.eithers.MapList.apply(
      (java.util.function.Function<T1, hydra.util.Either<T2, T3>>) (v1 -> (encodeBinding).apply(env).apply(v1)),
      bindings);
  }

  static <T0, T1> hydra.util.Either<T1, hydra.ext.python.syntax.CaseBlock> encodeCaseBlock(T0 cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name tname, java.util.List<hydra.core.FieldType> rowType, Boolean isEnum, java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, java.util.List<hydra.ext.python.syntax.Statement>>>> encodeBody, hydra.core.Field field) {
    hydra.core.Name fname = (field).name;
    Boolean isUnitVariant = hydra.ext.python.Coder.isVariantUnitType(
      rowType,
      fname);
    hydra.core.Term fterm = (field).term;
    hydra.core.Term stripped = hydra.Strip.deannotateAndDetypeTerm(fterm);
    hydra.util.Lazy<hydra.core.Lambda> effectiveLambda = new hydra.util.Lazy<>(() -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Lambda otherwise(hydra.core.Term instance) {
        hydra.core.Name syntheticVar = new hydra.core.Name("_matchValue");
        return new hydra.core.Lambda(syntheticVar, (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(stripped, new hydra.core.Term.Variable(syntheticVar))));
      }

      @Override
      public hydra.core.Lambda visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Lambda otherwise(hydra.core.Function instance) {
            hydra.core.Name syntheticVar2 = new hydra.core.Name("_matchValue");
            return new hydra.core.Lambda(syntheticVar2, (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(stripped, new hydra.core.Term.Variable(syntheticVar2))));
          }

          @Override
          public hydra.core.Lambda visit(hydra.core.Function.Lambda lam) {
            return (lam).value;
          }
        });
      }
    }));
    hydra.core.Term rawBody = effectiveLambda.get().body;
    hydra.core.Name v = effectiveLambda.get().parameter;
    hydra.util.Lazy<hydra.core.Term> effectiveBody = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      isUnitVariant,
      () -> hydra.ext.python.Coder.eliminateUnitVar(
        v,
        rawBody),
      () -> rawBody));
    hydra.ext.python.environment.PythonEnvironment env2 = hydra.ext.python.Coder.pythonEnvironmentSetGraph(
      hydra.Scoping.extendGraphForLambda(
        hydra.ext.python.Coder.pythonEnvironmentGetGraph(env),
        effectiveLambda.get()),
      env);
    hydra.ext.python.syntax.Name pyVariantName = hydra.ext.python.Coder.deconflictVariantName(
      true,
      env2,
      tname,
      fname,
      (env2).graph);
    Boolean shouldCapture = hydra.lib.logic.Not.apply(hydra.lib.logic.Or.apply(
      isUnitVariant,
      hydra.lib.logic.Or.apply(
        hydra.Variables.isFreeVariableInTerm(
          v,
          rawBody),
        hydra.Predicates.isUnitTerm(rawBody))));
    hydra.util.Lazy<hydra.ext.python.syntax.ClosedPattern> pattern = new hydra.util.Lazy<>(() -> hydra.ext.python.Coder.variantClosedPattern(
      env2,
      tname,
      fname,
      pyVariantName,
      rowType,
      isEnum,
      v,
      shouldCapture));
    return hydra.lib.eithers.Bind.apply(
      (encodeBody).apply(env2).apply(effectiveBody.get()),
      (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.util.Either<T1, hydra.ext.python.syntax.CaseBlock>>) (stmts -> {
        hydra.util.Lazy<hydra.ext.python.syntax.Block> pyBody = new hydra.util.Lazy<>(() -> hydra.ext.python.Utils.indentedBlock(
          (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
          java.util.Arrays.asList(stmts)));
        return hydra.util.Either.<T1, hydra.ext.python.syntax.CaseBlock>right(new hydra.ext.python.syntax.CaseBlock(hydra.ext.python.Utils.pyClosedPatternToPyPatterns(pattern.get()), (hydra.util.Maybe<hydra.ext.python.syntax.Guard>) (hydra.util.Maybe.<hydra.ext.python.syntax.Guard>nothing()), pyBody.get()));
      }));
  }

  static <T0, T1> hydra.util.Either<T1, java.util.List<hydra.ext.python.syntax.CaseBlock>> encodeDefaultCaseBlock(java.util.function.Function<T0, hydra.util.Either<T1, hydra.ext.python.syntax.Expression>> encodeTerm, Boolean isFull, hydra.util.Maybe<T0> mdflt, hydra.core.Name tname) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Either.<T1, hydra.ext.python.syntax.Statement>right(hydra.lib.logic.IfElse.lazy(
          isFull,
          () -> hydra.ext.python.Utils.raiseAssertionError("Unreachable: all variants handled"),
          () -> hydra.ext.python.Utils.raiseTypeError(hydra.lib.strings.Cat2.apply(
            "Unsupported ",
            hydra.Names.localNameOf(tname))))),
        (java.util.function.Function<T0, hydra.util.Either<T1, hydra.ext.python.syntax.Statement>>) (d -> hydra.lib.eithers.Bind.apply(
          (encodeTerm).apply(d),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T1, hydra.ext.python.syntax.Statement>>) (pyexpr -> hydra.util.Either.<T1, hydra.ext.python.syntax.Statement>right(hydra.ext.python.Utils.returnSingle(pyexpr))))),
        mdflt),
      (java.util.function.Function<hydra.ext.python.syntax.Statement, hydra.util.Either<T1, java.util.List<hydra.ext.python.syntax.CaseBlock>>>) (stmt -> {
        hydra.util.Lazy<hydra.ext.python.syntax.Block> body = new hydra.util.Lazy<>(() -> hydra.ext.python.Utils.indentedBlock(
          (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
          java.util.Arrays.asList(java.util.Arrays.asList(stmt))));
        hydra.ext.python.syntax.Patterns patterns = hydra.ext.python.Utils.pyClosedPatternToPyPatterns(new hydra.ext.python.syntax.ClosedPattern.Wildcard());
        return hydra.util.Either.<T1, java.util.List<hydra.ext.python.syntax.CaseBlock>>right(java.util.Arrays.asList(new hydra.ext.python.syntax.CaseBlock(patterns, (hydra.util.Maybe<hydra.ext.python.syntax.Guard>) (hydra.util.Maybe.<hydra.ext.python.syntax.Guard>nothing()), body.get())));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>> encodeDefinition(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.module.Definition def_) {
    return (def_).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>> visit(hydra.module.Definition.Term td) {
        hydra.core.Name name = (td).value.name;
        hydra.core.Term term = (td).value.term;
        hydra.util.Lazy<hydra.core.TypeScheme> typ = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
          () -> new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Unit")), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (x -> x),
          (td).value.type));
        return hydra.lib.eithers.Bind.apply(
          hydra.Annotations.getTermDescription(
            cx,
            hydra.ext.python.Coder.pythonEnvironmentGetGraph(env),
            term),
          (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>>>) (comment -> {
            hydra.util.Lazy<hydra.util.Maybe<String>> normComment = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
              hydra.CoderUtils::normalizeComment,
              comment));
            return hydra.lib.eithers.Bind.apply(
              hydra.ext.python.Coder.encodeTermAssignment(
                cx,
                env,
                name,
                term,
                typ.get(),
                normComment.get()),
              (java.util.function.Function<hydra.ext.python.syntax.Statement, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>>>) (stmt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>>right(java.util.Arrays.asList(java.util.Arrays.asList(stmt)))));
          }));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>> visit(hydra.module.Definition.Type td) {
        hydra.core.Name name = (td).value.name;
        hydra.core.Type typ = (td).value.type;
        return hydra.lib.eithers.Bind.apply(
          hydra.Annotations.getTypeDescription(
            cx,
            hydra.ext.python.Coder.pythonEnvironmentGetGraph(env),
            typ),
          (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>>>) (comment -> {
            hydra.util.Lazy<hydra.util.Maybe<String>> normComment = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
              hydra.CoderUtils::normalizeComment,
              comment));
            return hydra.ext.python.Coder.encodeTypeAssignment(
              cx,
              env,
              name,
              typ,
              normComment.get());
          }));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> encodeEnumValueAssignment(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.FieldType fieldType) {
    hydra.core.Name fname = (fieldType).name;
    hydra.core.Type ftype = (fieldType).type;
    return hydra.lib.eithers.Bind.apply(
      hydra.Annotations.getTypeDescription(
        cx,
        hydra.ext.python.Coder.pythonEnvironmentGetGraph(env),
        ftype),
      (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (mcomment -> {
        hydra.ext.python.syntax.Name pyName = hydra.ext.python.Names.encodeEnumValue(
          env,
          fname);
        String fnameStr = (fname).value;
        hydra.ext.python.syntax.Expression pyValue = hydra.ext.python.Utils.functionCall(
          hydra.ext.python.Utils.pyNameToPyPrimary(hydra.ext.python.Names.encodeName(
            true,
            new hydra.util.CaseConvention.Pascal(),
            env,
            new hydra.core.Name("hydra.core.Name"))),
          java.util.Arrays.asList(hydra.ext.python.Utils.doubleQuotedString(fnameStr)));
        hydra.ext.python.syntax.Statement assignStmt = hydra.ext.python.Utils.assignmentStatement(
          pyName,
          pyValue);
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>right(hydra.lib.maybes.Maybe.applyLazy(
          () -> java.util.Arrays.asList(assignStmt),
          (java.util.function.Function<String, java.util.List<hydra.ext.python.syntax.Statement>>) (c -> java.util.Arrays.asList(
            assignStmt,
            hydra.ext.python.Utils.pyExpressionToPyStatement(hydra.ext.python.Utils.tripleQuotedString(c)))),
          mcomment));
      }));
  }

  static <T0, T1, T2> hydra.util.Either<T1, hydra.util.Pair<hydra.ext.python.syntax.Name, T2>> encodeField(T0 cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Field field, java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, T2>> encodeTerm) {
    hydra.core.Name fname = (field).name;
    hydra.core.Term fterm = (field).term;
    return hydra.lib.eithers.Bind.apply(
      (encodeTerm).apply(fterm),
      (java.util.function.Function<T2, hydra.util.Either<T1, hydra.util.Pair<hydra.ext.python.syntax.Name, T2>>>) (pterm -> hydra.util.Either.<T1, hydra.util.Pair<hydra.ext.python.syntax.Name, T2>>right((hydra.util.Pair<hydra.ext.python.syntax.Name, T2>) ((hydra.util.Pair<hydra.ext.python.syntax.Name, T2>) (new hydra.util.Pair<hydra.ext.python.syntax.Name, T2>(hydra.ext.python.Names.encodeFieldName(
        env,
        fname), pterm))))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement> encodeFieldType(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.FieldType fieldType) {
    hydra.core.Name fname = (fieldType).name;
    hydra.core.Type ftype = (fieldType).type;
    return hydra.lib.eithers.Bind.apply(
      hydra.Annotations.getTypeDescription(
        cx,
        hydra.ext.python.Coder.pythonEnvironmentGetGraph(env),
        ftype),
      (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (comment -> {
        hydra.ext.python.syntax.SingleTarget pyName = new hydra.ext.python.syntax.SingleTarget.Name(hydra.ext.python.Names.encodeFieldName(
          env,
          fname));
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.python.Coder.encodeType(
            env,
            ftype),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (pyType -> {
            hydra.ext.python.syntax.Expression annotatedPyType = hydra.ext.python.Utils.annotatedExpression(
              comment,
              pyType);
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>right(hydra.ext.python.Utils.pyAssignmentToPyStatement(new hydra.ext.python.syntax.Assignment.Typed(new hydra.ext.python.syntax.TypedAssignment(pyName, annotatedPyType, (hydra.util.Maybe<hydra.ext.python.syntax.AnnotatedRhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.AnnotatedRhs>nothing())))));
          }));
      }));
  }

  static <T0> hydra.util.Either<T0, hydra.ext.python.syntax.Expression> encodeFloatValue(hydra.core.FloatValue fv) {
    return (fv).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.FloatValue.Bigfloat f) {
        return hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.functionCall(
          hydra.ext.python.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("Decimal")),
          java.util.Arrays.asList(hydra.ext.python.Utils.singleQuotedString(hydra.lib.literals.ShowBigfloat.apply((f).value)))));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.FloatValue.Float32 f) {
        return hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Number_(new hydra.ext.python.syntax.Number_.Float_(hydra.lib.literals.Float32ToBigfloat.apply((f).value)))));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.FloatValue.Float64 f) {
        return hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Number_(new hydra.ext.python.syntax.Number_.Float_(hydra.lib.literals.Float64ToBigfloat.apply((f).value)))));
      }
    });
  }

  static <T0> hydra.util.Either<T0, hydra.ext.python.syntax.Expression> encodeForallType(hydra.ext.python.environment.PythonEnvironment env, hydra.core.ForallType lt) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, java.util.function.Function<java.util.List<hydra.core.Name>, hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>>>> gatherParams = new java.util.concurrent.atomic.AtomicReference<>();
    gatherParams.set((java.util.function.Function<hydra.core.Type, java.util.function.Function<java.util.List<hydra.core.Name>, hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>>>) (t -> (java.util.function.Function<java.util.List<hydra.core.Name>, hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>>) (ps -> hydra.Strip.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Forall forallT) {
        return gatherParams.get().apply((forallT).value.body).apply(hydra.lib.lists.Cons.apply(
          (forallT).value.parameter,
          ps));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Annotated ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Application ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Function ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.List ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Literal ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Map ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Maybe ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Either ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Pair ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Record ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Set ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Union ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Unit ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Variable ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Void_ ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>> visit(hydra.core.Type.Wrap ignored) {
        return (hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>(t, hydra.lib.lists.Reverse.apply(ps))));
      }
    }))));
    hydra.util.Lazy<hydra.util.Pair<hydra.core.Type, java.util.List<hydra.core.Name>>> bodyAndParams = new hydra.util.Lazy<>(() -> gatherParams.get().apply(new hydra.core.Type.Forall(lt)).apply((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList())));
    hydra.util.Lazy<hydra.core.Type> body = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bodyAndParams.get()));
    hydra.util.Lazy<java.util.List<hydra.core.Name>> params = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bodyAndParams.get()));
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.python.Coder.<T0>encodeType(
        env,
        body.get()),
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (pyBody -> hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.primaryAndParams(
        hydra.ext.python.Utils.pyExpressionToPyPrimary(pyBody),
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Name, hydra.ext.python.syntax.Expression>) (n -> new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name((n).value)))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>emptyList()))))))))),
          params.get())))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> encodeFunction(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Function f) {
    return (f).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Function.Lambda lam) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.python.Coder.analyzePythonFunction(
            cx,
            env,
            new hydra.core.Term.Function(new hydra.core.Function.Lambda((lam).value))),
          (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (fs -> {
            hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, java.util.List<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
            hydra.util.Lazy<java.util.List<hydra.core.Name>> bindingNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Binding, hydra.core.Name>) (b -> (b).name),
              bindings.get()));
            hydra.util.Lazy<hydra.core.Term> innerBody = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
            hydra.util.Lazy<hydra.ext.python.environment.PythonEnvironment> innerEnv0 = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, hydra.ext.python.environment.PythonEnvironment>) (projected -> projected.environment)).apply(fs));
            hydra.util.Lazy<hydra.ext.python.environment.PythonEnvironment> innerEnv = new hydra.util.Lazy<>(() -> new hydra.ext.python.environment.PythonEnvironment(innerEnv0.get().namespaces, innerEnv0.get().boundTypeVariables, innerEnv0.get().graph, innerEnv0.get().nullaryBindings, innerEnv0.get().version, innerEnv0.get().skipCasts, hydra.lib.sets.Union.apply(
              hydra.lib.sets.FromList.apply(bindingNames.get()),
              innerEnv0.get().inlineVariables)));
            hydra.util.Lazy<java.util.List<hydra.core.Name>> params = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, java.util.List<hydra.core.Name>>) (projected -> projected.params)).apply(fs));
            return hydra.lib.eithers.Bind.apply(
              hydra.ext.python.Coder.encodeTermInline(
                cx,
                innerEnv.get(),
                false,
                innerBody.get()),
              (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (pbody -> {
                hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Name>> pparams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Name, hydra.ext.python.syntax.Name>) (v1 -> hydra.ext.python.Names.encodeName(
                    false,
                    new hydra.util.CaseConvention.LowerSnake(),
                    innerEnv.get(),
                    v1)),
                  params.get()));
                return hydra.lib.logic.IfElse.lazy(
                  hydra.lib.lists.Null.apply(bindings.get()),
                  () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Coder.makeUncurriedLambda(
                    pparams.get(),
                    pbody)),
                  () -> hydra.lib.eithers.Bind.apply(
                    hydra.lib.eithers.MapList.apply(
                      (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.NamedExpression>>) (v1 -> hydra.ext.python.Coder.encodeBindingAsAssignment(
                        cx,
                        false,
                        innerEnv.get(),
                        v1)),
                      bindings.get()),
                    (java.util.function.Function<java.util.List<hydra.ext.python.syntax.NamedExpression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (pbindingExprs -> {
                      hydra.util.Lazy<hydra.ext.python.syntax.Expression> indexValue = new hydra.util.Lazy<>(() -> hydra.ext.python.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Number_(new hydra.ext.python.syntax.Number_.Integer_(hydra.lib.literals.Int32ToBigint.apply(hydra.lib.lists.Length.apply(bindings.get()))))));
                      hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.StarNamedExpression>> pbindingStarExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                        (java.util.function.Function<hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.StarNamedExpression>) (ne -> new hydra.ext.python.syntax.StarNamedExpression.Simple(ne)),
                        pbindingExprs));
                      hydra.ext.python.syntax.StarNamedExpression pbodyStarExpr = hydra.ext.python.Utils.pyExpressionToPyStarNamedExpression(pbody);
                      hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.StarNamedExpression>> tupleElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                        pbindingStarExprs.get(),
                        java.util.Arrays.asList(pbodyStarExpr)));
                      hydra.ext.python.syntax.Expression tupleExpr = hydra.ext.python.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Tuple(new hydra.ext.python.syntax.Tuple(tupleElements.get())));
                      hydra.ext.python.syntax.Primary indexedExpr = hydra.ext.python.Utils.primaryWithExpressionSlices(
                        hydra.ext.python.Utils.pyExpressionToPyPrimary(tupleExpr),
                        java.util.Arrays.asList(indexValue.get()));
                      return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Coder.makeUncurriedLambda(
                        pparams.get(),
                        hydra.ext.python.Utils.pyPrimaryToPyExpression(indexedExpr)));
                    })));
              }));
          }));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Function.Primitive name) {
        return hydra.ext.python.Coder.encodeVariable(
          cx,
          env,
          (name).value,
          (java.util.List<hydra.ext.python.syntax.Expression>) (java.util.Collections.<hydra.ext.python.syntax.Expression>emptyList()));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Function.Elimination e) {
        return (e).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Elimination.Record proj) {
            hydra.core.Name fname = (proj).value.field;
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Coder.makeCurriedLambda(
              java.util.Arrays.asList(new hydra.ext.python.syntax.Name("v1")),
              hydra.ext.python.Utils.projectFromExpression(
                new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("v1")))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>emptyList())))))))),
                hydra.ext.python.Names.encodeFieldName(
                  env,
                  fname))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Elimination.Wrap ignored) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Coder.makeCurriedLambda(
              java.util.Arrays.asList(new hydra.ext.python.syntax.Name("v1")),
              hydra.ext.python.Utils.projectFromExpression(
                new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("v1")))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>emptyList())))))))),
                new hydra.ext.python.syntax.Name("value"))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Elimination.Union ignored) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Coder.unsupportedExpression("case expressions as values are not yet supported"));
          }
        });
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement> encodeFunctionDefinition(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name name, java.util.List<hydra.core.Name> tparams, java.util.List<hydra.core.Name> args, hydra.core.Term body, java.util.List<hydra.core.Type> doms, hydra.util.Maybe<hydra.core.Type> mcod, hydra.util.Maybe<String> comment, java.util.List<hydra.ext.python.syntax.Statement> prefixes) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.ParamNoDefault>>) (pair -> {
          hydra.util.Lazy<hydra.core.Name> argName = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
          hydra.util.Lazy<hydra.core.Type> typ = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
          return hydra.lib.eithers.Bind.apply(
            hydra.ext.python.Coder.encodeType(
              env,
              typ.get()),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.ParamNoDefault>>) (pyTyp -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.ParamNoDefault>right(new hydra.ext.python.syntax.ParamNoDefault(new hydra.ext.python.syntax.Param(hydra.ext.python.Names.encodeName(
              false,
              new hydra.util.CaseConvention.LowerSnake(),
              env,
              argName.get()), hydra.util.Maybe.just(new hydra.ext.python.syntax.Annotation(pyTyp))), (hydra.util.Maybe<hydra.ext.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.TypeComment>nothing())))));
        }),
        hydra.lib.lists.Zip.apply(
          args,
          doms)),
      (java.util.function.Function<java.util.List<hydra.ext.python.syntax.ParamNoDefault>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (pyArgs -> {
        hydra.util.Lazy<Boolean> isTCO = new hydra.util.Lazy<>(() -> hydra.lib.logic.And.apply(
          hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(args)),
          hydra.CoderUtils.isSelfTailRecursive(
            name,
            body)));
        hydra.util.Lazy<hydra.ext.python.syntax.Parameters> pyParams = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Parameters.ParamNoDefault(new hydra.ext.python.syntax.ParamNoDefaultParameters(pyArgs, (java.util.List<hydra.ext.python.syntax.ParamWithDefault>) (java.util.Collections.<hydra.ext.python.syntax.ParamWithDefault>emptyList()), (hydra.util.Maybe<hydra.ext.python.syntax.StarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.StarEtc>nothing()))));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.logic.IfElse.lazy(
            isTCO.get(),
            () -> hydra.lib.eithers.Bind.apply(
              hydra.ext.python.Coder.encodeTermMultilineTCO(
                cx,
                env,
                name,
                args,
                body),
              (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Block>>) (tcoStmts -> {
                hydra.ext.python.syntax.NamedExpression trueExpr = new hydra.ext.python.syntax.NamedExpression.Simple(hydra.ext.python.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.True()));
                hydra.util.Lazy<hydra.ext.python.syntax.Block> whileBody = new hydra.util.Lazy<>(() -> hydra.ext.python.Utils.indentedBlock(
                  (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
                  java.util.Arrays.asList(hydra.lib.lists.Concat2.apply(
                    prefixes,
                    tcoStmts))));
                hydra.util.Lazy<hydra.ext.python.syntax.Statement> whileStmt = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.While(new hydra.ext.python.syntax.WhileStatement(trueExpr, whileBody.get(), (hydra.util.Maybe<hydra.ext.python.syntax.Block>) (hydra.util.Maybe.<hydra.ext.python.syntax.Block>nothing())))));
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Block>right(hydra.ext.python.Utils.indentedBlock(
                  comment,
                  java.util.Arrays.asList(java.util.Arrays.asList(whileStmt.get()))));
              })),
            () -> hydra.lib.eithers.Bind.apply(
              hydra.ext.python.Coder.encodeTermMultiline(
                cx,
                env,
                body),
              (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Block>>) (stmts -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Block>right(hydra.ext.python.Utils.indentedBlock(
                comment,
                java.util.Arrays.asList(hydra.lib.lists.Concat2.apply(
                  prefixes,
                  stmts))))))),
          (java.util.function.Function<hydra.ext.python.syntax.Block, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (block -> hydra.lib.eithers.Bind.apply(
            hydra.lib.maybes.Maybe.applyLazy(
              () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.python.syntax.Expression>>right((hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing())),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.python.syntax.Expression>>>) (cod -> hydra.lib.eithers.Bind.apply(
                hydra.ext.python.Coder.encodeType(
                  env,
                  cod),
                (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.python.syntax.Expression>>>) (pytyp -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.python.syntax.Expression>>right(hydra.util.Maybe.just(pytyp))))),
              mcod),
            (java.util.function.Function<hydra.util.Maybe<hydra.ext.python.syntax.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (mreturnType -> {
              hydra.util.Lazy<Boolean> isThunk = new hydra.util.Lazy<>(() -> hydra.lib.lists.Null.apply(args));
              hydra.util.Lazy<hydra.util.Maybe<hydra.ext.python.syntax.Decorators>> mDecorators = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                isThunk.get(),
                () -> hydra.util.Maybe.just(new hydra.ext.python.syntax.Decorators(java.util.Arrays.asList(hydra.ext.python.Coder.lruCacheDecorator()))),
                () -> (hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing())));
              hydra.ext.python.syntax.Name pyName = hydra.ext.python.Names.encodeName(
                false,
                new hydra.util.CaseConvention.LowerSnake(),
                env,
                name);
              hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.TypeParameter>> pyTparams = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                hydra.ext.python.Coder.useInlineTypeParams(),
                () -> hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Name, hydra.ext.python.syntax.TypeParameter>) (arg_ -> hydra.ext.python.Utils.pyNameToPyTypeParameter(hydra.ext.python.Names.encodeTypeVariable(arg_))),
                  tparams),
                () -> (java.util.List<hydra.ext.python.syntax.TypeParameter>) (java.util.Collections.<hydra.ext.python.syntax.TypeParameter>emptyList())));
              return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>right(new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Function(new hydra.ext.python.syntax.FunctionDefinition(mDecorators.get(), new hydra.ext.python.syntax.FunctionDefRaw(false, pyName, pyTparams.get(), hydra.util.Maybe.just(pyParams.get()), mreturnType, (hydra.util.Maybe<hydra.ext.python.syntax.FuncTypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.FuncTypeComment>nothing()), block)))));
            }))));
      }));
  }

  static <T0> hydra.util.Either<T0, hydra.ext.python.syntax.Expression> encodeFunctionType(hydra.ext.python.environment.PythonEnvironment env, hydra.core.FunctionType ft) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.FunctionType, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>>>> gatherParams = new java.util.concurrent.atomic.AtomicReference<>();
    gatherParams.set((java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.FunctionType, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>>>) (rdoms -> (java.util.function.Function<hydra.core.FunctionType, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>>) (ftype -> {
      hydra.core.Type dom = (ftype).domain;
      hydra.core.Type innerCod = (ftype).codomain;
      return hydra.Strip.deannotateType(innerCod).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Function ft2) {
          return gatherParams.get().apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)).apply((ft2).value);
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Annotated ignored) {
          return (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Application ignored) {
          return (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Forall ignored) {
          return (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.List ignored) {
          return (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Literal ignored) {
          return (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Map ignored) {
          return (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Maybe ignored) {
          return (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Either ignored) {
          return (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Pair ignored) {
          return (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Record ignored) {
          return (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Set ignored) {
          return (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Union ignored) {
          return (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Unit ignored) {
          return (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Variable ignored) {
          return (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Void_ ignored) {
          return (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Wrap ignored) {
          return (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
            dom,
            rdoms)), innerCod)));
        }
      });
    })));
    hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.core.Type>> domsAndCod = new hydra.util.Lazy<>(() -> gatherParams.get().apply((java.util.List<hydra.core.Type>) (java.util.Collections.<hydra.core.Type>emptyList())).apply(ft));
    hydra.util.Lazy<hydra.core.Type> cod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(domsAndCod.get()));
    hydra.util.Lazy<java.util.List<hydra.core.Type>> doms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(domsAndCod.get()));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.Type, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (v1 -> hydra.ext.python.Coder.<T0>encodeType(
          env,
          v1)),
        doms.get()),
      (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Expression>, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (pydoms -> hydra.lib.eithers.Bind.apply(
        hydra.ext.python.Coder.<T0>encodeType(
          env,
          cod.get()),
        (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (pycod -> hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.pyPrimaryToPyExpression(hydra.ext.python.Utils.primaryWithSlices(
          new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("Callable"))),
          hydra.ext.python.Utils.pyPrimaryToPySlice(new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.List(hydra.ext.python.Utils.pyList(pydoms)))),
          java.util.Arrays.asList(new hydra.ext.python.syntax.SliceOrStarredExpression.Slice(hydra.ext.python.Utils.pyExpressionToPySlice(pycod))))))))));
  }

  static <T0> hydra.util.Either<T0, hydra.ext.python.syntax.Expression> encodeIntegerValue(hydra.core.IntegerValue iv) {
    return (iv).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Bigint i) {
        return hydra.ext.python.Coder.<T0>encodeIntegerValue_toPyInt(
          hydra.ext.python.Utils::pyAtomToPyExpression,
          (i).value);
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Int8 i) {
        return hydra.ext.python.Coder.<T0>encodeIntegerValue_toPyInt(
          hydra.ext.python.Utils::pyAtomToPyExpression,
          hydra.lib.literals.Int8ToBigint.apply((i).value));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Int16 i) {
        return hydra.ext.python.Coder.<T0>encodeIntegerValue_toPyInt(
          hydra.ext.python.Utils::pyAtomToPyExpression,
          hydra.lib.literals.Int16ToBigint.apply((i).value));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Int32 i) {
        return hydra.ext.python.Coder.<T0>encodeIntegerValue_toPyInt(
          hydra.ext.python.Utils::pyAtomToPyExpression,
          hydra.lib.literals.Int32ToBigint.apply((i).value));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Int64 i) {
        return hydra.ext.python.Coder.<T0>encodeIntegerValue_toPyInt(
          hydra.ext.python.Utils::pyAtomToPyExpression,
          hydra.lib.literals.Int64ToBigint.apply((i).value));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Uint8 i) {
        return hydra.ext.python.Coder.<T0>encodeIntegerValue_toPyInt(
          hydra.ext.python.Utils::pyAtomToPyExpression,
          hydra.lib.literals.Uint8ToBigint.apply((i).value));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Uint16 i) {
        return hydra.ext.python.Coder.<T0>encodeIntegerValue_toPyInt(
          hydra.ext.python.Utils::pyAtomToPyExpression,
          hydra.lib.literals.Uint16ToBigint.apply((i).value));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Uint32 i) {
        return hydra.ext.python.Coder.<T0>encodeIntegerValue_toPyInt(
          hydra.ext.python.Utils::pyAtomToPyExpression,
          hydra.lib.literals.Uint32ToBigint.apply((i).value));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.IntegerValue.Uint64 i) {
        return hydra.ext.python.Coder.<T0>encodeIntegerValue_toPyInt(
          hydra.ext.python.Utils::pyAtomToPyExpression,
          hydra.lib.literals.Uint64ToBigint.apply((i).value));
      }
    });
  }

  static <T1> hydra.util.Either<T1, hydra.ext.python.syntax.Expression> encodeIntegerValue_toPyInt(java.util.function.Function<hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.Expression> hydra_ext_python_utils_pyAtomToPyExpression, java.math.BigInteger n) {
    return hydra.util.Either.<T1, hydra.ext.python.syntax.Expression>right((hydra_ext_python_utils_pyAtomToPyExpression).apply(new hydra.ext.python.syntax.Atom.Number_(new hydra.ext.python.syntax.Number_.Integer_(n))));
  }

  static <T0> hydra.util.Either<T0, hydra.ext.python.syntax.Expression> encodeLiteral(hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Literal.Binary bs) {
        java.util.List<Integer> byteValues = hydra.lib.literals.BinaryToBytes.apply((bs).value);
        return hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.functionCall(
          new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("bytes"))),
          java.util.Arrays.asList(hydra.ext.python.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.List(hydra.ext.python.Utils.pyList(hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, hydra.ext.python.syntax.Expression>) (byteVal -> hydra.ext.python.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Number_(new hydra.ext.python.syntax.Number_.Integer_(hydra.lib.literals.Int32ToBigint.apply(byteVal))))),
            byteValues)))))));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Literal.Boolean_ b) {
        return hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.pyAtomToPyExpression(hydra.lib.logic.IfElse.lazy(
          (b).value,
          () -> new hydra.ext.python.syntax.Atom.True(),
          () -> new hydra.ext.python.syntax.Atom.False())));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Literal.Float_ f) {
        return hydra.ext.python.Coder.<T0>encodeFloatValue((f).value);
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Literal.Integer_ i) {
        return hydra.ext.python.Coder.<T0>encodeIntegerValue((i).value);
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Literal.String_ s) {
        return hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.stringToPyExpression(
          new hydra.ext.python.syntax.QuoteStyle.Double_(),
          (s).value));
      }
    });
  }

  static <T0> hydra.util.Either<T0, hydra.ext.python.syntax.Expression> encodeLiteralType(hydra.core.LiteralType lt) {
    String findName = (lt).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public String visit(hydra.core.LiteralType.Binary ignored) {
        return "bytes";
      }

      @Override
      public String visit(hydra.core.LiteralType.Boolean_ ignored) {
        return "bool";
      }

      @Override
      public String visit(hydra.core.LiteralType.Float_ ft) {
        return (ft).value.accept(new hydra.core.FloatType.PartialVisitor<>() {
          @Override
          public String visit(hydra.core.FloatType.Bigfloat ignored) {
            return "Decimal";
          }

          @Override
          public String visit(hydra.core.FloatType.Float32 ignored) {
            return "float";
          }

          @Override
          public String visit(hydra.core.FloatType.Float64 ignored) {
            return "float";
          }
        });
      }

      @Override
      public String visit(hydra.core.LiteralType.Integer_ ignored) {
        return "int";
      }

      @Override
      public String visit(hydra.core.LiteralType.String_ ignored) {
        return "str";
      }
    });
    return hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name(findName)))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>emptyList())))))))));
  }

  static java.util.List<hydra.ext.python.syntax.Statement> encodeNameConstants(hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name name, java.util.List<hydra.core.FieldType> fields) {
    hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.ext.python.syntax.Name, hydra.core.Name>>> fieldPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<hydra.ext.python.syntax.Name, hydra.core.Name>>) (field -> (hydra.util.Pair<hydra.ext.python.syntax.Name, hydra.core.Name>) ((hydra.util.Pair<hydra.ext.python.syntax.Name, hydra.core.Name>) (new hydra.util.Pair<hydra.ext.python.syntax.Name, hydra.core.Name>(hydra.ext.python.Names.encodeConstantForFieldName(
        env,
        name,
        (field).name), (field).name)))),
      fields));
    hydra.util.Lazy<hydra.util.Pair<hydra.ext.python.syntax.Name, hydra.core.Name>> namePair = new hydra.util.Lazy<>(() -> (hydra.util.Pair<hydra.ext.python.syntax.Name, hydra.core.Name>) ((hydra.util.Pair<hydra.ext.python.syntax.Name, hydra.core.Name>) (new hydra.util.Pair<hydra.ext.python.syntax.Name, hydra.core.Name>(hydra.ext.python.Names.encodeConstantForTypeName(
      env,
      name), name))));
    java.util.function.Function<hydra.util.Pair<hydra.ext.python.syntax.Name, hydra.core.Name>, hydra.ext.python.syntax.Statement> toStmt = (java.util.function.Function<hydra.util.Pair<hydra.ext.python.syntax.Name, hydra.core.Name>, hydra.ext.python.syntax.Statement>) (pair -> hydra.ext.python.Utils.assignmentStatement(
      hydra.lib.pairs.First.apply(pair),
      hydra.ext.python.Utils.functionCall(
        hydra.ext.python.Utils.pyNameToPyPrimary(hydra.ext.python.Names.encodeName(
          true,
          new hydra.util.CaseConvention.Pascal(),
          env,
          new hydra.core.Name("hydra.core.Name"))),
        java.util.Arrays.asList(hydra.ext.python.Utils.doubleQuotedString(hydra.lib.pairs.Second.apply(pair).value)))));
    return hydra.lib.lists.Map.apply(
      toStmt,
      hydra.lib.lists.Cons.apply(
        namePair.get(),
        fieldPairs.get()));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Module> encodePythonModule(hydra.context.Context cx, hydra.graph.Graph g, hydra.module.Module mod, java.util.List<hydra.module.Definition> defs0) {
    java.util.List<hydra.module.Definition> defs = hydra.CoderUtils.reorderDefs(defs0);
    hydra.ext.python.environment.PythonModuleMetadata meta0 = hydra.ext.python.Coder.gatherMetadata(
      (mod).namespace,
      defs);
    hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces0 = (meta0).namespaces;
    hydra.ext.python.environment.PythonEnvironment env0 = hydra.ext.python.Coder.initialEnvironment(
      namespaces0,
      g);
    Boolean isTypeMod = hydra.ext.python.Coder.isTypeModuleCheck(defs0);
    return hydra.ext.python.Coder.withDefinitions(
      env0,
      defs,
      (java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Module>>) (env -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.util.List<java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>>, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>>) (xs -> hydra.lib.lists.Concat.apply(xs)),
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.module.Definition, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>>>) (d -> hydra.ext.python.Coder.encodeDefinition(
              cx,
              env,
              d)),
            defs)),
        (java.util.function.Function<java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Module>>) (defStmts -> {
          hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Statement>> commentStmts = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
            () -> (java.util.List<hydra.ext.python.syntax.Statement>) (java.util.Collections.<hydra.ext.python.syntax.Statement>emptyList()),
            (java.util.function.Function<String, java.util.List<hydra.ext.python.syntax.Statement>>) (c -> java.util.Arrays.asList(hydra.ext.python.Utils.commentStatement(c))),
            hydra.lib.maybes.Map.apply(
              hydra.CoderUtils::normalizeComment,
              (mod).description)));
          hydra.util.Lazy<hydra.ext.python.environment.PythonModuleMetadata> meta2 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.logic.And.apply(
              hydra.lib.logic.Not.apply(isTypeMod),
              hydra.ext.python.Coder.useInlineTypeParams()),
            () -> hydra.ext.python.Coder.setMetaUsesTypeVar(
              meta0,
              false),
            () -> meta0));
          hydra.util.Lazy<hydra.ext.python.environment.PythonModuleMetadata> meta = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.logic.And.apply(
              isTypeMod,
              hydra.lib.equality.Equal.apply(
                hydra.ext.python.Coder.targetPythonVersion(),
                new hydra.ext.python.environment.PythonVersion.Python310())),
            () -> hydra.ext.python.Coder.setMetaUsesTypeAlias(
              meta2.get(),
              true),
            () -> meta2.get()));
          hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces = (meta0).namespaces;
          java.util.List<hydra.ext.python.syntax.Statement> importStmts = hydra.ext.python.Coder.moduleImports(
            namespaces,
            meta.get());
          hydra.util.Lazy<java.util.Set<hydra.core.Name>> tvars = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.logic.Or.apply(
              isTypeMod,
              hydra.lib.logic.Not.apply(hydra.ext.python.Coder.useInlineTypeParams())),
            () -> meta.get().typeVariables,
            () -> (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())));
          hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Statement>> tvarStmts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Name, hydra.ext.python.syntax.Statement>) (tv -> hydra.ext.python.Coder.tvarStatement(hydra.ext.python.Names.encodeTypeVariable(tv))),
            hydra.lib.sets.ToList.apply(tvars.get())));
          hydra.util.Lazy<java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>> body = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
            (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, Boolean>) (group -> hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(group))),
            hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              java.util.Arrays.asList(
                commentStmts.get(),
                importStmts,
                tvarStmts.get()),
              defStmts))));
          return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Module>right(new hydra.ext.python.syntax.Module(body.get()));
        }))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement> encodeRecordType(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name name, java.util.List<hydra.core.FieldType> rowType, hydra.util.Maybe<String> comment) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (v1 -> hydra.ext.python.Coder.encodeFieldType(
          cx,
          env,
          v1)),
        rowType),
      (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (pyFields -> {
        hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> boundVars = (env).boundTypeVariables;
        hydra.util.Lazy<java.util.List<hydra.core.Name>> tparamList = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(boundVars));
        hydra.util.Maybe<hydra.ext.python.syntax.Expression> mGenericArg = hydra.ext.python.Coder.genericArg(tparamList.get());
        hydra.util.Lazy<hydra.util.Maybe<hydra.ext.python.syntax.Args>> args = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
          () -> (hydra.util.Maybe<hydra.ext.python.syntax.Args>) (hydra.util.Maybe.<hydra.ext.python.syntax.Args>nothing()),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Maybe<hydra.ext.python.syntax.Args>>) (a -> hydra.util.Maybe.just(hydra.ext.python.Utils.pyExpressionsToPyArgs(java.util.Arrays.asList(a)))),
          mGenericArg));
        java.util.List<hydra.ext.python.syntax.Statement> constStmts = hydra.ext.python.Coder.encodeNameConstants(
          env,
          name,
          rowType);
        hydra.ext.python.syntax.Block body = hydra.ext.python.Utils.indentedBlock(
          comment,
          java.util.Arrays.asList(
            pyFields,
            constStmts));
        hydra.util.Maybe<hydra.ext.python.syntax.Decorators> decs = hydra.util.Maybe.just(new hydra.ext.python.syntax.Decorators(java.util.Arrays.asList(hydra.ext.python.Coder.dataclassDecorator())));
        hydra.ext.python.syntax.Name pyName = hydra.ext.python.Names.encodeName(
          false,
          new hydra.util.CaseConvention.Pascal(),
          env,
          name);
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>right(hydra.ext.python.Utils.pyClassDefinitionToPyStatement(new hydra.ext.python.syntax.ClassDefinition(decs, pyName, hydra.ext.python.Coder.<hydra.ext.python.syntax.TypeParameter>encodeRecordType_noTypeParams(), args.get(), body)));
      }));
  }

  static <T0> java.util.List<T0> encodeRecordType_noTypeParams() {
    return (java.util.List<T0>) (java.util.Collections.<T0>emptyList());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement> encodeTermAssignment(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name name, hydra.core.Term term, hydra.core.TypeScheme ts, hydra.util.Maybe<String> comment) {
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.python.Coder.analyzePythonFunction(
        cx,
        env,
        term),
      (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (fs -> {
        hydra.core.Binding binding = new hydra.core.Binding(name, term, hydra.util.Maybe.just(ts));
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, java.util.List<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
        hydra.util.Lazy<hydra.core.Term> body = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
        hydra.util.Lazy<java.util.List<hydra.core.Type>> doms = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, java.util.List<hydra.core.Type>>) (projected -> projected.domains)).apply(fs));
        hydra.util.Lazy<hydra.ext.python.environment.PythonEnvironment> env2 = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, hydra.ext.python.environment.PythonEnvironment>) (projected -> projected.environment)).apply(fs));
        hydra.graph.Graph tc = env2.get().graph;
        Boolean isComplex = hydra.CoderUtils.isComplexBinding(
          tc,
          binding);
        Boolean isTrivial = hydra.CoderUtils.isTrivialTerm(term);
        hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> mcod = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, hydra.util.Maybe<hydra.core.Type>>) (projected -> projected.codomain)).apply(fs));
        hydra.util.Lazy<java.util.List<hydra.core.Name>> params = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, java.util.List<hydra.core.Name>>) (projected -> projected.params)).apply(fs));
        hydra.util.Lazy<java.util.List<hydra.core.Name>> tparams = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, java.util.List<hydra.core.Name>>) (projected -> projected.typeParams)).apply(fs));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.And.apply(
            isComplex,
            hydra.lib.logic.Not.apply(isTrivial)),
          () -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (v1 -> hydra.ext.python.Coder.encodeBindingAs(
                cx,
                env2.get(),
                v1)),
              bindings.get()),
            (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (bindingStmts -> hydra.ext.python.Coder.encodeFunctionDefinition(
              cx,
              env2.get(),
              name,
              tparams.get(),
              params.get(),
              body.get(),
              doms.get(),
              mcod.get(),
              comment,
              bindingStmts))),
          () -> hydra.lib.eithers.Bind.apply(
            hydra.ext.python.Coder.encodeTermInline(
              cx,
              env2.get(),
              false,
              body.get()),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (bodyExpr -> {
              hydra.ext.python.syntax.Name pyName = hydra.ext.python.Names.encodeName(
                false,
                new hydra.util.CaseConvention.LowerSnake(),
                env2.get(),
                name);
              return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>right(hydra.ext.python.Utils.annotatedStatement(
                comment,
                hydra.ext.python.Utils.assignmentStatement(
                  pyName,
                  bodyExpr)));
            })));
      }));
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> encodeTermInline(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, Boolean noCast, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>> encode = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (t -> hydra.ext.python.Coder.encodeTermInline(
      cx,
      env,
      false,
      t));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, hydra.core.Term>> stripTypeApps = new java.util.concurrent.atomic.AtomicReference<>();
    stripTypeApps.set((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated ann) {
        return stripTypeApps.get().apply((ann).value.body);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication ta) {
        return stripTypeApps.get().apply((ta).value.body);
      }
    })));
    return hydra.Strip.deannotateAndDetypeTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Application app) {
        return hydra.ext.python.Coder.encodeApplication(
          cx,
          env,
          (app).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Either et) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (t1 -> hydra.lib.eithers.Bind.apply(
            (encode).apply(t1),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (pyexp -> hydra.ext.python.Coder.encodeTermInline_withCast(
              cx,
              env,
              (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>>>>) (p0 -> p1 -> p2 -> p3 -> hydra.Checking.typeOf(
                p0,
                p1,
                p2,
                p3)),
              (java.util.function.Function<hydra.ext.python.syntax.Expression, java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>>) (p0 -> p1 -> hydra.ext.python.Utils.castTo(
                p0,
                p1)),
              noCast,
              term,
              hydra.ext.python.Utils.functionCall(
                hydra.ext.python.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("Left")),
                java.util.Arrays.asList(pyexp)))))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (t1 -> hydra.lib.eithers.Bind.apply(
            (encode).apply(t1),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (pyexp -> hydra.ext.python.Coder.encodeTermInline_withCast(
              cx,
              env,
              (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>>>>) (p0 -> p1 -> p2 -> p3 -> hydra.Checking.typeOf(
                p0,
                p1,
                p2,
                p3)),
              (java.util.function.Function<hydra.ext.python.syntax.Expression, java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>>) (p0 -> p1 -> hydra.ext.python.Utils.castTo(
                p0,
                p1)),
              noCast,
              term,
              hydra.ext.python.Utils.functionCall(
                hydra.ext.python.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("Right")),
                java.util.Arrays.asList(pyexp)))))),
          (et).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Function f) {
        return hydra.ext.python.Coder.encodeFunction(
          cx,
          env,
          (f).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Let lt) {
        java.util.List<hydra.core.Binding> bindings = (lt).value.bindings;
        hydra.core.Term body = (lt).value.body;
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(bindings),
          () -> hydra.ext.python.Coder.encodeTermInline(
            cx,
            env,
            false,
            body),
          () -> hydra.ext.python.Coder.withLetInline(
            env,
            (lt).value,
            (java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (innerEnv -> hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.MapList.apply(
                (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.NamedExpression>>) (v1 -> hydra.ext.python.Coder.encodeBindingAsAssignment(
                  cx,
                  false,
                  innerEnv,
                  v1)),
                bindings),
              (java.util.function.Function<java.util.List<hydra.ext.python.syntax.NamedExpression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (pbindingExprs -> hydra.lib.eithers.Bind.apply(
                hydra.ext.python.Coder.encodeTermInline(
                  cx,
                  innerEnv,
                  false,
                  body),
                (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (pbody -> {
                  hydra.util.Lazy<hydra.ext.python.syntax.Expression> indexValue = new hydra.util.Lazy<>(() -> hydra.ext.python.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Number_(new hydra.ext.python.syntax.Number_.Integer_(hydra.lib.literals.Int32ToBigint.apply(hydra.lib.lists.Length.apply(bindings))))));
                  hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.StarNamedExpression>> pbindingStarExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.StarNamedExpression>) (ne -> new hydra.ext.python.syntax.StarNamedExpression.Simple(ne)),
                    pbindingExprs));
                  hydra.ext.python.syntax.StarNamedExpression pbodyStarExpr = hydra.ext.python.Utils.pyExpressionToPyStarNamedExpression(pbody);
                  hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.StarNamedExpression>> tupleElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                    pbindingStarExprs.get(),
                    java.util.Arrays.asList(pbodyStarExpr)));
                  hydra.ext.python.syntax.Expression tupleExpr = hydra.ext.python.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Tuple(new hydra.ext.python.syntax.Tuple(tupleElements.get())));
                  hydra.ext.python.syntax.Primary indexedExpr = hydra.ext.python.Utils.primaryWithExpressionSlices(
                    hydra.ext.python.Utils.pyExpressionToPyPrimary(tupleExpr),
                    java.util.Arrays.asList(indexValue.get()));
                  return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.pyPrimaryToPyExpression(indexedExpr));
                })))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.List terms) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            encode,
            (terms).value),
          (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (pyExprs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Tuple(new hydra.ext.python.syntax.Tuple(hydra.lib.lists.Map.apply(
            hydra.ext.python.Utils::pyExpressionToPyStarNamedExpression,
            pyExprs)))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Literal lit) {
        return hydra.ext.python.Coder.encodeLiteral((lit).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Map m) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.DoubleStarredKvpair>>) (kv -> {
              hydra.util.Lazy<hydra.core.Term> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kv));
              hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(kv));
              return hydra.lib.eithers.Bind.apply(
                (encode).apply(k.get()),
                (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.DoubleStarredKvpair>>) (pyK -> hydra.lib.eithers.Bind.apply(
                  (encode).apply(v.get()),
                  (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.DoubleStarredKvpair>>) (pyV -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.DoubleStarredKvpair>right(new hydra.ext.python.syntax.DoubleStarredKvpair.Pair(new hydra.ext.python.syntax.Kvpair(pyK, pyV)))))));
            }),
            hydra.lib.maps.ToList.apply((m).value)),
          (java.util.function.Function<java.util.List<hydra.ext.python.syntax.DoubleStarredKvpair>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (pairs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.functionCall(
            hydra.ext.python.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("FrozenDict")),
            java.util.Arrays.asList(hydra.ext.python.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Dict(new hydra.ext.python.syntax.Dict(pairs))))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.functionCall(
            hydra.ext.python.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("Nothing")),
            (java.util.List<hydra.ext.python.syntax.Expression>) (java.util.Collections.<hydra.ext.python.syntax.Expression>emptyList()))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (t1 -> hydra.lib.eithers.Bind.apply(
            (encode).apply(t1),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (pyexp -> hydra.ext.python.Coder.encodeTermInline_withCast(
              cx,
              env,
              (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>>>>) (p0 -> p1 -> p2 -> p3 -> hydra.Checking.typeOf(
                p0,
                p1,
                p2,
                p3)),
              (java.util.function.Function<hydra.ext.python.syntax.Expression, java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>>) (p0 -> p1 -> hydra.ext.python.Utils.castTo(
                p0,
                p1)),
              noCast,
              term,
              hydra.ext.python.Utils.functionCall(
                hydra.ext.python.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("Just")),
                java.util.Arrays.asList(pyexp)))))),
          (mt).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Pair p) {
        hydra.util.Lazy<hydra.core.Term> t1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((p).value));
        hydra.util.Lazy<hydra.core.Term> t2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((p).value));
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(t1.get()),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (pyExpr1 -> hydra.lib.eithers.Bind.apply(
            (encode).apply(t2.get()),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (pyExpr2 -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Tuple(new hydra.ext.python.syntax.Tuple(java.util.Arrays.asList(
              hydra.ext.python.Utils.pyExpressionToPyStarNamedExpression(pyExpr1),
              hydra.ext.python.Utils.pyExpressionToPyStarNamedExpression(pyExpr2))))))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Record r) {
        java.util.List<hydra.core.Field> fields = (r).value.fields;
        hydra.core.Name tname = (r).value.typeName;
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (fld -> (encode).apply((fld).term)),
            fields),
          (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (pargs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.functionCall(
            hydra.ext.python.Utils.pyNameToPyPrimary(hydra.ext.python.Names.encodeNameQualified(
              env,
              tname)),
            pargs))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Set s) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            encode,
            hydra.lib.sets.ToList.apply((s).value)),
          (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (pyEls -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.functionCall(
            hydra.ext.python.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("frozenset")),
            java.util.Arrays.asList(hydra.ext.python.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Set(new hydra.ext.python.syntax.Set(hydra.lib.lists.Map.apply(
              hydra.ext.python.Utils::pyExpressionToPyStarNamedExpression,
              pyEls)))))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.TypeApplication ta) {
        hydra.core.Term body = (ta).value.body;
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.python.Coder.encodeTermInline(
            cx,
            env,
            true,
            stripTypeApps.get().apply(body)),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (pybase -> hydra.ext.python.Coder.encodeTermInline_withCast(
            cx,
            env,
            (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>>>>) (p0 -> p1 -> p2 -> p3 -> hydra.Checking.typeOf(
              p0,
              p1,
              p2,
              p3)),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>>) (p0 -> p1 -> hydra.ext.python.Utils.castTo(
              p0,
              p1)),
            noCast,
            term,
            pybase)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.TypeLambda tl) {
        hydra.core.Term body = (tl).value.body;
        return hydra.ext.python.Coder.withTypeLambda(
          env,
          (tl).value,
          (java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (env2 -> hydra.ext.python.Coder.encodeTermInline(
            cx,
            env2,
            noCast,
            body)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Union inj) {
        hydra.core.Field field = (inj).value.field;
        hydra.core.Name tname = (inj).value.typeName;
        return hydra.lib.eithers.Bind.apply(
          hydra.Resolution.requireUnionType(
            cx,
            hydra.ext.python.Coder.pythonEnvironmentGetGraph(env),
            tname),
          (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (rt -> hydra.lib.logic.IfElse.lazy(
            hydra.Predicates.isEnumRowType(rt),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.projectFromExpression(
              hydra.ext.python.Utils.pyNameToPyExpression(hydra.ext.python.Names.encodeNameQualified(
                env,
                tname)),
              hydra.ext.python.Names.encodeEnumValue(
                env,
                (field).name))),
            () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (() -> {
              hydra.core.Name fname = (field).name;
              return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (() -> {
                hydra.util.Lazy<Boolean> isUnitVariant = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
                  () -> false,
                  (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.Predicates.isUnitType(hydra.Strip.deannotateType((ft).type))),
                  hydra.lib.lists.Find.apply(
                    (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.lib.equality.Equal.apply(
                      (ft).name.value,
                      (fname).value)),
                    rt)));
                return hydra.lib.eithers.Bind.apply(
                  hydra.lib.logic.IfElse.lazy(
                    hydra.lib.logic.Or.apply(
                      hydra.Predicates.isUnitTerm((field).term),
                      isUnitVariant.get()),
                    () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Expression>>right((java.util.List<hydra.ext.python.syntax.Expression>) (java.util.Collections.<hydra.ext.python.syntax.Expression>emptyList())),
                    () -> hydra.lib.eithers.Bind.apply(
                      (encode).apply((field).term),
                      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Expression>>>) (parg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Expression>>right(java.util.Arrays.asList(parg))))),
                  (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (args -> {
                    hydra.ext.python.syntax.Name deconflictedName = hydra.ext.python.Coder.deconflictVariantName(
                      true,
                      env,
                      tname,
                      fname,
                      (env).graph);
                    return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.castTo(
                      hydra.ext.python.Names.typeVariableReference(
                        env,
                        tname),
                      hydra.ext.python.Utils.functionCall(
                        hydra.ext.python.Utils.pyNameToPyPrimary(deconflictedName),
                        args)));
                  }));
              })).get();
            })).get())));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Unit ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.pyNameToPyExpression(hydra.ext.python.Utils.pyNone()));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Variable name) {
        return hydra.ext.python.Coder.encodeVariable(
          cx,
          env,
          (name).value,
          (java.util.List<hydra.ext.python.syntax.Expression>) (java.util.Collections.<hydra.ext.python.syntax.Expression>emptyList()));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> visit(hydra.core.Term.Wrap wrapped) {
        hydra.core.Term inner = (wrapped).value.body;
        hydra.core.Name tname = (wrapped).value.typeName;
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(inner),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (parg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.functionCall(
            hydra.ext.python.Utils.pyNameToPyPrimary(hydra.ext.python.Names.encodeNameQualified(
              env,
              tname)),
            java.util.Arrays.asList(parg)))));
      }
    });
  }

  static <T1> hydra.util.Either<T1, hydra.ext.python.syntax.Expression> encodeTermInline_withCast(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>>>> hydra_checking_typeOf, java.util.function.Function<hydra.ext.python.syntax.Expression, java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>> hydra_ext_python_utils_castTo, Boolean noCast, hydra.core.Term term, hydra.ext.python.syntax.Expression pyexp) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Or.apply(
        noCast,
        (env).skipCasts),
      () -> hydra.util.Either.<T1, hydra.ext.python.syntax.Expression>right(pyexp),
      () -> ((java.util.function.Supplier<hydra.util.Either<T1, hydra.ext.python.syntax.Expression>>) (() -> {
        hydra.graph.Graph tc = (env).graph;
        return ((java.util.function.Supplier<hydra.util.Either<T1, hydra.ext.python.syntax.Expression>>) (() -> {
          hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>> mtyp = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.core.Type>) (_r -> hydra.lib.pairs.First.apply(_r)),
            (hydra_checking_typeOf).apply(cx).apply(tc).apply((java.util.List<hydra.core.Type>) (java.util.Collections.<hydra.core.Type>emptyList())).apply(term)));
          return hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Either<T1, hydra.ext.python.syntax.Expression>>) (ignored -> hydra.util.Either.<T1, hydra.ext.python.syntax.Expression>right(pyexp)),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.ext.python.syntax.Expression>>) (typ -> hydra.lib.eithers.Either.apply(
              ignored -> hydra.util.Either.<T1, hydra.ext.python.syntax.Expression>right(pyexp),
              (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T1, hydra.ext.python.syntax.Expression>>) (pytyp -> hydra.util.Either.<T1, hydra.ext.python.syntax.Expression>right((hydra_ext_python_utils_castTo).apply(pytyp).apply(pyexp))),
              hydra.ext.python.Coder.encodeType(
                env,
                typ))),
            mtyp.get());
        })).get();
      })).get());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> encodeTermMultiline(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Term term) {
    hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.core.Term> gathered = hydra.CoderUtils.gatherApplications(term);
    hydra.util.Lazy<java.util.List<hydra.core.Term>> args = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(gathered));
    hydra.util.Lazy<hydra.core.Term> body = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(gathered));
    hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>> dfltLogic = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Bind.apply(
      hydra.ext.python.Coder.analyzePythonFunction(
        cx,
        env,
        term),
      (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (fs -> {
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, java.util.List<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
        hydra.util.Lazy<hydra.ext.python.environment.PythonEnvironment> env2 = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, hydra.ext.python.environment.PythonEnvironment>) (projected -> projected.environment)).apply(fs));
        hydra.util.Lazy<hydra.core.Term> innerBody = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
        hydra.util.Lazy<java.util.List<hydra.core.Name>> params = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.python.environment.PythonEnvironment>, java.util.List<hydra.core.Name>>) (projected -> projected.params)).apply(fs));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(bindings.get()),
          () -> hydra.lib.eithers.Bind.apply(
            hydra.ext.python.Coder.encodeTermInline(
              cx,
              env,
              false,
              term),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>right(java.util.Arrays.asList(hydra.ext.python.Utils.returnSingle(expr))))),
          () -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (v1 -> hydra.ext.python.Coder.encodeBindingAs(
                cx,
                env2.get(),
                v1)),
              bindings.get()),
            (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (bindingStmts -> hydra.lib.eithers.Bind.apply(
              hydra.ext.python.Coder.encodeTermMultiline(
                cx,
                env2.get(),
                innerBody.get()),
              (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (bodyStmts -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>right(hydra.lib.lists.Concat2.apply(
                bindingStmts,
                bodyStmts)))))));
      })));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply(args.get()),
        1),
      () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (() -> {
        hydra.util.Lazy<hydra.core.Term> arg = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(args.get()));
        return hydra.Strip.deannotateAndDetypeTerm(body.get()).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> otherwise(hydra.core.Term instance) {
            return dfltLogic.get();
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Term.Function f) {
            return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> otherwise(hydra.core.Function instance) {
                return dfltLogic.get();
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Function.Elimination e) {
                return (e).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> otherwise(hydra.core.Elimination instance) {
                    return dfltLogic.get();
                  }

                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Elimination.Union cs) {
                    java.util.List<hydra.core.Field> cases_ = (cs).value.cases;
                    hydra.util.Maybe<hydra.core.Term> dflt = (cs).value.default_;
                    hydra.core.Name tname = (cs).value.typeName;
                    return hydra.lib.eithers.Bind.apply(
                      hydra.Resolution.requireUnionType(
                        cx,
                        hydra.ext.python.Coder.pythonEnvironmentGetGraph(env),
                        tname),
                      (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (rt -> {
                        Boolean isEnum = hydra.Predicates.isEnumRowType(rt);
                        hydra.util.Lazy<Boolean> isFull = new hydra.util.Lazy<>(() -> hydra.ext.python.Coder.isCasesFull(
                          rt,
                          cases_));
                        return hydra.lib.eithers.Bind.apply(
                          hydra.ext.python.Coder.encodeTermInline(
                            cx,
                            env,
                            false,
                            arg.get()),
                          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (pyArg -> hydra.lib.eithers.Bind.apply(
                            hydra.lib.eithers.MapList.apply(
                              (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.CaseBlock>>) (v1 -> hydra.ext.python.Coder.encodeCaseBlock(
                                cx,
                                env,
                                tname,
                                rt,
                                isEnum,
                                (java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>>) (e2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (t -> hydra.ext.python.Coder.encodeTermMultiline(
                                  cx,
                                  e2,
                                  t))),
                                v1)),
                              hydra.ext.python.Coder.deduplicateCaseVariables(cases_)),
                            (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (pyCases -> hydra.lib.eithers.Bind.apply(
                              hydra.ext.python.Coder.encodeDefaultCaseBlock(
                                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (t -> hydra.ext.python.Coder.encodeTermInline(
                                  cx,
                                  env,
                                  false,
                                  t)),
                                isFull.get(),
                                dflt,
                                tname),
                              (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (pyDflt -> {
                                hydra.ext.python.syntax.SubjectExpression subj = new hydra.ext.python.syntax.SubjectExpression.Simple(new hydra.ext.python.syntax.NamedExpression.Simple(pyArg));
                                hydra.util.Lazy<hydra.ext.python.syntax.Statement> matchStmt = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Match(new hydra.ext.python.syntax.MatchStatement(subj, hydra.lib.lists.Concat2.apply(
                                  pyCases,
                                  pyDflt)))));
                                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>right(java.util.Arrays.asList(matchStmt.get()));
                              }))))));
                      }));
                  }
                });
              }
            });
          }
        });
      })).get(),
      () -> dfltLogic.get());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> encodeTermMultilineTCO(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name funcName, java.util.List<hydra.core.Name> paramNames, hydra.core.Term term) {
    hydra.core.Term stripped = hydra.Strip.deannotateAndDetypeTerm(term);
    hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.core.Term> gathered = hydra.CoderUtils.gatherApplications(stripped);
    hydra.util.Lazy<java.util.List<hydra.core.Term>> gatherArgs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(gathered));
    hydra.util.Lazy<hydra.core.Term> gatherFun = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(gathered));
    hydra.core.Term strippedFun = hydra.Strip.deannotateAndDetypeTerm(gatherFun.get());
    hydra.util.Lazy<Boolean> isSelfCall = new hydra.util.Lazy<>(() -> (strippedFun).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Term.Variable n) {
        return hydra.lib.equality.Equal.apply(
          (n).value,
          funcName);
      }
    }));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        isSelfCall.get(),
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(gatherArgs.get()),
          hydra.lib.lists.Length.apply(paramNames))),
      () -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (a -> hydra.ext.python.Coder.encodeTermInline(
            cx,
            env,
            false,
            a)),
          gatherArgs.get()),
        (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (pyArgs -> {
          hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Statement>> assignments = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.ext.python.syntax.Expression>, hydra.ext.python.syntax.Statement>) (pair -> {
              hydra.util.Lazy<hydra.core.Name> paramName = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
              hydra.util.Lazy<hydra.ext.python.syntax.Expression> pyArg = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
              return hydra.ext.python.Utils.assignmentStatement(
                hydra.ext.python.Names.encodeName(
                  false,
                  new hydra.util.CaseConvention.LowerSnake(),
                  env,
                  paramName.get()),
                pyArg.get());
            }),
            hydra.lib.lists.Zip.apply(
              paramNames,
              pyArgs)));
          hydra.ext.python.syntax.Statement continueStmt = new hydra.ext.python.syntax.Statement.Simple(java.util.Arrays.asList(new hydra.ext.python.syntax.SimpleStatement.Continue()));
          return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>right(hydra.lib.lists.Concat2.apply(
            assignments.get(),
            java.util.Arrays.asList(continueStmt)));
        })),
      () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (() -> {
        hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.core.Term> gathered2 = hydra.CoderUtils.gatherApplications(term);
        return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (() -> {
          hydra.util.Lazy<java.util.List<hydra.core.Term>> args2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(gathered2));
          return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (() -> {
            hydra.util.Lazy<hydra.core.Term> body2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(gathered2));
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                hydra.lib.lists.Length.apply(args2.get()),
                1),
              () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (() -> {
                hydra.util.Lazy<hydra.core.Term> arg = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(args2.get()));
                return hydra.Strip.deannotateAndDetypeTerm(body2.get()).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> otherwise(hydra.core.Term instance) {
                    return hydra.lib.eithers.Bind.apply(
                      hydra.ext.python.Coder.encodeTermInline(
                        cx,
                        env,
                        false,
                        term),
                      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>right(java.util.Arrays.asList(hydra.ext.python.Utils.returnSingle(expr)))));
                  }

                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Term.Function f) {
                    return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> otherwise(hydra.core.Function instance) {
                        return hydra.lib.eithers.Bind.apply(
                          hydra.ext.python.Coder.encodeTermInline(
                            cx,
                            env,
                            false,
                            term),
                          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>right(java.util.Arrays.asList(hydra.ext.python.Utils.returnSingle(expr)))));
                      }

                      @Override
                      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Function.Elimination e) {
                        return (e).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> otherwise(hydra.core.Elimination instance) {
                            return hydra.lib.eithers.Bind.apply(
                              hydra.ext.python.Coder.encodeTermInline(
                                cx,
                                env,
                                false,
                                term),
                              (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>right(java.util.Arrays.asList(hydra.ext.python.Utils.returnSingle(expr)))));
                          }

                          @Override
                          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Elimination.Union cs) {
                            java.util.List<hydra.core.Field> cases_ = (cs).value.cases;
                            hydra.util.Maybe<hydra.core.Term> dflt = (cs).value.default_;
                            hydra.core.Name tname = (cs).value.typeName;
                            return hydra.lib.eithers.Bind.apply(
                              hydra.Resolution.requireUnionType(
                                cx,
                                hydra.ext.python.Coder.pythonEnvironmentGetGraph(env),
                                tname),
                              (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (rt -> {
                                Boolean isEnum = hydra.Predicates.isEnumRowType(rt);
                                hydra.util.Lazy<Boolean> isFull = new hydra.util.Lazy<>(() -> hydra.ext.python.Coder.isCasesFull(
                                  rt,
                                  cases_));
                                return hydra.lib.eithers.Bind.apply(
                                  hydra.ext.python.Coder.encodeTermInline(
                                    cx,
                                    env,
                                    false,
                                    arg.get()),
                                  (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (pyArg -> hydra.lib.eithers.Bind.apply(
                                    hydra.lib.eithers.MapList.apply(
                                      (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.CaseBlock>>) (v1 -> hydra.ext.python.Coder.encodeCaseBlock(
                                        cx,
                                        env,
                                        tname,
                                        rt,
                                        isEnum,
                                        (java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>>) (e2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (t2 -> hydra.ext.python.Coder.encodeTermMultilineTCO(
                                          cx,
                                          e2,
                                          funcName,
                                          paramNames,
                                          t2))),
                                        v1)),
                                      hydra.ext.python.Coder.deduplicateCaseVariables(cases_)),
                                    (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (pyCases -> hydra.lib.eithers.Bind.apply(
                                      hydra.ext.python.Coder.encodeDefaultCaseBlock(
                                        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (t2 -> hydra.ext.python.Coder.encodeTermInline(
                                          cx,
                                          env,
                                          false,
                                          t2)),
                                        isFull.get(),
                                        dflt,
                                        tname),
                                      (java.util.function.Function<java.util.List<hydra.ext.python.syntax.CaseBlock>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (pyDflt -> {
                                        hydra.ext.python.syntax.SubjectExpression subj = new hydra.ext.python.syntax.SubjectExpression.Simple(new hydra.ext.python.syntax.NamedExpression.Simple(pyArg));
                                        hydra.util.Lazy<hydra.ext.python.syntax.Statement> matchStmt = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Match(new hydra.ext.python.syntax.MatchStatement(subj, hydra.lib.lists.Concat2.apply(
                                          pyCases,
                                          pyDflt)))));
                                        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>right(java.util.Arrays.asList(matchStmt.get()));
                                      }))))));
                              }));
                          }
                        });
                      }
                    });
                  }
                });
              })).get(),
              () -> hydra.lib.eithers.Bind.apply(
                hydra.ext.python.Coder.encodeTermInline(
                  cx,
                  env,
                  false,
                  term),
                (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>right(java.util.Arrays.asList(hydra.ext.python.Utils.returnSingle(expr))))));
          })).get();
        })).get();
      })).get());
  }

  static <T0> hydra.util.Either<T0, hydra.ext.python.syntax.Expression> encodeType(hydra.ext.python.environment.PythonEnvironment env, hydra.core.Type typ) {
    return hydra.Strip.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Application at) {
        return hydra.ext.python.Coder.<T0>encodeApplicationType(
          env,
          (at).value);
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Function ft) {
        return hydra.ext.python.Coder.<T0>encodeFunctionType(
          env,
          (ft).value);
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Forall lt) {
        return hydra.ext.python.Coder.<T0>encodeForallType(
          env,
          (lt).value);
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.List et) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.python.Coder.<T0>encodeType(
            env,
            (et).value),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (pyet -> hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.nameAndParams(
            new hydra.ext.python.syntax.Name("frozenlist"),
            java.util.Arrays.asList(pyet)))));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Map mt) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.python.Coder.<T0>encodeType(
            env,
            (mt).value.keys),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (pykt -> hydra.lib.eithers.Bind.apply(
            hydra.ext.python.Coder.<T0>encodeType(
              env,
              (mt).value.values),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (pyvt -> hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.nameAndParams(
              new hydra.ext.python.syntax.Name("FrozenDict"),
              java.util.Arrays.asList(
                pykt,
                pyvt)))))));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Literal lt) {
        return hydra.ext.python.Coder.<T0>encodeLiteralType((lt).value);
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Maybe et) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.python.Coder.<T0>encodeType(
            env,
            (et).value),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (ptype -> hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.pyPrimaryToPyExpression(hydra.ext.python.Utils.primaryWithExpressionSlices(
            new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("Maybe"))),
            java.util.Arrays.asList(ptype))))));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Either eitherT) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.python.Coder.<T0>encodeType(
            env,
            (eitherT).value.left),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (pyleft -> hydra.lib.eithers.Bind.apply(
            hydra.ext.python.Coder.<T0>encodeType(
              env,
              (eitherT).value.right),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (pyright -> hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.pyPrimaryToPyExpression(hydra.ext.python.Utils.primaryWithExpressionSlices(
              new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("Either"))),
              java.util.Arrays.asList(
                pyleft,
                pyright))))))));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Pair pairT) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.python.Coder.<T0>encodeType(
            env,
            (pairT).value.first),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (pyFirst -> hydra.lib.eithers.Bind.apply(
            hydra.ext.python.Coder.<T0>encodeType(
              env,
              (pairT).value.second),
            (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (pySecond -> hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.nameAndParams(
              new hydra.ext.python.syntax.Name("tuple"),
              java.util.Arrays.asList(
                pyFirst,
                pySecond)))))));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Record ignored) {
        return hydra.ext.python.Coder.<T0>encodeType_dflt(
          hydra.ext.python.Utils::doubleQuotedString,
          hydra.show.Core::type,
          hydra.Strip::deannotateType,
          typ);
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Set et) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.python.Coder.<T0>encodeType(
            env,
            (et).value),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (pyet -> hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.nameAndParams(
            new hydra.ext.python.syntax.Name("frozenset"),
            java.util.Arrays.asList(pyet)))));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Union ignored) {
        return hydra.ext.python.Coder.<T0>encodeType_dflt(
          hydra.ext.python.Utils::doubleQuotedString,
          hydra.show.Core::type,
          hydra.Strip::deannotateType,
          typ);
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Unit ignored) {
        return hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.pyNameToPyExpression(hydra.ext.python.Utils.pyNone()));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Void_ ignored) {
        return hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Utils.pyNameToPyExpression(hydra.ext.python.Utils.pyNone()));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Variable name) {
        return hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Names.typeVariableReference(
          env,
          (name).value));
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Wrap ignored) {
        return hydra.ext.python.Coder.<T0>encodeType_dflt(
          hydra.ext.python.Utils::doubleQuotedString,
          hydra.show.Core::type,
          hydra.Strip::deannotateType,
          typ);
      }

      @Override
      public hydra.util.Either<T0, hydra.ext.python.syntax.Expression> visit(hydra.core.Type.Annotated ignored) {
        return hydra.ext.python.Coder.<T0>encodeType_dflt(
          hydra.ext.python.Utils::doubleQuotedString,
          hydra.show.Core::type,
          hydra.Strip::deannotateType,
          typ);
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>> encodeTypeAssignment(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name name, hydra.core.Type typ, hydra.util.Maybe<String> comment) {
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.python.Coder.encodeTypeAssignmentInner(
        cx,
        env,
        name,
        typ,
        comment),
      (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>>>) (defStmts -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>>right(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.Statement, java.util.List<hydra.ext.python.syntax.Statement>>) (s -> java.util.Arrays.asList(s)),
        defStmts))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> encodeTypeAssignmentInner(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name name, hydra.core.Type typ, hydra.util.Maybe<String> comment) {
    hydra.core.Type stripped = hydra.Strip.deannotateType(typ);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> otherwise(hydra.core.Type instance) {
        return hydra.ext.python.Coder.encodeTypeAssignmentInner_dflt(
          comment,
          env,
          (java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.util.Maybe<String>, java.util.function.Function<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Statement>>>>>) (p0 -> p1 -> p2 -> p3 -> hydra.ext.python.Coder.encodeTypeDefSingle(
            p0,
            p1,
            p2,
            p3)),
          name,
          typ);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Type.Forall ft) {
        hydra.core.Type body = (ft).value.body;
        hydra.core.Name tvar = (ft).value.parameter;
        hydra.ext.python.environment.PythonEnvironment newEnv = hydra.ext.python.Coder.extendEnvWithTypeVar(
          env,
          tvar);
        return hydra.ext.python.Coder.encodeTypeAssignmentInner(
          cx,
          newEnv,
          name,
          body,
          comment);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Type.Record rt) {
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.ext.python.syntax.Statement, java.util.List<hydra.ext.python.syntax.Statement>>) (s -> java.util.Arrays.asList(s)),
          hydra.ext.python.Coder.encodeRecordType(
            cx,
            env,
            name,
            (rt).value,
            comment));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Type.Union rt) {
        return hydra.ext.python.Coder.encodeUnionType(
          cx,
          env,
          name,
          (rt).value,
          comment);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> visit(hydra.core.Type.Wrap wt) {
        return hydra.ext.python.Coder.encodeWrappedType(
          env,
          name,
          (wt).value,
          comment);
      }
    });
  }

  static <T0> hydra.util.Either<T0, java.util.List<hydra.ext.python.syntax.Statement>> encodeTypeAssignmentInner_dflt(hydra.util.Maybe<String> comment, hydra.ext.python.environment.PythonEnvironment env, java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.util.Maybe<String>, java.util.function.Function<hydra.ext.python.syntax.Expression, java.util.List<hydra.ext.python.syntax.Statement>>>>> hydra_ext_python_coder_encodeTypeDefSingle, hydra.core.Name name, hydra.core.Type typ) {
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.python.Coder.<T0>encodeType(
        env,
        typ),
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T0, java.util.List<hydra.ext.python.syntax.Statement>>>) (typeExpr -> hydra.util.Either.<T0, java.util.List<hydra.ext.python.syntax.Statement>>right((hydra_ext_python_coder_encodeTypeDefSingle).apply(env).apply(name).apply(comment).apply(typeExpr))));
  }

  static java.util.List<hydra.ext.python.syntax.Statement> encodeTypeDefSingle(hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name name, hydra.util.Maybe<String> comment, hydra.ext.python.syntax.Expression typeExpr) {
    hydra.ext.python.syntax.Name pyName = hydra.ext.python.Names.encodeName(
      false,
      new hydra.util.CaseConvention.Pascal(),
      env,
      name);
    java.util.List<hydra.ext.python.syntax.TypeParameter> tparams = hydra.ext.python.Coder.environmentTypeParameters(env);
    return java.util.Arrays.asList(hydra.ext.python.Coder.typeAliasStatementFor(
      env,
      pyName,
      tparams,
      comment,
      typeExpr));
  }

  static <T0> hydra.util.Either<T0, hydra.ext.python.syntax.Expression> encodeTypeQuoted(hydra.ext.python.environment.PythonEnvironment env, hydra.core.Type typ) {
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.python.Coder.<T0>encodeType(
        env,
        typ),
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T0, hydra.ext.python.syntax.Expression>>) (pytype -> hydra.util.Either.<T0, hydra.ext.python.syntax.Expression>right(hydra.lib.logic.IfElse.lazy(
        hydra.lib.sets.Null.apply(hydra.Variables.freeVariablesInType(typ)),
        () -> pytype,
        () -> hydra.ext.python.Utils.doubleQuotedString(hydra.Serialization.printExpr(hydra.ext.python.Serde.encodeExpression(pytype)))))));
  }

  static <T1> hydra.util.Either<T1, hydra.ext.python.syntax.Expression> encodeType_dflt(java.util.function.Function<String, hydra.ext.python.syntax.Expression> hydra_ext_python_utils_doubleQuotedString, java.util.function.Function<hydra.core.Type, String> hydra_show_core_type, java.util.function.Function<hydra.core.Type, hydra.core.Type> hydra_strip_deannotateType, hydra.core.Type typ) {
    return hydra.util.Either.<T1, hydra.ext.python.syntax.Expression>right((hydra_ext_python_utils_doubleQuotedString).apply(hydra.lib.strings.Cat2.apply(
      "type = ",
      (hydra_show_core_type).apply((hydra_strip_deannotateType).apply(typ)))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> encodeUnionEliminationInline(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.CaseStatement cs, hydra.ext.python.syntax.Expression pyArg) {
    java.util.List<hydra.core.Field> cases_ = (cs).cases;
    hydra.util.Maybe<hydra.core.Term> mdefault = (cs).default_;
    hydra.core.Name tname = (cs).typeName;
    return hydra.lib.eithers.Bind.apply(
      hydra.Resolution.requireUnionType(
        cx,
        hydra.ext.python.Coder.pythonEnvironmentGetGraph(env),
        tname),
      (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (rt -> {
        Boolean isEnum = hydra.Predicates.isEnumRowType(rt);
        hydra.ext.python.syntax.Primary isinstancePrimary = hydra.ext.python.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("isinstance"));
        hydra.ext.python.syntax.Expression valueExpr = hydra.ext.python.Utils.projectFromExpression(
          pyArg,
          new hydra.ext.python.syntax.Name("value"));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Coder.unsupportedExpression("no matching case in inline union elimination")),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (dflt -> hydra.ext.python.Coder.encodeTermInline(
              cx,
              env,
              false,
              dflt)),
            mdefault),
          (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (pyDefault -> {
            java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>>> encodeBranch = (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>>>) (field -> {
              hydra.core.Name fname = (field).name;
              hydra.core.Term fterm = (field).term;
              Boolean isUnitVariant = hydra.ext.python.Coder.isVariantUnitType(
                rt,
                fname);
              hydra.ext.python.syntax.Name pyVariantName = hydra.ext.python.Coder.deconflictVariantName(
                true,
                env,
                tname,
                fname,
                (env).graph);
              hydra.util.Lazy<hydra.ext.python.syntax.Expression> isinstanceCheck = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                isEnum,
                () -> new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(hydra.ext.python.Utils.pyExpressionToBitwiseOr(pyArg), java.util.Arrays.asList(new hydra.ext.python.syntax.CompareOpBitwiseOrPair(new hydra.ext.python.syntax.CompareOp.Eq(), hydra.ext.python.Utils.pyExpressionToBitwiseOr(hydra.ext.python.Utils.pyNameToPyExpression(pyVariantName))))))))))),
                () -> hydra.ext.python.Utils.functionCall(
                  isinstancePrimary,
                  java.util.Arrays.asList(
                    pyArg,
                    hydra.ext.python.Utils.pyNameToPyExpression(pyVariantName)))));
              return hydra.lib.eithers.Bind.apply(
                hydra.ext.python.Coder.encodeTermInline(
                  cx,
                  env,
                  false,
                  fterm),
                (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>>>) (pyBranch -> {
                  hydra.util.Lazy<hydra.ext.python.syntax.Expression> pyResult = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                    isEnum,
                    () -> hydra.ext.python.Utils.functionCall(
                      hydra.ext.python.Utils.pyExpressionToPyPrimary(pyBranch),
                      java.util.Arrays.asList(pyArg)),
                    () -> hydra.lib.logic.IfElse.lazy(
                      isUnitVariant,
                      () -> hydra.ext.python.Utils.functionCall(
                        hydra.ext.python.Utils.pyExpressionToPyPrimary(pyBranch),
                        java.util.Arrays.asList(pyArg)),
                      () -> hydra.ext.python.Utils.functionCall(
                        hydra.ext.python.Utils.pyExpressionToPyPrimary(pyBranch),
                        java.util.Arrays.asList(valueExpr)))));
                  return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>>right((hydra.util.Pair<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>) ((hydra.util.Pair<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>) (new hydra.util.Pair<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>(isinstanceCheck.get(), pyResult.get()))));
                }));
            });
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.MapList.apply(
                encodeBranch,
                cases_),
              (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (encodedBranches -> {
                java.util.function.Function<hydra.ext.python.syntax.Expression, java.util.function.Function<hydra.util.Pair<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>, hydra.ext.python.syntax.Expression>> buildChain = (java.util.function.Function<hydra.ext.python.syntax.Expression, java.util.function.Function<hydra.util.Pair<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>, hydra.ext.python.syntax.Expression>>) (elseExpr -> (java.util.function.Function<hydra.util.Pair<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression>, hydra.ext.python.syntax.Expression>) (branchPair -> {
                  hydra.util.Lazy<hydra.ext.python.syntax.Expression> checkExpr = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(branchPair));
                  hydra.util.Lazy<hydra.ext.python.syntax.Expression> resultExpr = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(branchPair));
                  return new hydra.ext.python.syntax.Expression.Conditional(new hydra.ext.python.syntax.Conditional(hydra.ext.python.Utils.pyExpressionToDisjunction(resultExpr.get()), hydra.ext.python.Utils.pyExpressionToDisjunction(checkExpr.get()), elseExpr));
                }));
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.lib.lists.Foldl.apply(
                  buildChain,
                  pyDefault,
                  hydra.lib.lists.Reverse.apply(encodedBranches)));
              }));
          }));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement> encodeUnionField(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name unionName, hydra.core.FieldType fieldType) {
    hydra.core.Name fname = (fieldType).name;
    hydra.core.Type ftype = (fieldType).type;
    return hydra.lib.eithers.Bind.apply(
      hydra.Annotations.getTypeDescription(
        cx,
        hydra.ext.python.Coder.pythonEnvironmentGetGraph(env),
        ftype),
      (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (fcomment -> {
        hydra.util.Lazy<Boolean> isUnit = new hydra.util.Lazy<>(() -> hydra.lib.equality.Equal.apply(
          hydra.Strip.deannotateType(ftype),
          new hydra.core.Type.Unit()));
        hydra.ext.python.syntax.Name varName = hydra.ext.python.Coder.deconflictVariantName(
          false,
          env,
          unionName,
          fname,
          (env).graph);
        hydra.util.Lazy<hydra.ext.python.syntax.Block> body = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          isUnit.get(),
          () -> hydra.ext.python.Utils.indentedBlock(
            fcomment,
            java.util.Arrays.asList(hydra.ext.python.Utils.unitVariantMethods(varName))),
          () -> hydra.ext.python.Utils.indentedBlock(
            fcomment,
            (java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>) (java.util.Collections.<java.util.List<hydra.ext.python.syntax.Statement>>emptyList()))));
        java.util.List<hydra.core.Name> tparamNames = hydra.ext.python.Coder.findTypeParams(
          env,
          ftype);
        hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Name>> tparamPyNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          hydra.ext.python.Names::encodeTypeVariable,
          tparamNames));
        hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.TypeParameter>> fieldParams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          hydra.ext.python.Utils::pyNameToPyTypeParameter,
          tparamPyNames.get()));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.logic.IfElse.lazy(
            isUnit.get(),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.python.syntax.Args>>right((hydra.util.Maybe<hydra.ext.python.syntax.Args>) (hydra.util.Maybe.<hydra.ext.python.syntax.Args>nothing())),
            () -> hydra.lib.eithers.Bind.apply(
              hydra.ext.python.Coder.encodeTypeQuoted(
                env,
                ftype),
              (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.python.syntax.Args>>>) (quotedType -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.python.syntax.Args>>right(hydra.util.Maybe.just(hydra.ext.python.Coder.variantArgs(
                quotedType,
                (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))))))),
          (java.util.function.Function<hydra.util.Maybe<hydra.ext.python.syntax.Args>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (margs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>right(hydra.ext.python.Utils.pyClassDefinitionToPyStatement(new hydra.ext.python.syntax.ClassDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), varName, fieldParams.get(), margs, body.get())))));
      }));
  }

  static hydra.ext.python.syntax.Primary encodeUnionFieldAlt(hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name unionName, hydra.core.FieldType fieldType) {
    hydra.core.Name fname = (fieldType).name;
    hydra.core.Type ftype = (fieldType).type;
    hydra.ext.python.syntax.Primary namePrim = hydra.ext.python.Utils.pyNameToPyPrimary(hydra.ext.python.Names.variantName(
      false,
      env,
      unionName,
      fname));
    java.util.List<hydra.core.Name> tparamNames = hydra.ext.python.Coder.findTypeParams(
      env,
      ftype);
    hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Name>> tparams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.ext.python.Names::encodeTypeVariable,
      tparamNames));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(tparams.get()),
      () -> namePrim,
      () -> ((java.util.function.Supplier<hydra.ext.python.syntax.Primary>) (() -> {
        hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> tparamExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          hydra.ext.python.Utils::pyNameToPyExpression,
          tparams.get()));
        return hydra.ext.python.Utils.primaryWithExpressionSlices(
          namePrim,
          tparamExprs.get());
      })).get());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>> encodeUnionType(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name name, java.util.List<hydra.core.FieldType> rowType, hydra.util.Maybe<String> comment) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.Predicates.isEnumRowType(rowType),
      () -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (v1 -> hydra.ext.python.Coder.encodeEnumValueAssignment(
            cx,
            env,
            v1)),
          rowType),
        (java.util.function.Function<java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (vals -> {
          hydra.ext.python.syntax.Name enumName = new hydra.ext.python.syntax.Name("Enum");
          hydra.util.Maybe<hydra.ext.python.syntax.Args> args = hydra.util.Maybe.just(hydra.ext.python.Utils.pyExpressionsToPyArgs(java.util.Arrays.asList(hydra.ext.python.Utils.pyNameToPyExpression(enumName))));
          hydra.ext.python.syntax.Block body = hydra.ext.python.Utils.indentedBlock(
            comment,
            vals);
          hydra.ext.python.syntax.Name pyName = hydra.ext.python.Names.encodeName(
            false,
            new hydra.util.CaseConvention.Pascal(),
            env,
            name);
          hydra.util.Lazy<hydra.ext.python.syntax.Statement> typeConstStmt = new hydra.util.Lazy<>(() -> hydra.ext.python.Utils.dottedAssignmentStatement(
            pyName,
            hydra.ext.python.Names.encodeConstantForTypeName(
              env,
              name),
            hydra.ext.python.Utils.functionCall(
              hydra.ext.python.Utils.pyNameToPyPrimary(hydra.ext.python.Names.encodeName(
                true,
                new hydra.util.CaseConvention.Pascal(),
                env,
                new hydra.core.Name("hydra.core.Name"))),
              java.util.Arrays.asList(hydra.ext.python.Utils.doubleQuotedString((name).value)))));
          return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>right(java.util.Arrays.asList(
            hydra.ext.python.Utils.pyClassDefinitionToPyStatement(new hydra.ext.python.syntax.ClassDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), pyName, (java.util.List<hydra.ext.python.syntax.TypeParameter>) (java.util.Collections.<hydra.ext.python.syntax.TypeParameter>emptyList()), args, body)),
            typeConstStmt.get()));
        })),
      () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (() -> {
        java.util.List<hydra.ext.python.syntax.Statement> constStmts = hydra.ext.python.Coder.encodeNameConstants(
          env,
          name,
          rowType);
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Statement>>) (v1 -> hydra.ext.python.Coder.encodeUnionField(
              cx,
              env,
              name,
              v1)),
            rowType),
          (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>>) (fieldStmts -> {
            java.util.List<hydra.ext.python.syntax.TypeParameter> tparams = hydra.ext.python.Coder.environmentTypeParameters(env);
            hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Primary>> unionAlts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.FieldType, hydra.ext.python.syntax.Primary>) (v1 -> hydra.ext.python.Coder.encodeUnionFieldAlt(
                env,
                name,
                v1)),
              rowType));
            java.util.List<hydra.ext.python.syntax.Statement> unionStmts = hydra.ext.python.Coder.unionTypeStatementsFor(
              env,
              hydra.ext.python.Names.encodeName(
                false,
                new hydra.util.CaseConvention.Pascal(),
                env,
                name),
              tparams,
              comment,
              hydra.ext.python.Utils.orExpression(unionAlts.get()),
              constStmts);
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.ext.python.syntax.Statement>>right(hydra.lib.lists.Concat2.apply(
              fieldStmts,
              unionStmts));
          }));
      })).get());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression> encodeVariable(hydra.context.Context cx, hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name name, java.util.List<hydra.ext.python.syntax.Expression> args) {
    hydra.ext.python.syntax.Expression asFunctionCall = hydra.ext.python.Utils.functionCall(
      hydra.ext.python.Utils.pyNameToPyPrimary(hydra.ext.python.Names.encodeName(
        true,
        new hydra.util.CaseConvention.LowerSnake(),
        env,
        name)),
      args);
    hydra.ext.python.syntax.Expression asVariable = hydra.ext.python.Names.termVariableReference(
      env,
      name);
    hydra.graph.Graph g = hydra.ext.python.Coder.pythonEnvironmentGetGraph(env);
    java.util.Set<hydra.core.Name> inlineVars = (env).inlineVariables;
    hydra.graph.Graph tc = (env).graph;
    java.util.Map<hydra.core.Name, hydra.core.TypeScheme> tcTypes = (tc).boundTypes;
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.TypeScheme>> mTypScheme = new hydra.util.Lazy<>(() -> hydra.lib.maps.Lookup.apply(
      name,
      tcTypes));
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> mTyp = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.core.TypeScheme, hydra.core.Type>) (ts_ -> (ts_).type),
      mTypScheme.get()));
    java.util.Set<hydra.core.Name> tcLambdaVars = (tc).lambdaVariables;
    java.util.Map<hydra.core.Name, hydra.core.Term> tcMetadata = (tc).metadata;
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(args)),
      () -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asFunctionCall),
        (java.util.function.Function<hydra.graph.Primitive, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (prim -> {
          Integer primArity = hydra.Arity.primitiveArity(prim);
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              primArity,
              hydra.lib.lists.Length.apply(args)),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asFunctionCall),
            () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (() -> {
              hydra.util.Lazy<Integer> numRemaining = new hydra.util.Lazy<>(() -> hydra.lib.math.Sub.apply(
                primArity,
                hydra.lib.lists.Length.apply(args)));
              return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (() -> {
                hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Name>> remainingParams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                  (java.util.function.Function<Integer, hydra.ext.python.syntax.Name>) (i -> new hydra.ext.python.syntax.Name(hydra.lib.strings.Cat2.apply(
                    "x",
                    hydra.lib.literals.ShowInt32.apply(i)))),
                  hydra.lib.math.Range.apply(
                    1,
                    numRemaining.get())));
                return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (() -> {
                  hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> remainingExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ext.python.syntax.Expression>) (n -> new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(n))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>emptyList()))))))))),
                    remainingParams.get()));
                  return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (() -> {
                    hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Expression>> allArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                      args,
                      remainingExprs.get()));
                    return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (() -> {
                      hydra.ext.python.syntax.Expression fullCall = hydra.ext.python.Utils.functionCall(
                        hydra.ext.python.Utils.pyNameToPyPrimary(hydra.ext.python.Names.encodeName(
                          true,
                          new hydra.util.CaseConvention.LowerSnake(),
                          env,
                          name)),
                        allArgs.get());
                      return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(hydra.ext.python.Coder.makeUncurriedLambda(
                        remainingParams.get(),
                        fullCall));
                    })).get();
                  })).get();
                })).get();
              })).get();
            })).get());
        }),
        hydra.Lexical.lookupPrimitive(
          g,
          name)),
      () -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.sets.Member.apply(
            name,
            tcLambdaVars),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asVariable),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.sets.Member.apply(
              name,
              inlineVars),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asVariable),
            () -> hydra.lib.maybes.Maybe.applyLazy(
              () -> hydra.lib.maybes.Maybe.applyLazy(
                () -> hydra.lib.maybes.Maybe.applyLazy(
                  () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
                    "Unknown variable: ",
                    (name).value))), cx))),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (ignored -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asFunctionCall)),
                  hydra.lib.maps.Lookup.apply(
                    name,
                    tcMetadata)),
                (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (el -> {
                  Boolean elTrivial1 = hydra.CoderUtils.isTrivialTerm((el).term);
                  return hydra.lib.maybes.Maybe.applyLazy(
                    () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asVariable),
                    (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (ts -> hydra.lib.logic.IfElse.lazy(
                      hydra.lib.logic.And.apply(
                        hydra.lib.logic.And.apply(
                          hydra.lib.equality.Equal.apply(
                            hydra.Arity.typeSchemeArity(ts),
                            0),
                          hydra.CoderUtils.isComplexBinding(
                            tc,
                            el)),
                        hydra.lib.logic.Not.apply(elTrivial1)),
                      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asFunctionCall),
                      () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (() -> {
                        hydra.util.Lazy<hydra.ext.python.syntax.Expression> asFunctionRef = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                          hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply((ts).variables)),
                          () -> hydra.ext.python.Coder.makeSimpleLambda(
                            hydra.Arity.typeArity((ts).type),
                            asVariable),
                          () -> asVariable));
                        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asFunctionRef.get());
                      })).get())),
                    (el).type);
                }),
                hydra.Lexical.lookupElement(
                  g,
                  name)),
              (java.util.function.Function<hydra.graph.Primitive, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (prim -> {
                Integer primArity = hydra.Arity.primitiveArity(prim);
                return hydra.lib.logic.IfElse.lazy(
                  hydra.lib.equality.Equal.apply(
                    primArity,
                    0),
                  () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asFunctionCall),
                  () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (() -> {
                    hydra.core.TypeScheme ts = (prim).type;
                    return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (() -> {
                      hydra.util.Lazy<hydra.ext.python.syntax.Expression> asFunctionRef = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                        hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply((ts).variables)),
                        () -> hydra.ext.python.Coder.makeSimpleLambda(
                          hydra.Arity.typeArity((ts).type),
                          asVariable),
                        () -> asVariable));
                      return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asFunctionRef.get());
                    })).get();
                  })).get());
              }),
              hydra.Lexical.lookupPrimitive(
                g,
                name)))),
        (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (typ -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.sets.Member.apply(
            name,
            tcLambdaVars),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asVariable),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.sets.Member.apply(
              name,
              inlineVars),
            () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (() -> {
              hydra.util.Lazy<hydra.ext.python.syntax.Expression> asFunctionRef = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.Variables.freeVariablesInType(typ))),
                () -> hydra.ext.python.Coder.makeSimpleLambda(
                  hydra.Arity.typeArity(typ),
                  asVariable),
                () -> asVariable));
              return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asFunctionRef.get());
            })).get(),
            () -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.Not.apply(hydra.lib.maps.Member.apply(
                name,
                tcMetadata)),
              () -> hydra.lib.maybes.Maybe.applyLazy(
                () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (() -> {
                  hydra.util.Lazy<hydra.ext.python.syntax.Expression> asFunctionRef = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                    hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.Variables.freeVariablesInType(typ))),
                    () -> hydra.ext.python.Coder.makeSimpleLambda(
                      hydra.Arity.typeArity(typ),
                      asVariable),
                    () -> asVariable));
                  return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asFunctionRef.get());
                })).get(),
                (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (el -> {
                  Boolean elTrivial = hydra.CoderUtils.isTrivialTerm((el).term);
                  return hydra.lib.maybes.Maybe.applyLazy(
                    () -> hydra.lib.logic.IfElse.lazy(
                      hydra.lib.logic.And.apply(
                        hydra.lib.equality.Equal.apply(
                          hydra.Arity.typeArity(typ),
                          0),
                        hydra.lib.logic.Not.apply(elTrivial)),
                      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asFunctionCall),
                      () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (() -> {
                        hydra.util.Lazy<hydra.ext.python.syntax.Expression> asFunctionRef = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                          hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.Variables.freeVariablesInType(typ))),
                          () -> hydra.ext.python.Coder.makeSimpleLambda(
                            hydra.Arity.typeArity(typ),
                            asVariable),
                          () -> asVariable));
                        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asFunctionRef.get());
                      })).get()),
                    (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (ts -> hydra.lib.logic.IfElse.lazy(
                      hydra.lib.logic.And.apply(
                        hydra.lib.logic.And.apply(
                          hydra.lib.equality.Equal.apply(
                            hydra.Arity.typeArity(typ),
                            0),
                          hydra.CoderUtils.isComplexBinding(
                            tc,
                            el)),
                        hydra.lib.logic.Not.apply(elTrivial)),
                      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asFunctionCall),
                      () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (() -> {
                        hydra.util.Lazy<hydra.ext.python.syntax.Expression> asFunctionRef = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                          hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.Variables.freeVariablesInType(typ))),
                          () -> hydra.ext.python.Coder.makeSimpleLambda(
                            hydra.Arity.typeArity(typ),
                            asVariable),
                          () -> asVariable));
                        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asFunctionRef.get());
                      })).get())),
                    (el).type);
                }),
                hydra.Lexical.lookupElement(
                  g,
                  name)),
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.logic.And.apply(
                  hydra.lib.equality.Equal.apply(
                    hydra.Arity.typeArity(typ),
                    0),
                  hydra.CoderUtils.isComplexVariable(
                    tc,
                    name)),
                () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asFunctionCall),
                () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>>) (() -> {
                  hydra.util.Lazy<hydra.ext.python.syntax.Expression> asFunctionRef = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                    hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.Variables.freeVariablesInType(typ))),
                    () -> hydra.ext.python.Coder.makeSimpleLambda(
                      hydra.Arity.typeArity(typ),
                      asVariable),
                    () -> asVariable));
                  return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.python.syntax.Expression>right(asFunctionRef.get());
                })).get()))))),
        mTyp.get()));
  }

  static <T0> hydra.util.Either<T0, java.util.List<hydra.ext.python.syntax.Statement>> encodeWrappedType(hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name name, hydra.core.Type typ, hydra.util.Maybe<String> comment) {
    hydra.util.Lazy<java.util.List<hydra.core.Name>> tparamList = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((env).boundTypeVariables));
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.python.Coder.<T0>encodeTypeQuoted(
        env,
        typ),
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.util.Either<T0, java.util.List<hydra.ext.python.syntax.Statement>>>) (ptypeQuoted -> {
        hydra.util.Lazy<hydra.ext.python.syntax.Block> body = new hydra.util.Lazy<>(() -> hydra.ext.python.Utils.indentedBlock(
          comment,
          (java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>) (java.util.Collections.<java.util.List<hydra.ext.python.syntax.Statement>>emptyList())));
        hydra.ext.python.syntax.Name pyName = hydra.ext.python.Names.encodeName(
          false,
          new hydra.util.CaseConvention.Pascal(),
          env,
          name);
        hydra.util.Lazy<hydra.ext.python.syntax.Statement> typeConstStmt = new hydra.util.Lazy<>(() -> hydra.ext.python.Utils.dottedAssignmentStatement(
          pyName,
          hydra.ext.python.Names.encodeConstantForTypeName(
            env,
            name),
          hydra.ext.python.Utils.functionCall(
            hydra.ext.python.Utils.pyNameToPyPrimary(hydra.ext.python.Names.encodeName(
              true,
              new hydra.util.CaseConvention.Pascal(),
              env,
              new hydra.core.Name("hydra.core.Name"))),
            java.util.Arrays.asList(hydra.ext.python.Utils.doubleQuotedString((name).value)))));
        return hydra.util.Either.<T0, java.util.List<hydra.ext.python.syntax.Statement>>right(java.util.Arrays.asList(
          hydra.ext.python.Utils.pyClassDefinitionToPyStatement(new hydra.ext.python.syntax.ClassDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), pyName, hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Name, hydra.ext.python.syntax.TypeParameter>) (arg_ -> hydra.ext.python.Utils.pyNameToPyTypeParameter(hydra.ext.python.Names.encodeTypeVariable(arg_))),
            hydra.ext.python.Coder.findTypeParams(
              env,
              typ)), hydra.util.Maybe.just(hydra.ext.python.Coder.variantArgs(
            ptypeQuoted,
            tparamList.get())), body.get())),
          typeConstStmt.get()));
      }));
  }

  static hydra.ext.python.syntax.ClosedPattern enumVariantPattern(hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name typeName, hydra.core.Name fieldName) {
    return new hydra.ext.python.syntax.ClosedPattern.Value(new hydra.ext.python.syntax.ValuePattern(new hydra.ext.python.syntax.Attribute(java.util.Arrays.asList(
      hydra.ext.python.Names.encodeName(
        true,
        new hydra.util.CaseConvention.Pascal(),
        env,
        typeName),
      hydra.ext.python.Names.encodeEnumValue(
        env,
        fieldName)))));
  }

  static java.util.List<hydra.ext.python.syntax.TypeParameter> environmentTypeParameters(hydra.ext.python.environment.PythonEnvironment env) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.ext.python.syntax.TypeParameter>) (arg_ -> hydra.ext.python.Utils.pyNameToPyTypeParameter(hydra.ext.python.Names.encodeTypeVariable(arg_))),
      hydra.lib.pairs.First.apply((env).boundTypeVariables));
  }

  static hydra.ext.python.environment.PythonEnvironment extendEnvWithLambdaParams(hydra.ext.python.environment.PythonEnvironment env, hydra.core.Term term) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, java.util.function.Function<hydra.core.Term, hydra.ext.python.environment.PythonEnvironment>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, java.util.function.Function<hydra.core.Term, hydra.ext.python.environment.PythonEnvironment>>) (e -> (java.util.function.Function<hydra.core.Term, hydra.ext.python.environment.PythonEnvironment>) (t -> hydra.Strip.deannotateAndDetypeTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.ext.python.environment.PythonEnvironment otherwise(hydra.core.Term instance) {
        return e;
      }

      @Override
      public hydra.ext.python.environment.PythonEnvironment visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.ext.python.environment.PythonEnvironment otherwise(hydra.core.Function instance) {
            return e;
          }

          @Override
          public hydra.ext.python.environment.PythonEnvironment visit(hydra.core.Function.Lambda lam) {
            hydra.graph.Graph newTc = hydra.Scoping.extendGraphForLambda(
              hydra.ext.python.Coder.pythonEnvironmentGetGraph(e),
              (lam).value);
            hydra.ext.python.environment.PythonEnvironment newEnv = hydra.ext.python.Coder.pythonEnvironmentSetGraph(
              newTc,
              e);
            return go.get().apply(newEnv).apply((lam).value.body);
          }
        });
      }
    }))));
    return go.get().apply(env).apply(term);
  }

  static hydra.ext.python.environment.PythonEnvironment extendEnvWithTypeVar(hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name var_) {
    hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> oldBound = (env).boundTypeVariables;
    hydra.util.Lazy<java.util.List<hydra.core.Name>> tparamList = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(oldBound));
    hydra.util.Lazy<java.util.List<hydra.core.Name>> newList = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
      tparamList.get(),
      java.util.Arrays.asList(var_)));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> tparamMap = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(oldBound));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> newMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.Insert.apply(
      var_,
      hydra.ext.python.Names.encodeTypeVariable(var_),
      tparamMap.get()));
    return new hydra.ext.python.environment.PythonEnvironment((env).namespaces, (hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>) ((hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>) (new hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>(newList.get(), newMap.get()))), (env).graph, (env).nullaryBindings, (env).version, (env).skipCasts, (env).inlineVariables);
  }

  static hydra.ext.python.environment.PythonModuleMetadata extendMetaForTerm(Boolean topLevel, hydra.ext.python.environment.PythonModuleMetadata meta0, hydra.core.Term term) {
    java.util.function.Function<hydra.ext.python.environment.PythonModuleMetadata, java.util.function.Function<hydra.core.Term, hydra.ext.python.environment.PythonModuleMetadata>> step = (java.util.function.Function<hydra.ext.python.environment.PythonModuleMetadata, java.util.function.Function<hydra.core.Term, hydra.ext.python.environment.PythonModuleMetadata>>) (meta -> (java.util.function.Function<hydra.core.Term, hydra.ext.python.environment.PythonModuleMetadata>) (t -> (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.ext.python.environment.PythonModuleMetadata otherwise(hydra.core.Term instance) {
        return meta;
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Term.Either e) {
        hydra.ext.python.environment.PythonModuleMetadata metaWithCast = hydra.ext.python.Coder.setMetaUsesCast(
          true,
          meta);
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.ext.python.environment.PythonModuleMetadata>) (ignored -> hydra.ext.python.Coder.setMetaUsesLeft(
            metaWithCast,
            true)),
          (java.util.function.Function<hydra.core.Term, hydra.ext.python.environment.PythonModuleMetadata>) (ignored -> hydra.ext.python.Coder.setMetaUsesRight(
            metaWithCast,
            true)),
          (e).value);
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.ext.python.environment.PythonModuleMetadata otherwise(hydra.core.Function instance) {
            return meta;
          }

          @Override
          public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Function.Lambda lam) {
            return hydra.lib.maybes.Maybe.applyLazy(
              () -> meta,
              (java.util.function.Function<hydra.core.Type, hydra.ext.python.environment.PythonModuleMetadata>) (dom -> hydra.lib.logic.IfElse.lazy(
                topLevel,
                () -> hydra.ext.python.Coder.extendMetaForType(
                  true,
                  false,
                  dom,
                  meta),
                () -> meta)),
              (lam).value.domain);
          }
        });
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Term.Let lt) {
        java.util.List<hydra.core.Binding> bindings = (lt).value.bindings;
        return hydra.lib.lists.Foldl.apply(
          ((java.util.function.Supplier<java.util.function.Function<hydra.ext.python.environment.PythonModuleMetadata, java.util.function.Function<hydra.core.Binding, hydra.ext.python.environment.PythonModuleMetadata>>>) (() -> {
            java.util.function.Function<hydra.ext.python.environment.PythonModuleMetadata, java.util.function.Function<hydra.core.Binding, hydra.ext.python.environment.PythonModuleMetadata>> forBinding = (java.util.function.Function<hydra.ext.python.environment.PythonModuleMetadata, java.util.function.Function<hydra.core.Binding, hydra.ext.python.environment.PythonModuleMetadata>>) (m -> (java.util.function.Function<hydra.core.Binding, hydra.ext.python.environment.PythonModuleMetadata>) (b -> hydra.lib.maybes.Maybe.applyLazy(
              () -> m,
              (java.util.function.Function<hydra.core.TypeScheme, hydra.ext.python.environment.PythonModuleMetadata>) (ts -> {
                hydra.core.Term term1 = (b).term;
                return hydra.lib.logic.IfElse.lazy(
                  hydra.CoderUtils.isSimpleAssignment(term1),
                  () -> m,
                  () -> hydra.ext.python.Coder.extendMetaForType(
                    true,
                    true,
                    (ts).type,
                    m));
              }),
              (b).type)));
            return forBinding;
          })).get(),
          meta,
          bindings);
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Term.Literal l) {
        return (l).value.accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public hydra.ext.python.environment.PythonModuleMetadata otherwise(hydra.core.Literal instance) {
            return meta;
          }

          @Override
          public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Literal.Float_ fv) {
            return (fv).value.accept(new hydra.core.FloatValue.PartialVisitor<>() {
              @Override
              public hydra.ext.python.environment.PythonModuleMetadata otherwise(hydra.core.FloatValue instance) {
                return meta;
              }

              @Override
              public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.FloatValue.Bigfloat ignored) {
                return hydra.ext.python.Coder.setMetaUsesDecimal(
                  meta,
                  true);
              }
            });
          }
        });
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Term.Map ignored) {
        return hydra.ext.python.Coder.setMetaUsesFrozenDict(
          meta,
          true);
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Term.Maybe m) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.ext.python.Coder.setMetaUsesNothing(
            meta,
            true),
          (java.util.function.Function<hydra.core.Term, hydra.ext.python.environment.PythonModuleMetadata>) (ignored -> hydra.ext.python.Coder.setMetaUsesJust(
            meta,
            true)),
          (m).value);
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Term.Union ignored) {
        return hydra.ext.python.Coder.setMetaUsesCast(
          true,
          meta);
      }
    })));
    return hydra.Rewriting.foldOverTerm(
      new hydra.coders.TraversalOrder.Pre(),
      step,
      meta0,
      term);
  }

  static hydra.ext.python.environment.PythonModuleMetadata extendMetaForType(Boolean topLevel, Boolean isTermAnnot, hydra.core.Type typ, hydra.ext.python.environment.PythonModuleMetadata meta) {
    java.util.Set<hydra.core.Name> currentTvars = (meta).typeVariables;
    java.util.Set<hydra.core.Name> newTvars = hydra.ext.python.Coder.collectTypeVariables(
      currentTvars,
      typ);
    hydra.ext.python.environment.PythonModuleMetadata metaWithTvars = hydra.ext.python.Coder.setMetaTypeVariables(
      meta,
      newTvars);
    hydra.util.Lazy<hydra.ext.python.environment.PythonModuleMetadata> metaWithSubtypes = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.ext.python.environment.PythonModuleMetadata, java.util.function.Function<hydra.core.Type, hydra.ext.python.environment.PythonModuleMetadata>>) (m -> (java.util.function.Function<hydra.core.Type, hydra.ext.python.environment.PythonModuleMetadata>) (t -> hydra.ext.python.Coder.extendMetaForType(
        false,
        isTermAnnot,
        t,
        m))),
      metaWithTvars,
      hydra.Rewriting.subtypes(typ)));
    return hydra.Strip.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.ext.python.environment.PythonModuleMetadata otherwise(hydra.core.Type instance) {
        return metaWithSubtypes.get();
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Type.Function ft) {
        hydra.core.Type cod = (ft).value.codomain;
        hydra.core.Type dom = (ft).value.domain;
        hydra.ext.python.environment.PythonModuleMetadata meta2 = hydra.ext.python.Coder.extendMetaForType(
          topLevel,
          isTermAnnot,
          cod,
          metaWithSubtypes.get());
        hydra.ext.python.environment.PythonModuleMetadata meta3 = hydra.ext.python.Coder.extendMetaForType(
          false,
          isTermAnnot,
          dom,
          meta2);
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.And.apply(
            isTermAnnot,
            topLevel),
          () -> meta3,
          () -> hydra.ext.python.Coder.setMetaUsesCallable(
            meta3,
            true));
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Type.List ignored) {
        return hydra.ext.python.Coder.setMetaUsesFrozenList(
          metaWithSubtypes.get(),
          true);
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Type.Map ignored) {
        return hydra.ext.python.Coder.setMetaUsesFrozenDict(
          metaWithSubtypes.get(),
          true);
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Type.Maybe ignored) {
        return hydra.ext.python.Coder.setMetaUsesMaybe(
          metaWithSubtypes.get(),
          true);
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Type.Either ignored) {
        return hydra.ext.python.Coder.setMetaUsesEither(
          metaWithSubtypes.get(),
          true);
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Type.Literal lt) {
        return (lt).value.accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public hydra.ext.python.environment.PythonModuleMetadata otherwise(hydra.core.LiteralType instance) {
            return metaWithSubtypes.get();
          }

          @Override
          public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.LiteralType.Float_ ft) {
            return (ft).value.accept(new hydra.core.FloatType.PartialVisitor<>() {
              @Override
              public hydra.ext.python.environment.PythonModuleMetadata otherwise(hydra.core.FloatType instance) {
                return metaWithSubtypes.get();
              }

              @Override
              public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.FloatType.Bigfloat ignored) {
                return hydra.ext.python.Coder.setMetaUsesDecimal(
                  metaWithSubtypes.get(),
                  true);
              }
            });
          }
        });
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Type.Union rt) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.Predicates.isEnumRowType((rt).value),
          () -> hydra.ext.python.Coder.setMetaUsesEnum(
            metaWithSubtypes.get(),
            true),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply((rt).value)),
            () -> hydra.ext.python.Coder.setMetaUsesNode(
              metaWithSubtypes.get(),
              true),
            () -> metaWithSubtypes.get()));
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Type.Forall ft) {
        hydra.core.Type body = (ft).value.body;
        hydra.ext.python.environment.PythonModuleMetadata metaForWrap = hydra.ext.python.Coder.digForWrap(
          isTermAnnot,
          metaWithSubtypes.get(),
          body);
        return hydra.Strip.deannotateType(body).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.ext.python.environment.PythonModuleMetadata otherwise(hydra.core.Type instance) {
            return metaForWrap;
          }

          @Override
          public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Type.Record ignored) {
            return hydra.ext.python.Coder.setMetaUsesGeneric(
              metaForWrap,
              true);
          }
        });
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Type.Record rt) {
        hydra.util.Lazy<Boolean> hasAnnotated = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.FieldType, Boolean>>) (b -> (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.lib.logic.Or.apply(
            b,
            hydra.Annotations.hasTypeDescription((ft).type)))),
          false,
          (rt).value));
        hydra.util.Lazy<hydra.ext.python.environment.PythonModuleMetadata> meta1 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply((rt).value),
          () -> metaWithSubtypes.get(),
          () -> hydra.ext.python.Coder.setMetaUsesDataclass(
            metaWithSubtypes.get(),
            true)));
        return hydra.lib.logic.IfElse.lazy(
          hasAnnotated.get(),
          () -> hydra.ext.python.Coder.setMetaUsesAnnotated(
            meta1.get(),
            true),
          () -> meta1.get());
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.core.Type.Wrap ignored) {
        return hydra.lib.logic.IfElse.lazy(
          isTermAnnot,
          () -> metaWithSubtypes.get(),
          () -> hydra.ext.python.Coder.setMetaUsesNode(
            metaWithSubtypes.get(),
            true));
      }
    });
  }

  static hydra.ext.python.environment.PythonModuleMetadata extendMetaForTypes(java.util.List<hydra.core.Type> types, hydra.ext.python.environment.PythonModuleMetadata meta) {
    hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> currentNs = (meta).namespaces;
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> names = new hydra.util.Lazy<>(() -> hydra.lib.sets.Unions.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>) (t -> hydra.Dependencies.typeDependencyNames(
        false,
        t)),
      types)));
    hydra.util.Lazy<hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>> updatedNs = new hydra.util.Lazy<>(() -> hydra.Analysis.addNamesToNamespaces(
      hydra.ext.python.Names::encodeNamespace,
      names.get(),
      currentNs));
    hydra.ext.python.environment.PythonModuleMetadata meta1 = hydra.ext.python.Coder.setMetaNamespaces(
      updatedNs.get(),
      meta);
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.ext.python.environment.PythonModuleMetadata, java.util.function.Function<hydra.core.Type, hydra.ext.python.environment.PythonModuleMetadata>>) (m -> (java.util.function.Function<hydra.core.Type, hydra.ext.python.environment.PythonModuleMetadata>) (t -> hydra.ext.python.Coder.extendMetaForType(
        true,
        false,
        t,
        m))),
      meta1,
      types);
  }

  static hydra.util.Maybe<hydra.core.CaseStatement> extractCaseElimination(hydra.core.Term term) {
    return hydra.Strip.deannotateAndDetypeTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.CaseStatement> otherwise(hydra.core.Term instance) {
        return (hydra.util.Maybe<hydra.core.CaseStatement>) (hydra.util.Maybe.<hydra.core.CaseStatement>nothing());
      }

      @Override
      public hydra.util.Maybe<hydra.core.CaseStatement> visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.core.CaseStatement> otherwise(hydra.core.Function instance) {
            return (hydra.util.Maybe<hydra.core.CaseStatement>) (hydra.util.Maybe.<hydra.core.CaseStatement>nothing());
          }

          @Override
          public hydra.util.Maybe<hydra.core.CaseStatement> visit(hydra.core.Function.Elimination e) {
            return (e).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public hydra.util.Maybe<hydra.core.CaseStatement> otherwise(hydra.core.Elimination instance) {
                return (hydra.util.Maybe<hydra.core.CaseStatement>) (hydra.util.Maybe.<hydra.core.CaseStatement>nothing());
              }

              @Override
              public hydra.util.Maybe<hydra.core.CaseStatement> visit(hydra.core.Elimination.Union cs) {
                return hydra.util.Maybe.just((cs).value);
              }
            });
          }
        });
      }
    });
  }

  static java.util.List<hydra.core.Name> findTypeParams(hydra.ext.python.environment.PythonEnvironment env, hydra.core.Type typ) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> boundVars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((env).boundTypeVariables));
    java.util.function.Function<hydra.core.Name, Boolean> isBound = (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.maybes.IsJust.apply(hydra.lib.maps.Lookup.apply(
      v,
      boundVars.get())));
    return hydra.lib.lists.Filter.apply(
      isBound,
      hydra.lib.sets.ToList.apply(hydra.Variables.freeVariablesInType(typ)));
  }

  static Integer functionArityWithPrimitives(hydra.graph.Graph graph, hydra.core.Function f) {
    return (f).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Function instance) {
        return 0;
      }

      @Override
      public Integer visit(hydra.core.Function.Elimination ignored) {
        return 1;
      }

      @Override
      public Integer visit(hydra.core.Function.Lambda lam) {
        return hydra.lib.math.Add.apply(
          1,
          hydra.ext.python.Coder.termArityWithPrimitives(
            graph,
            (lam).value.body));
      }

      @Override
      public Integer visit(hydra.core.Function.Primitive name) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> 0,
          (java.util.function.Function<hydra.graph.Primitive, Integer>) (prim -> hydra.Arity.primitiveArity(prim)),
          hydra.lib.maps.Lookup.apply(
            (name).value,
            (graph).primitives));
      }
    });
  }

  static hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Term> gatherLambdas(hydra.core.Term term) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Term>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Term>>>) (params -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Term>>) (t -> hydra.Strip.deannotateAndDetypeTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Term>) ((hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Term>) (new hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Term>(params, t)));
      }

      @Override
      public hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Term> visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Term> otherwise(hydra.core.Function instance) {
            return (hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Term>) ((hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Term>) (new hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Term>(params, t)));
          }

          @Override
          public hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Term> visit(hydra.core.Function.Lambda l) {
            return go.get().apply(hydra.lib.lists.Concat2.apply(
              params,
              java.util.Arrays.asList((l).value.parameter))).apply((l).value.body);
          }
        });
      }
    }))));
    return go.get().apply((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList())).apply(term);
  }

  static hydra.ext.python.environment.PythonModuleMetadata gatherMetadata(hydra.module.Namespace focusNs, java.util.List<hydra.module.Definition> defs) {
    java.util.function.Function<hydra.ext.python.environment.PythonModuleMetadata, java.util.function.Function<hydra.module.Definition, hydra.ext.python.environment.PythonModuleMetadata>> addDef = (java.util.function.Function<hydra.ext.python.environment.PythonModuleMetadata, java.util.function.Function<hydra.module.Definition, hydra.ext.python.environment.PythonModuleMetadata>>) (meta -> (java.util.function.Function<hydra.module.Definition, hydra.ext.python.environment.PythonModuleMetadata>) (def -> (def).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.module.Definition.Term termDef) {
        hydra.util.Lazy<hydra.core.Type> typ = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
          () -> new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Unit")),
          projected -> projected.type,
          (termDef).value.type));
        hydra.ext.python.environment.PythonModuleMetadata meta2 = hydra.ext.python.Coder.extendMetaForType(
          true,
          true,
          typ.get(),
          meta);
        hydra.core.Term term = (termDef).value.term;
        return hydra.ext.python.Coder.extendMetaForTerm(
          true,
          meta2,
          term);
      }

      @Override
      public hydra.ext.python.environment.PythonModuleMetadata visit(hydra.module.Definition.Type typeDef) {
        hydra.ext.python.environment.PythonModuleMetadata meta2 = hydra.ext.python.Coder.setMetaUsesName(
          meta,
          true);
        hydra.core.Type typ = (typeDef).value.type;
        return hydra.Rewriting.foldOverType(
          new hydra.coders.TraversalOrder.Pre(),
          (java.util.function.Function<hydra.ext.python.environment.PythonModuleMetadata, java.util.function.Function<hydra.core.Type, hydra.ext.python.environment.PythonModuleMetadata>>) (m -> (java.util.function.Function<hydra.core.Type, hydra.ext.python.environment.PythonModuleMetadata>) (t -> hydra.ext.python.Coder.extendMetaForType(
            true,
            false,
            t,
            m))),
          meta2,
          typ);
      }
    })));
    hydra.ext.python.environment.PythonModuleMetadata start = hydra.ext.python.Coder.emptyMetadata(hydra.ext.python.Utils.findNamespaces(
      focusNs,
      defs));
    hydra.util.Lazy<hydra.ext.python.environment.PythonModuleMetadata> result = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      addDef,
      start,
      defs));
    hydra.ext.python.environment.PythonModuleMetadata result2 = hydra.ext.python.Coder.setMetaUsesCast(
      true,
      hydra.ext.python.Coder.setMetaUsesLruCache(
        true,
        result.get()));
    java.util.Set<hydra.core.Name> tvars = result.get().typeVariables;
    return hydra.ext.python.Coder.setMetaUsesTypeVar(
      result2,
      hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(tvars)));
  }

  static hydra.util.Maybe<hydra.ext.python.syntax.Expression> genericArg(java.util.List<hydra.core.Name> tparamList) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(tparamList),
      () -> (hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing()),
      () -> hydra.util.Maybe.just(hydra.ext.python.Utils.pyPrimaryToPyExpression(hydra.ext.python.Utils.primaryWithExpressionSlices(
        new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("Generic"))),
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Name, hydra.ext.python.syntax.Expression>) (n -> new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(hydra.ext.python.Names.encodeTypeVariable(n)))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>emptyList()))))))))),
          tparamList)))));
  }

  static hydra.ext.python.environment.PythonEnvironment initialEnvironment(hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces, hydra.graph.Graph tcontext) {
    return new hydra.ext.python.environment.PythonEnvironment(namespaces, (hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>) ((hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>) (new hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), (java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>) ((java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.ext.python.syntax.Name>apply()))))), tcontext, (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), hydra.ext.python.Coder.targetPythonVersion(), true, (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()));
  }

  static hydra.ext.python.environment.PythonModuleMetadata initialMetadata(hydra.module.Namespace ns) {
    hydra.ext.python.syntax.DottedName dottedNs = hydra.ext.python.Names.encodeNamespace(ns);
    hydra.util.Lazy<hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>> emptyNs = new hydra.util.Lazy<>(() -> (hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>) (new hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>((hydra.util.Pair<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>) ((hydra.util.Pair<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>) (new hydra.util.Pair<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>(ns, dottedNs))), (java.util.Map<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>) ((java.util.Map<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>) (hydra.lib.maps.Empty.<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>apply())))));
    return new hydra.ext.python.environment.PythonModuleMetadata(emptyNs.get(), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false);
  }

  static hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>> isCaseStatementApplication(hydra.core.Term term) {
    hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.core.Term> gathered = hydra.CoderUtils.gatherApplications(term);
    hydra.util.Lazy<java.util.List<hydra.core.Term>> args = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(gathered));
    hydra.util.Lazy<hydra.core.Term> body = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(gathered));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply(args.get()),
        1)),
      () -> (hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>>nothing()),
      () -> ((java.util.function.Supplier<hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>>>) (() -> {
        hydra.util.Lazy<hydra.core.Term> arg = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(args.get()));
        return hydra.Strip.deannotateAndDetypeTerm(body.get()).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>> otherwise(hydra.core.Term instance) {
            return (hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>>nothing());
          }

          @Override
          public hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>> visit(hydra.core.Term.Function f) {
            return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
              @Override
              public hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>> otherwise(hydra.core.Function instance) {
                return (hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>>nothing());
              }

              @Override
              public hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>> visit(hydra.core.Function.Elimination e) {
                return (e).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
                  @Override
                  public hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>> otherwise(hydra.core.Elimination instance) {
                    return (hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>>nothing());
                  }

                  @Override
                  public hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>> visit(hydra.core.Elimination.Union cs) {
                    return hydra.util.Maybe.just((hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>>((cs).value.typeName, (hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>) ((hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>) (new hydra.util.Pair<hydra.util.Maybe<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>>((cs).value.default_, (hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>) ((hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>) (new hydra.util.Pair<java.util.List<hydra.core.Field>, hydra.core.Term>((cs).value.cases, arg.get()))))))))));
                  }
                });
              }
            });
          }
        });
      })).get());
  }

  static <T0, T1> Boolean isCasesFull(java.util.List<T0> rowType, java.util.List<T1> cases_) {
    hydra.util.Lazy<Integer> numCases = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(cases_));
    hydra.util.Lazy<Integer> numFields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(rowType));
    return hydra.lib.logic.Not.apply(hydra.lib.equality.Lt.apply(
      numCases.get(),
      numFields.get()));
  }

  static Boolean isTypeModuleCheck(java.util.List<hydra.module.Definition> defs) {
    return hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.module.Definition, Boolean>) (d -> (d).accept(new hydra.module.Definition.PartialVisitor<>() {
        @Override
        public Boolean otherwise(hydra.module.Definition instance) {
          return false;
        }

        @Override
        public Boolean visit(hydra.module.Definition.Type ignored) {
          return true;
        }
      })),
      defs)));
  }

  static Boolean isTypeVariableName(hydra.core.Name name) {
    return hydra.lib.equality.Equal.apply(
      1,
      hydra.lib.lists.Length.apply(hydra.lib.strings.SplitOn.apply(
        ".",
        (name).value)));
  }

  static Boolean isVariantUnitType(java.util.List<hydra.core.FieldType> rowType, hydra.core.Name fieldName) {
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.FieldType>> mfield = new hydra.util.Lazy<>(() -> hydra.lib.lists.Find.apply(
      (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.lib.equality.Equal.apply(
        (ft).name,
        fieldName)),
      rowType));
    return hydra.lib.maybes.FromMaybe.applyLazy(
      () -> false,
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.Predicates.isUnitType(hydra.Strip.deannotateType((ft).type))),
        mfield.get()));
  }

  static hydra.ext.python.syntax.NamedExpression lruCacheDecorator() {
    return new hydra.ext.python.syntax.NamedExpression.Simple(hydra.ext.python.Utils.functionCall(
      new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("lru_cache"))),
      java.util.Arrays.asList(hydra.ext.python.Coder.pyInt(new java.math.BigInteger("1")))));
  }

  static hydra.ext.python.syntax.Expression makeCurriedLambda(java.util.List<hydra.ext.python.syntax.Name> params, hydra.ext.python.syntax.Expression body) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.ext.python.syntax.Expression, java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ext.python.syntax.Expression>>) (acc -> (java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ext.python.syntax.Expression>) (p -> new hydra.ext.python.syntax.Expression.Lambda(new hydra.ext.python.syntax.Lambda(new hydra.ext.python.syntax.LambdaParameters((hydra.util.Maybe<hydra.ext.python.syntax.LambdaSlashNoDefault>) (hydra.util.Maybe.<hydra.ext.python.syntax.LambdaSlashNoDefault>nothing()), java.util.Arrays.asList(new hydra.ext.python.syntax.LambdaParamNoDefault(p)), (java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault>) (java.util.Collections.<hydra.ext.python.syntax.LambdaParamWithDefault>emptyList()), (hydra.util.Maybe<hydra.ext.python.syntax.LambdaStarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.LambdaStarEtc>nothing())), acc)))),
      body,
      hydra.lib.lists.Reverse.apply(params));
  }

  static hydra.ext.python.environment.PyGraph makePyGraph(hydra.graph.Graph g, hydra.ext.python.environment.PythonModuleMetadata m) {
    return new hydra.ext.python.environment.PyGraph(g, m);
  }

  static hydra.ext.python.syntax.Expression makeSimpleLambda(Integer arity, hydra.ext.python.syntax.Expression lhs) {
    hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Name>> args = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<Integer, hydra.ext.python.syntax.Name>) (i -> new hydra.ext.python.syntax.Name(hydra.lib.strings.Cat2.apply(
        "x",
        hydra.lib.literals.ShowInt32.apply(i)))),
      hydra.lib.math.Range.apply(
        1,
        arity)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        arity,
        0),
      () -> lhs,
      () -> new hydra.ext.python.syntax.Expression.Lambda(new hydra.ext.python.syntax.Lambda(new hydra.ext.python.syntax.LambdaParameters((hydra.util.Maybe<hydra.ext.python.syntax.LambdaSlashNoDefault>) (hydra.util.Maybe.<hydra.ext.python.syntax.LambdaSlashNoDefault>nothing()), hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ext.python.syntax.LambdaParamNoDefault>) (a -> new hydra.ext.python.syntax.LambdaParamNoDefault(a)),
        args.get()), (java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault>) (java.util.Collections.<hydra.ext.python.syntax.LambdaParamWithDefault>emptyList()), (hydra.util.Maybe<hydra.ext.python.syntax.LambdaStarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.LambdaStarEtc>nothing())), hydra.ext.python.Utils.functionCall(
        hydra.ext.python.Utils.pyExpressionToPyPrimary(lhs),
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ext.python.syntax.Expression>) (a -> new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(a))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>emptyList()))))))))),
          args.get())))));
  }

  static hydra.ext.python.syntax.Expression makeThunk(hydra.ext.python.syntax.Expression pbody) {
    return hydra.ext.python.Utils.functionCall(
      hydra.ext.python.Utils.pyExpressionToPyPrimary(hydra.ext.python.Utils.functionCall(
        new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("lru_cache"))),
        java.util.Arrays.asList(hydra.ext.python.Coder.pyInt(new java.math.BigInteger("1"))))),
      java.util.Arrays.asList(hydra.ext.python.Coder.wrapInNullaryLambda(pbody)));
  }

  static hydra.ext.python.syntax.Expression makeUncurriedLambda(java.util.List<hydra.ext.python.syntax.Name> params, hydra.ext.python.syntax.Expression body) {
    return new hydra.ext.python.syntax.Expression.Lambda(new hydra.ext.python.syntax.Lambda(new hydra.ext.python.syntax.LambdaParameters((hydra.util.Maybe<hydra.ext.python.syntax.LambdaSlashNoDefault>) (hydra.util.Maybe.<hydra.ext.python.syntax.LambdaSlashNoDefault>nothing()), hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ext.python.syntax.LambdaParamNoDefault>) (p -> new hydra.ext.python.syntax.LambdaParamNoDefault(p)),
      params), (java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault>) (java.util.Collections.<hydra.ext.python.syntax.LambdaParamWithDefault>emptyList()), (hydra.util.Maybe<hydra.ext.python.syntax.LambdaStarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.LambdaStarEtc>nothing())), body));
  }

  static java.util.List<hydra.ext.python.syntax.ImportStatement> moduleDomainImports(hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces) {
    hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.DottedName>> names = new hydra.util.Lazy<>(() -> hydra.lib.lists.Sort.apply(hydra.lib.maps.Elems.apply(((java.util.function.Function<hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>, java.util.Map<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>>) (projected -> projected.mapping)).apply(namespaces))));
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.python.syntax.DottedName, hydra.ext.python.syntax.ImportStatement>) (ns -> new hydra.ext.python.syntax.ImportStatement.Name(new hydra.ext.python.syntax.ImportName(java.util.Arrays.asList(new hydra.ext.python.syntax.DottedAsName(ns, (hydra.util.Maybe<hydra.ext.python.syntax.Name>) (hydra.util.Maybe.<hydra.ext.python.syntax.Name>nothing())))))),
      names.get());
  }

  static java.util.List<hydra.ext.python.syntax.Statement> moduleImports(hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces, hydra.ext.python.environment.PythonModuleMetadata meta) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.python.syntax.ImportStatement, hydra.ext.python.syntax.Statement>) (imp -> hydra.ext.python.Utils.pySimpleStatementToPyStatement(new hydra.ext.python.syntax.SimpleStatement.Import(imp))),
      hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
        hydra.ext.python.Coder.moduleStandardImports(meta),
        hydra.ext.python.Coder.moduleDomainImports(namespaces))));
  }

  static java.util.List<hydra.ext.python.syntax.ImportStatement> moduleStandardImports(hydra.ext.python.environment.PythonModuleMetadata meta) {
    hydra.util.Lazy<java.util.List<hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>>> pairs = new hydra.util.Lazy<>(() -> java.util.Arrays.asList(
      (hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>) ((hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>) (new hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>("__future__", java.util.Arrays.asList(hydra.ext.python.Coder.condImportSymbol(
        "annotations",
        hydra.ext.python.Names.useFutureAnnotations()))))),
      (hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>) ((hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>) (new hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>("collections.abc", java.util.Arrays.asList(hydra.ext.python.Coder.condImportSymbol(
        "Callable",
        (meta).usesCallable))))),
      (hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>) ((hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>) (new hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>("dataclasses", java.util.Arrays.asList(hydra.ext.python.Coder.condImportSymbol(
        "dataclass",
        (meta).usesDataclass))))),
      (hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>) ((hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>) (new hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>("decimal", java.util.Arrays.asList(hydra.ext.python.Coder.condImportSymbol(
        "Decimal",
        (meta).usesDecimal))))),
      (hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>) ((hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>) (new hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>("enum", java.util.Arrays.asList(hydra.ext.python.Coder.condImportSymbol(
        "Enum",
        (meta).usesEnum))))),
      (hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>) ((hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>) (new hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>("functools", java.util.Arrays.asList(hydra.ext.python.Coder.condImportSymbol(
        "lru_cache",
        (meta).usesLruCache))))),
      (hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>) ((hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>) (new hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>("hydra.dsl.python", java.util.Arrays.asList(
        hydra.ext.python.Coder.condImportSymbol(
          "Either",
          (meta).usesEither),
        hydra.ext.python.Coder.condImportSymbol(
          "FrozenDict",
          (meta).usesFrozenDict),
        hydra.ext.python.Coder.condImportSymbol(
          "Just",
          (meta).usesJust),
        hydra.ext.python.Coder.condImportSymbol(
          "Left",
          (meta).usesLeft),
        hydra.ext.python.Coder.condImportSymbol(
          "Maybe",
          (meta).usesMaybe),
        hydra.ext.python.Coder.condImportSymbol(
          "Node",
          (meta).usesNode),
        hydra.ext.python.Coder.condImportSymbol(
          "Nothing",
          (meta).usesNothing),
        hydra.ext.python.Coder.condImportSymbol(
          "Right",
          (meta).usesRight),
        hydra.ext.python.Coder.condImportSymbol(
          "frozenlist",
          (meta).usesFrozenList))))),
      (hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>) ((hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>) (new hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>("typing", java.util.Arrays.asList(
        hydra.ext.python.Coder.condImportSymbol(
          "Annotated",
          (meta).usesAnnotated),
        hydra.ext.python.Coder.condImportSymbol(
          "Generic",
          (meta).usesGeneric),
        hydra.ext.python.Coder.condImportSymbol(
          "TypeAlias",
          (meta).usesTypeAlias),
        hydra.ext.python.Coder.condImportSymbol(
          "TypeVar",
          (meta).usesTypeVar),
        hydra.ext.python.Coder.condImportSymbol(
          "cast",
          (meta).usesCast)))))));
    hydra.util.Lazy<java.util.List<hydra.util.Pair<String, java.util.List<String>>>> simplified = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<String, java.util.List<hydra.util.Maybe<String>>>, hydra.util.Maybe<hydra.util.Pair<String, java.util.List<String>>>>) (p -> {
        hydra.util.Lazy<String> modName = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
        hydra.util.Lazy<java.util.List<String>> symbols = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.lib.pairs.Second.apply(p)));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(symbols.get()),
          () -> (hydra.util.Maybe<hydra.util.Pair<String, java.util.List<String>>>) (hydra.util.Maybe.<hydra.util.Pair<String, java.util.List<String>>>nothing()),
          () -> hydra.util.Maybe.just((hydra.util.Pair<String, java.util.List<String>>) ((hydra.util.Pair<String, java.util.List<String>>) (new hydra.util.Pair<String, java.util.List<String>>(modName.get(), symbols.get())))));
      }),
      pairs.get())));
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<String, java.util.List<String>>, hydra.ext.python.syntax.ImportStatement>) (p -> hydra.ext.python.Coder.standardImportStatement(
        hydra.lib.pairs.First.apply(p),
        hydra.lib.pairs.Second.apply(p))),
      simplified.get());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.Map<String, String>> moduleToPython(hydra.module.Module mod, java.util.List<hydra.module.Definition> defs, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.python.Coder.encodePythonModule(
        cx,
        g,
        mod,
        defs),
      (java.util.function.Function<hydra.ext.python.syntax.Module, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.Map<String, String>>>) (file -> {
        String path = hydra.Names.namespaceToFilePath(
          new hydra.util.CaseConvention.LowerSnake(),
          new hydra.module.FileExtension("py"),
          (mod).namespace);
        String s = hydra.Serialization.printExpr(hydra.Serialization.parenthesize(hydra.ext.python.Serde.encodeModule(file)));
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.Map<String, String>>right(hydra.lib.maps.Singleton.apply(
          path,
          s));
      }));
  }

  static hydra.graph.Graph pyGraphGraph(hydra.ext.python.environment.PyGraph pyg) {
    return (pyg).graph;
  }

  static hydra.ext.python.environment.PythonModuleMetadata pyGraphMetadata(hydra.ext.python.environment.PyGraph pyg) {
    return (pyg).metadata;
  }

  static hydra.ext.python.syntax.Expression pyInt(java.math.BigInteger n) {
    return hydra.ext.python.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Number_(new hydra.ext.python.syntax.Number_.Integer_(n)));
  }

  static hydra.util.Maybe<hydra.core.Term> pythonBindingMetadata(hydra.graph.Graph g, hydra.core.Binding b) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.ext.python.Coder.shouldThunkBinding(
        g,
        b),
      () -> hydra.CoderUtils.bindingMetadata(
        g,
        b),
      () -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()));
  }

  static hydra.graph.Graph pythonEnvironmentGetGraph(hydra.ext.python.environment.PythonEnvironment env) {
    return (env).graph;
  }

  static hydra.ext.python.environment.PythonEnvironment pythonEnvironmentSetGraph(hydra.graph.Graph tc, hydra.ext.python.environment.PythonEnvironment env) {
    return new hydra.ext.python.environment.PythonEnvironment((env).namespaces, (env).boundTypeVariables, tc, (env).nullaryBindings, (env).version, (env).skipCasts, (env).inlineVariables);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaNamespaces(hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> ns, hydra.ext.python.environment.PythonModuleMetadata m) {
    return new hydra.ext.python.environment.PythonModuleMetadata(ns, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaTypeVariables(hydra.ext.python.environment.PythonModuleMetadata m, java.util.Set<hydra.core.Name> tvars) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, tvars, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesAnnotated(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, b, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesCallable(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, b, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesCast(Boolean b, hydra.ext.python.environment.PythonModuleMetadata m) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, b, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesDataclass(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, b, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesDecimal(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, b, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesEither(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, b, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesEnum(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, b, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesFrozenDict(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, b, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesFrozenList(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, b, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesGeneric(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, b, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesJust(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, b, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesLeft(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, b, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesLruCache(Boolean b, hydra.ext.python.environment.PythonModuleMetadata m) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, b, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesMaybe(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, b, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesName(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, b, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesNode(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, b, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesNothing(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, b, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesRight(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, b, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesTypeAlias(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, b, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, (m).usesTypeVar);
  }

  static hydra.ext.python.environment.PythonModuleMetadata setMetaUsesTypeVar(hydra.ext.python.environment.PythonModuleMetadata m, Boolean b) {
    return new hydra.ext.python.environment.PythonModuleMetadata((m).namespaces, (m).typeVariables, (m).usesAnnotated, (m).usesCallable, (m).usesCast, (m).usesLruCache, (m).usesTypeAlias, (m).usesDataclass, (m).usesDecimal, (m).usesEither, (m).usesEnum, (m).usesFrozenDict, (m).usesFrozenList, (m).usesGeneric, (m).usesJust, (m).usesLeft, (m).usesMaybe, (m).usesName, (m).usesNode, (m).usesNothing, (m).usesRight, b);
  }

  static Boolean shouldThunkBinding(hydra.graph.Graph g, hydra.core.Binding b) {
    return hydra.lib.logic.And.apply(
      hydra.CoderUtils.isComplexBinding(
        g,
        b),
      hydra.lib.logic.Not.apply(hydra.CoderUtils.isTrivialTerm((b).term)));
  }

  static hydra.ext.python.syntax.ImportStatement standardImportStatement(String modName, java.util.List<String> symbols) {
    return new hydra.ext.python.syntax.ImportStatement.From(new hydra.ext.python.syntax.ImportFrom((java.util.List<hydra.ext.python.syntax.RelativeImportPrefix>) (java.util.Collections.<hydra.ext.python.syntax.RelativeImportPrefix>emptyList()), hydra.util.Maybe.just(new hydra.ext.python.syntax.DottedName(java.util.Arrays.asList(new hydra.ext.python.syntax.Name(modName)))), new hydra.ext.python.syntax.ImportFromTargets.Simple(hydra.lib.lists.Map.apply(
      (java.util.function.Function<String, hydra.ext.python.syntax.ImportFromAsName>) (s -> new hydra.ext.python.syntax.ImportFromAsName(new hydra.ext.python.syntax.Name(s), (hydra.util.Maybe<hydra.ext.python.syntax.Name>) (hydra.util.Maybe.<hydra.ext.python.syntax.Name>nothing()))),
      symbols))));
  }

  static hydra.ext.python.environment.PythonVersion targetPythonVersion() {
    return hydra.ext.python.Utils.targetPythonVersion();
  }

  static Integer termArityWithPrimitives(hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.Strip.deannotateAndDetypeTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Term instance) {
        return 0;
      }

      @Override
      public Integer visit(hydra.core.Term.Application app) {
        return hydra.lib.math.Max.apply(
          0,
          hydra.lib.math.Sub.apply(
            hydra.ext.python.Coder.termArityWithPrimitives(
              graph,
              (app).value.function),
            1));
      }

      @Override
      public Integer visit(hydra.core.Term.Function f) {
        return hydra.ext.python.Coder.functionArityWithPrimitives(
          graph,
          (f).value);
      }

      @Override
      public Integer visit(hydra.core.Term.Variable name) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> 0,
          (java.util.function.Function<hydra.core.Binding, Integer>) (el -> hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.Arity.termArity((el).term),
            (java.util.function.Function<hydra.core.TypeScheme, Integer>) (ts -> hydra.Arity.typeSchemeArity(ts)),
            (el).type)),
          hydra.Lexical.lookupElement(
            graph,
            (name).value));
      }
    });
  }

  static hydra.ext.python.syntax.Statement tvarStatement(hydra.ext.python.syntax.Name name) {
    return hydra.ext.python.Utils.assignmentStatement(
      name,
      hydra.ext.python.Utils.functionCall(
        new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("TypeVar"))),
        java.util.Arrays.asList(hydra.ext.python.Utils.doubleQuotedString((name).value))));
  }

  static hydra.ext.python.syntax.Statement typeAliasStatementFor(hydra.ext.python.environment.PythonEnvironment env, hydra.ext.python.syntax.Name name, java.util.List<hydra.ext.python.syntax.TypeParameter> tparams, hydra.util.Maybe<String> mcomment, hydra.ext.python.syntax.Expression tyexpr) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.ext.python.Coder.useInlineTypeParamsFor((env).version),
      () -> hydra.ext.python.Utils.typeAliasStatement(
        name,
        tparams,
        mcomment,
        tyexpr),
      () -> hydra.ext.python.Utils.typeAliasStatement310(
        name,
        tparams,
        mcomment,
        tyexpr));
  }

  static java.util.List<hydra.ext.python.syntax.Statement> unionTypeStatementsFor(hydra.ext.python.environment.PythonEnvironment env, hydra.ext.python.syntax.Name name, java.util.List<hydra.ext.python.syntax.TypeParameter> tparams, hydra.util.Maybe<String> mcomment, hydra.ext.python.syntax.Expression tyexpr, java.util.List<hydra.ext.python.syntax.Statement> extraStmts) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.ext.python.Coder.useInlineTypeParamsFor((env).version),
      () -> hydra.lib.lists.Concat2.apply(
        java.util.Arrays.asList(hydra.ext.python.Utils.typeAliasStatement(
          name,
          tparams,
          mcomment,
          tyexpr)),
        extraStmts),
      () -> hydra.ext.python.Utils.unionTypeClassStatements310(
        name,
        mcomment,
        tyexpr,
        extraStmts));
  }

  static hydra.ext.python.syntax.Expression unsupportedExpression(String msg) {
    return hydra.ext.python.Utils.functionCall(
      hydra.ext.python.Utils.pyExpressionToPyPrimary(hydra.ext.python.Utils.projectFromExpression(
        hydra.ext.python.Utils.projectFromExpression(
          hydra.ext.python.Utils.projectFromExpression(
            new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("hydra")))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>emptyList())))))))),
            new hydra.ext.python.syntax.Name("dsl")),
          new hydra.ext.python.syntax.Name("python")),
        new hydra.ext.python.syntax.Name("unsupported"))),
      java.util.Arrays.asList(hydra.ext.python.Utils.stringToPyExpression(
        new hydra.ext.python.syntax.QuoteStyle.Double_(),
        msg)));
  }

  static Boolean useInlineTypeParams() {
    return hydra.ext.python.Coder.useInlineTypeParamsFor(hydra.ext.python.Utils.targetPythonVersion());
  }

  static Boolean useInlineTypeParamsFor(hydra.ext.python.environment.PythonVersion version) {
    return hydra.lib.equality.Equal.apply(
      version,
      new hydra.ext.python.environment.PythonVersion.Python312());
  }

  static hydra.ext.python.syntax.Args variantArgs(hydra.ext.python.syntax.Expression ptype, java.util.List<hydra.core.Name> tparams) {
    return hydra.ext.python.Utils.pyExpressionsToPyArgs(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.ext.python.Utils.pyPrimaryToPyExpression(hydra.ext.python.Utils.primaryWithExpressionSlices(
        new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("Node"))),
        java.util.Arrays.asList(ptype)))),
      hydra.ext.python.Coder.genericArg(tparams))));
  }

  static <T0> hydra.ext.python.syntax.ClosedPattern variantClosedPattern(hydra.ext.python.environment.PythonEnvironment env, hydra.core.Name typeName, hydra.core.Name fieldName, hydra.ext.python.syntax.Name pyVariantName, T0 rowType, Boolean isEnum, hydra.core.Name varName, Boolean shouldCapture) {
    return hydra.lib.logic.IfElse.lazy(
      isEnum,
      () -> hydra.ext.python.Coder.enumVariantPattern(
        env,
        typeName,
        fieldName),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.logic.Not.apply(shouldCapture),
        () -> hydra.ext.python.Coder.classVariantPatternUnit(pyVariantName),
        () -> hydra.ext.python.Coder.classVariantPatternWithCapture(
          env,
          pyVariantName,
          varName)));
  }

  static hydra.ext.python.syntax.CaseBlock wildcardCaseBlock(hydra.ext.python.syntax.Statement stmt) {
    return new hydra.ext.python.syntax.CaseBlock(hydra.ext.python.Utils.pyClosedPatternToPyPatterns(new hydra.ext.python.syntax.ClosedPattern.Wildcard()), (hydra.util.Maybe<hydra.ext.python.syntax.Guard>) (hydra.util.Maybe.<hydra.ext.python.syntax.Guard>nothing()), hydra.ext.python.Utils.indentedBlock(
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      java.util.Arrays.asList(java.util.Arrays.asList(stmt))));
  }

  static <T0> T0 withDefinitions(hydra.ext.python.environment.PythonEnvironment env, java.util.List<hydra.module.Definition> defs, java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, T0> body) {
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.core.Binding>>) (def_ -> (def_).accept(new hydra.module.Definition.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<hydra.core.Binding> otherwise(hydra.module.Definition instance) {
          return (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing());
        }

        @Override
        public hydra.util.Maybe<hydra.core.Binding> visit(hydra.module.Definition.Term td) {
          return hydra.util.Maybe.just(new hydra.core.Binding((td).value.name, (td).value.term, (td).value.type));
        }

        @Override
        public hydra.util.Maybe<hydra.core.Binding> visit(hydra.module.Definition.Type ignored) {
          return (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing());
        }
      })),
      defs)));
    hydra.core.Let dummyLet = new hydra.core.Let(bindings.get(), new hydra.core.Term.Literal(new hydra.core.Literal.String_("dummy")));
    return hydra.ext.python.Coder.<T0>withLet(
      env,
      dummyLet,
      body);
  }

  static <T0> T0 withLambda(hydra.ext.python.environment.PythonEnvironment v1, hydra.core.Lambda v2, java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, T0> v3) {
    return hydra.Environment.withLambdaContext(
      hydra.ext.python.Coder::pythonEnvironmentGetGraph,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, hydra.ext.python.environment.PythonEnvironment>>) (p0 -> p1 -> hydra.ext.python.Coder.pythonEnvironmentSetGraph(
        p0,
        p1)),
      v1,
      v2,
      v3);
  }

  static <T0> T0 withLet(hydra.ext.python.environment.PythonEnvironment v1, hydra.core.Let v2, java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, T0> v3) {
    return hydra.Environment.withLetContext(
      hydra.ext.python.Coder::pythonEnvironmentGetGraph,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, hydra.ext.python.environment.PythonEnvironment>>) (p0 -> p1 -> hydra.ext.python.Coder.pythonEnvironmentSetGraph(
        p0,
        p1)),
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (p0 -> p1 -> hydra.ext.python.Coder.pythonBindingMetadata(
        p0,
        p1)),
      v1,
      v2,
      v3);
  }

  static <T0> T0 withLetInline(hydra.ext.python.environment.PythonEnvironment env, hydra.core.Let lt, java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, T0> body) {
    hydra.util.Lazy<java.util.List<hydra.core.Name>> bindingNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.core.Name>) (b -> (b).name),
      (lt).bindings));
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> inlineVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(bindingNames.get()));
    return hydra.Environment.withLetContext(
      hydra.ext.python.Coder::pythonEnvironmentGetGraph,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, hydra.ext.python.environment.PythonEnvironment>>) (p0 -> p1 -> hydra.ext.python.Coder.pythonEnvironmentSetGraph(
        p0,
        p1)),
      p0 -> p1 -> hydra.ext.python.Coder.<hydra.graph.Graph, hydra.core.Binding, hydra.core.Term>withLetInline_noMetadata(
        p0,
        p1),
      env,
      lt,
      (java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, T0>) (innerEnv -> {
        hydra.util.Lazy<hydra.ext.python.environment.PythonEnvironment> updatedEnv = new hydra.util.Lazy<>(() -> new hydra.ext.python.environment.PythonEnvironment((innerEnv).namespaces, (innerEnv).boundTypeVariables, (innerEnv).graph, (innerEnv).nullaryBindings, (innerEnv).version, (innerEnv).skipCasts, hydra.lib.sets.Union.apply(
          inlineVars.get(),
          (innerEnv).inlineVariables)));
        return (body).apply(updatedEnv.get());
      }));
  }

  static <T1, T2, T3> hydra.util.Maybe<T3> withLetInline_noMetadata(T1 tc, T2 b) {
    return (hydra.util.Maybe<T3>) (hydra.util.Maybe.<T3>nothing());
  }

  static <T0> T0 withTypeLambda(hydra.ext.python.environment.PythonEnvironment v1, hydra.core.TypeLambda v2, java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, T0> v3) {
    return hydra.Environment.withTypeLambdaContext(
      hydra.ext.python.Coder::pythonEnvironmentGetGraph,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.ext.python.environment.PythonEnvironment, hydra.ext.python.environment.PythonEnvironment>>) (p0 -> p1 -> hydra.ext.python.Coder.pythonEnvironmentSetGraph(
        p0,
        p1)),
      v1,
      v2,
      v3);
  }

  static hydra.ext.python.syntax.Expression wrapInNullaryLambda(hydra.ext.python.syntax.Expression expr) {
    return new hydra.ext.python.syntax.Expression.Lambda(new hydra.ext.python.syntax.Lambda(new hydra.ext.python.syntax.LambdaParameters((hydra.util.Maybe<hydra.ext.python.syntax.LambdaSlashNoDefault>) (hydra.util.Maybe.<hydra.ext.python.syntax.LambdaSlashNoDefault>nothing()), (java.util.List<hydra.ext.python.syntax.LambdaParamNoDefault>) (java.util.Collections.<hydra.ext.python.syntax.LambdaParamNoDefault>emptyList()), (java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault>) (java.util.Collections.<hydra.ext.python.syntax.LambdaParamWithDefault>emptyList()), (hydra.util.Maybe<hydra.ext.python.syntax.LambdaStarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.LambdaStarEtc>nothing())), expr));
  }

  static java.util.List<hydra.ext.python.syntax.Expression> wrapLazyArguments(hydra.core.Name name, java.util.List<hydra.ext.python.syntax.Expression> args) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        hydra.lib.equality.Equal.apply(
          name,
          new hydra.core.Name("hydra.lib.logic.ifElse")),
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(args),
          3)),
      () -> java.util.Arrays.asList(
        hydra.lib.lists.At.apply(
          0,
          args),
        hydra.ext.python.Coder.wrapInNullaryLambda(hydra.lib.lists.At.apply(
          1,
          args)),
        hydra.ext.python.Coder.wrapInNullaryLambda(hydra.lib.lists.At.apply(
          2,
          args))),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.logic.And.apply(
          hydra.lib.equality.Equal.apply(
            name,
            new hydra.core.Name("hydra.lib.maybes.cases")),
          hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply(args),
            3)),
        () -> java.util.Arrays.asList(
          hydra.lib.lists.At.apply(
            0,
            args),
          hydra.ext.python.Coder.wrapInNullaryLambda(hydra.lib.lists.At.apply(
            1,
            args)),
          hydra.lib.lists.At.apply(
            2,
            args)),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.And.apply(
            hydra.lib.logic.Or.apply(
              hydra.lib.equality.Equal.apply(
                name,
                new hydra.core.Name("hydra.lib.maybes.maybe")),
              hydra.lib.equality.Equal.apply(
                name,
                new hydra.core.Name("hydra.lib.maybes.fromMaybe"))),
            hydra.lib.equality.Gte.apply(
              hydra.lib.lists.Length.apply(args),
              1)),
          () -> hydra.lib.lists.Cons.apply(
            hydra.ext.python.Coder.wrapInNullaryLambda(hydra.lib.lists.At.apply(
              0,
              args)),
            hydra.lib.lists.Tail.apply(args)),
          () -> args)));
  }
}
