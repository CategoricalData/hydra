// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Simple, one-way adapters for types and terms
 */
public interface Adapt {
  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>> adaptDataGraph(hydra.coders.LanguageConstraints constraints, Boolean doExpand, java.util.List<hydra.core.Binding> els0, T0 cx, hydra.graph.Graph graph0) {
    java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap = hydra.Adapt.adaptLiteralTypesMap(constraints);
    java.util.Map<hydra.core.Name, hydra.graph.Primitive> prims0 = (graph0).primitives;
    java.util.Map<hydra.core.Name, hydra.core.TypeScheme> schemaTypes0 = (graph0).schemaTypes;
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> schemaBindings = new hydra.util.Lazy<>(() -> hydra.Environment.typesToDefinitions(hydra.lib.maps.Map.apply(
      (java.util.function.Function<hydra.core.TypeScheme, hydra.core.Type>) (ts -> hydra.Scoping.typeSchemeToFType(ts)),
      schemaTypes0)));
    java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.core.Term>> transformTerm = (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (g -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> {
      hydra.core.Term t1 = hydra.Variables.unshadowVariables(hydra.Adapt.pushTypeAppsInward(term));
      hydra.graph.Graph tx = g;
      hydra.util.Lazy<hydra.core.Term> t2 = new hydra.util.Lazy<>(() -> hydra.Variables.unshadowVariables(hydra.lib.logic.IfElse.lazy(
        doExpand,
        () -> hydra.Adapt.pushTypeAppsInward(hydra.Reduction.etaExpandTerm(
          tx,
          t1)),
        () -> t1)));
      return hydra.Dependencies.liftLambdaAboveLet(t2.get());
    }));
    java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.core.Binding>> transformBinding = (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.core.Binding>>) (g -> (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (el -> new hydra.core.Binding((el).name, (transformTerm).apply(g).apply((el).term), (el).type)));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.maps.Null.apply(schemaTypes0),
        () -> hydra.util.Either.<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>right((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply()))),
        () -> hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Bimap.apply(
            (java.util.function.Function<hydra.errors.DecodingError, hydra.errors.Error_>) (e -> new hydra.errors.Error_.Decoding(e)),
            (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.Map<hydra.core.Name, hydra.core.Type>>) (x -> x),
            hydra.Environment.graphAsTypes(
              graph0,
              schemaBindings.get())),
          (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.util.Either<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>>) (tmap0 -> hydra.lib.eithers.Bind.apply(
            hydra.Adapt.adaptGraphSchema(
              constraints,
              litmap,
              tmap0),
            (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.util.Either<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>>) (tmap1 -> hydra.util.Either.<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>right(hydra.lib.maps.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>) (t -> hydra.Resolution.typeToTypeScheme(t)),
              tmap1))))))),
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>>>) (schemaResult -> {
        java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>> adaptBinding = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (el -> {
          hydra.core.Binding transformed = (transformBinding).apply(graph0).apply(el);
          hydra.util.Lazy<hydra.core.Term> wrapped = new hydra.util.Lazy<>(() -> new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Pure.apply(transformed), new hydra.core.Term.Unit())));
          return hydra.lib.eithers.Bind.apply(
            hydra.Adapt.<T0>adaptTerm(
              constraints,
              litmap,
              cx,
              graph0,
              wrapped.get()),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (adapted -> hydra.Rewriting.rewriteTermM(
              (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (v2 -> hydra.Adapt.adaptLambdaDomains(
                constraints,
                litmap,
                v1,
                v2))),
              adapted)));
        });
        java.util.Map<hydra.core.Name, hydra.core.TypeScheme> adaptedSchemaTypes = schemaResult;
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            adaptBinding,
            els0),
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>>>) (adaptedTerms -> {
            hydra.util.Lazy<java.util.List<hydra.core.Binding>> els1Raw = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
              hydra.Environment::termAsBindings,
              adaptedTerms)));
            java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, hydra.core.Binding>> processBinding = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, hydra.core.Binding>>) (el -> hydra.lib.eithers.Bind.apply(
              hydra.Rewriting.rewriteTermM(
                (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (v2 -> hydra.Adapt.adaptNestedTypes(
                  constraints,
                  litmap,
                  v1,
                  v2))),
                (el).term),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Binding>>) (newTerm -> hydra.lib.eithers.Bind.apply(
                hydra.lib.maybes.Maybe.applyLazy(
                  () -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>>right((hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
                  (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>>>) (ts -> hydra.lib.eithers.Bind.apply(
                    hydra.Adapt.adaptTypeScheme(
                      constraints,
                      litmap,
                      ts),
                    (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>>>) (ts1 -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>>right(hydra.util.Maybe.just(ts1))))),
                  (el).type),
                (java.util.function.Function<hydra.util.Maybe<hydra.core.TypeScheme>, hydra.util.Either<hydra.errors.Error_, hydra.core.Binding>>) (adaptedType -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Binding>right(new hydra.core.Binding((el).name, newTerm, adaptedType)))))));
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.MapList.apply(
                processBinding,
                els1Raw.get()),
              (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>>>) (els1 -> hydra.lib.eithers.Bind.apply(
                hydra.lib.eithers.MapList.apply(
                  (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.graph.Primitive>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.core.Name, hydra.graph.Primitive>>>) (kv -> hydra.lib.eithers.Bind.apply(
                    hydra.Adapt.adaptPrimitive(
                      constraints,
                      litmap,
                      hydra.lib.pairs.Second.apply(kv)),
                    (java.util.function.Function<hydra.graph.Primitive, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.core.Name, hydra.graph.Primitive>>>) (prim1 -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<hydra.core.Name, hydra.graph.Primitive>>right((hydra.util.Pair<hydra.core.Name, hydra.graph.Primitive>) ((hydra.util.Pair<hydra.core.Name, hydra.graph.Primitive>) (new hydra.util.Pair<hydra.core.Name, hydra.graph.Primitive>(hydra.lib.pairs.First.apply(kv), prim1))))))),
                  hydra.lib.maps.ToList.apply(prims0)),
                (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.graph.Primitive>>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>>>) (primPairs -> {
                  hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.graph.Primitive>> prims1 = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(primPairs));
                  hydra.util.Lazy<hydra.graph.Graph> adaptedGraphRaw = new hydra.util.Lazy<>(() -> hydra.Lexical.buildGraph(
                    els1,
                    (java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) ((java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>apply())),
                    prims1.get()));
                  hydra.graph.Graph adaptedGraph = new hydra.graph.Graph(adaptedGraphRaw.get().boundTerms, adaptedGraphRaw.get().boundTypes, adaptedGraphRaw.get().classConstraints, adaptedGraphRaw.get().lambdaVariables, adaptedGraphRaw.get().metadata, adaptedGraphRaw.get().primitives, adaptedSchemaTypes, adaptedGraphRaw.get().typeVariables);
                  return hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>>right((hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>) ((hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>) (new hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>(adaptedGraph, els1))));
                }))));
          }));
      }));
  }

  static hydra.util.Maybe<hydra.core.FloatType> adaptFloatType(hydra.coders.LanguageConstraints constraints, hydra.core.FloatType ft) {
    java.util.function.Function<hydra.core.FloatType, hydra.util.Maybe<hydra.core.FloatType>> alt = (java.util.function.Function<hydra.core.FloatType, hydra.util.Maybe<hydra.core.FloatType>>) (v1 -> hydra.Adapt.adaptFloatType(
      constraints,
      v1));
    java.util.function.Function<hydra.core.FloatType, hydra.util.Maybe<hydra.core.FloatType>> forUnsupported = (java.util.function.Function<hydra.core.FloatType, hydra.util.Maybe<hydra.core.FloatType>>) (ft2 -> (ft2).accept(new hydra.core.FloatType.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.FloatType> visit(hydra.core.FloatType.Bigfloat ignored) {
        return (alt).apply(new hydra.core.FloatType.Float64());
      }

      @Override
      public hydra.util.Maybe<hydra.core.FloatType> visit(hydra.core.FloatType.Float32 ignored) {
        return (alt).apply(new hydra.core.FloatType.Float64());
      }

      @Override
      public hydra.util.Maybe<hydra.core.FloatType> visit(hydra.core.FloatType.Float64 ignored) {
        return (alt).apply(new hydra.core.FloatType.Bigfloat());
      }
    }));
    hydra.util.Lazy<Boolean> supported = new hydra.util.Lazy<>(() -> hydra.lib.sets.Member.apply(
      ft,
      (constraints).floatTypes));
    return hydra.lib.logic.IfElse.lazy(
      supported.get(),
      () -> hydra.util.Maybe.just(ft),
      () -> (forUnsupported).apply(ft));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, java.util.Map<T0, hydra.core.Type>> adaptGraphSchema(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, java.util.Map<T0, hydra.core.Type> types0) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.util.Pair<T0, hydra.core.Type>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<T0, hydra.core.Type>>>) (v1 -> hydra.Adapt.<T0>adaptGraphSchema_mapPair(
          constraints,
          (java.util.function.Function<hydra.coders.LanguageConstraints, java.util.function.Function<java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType>, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>>>) (p0 -> p1 -> p2 -> hydra.Adapt.adaptType(
            p0,
            p1,
            p2)),
          litmap,
          v1)),
        hydra.lib.maps.ToList.apply(types0)),
      (java.util.function.Function<java.util.List<hydra.util.Pair<T0, hydra.core.Type>>, hydra.util.Either<hydra.errors.Error_, java.util.Map<T0, hydra.core.Type>>>) (pairs -> hydra.util.Either.<hydra.errors.Error_, java.util.Map<T0, hydra.core.Type>>right(hydra.lib.maps.FromList.apply(pairs))));
  }

  static <T1> hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<T1, hydra.core.Type>> adaptGraphSchema_mapPair(hydra.coders.LanguageConstraints constraints, java.util.function.Function<hydra.coders.LanguageConstraints, java.util.function.Function<java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType>, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>>> hydra_adapt_adaptType, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, hydra.util.Pair<T1, hydra.core.Type> pair) {
    hydra.util.Lazy<hydra.core.Type> typ = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
    return hydra.lib.eithers.Bind.apply(
      (hydra_adapt_adaptType).apply(constraints).apply(litmap).apply(typ.get()),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<T1, hydra.core.Type>>>) (typ1 -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<T1, hydra.core.Type>>right((hydra.util.Pair<T1, hydra.core.Type>) ((hydra.util.Pair<T1, hydra.core.Type>) (new hydra.util.Pair<T1, hydra.core.Type>(hydra.Adapt.<T1>adaptGraphSchema_name(pair), typ1))))));
  }

  static <T1> T1 adaptGraphSchema_name(hydra.util.Pair<T1, hydra.core.Type> pair) {
    return hydra.lib.pairs.First.apply(pair);
  }

  static hydra.util.Maybe<hydra.core.IntegerType> adaptIntegerType(hydra.coders.LanguageConstraints constraints, hydra.core.IntegerType it) {
    java.util.function.Function<hydra.core.IntegerType, hydra.util.Maybe<hydra.core.IntegerType>> alt = (java.util.function.Function<hydra.core.IntegerType, hydra.util.Maybe<hydra.core.IntegerType>>) (v1 -> hydra.Adapt.adaptIntegerType(
      constraints,
      v1));
    java.util.function.Function<hydra.core.IntegerType, hydra.util.Maybe<hydra.core.IntegerType>> forUnsupported = (java.util.function.Function<hydra.core.IntegerType, hydra.util.Maybe<hydra.core.IntegerType>>) (it2 -> (it2).accept(new hydra.core.IntegerType.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.IntegerType> visit(hydra.core.IntegerType.Bigint ignored) {
        return (hydra.util.Maybe<hydra.core.IntegerType>) (hydra.util.Maybe.<hydra.core.IntegerType>nothing());
      }

      @Override
      public hydra.util.Maybe<hydra.core.IntegerType> visit(hydra.core.IntegerType.Int8 ignored) {
        return (alt).apply(new hydra.core.IntegerType.Uint16());
      }

      @Override
      public hydra.util.Maybe<hydra.core.IntegerType> visit(hydra.core.IntegerType.Int16 ignored) {
        return (alt).apply(new hydra.core.IntegerType.Uint32());
      }

      @Override
      public hydra.util.Maybe<hydra.core.IntegerType> visit(hydra.core.IntegerType.Int32 ignored) {
        return (alt).apply(new hydra.core.IntegerType.Uint64());
      }

      @Override
      public hydra.util.Maybe<hydra.core.IntegerType> visit(hydra.core.IntegerType.Int64 ignored) {
        return (alt).apply(new hydra.core.IntegerType.Bigint());
      }

      @Override
      public hydra.util.Maybe<hydra.core.IntegerType> visit(hydra.core.IntegerType.Uint8 ignored) {
        return (alt).apply(new hydra.core.IntegerType.Int16());
      }

      @Override
      public hydra.util.Maybe<hydra.core.IntegerType> visit(hydra.core.IntegerType.Uint16 ignored) {
        return (alt).apply(new hydra.core.IntegerType.Int32());
      }

      @Override
      public hydra.util.Maybe<hydra.core.IntegerType> visit(hydra.core.IntegerType.Uint32 ignored) {
        return (alt).apply(new hydra.core.IntegerType.Int64());
      }

      @Override
      public hydra.util.Maybe<hydra.core.IntegerType> visit(hydra.core.IntegerType.Uint64 ignored) {
        return (alt).apply(new hydra.core.IntegerType.Bigint());
      }
    }));
    hydra.util.Lazy<Boolean> supported = new hydra.util.Lazy<>(() -> hydra.lib.sets.Member.apply(
      it,
      (constraints).integerTypes));
    return hydra.lib.logic.IfElse.lazy(
      supported.get(),
      () -> hydra.util.Maybe.just(it),
      () -> (forUnsupported).apply(it));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.core.Term> adaptLambdaDomains(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, java.util.function.Function<T0, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>> recurse, T0 term) {
    return hydra.lib.eithers.Bind.apply(
      (recurse).apply(term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (rewritten -> (rewritten).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(rewritten);
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> visit(hydra.core.Term.Function f) {
          return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> otherwise(hydra.core.Function instance) {
              return hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(new hydra.core.Term.Function((f).value));
            }

            @Override
            public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> visit(hydra.core.Function.Lambda l) {
              return hydra.lib.eithers.Bind.apply(
                hydra.lib.maybes.Maybe.applyLazy(
                  () -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Type>>right((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
                  (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Type>>>) (dom -> hydra.lib.eithers.Bind.apply(
                    hydra.Adapt.adaptType(
                      constraints,
                      litmap,
                      dom),
                    (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Type>>>) (dom1 -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Type>>right(hydra.util.Maybe.just(dom1))))),
                  (l).value.domain),
                (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (adaptedDomain -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda((l).value.parameter, adaptedDomain, (l).value.body))))));
            }
          });
        }
      })));
  }

  static hydra.core.Literal adaptLiteral(hydra.core.LiteralType lt, hydra.core.Literal l) {
    return (l).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.core.Literal visit(hydra.core.Literal.Binary b) {
        return (lt).accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public hydra.core.Literal visit(hydra.core.LiteralType.String_ ignored) {
            return new hydra.core.Literal.String_(hydra.lib.literals.BinaryToString.apply((b).value));
          }
        });
      }

      @Override
      public hydra.core.Literal visit(hydra.core.Literal.Boolean_ b) {
        return (lt).accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public hydra.core.Literal visit(hydra.core.LiteralType.Integer_ it) {
            return new hydra.core.Literal.Integer_(hydra.Literals.bigintToIntegerValue(
              (it).value,
              hydra.lib.logic.IfElse.lazy(
                (b).value,
                () -> new java.math.BigInteger("1"),
                () -> new java.math.BigInteger("0"))));
          }
        });
      }

      @Override
      public hydra.core.Literal visit(hydra.core.Literal.Float_ f) {
        return (lt).accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public hydra.core.Literal visit(hydra.core.LiteralType.Float_ ft) {
            return new hydra.core.Literal.Float_(hydra.Literals.bigfloatToFloatValue(
              (ft).value,
              hydra.Literals.floatValueToBigfloat((f).value)));
          }
        });
      }

      @Override
      public hydra.core.Literal visit(hydra.core.Literal.Integer_ i) {
        return (lt).accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public hydra.core.Literal visit(hydra.core.LiteralType.Integer_ it) {
            return new hydra.core.Literal.Integer_(hydra.Literals.bigintToIntegerValue(
              (it).value,
              hydra.Literals.integerValueToBigint((i).value)));
          }
        });
      }
    });
  }

  static hydra.util.Maybe<hydra.core.LiteralType> adaptLiteralType(hydra.coders.LanguageConstraints constraints, hydra.core.LiteralType lt) {
    java.util.function.Function<hydra.core.LiteralType, hydra.util.Maybe<hydra.core.LiteralType>> forUnsupported = (java.util.function.Function<hydra.core.LiteralType, hydra.util.Maybe<hydra.core.LiteralType>>) (lt2 -> (lt2).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.LiteralType> otherwise(hydra.core.LiteralType instance) {
        return (hydra.util.Maybe<hydra.core.LiteralType>) (hydra.util.Maybe.<hydra.core.LiteralType>nothing());
      }

      @Override
      public hydra.util.Maybe<hydra.core.LiteralType> visit(hydra.core.LiteralType.Binary ignored) {
        return hydra.util.Maybe.just(new hydra.core.LiteralType.String_());
      }

      @Override
      public hydra.util.Maybe<hydra.core.LiteralType> visit(hydra.core.LiteralType.Boolean_ ignored) {
        return hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.IntegerType, hydra.core.LiteralType>) (x -> new hydra.core.LiteralType.Integer_(x)),
          hydra.Adapt.adaptIntegerType(
            constraints,
            new hydra.core.IntegerType.Int8()));
      }

      @Override
      public hydra.util.Maybe<hydra.core.LiteralType> visit(hydra.core.LiteralType.Float_ ft) {
        return hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.FloatType, hydra.core.LiteralType>) (x -> new hydra.core.LiteralType.Float_(x)),
          hydra.Adapt.adaptFloatType(
            constraints,
            (ft).value));
      }

      @Override
      public hydra.util.Maybe<hydra.core.LiteralType> visit(hydra.core.LiteralType.Integer_ it) {
        return hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.IntegerType, hydra.core.LiteralType>) (x -> new hydra.core.LiteralType.Integer_(x)),
          hydra.Adapt.adaptIntegerType(
            constraints,
            (it).value));
      }
    }));
    return hydra.lib.logic.IfElse.lazy(
      hydra.Adapt.literalTypeSupported(
        constraints,
        lt),
      () -> (hydra.util.Maybe<hydra.core.LiteralType>) (hydra.util.Maybe.<hydra.core.LiteralType>nothing()),
      () -> (forUnsupported).apply(lt));
  }

  static java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> adaptLiteralTypesMap(hydra.coders.LanguageConstraints constraints) {
    java.util.function.Function<hydra.core.LiteralType, hydra.util.Maybe<hydra.util.Pair<hydra.core.LiteralType, hydra.core.LiteralType>>> tryType = (java.util.function.Function<hydra.core.LiteralType, hydra.util.Maybe<hydra.util.Pair<hydra.core.LiteralType, hydra.core.LiteralType>>>) (lt -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (hydra.util.Maybe<hydra.util.Pair<hydra.core.LiteralType, hydra.core.LiteralType>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.core.LiteralType, hydra.core.LiteralType>>nothing()),
      (java.util.function.Function<hydra.core.LiteralType, hydra.util.Maybe<hydra.util.Pair<hydra.core.LiteralType, hydra.core.LiteralType>>>) (lt2 -> hydra.util.Maybe.just((hydra.util.Pair<hydra.core.LiteralType, hydra.core.LiteralType>) ((hydra.util.Pair<hydra.core.LiteralType, hydra.core.LiteralType>) (new hydra.util.Pair<hydra.core.LiteralType, hydra.core.LiteralType>(lt, lt2))))),
      hydra.Adapt.adaptLiteralType(
        constraints,
        lt)));
    return hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      tryType,
      hydra.Reflect.literalTypes())));
  }

  static <T0> hydra.core.Literal adaptLiteralValue(java.util.Map<T0, hydra.core.LiteralType> litmap, T0 lt, hydra.core.Literal l) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> new hydra.core.Literal.String_(hydra.show.Core.literal(l)),
      (java.util.function.Function<hydra.core.LiteralType, hydra.core.Literal>) (lt2 -> hydra.Adapt.adaptLiteral(
        lt2,
        l)),
      hydra.lib.maps.Lookup.apply(
        lt,
        litmap));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.core.Term> adaptNestedTypes(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, java.util.function.Function<T0, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>> recurse, T0 term) {
    return hydra.lib.eithers.Bind.apply(
      (recurse).apply(term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (rewritten -> (rewritten).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(rewritten);
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> visit(hydra.core.Term.Let lt) {
          java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, hydra.core.Binding>> adaptB = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, hydra.core.Binding>>) (b -> hydra.lib.eithers.Bind.apply(
            hydra.lib.maybes.Maybe.applyLazy(
              () -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>>right((hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>>>) (ts -> hydra.lib.eithers.Bind.apply(
                hydra.Adapt.adaptTypeScheme(
                  constraints,
                  litmap,
                  ts),
                (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>>>) (ts1 -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>>right(hydra.util.Maybe.just(ts1))))),
              (b).type),
            (java.util.function.Function<hydra.util.Maybe<hydra.core.TypeScheme>, hydra.util.Either<hydra.errors.Error_, hydra.core.Binding>>) (adaptedBType -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Binding>right(new hydra.core.Binding((b).name, (b).term, adaptedBType)))));
          return hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapList.apply(
              adaptB,
              (lt).value.bindings),
            (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (adaptedBindings -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(new hydra.core.Term.Let(new hydra.core.Let(adaptedBindings, (lt).value.body)))));
        }
      })));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.graph.Primitive> adaptPrimitive(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, hydra.graph.Primitive prim0) {
    hydra.core.TypeScheme ts0 = (prim0).type;
    return hydra.lib.eithers.Bind.apply(
      hydra.Adapt.adaptTypeScheme(
        constraints,
        litmap,
        ts0),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.Error_, hydra.graph.Primitive>>) (ts1 -> hydra.util.Either.<hydra.errors.Error_, hydra.graph.Primitive>right(new hydra.graph.Primitive((prim0).name, ts1, (prim0).implementation))));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.core.Term> adaptTerm(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, T0 cx, hydra.graph.Graph graph, hydra.core.Term term0) {
    return hydra.Rewriting.rewriteTermM(
      (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (v2 -> hydra.Adapt.adaptTerm_rewrite(
        constraints,
        cx,
        graph,
        (java.util.function.Function<hydra.coders.LanguageConstraints, java.util.function.Function<java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType>, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>>>) (p0 -> p1 -> p2 -> hydra.Adapt.adaptType(
          p0,
          p1,
          p2)),
        (java.util.function.Function<hydra.coders.LanguageConstraints, java.util.function.Function<hydra.core.LiteralType, Boolean>>) (p0 -> p1 -> hydra.Adapt.literalTypeSupported(
          p0,
          p1)),
        hydra.Reflect::literalType,
        hydra.Reflect::termVariant,
        hydra.show.Core::term,
        litmap,
        v1,
        v2))),
      term0);
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.core.Term> adaptTermForLanguage(hydra.coders.Language lang, T0 cx, hydra.graph.Graph g, hydra.core.Term term) {
    hydra.coders.LanguageConstraints constraints = (lang).constraints;
    java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap = hydra.Adapt.adaptLiteralTypesMap(constraints);
    return hydra.Adapt.<T0>adaptTerm(
      constraints,
      litmap,
      cx,
      g,
      term);
  }

  static <T2> hydra.util.Either<T2, hydra.util.Maybe<hydra.core.Term>> adaptTerm_forSupported(hydra.coders.LanguageConstraints constraints, java.util.function.Function<hydra.coders.LanguageConstraints, java.util.function.Function<hydra.core.LiteralType, Boolean>> hydra_adapt_literalTypeSupported, java.util.function.Function<hydra.core.Literal, hydra.core.LiteralType> hydra_reflect_literalType, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T2, hydra.util.Maybe<hydra.core.Term>> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<T2, hydra.util.Maybe<hydra.core.Term>>right(hydra.util.Maybe.just(term));
      }

      @Override
      public hydra.util.Either<T2, hydra.util.Maybe<hydra.core.Term>> visit(hydra.core.Term.Literal l) {
        hydra.core.LiteralType lt = (hydra_reflect_literalType).apply((l).value);
        return hydra.util.Either.<T2, hydra.util.Maybe<hydra.core.Term>>right(hydra.util.Maybe.just(hydra.lib.logic.IfElse.lazy(
          (hydra_adapt_literalTypeSupported).apply(constraints).apply(lt),
          () -> term,
          () -> new hydra.core.Term.Literal(hydra.Adapt.adaptLiteralValue(
            litmap,
            lt,
            (l).value)))));
      }
    });
  }

  static <T0, T1> hydra.util.Either<hydra.errors.Error_, hydra.core.Term> adaptTerm_rewrite(hydra.coders.LanguageConstraints constraints, T0 cx, hydra.graph.Graph graph, java.util.function.Function<hydra.coders.LanguageConstraints, java.util.function.Function<java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType>, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>>> hydra_adapt_adaptType, java.util.function.Function<hydra.coders.LanguageConstraints, java.util.function.Function<hydra.core.LiteralType, Boolean>> hydra_adapt_literalTypeSupported, java.util.function.Function<hydra.core.Literal, hydra.core.LiteralType> hydra_reflect_literalType, java.util.function.Function<hydra.core.Term, hydra.variants.TermVariant> hydra_reflect_termVariant, java.util.function.Function<hydra.core.Term, String> hydra_show_core_term, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, java.util.function.Function<T1, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>> recurse, T1 term0) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Term>>>> forUnsupported = new java.util.concurrent.atomic.AtomicReference<>();
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Term>>>> tryTerm = new java.util.concurrent.atomic.AtomicReference<>();
    forUnsupported.set((java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Term>>>) (term -> {
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Term>>>> forNonNull = new java.util.concurrent.atomic.AtomicReference<>();
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Term>>>> tryAlts = new java.util.concurrent.atomic.AtomicReference<>();
      forNonNull.set((java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Term>>>) (alts -> hydra.lib.eithers.Bind.apply(
        tryTerm.get().apply(hydra.lib.lists.Head.apply(alts)),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Term>>>) (mterm -> hydra.lib.maybes.Maybe.applyLazy(
          () -> tryAlts.get().apply(hydra.lib.lists.Tail.apply(alts)),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Term>>>) (t -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Term>>right(hydra.util.Maybe.just(t))),
          mterm)))));
      tryAlts.set((java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Term>>>) (alts -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(alts),
        () -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Term>>right((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())),
        () -> forNonNull.get().apply(alts))));
      return hydra.lib.eithers.Bind.apply(
        hydra.Adapt.<T0>termAlternatives(
          cx,
          graph,
          term),
        (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Term>>>) (alts0 -> tryAlts.get().apply(alts0)));
    }));
    tryTerm.set((java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Term>>>) (term -> {
      hydra.util.Lazy<Boolean> supportedVariant = new hydra.util.Lazy<>(() -> hydra.lib.sets.Member.apply(
        (hydra_reflect_termVariant).apply(term),
        (constraints).termVariants));
      return hydra.lib.logic.IfElse.lazy(
        supportedVariant.get(),
        () -> hydra.Adapt.adaptTerm_forSupported(
          constraints,
          hydra_adapt_literalTypeSupported,
          hydra_reflect_literalType,
          litmap,
          term),
        () -> forUnsupported.get().apply(term));
    }));
    return hydra.lib.eithers.Bind.apply(
      (recurse).apply(term0),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (term1 -> (term1).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> otherwise(hydra.core.Term instance) {
          return hydra.lib.eithers.Bind.apply(
            tryTerm.get().apply(term1),
            (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (mterm -> hydra.lib.maybes.Maybe.applyLazy(
              () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>left(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
                "no alternatives for term: ",
                (hydra_show_core_term).apply(term1))))),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (term2 -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(term2)),
              mterm)));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> visit(hydra.core.Term.TypeApplication ta) {
          return hydra.lib.eithers.Bind.apply(
            (hydra_adapt_adaptType).apply(constraints).apply(litmap).apply((ta).value.type),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (atyp -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm((ta).value.body, atyp)))));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> visit(hydra.core.Term.TypeLambda ignored) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(term1);
        }
      })));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.Type> adaptType(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, hydra.core.Type type0) {
    java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>> forSupported = (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (typ -> (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.util.Maybe.just(typ);
      }

      @Override
      public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Type.Literal lt) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.Adapt.literalTypeSupported(
            constraints,
            (lt).value),
          () -> hydra.util.Maybe.just(typ),
          () -> hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Maybe.just(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            (java.util.function.Function<hydra.core.LiteralType, hydra.util.Maybe<hydra.core.Type>>) (lt2 -> hydra.util.Maybe.just(new hydra.core.Type.Literal(lt2))),
            hydra.lib.maps.Lookup.apply(
              (lt).value,
              litmap)));
      }
    }));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>> forUnsupported = new java.util.concurrent.atomic.AtomicReference<>();
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>> tryType = new java.util.concurrent.atomic.AtomicReference<>();
    forUnsupported.set((java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (typ -> {
      java.util.List<hydra.core.Type> alts0 = hydra.Adapt.typeAlternatives(typ);
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>> tryAlts = new java.util.concurrent.atomic.AtomicReference<>();
      tryAlts.set((java.util.function.Function<java.util.List<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (alts -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(alts),
        () -> (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()),
        () -> hydra.lib.maybes.Maybe.applyLazy(
          () -> tryAlts.get().apply(hydra.lib.lists.Tail.apply(alts)),
          (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (t -> hydra.util.Maybe.just(t)),
          tryType.get().apply(hydra.lib.lists.Head.apply(alts))))));
      return tryAlts.get().apply(alts0);
    }));
    tryType.set((java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (typ -> {
      hydra.util.Lazy<Boolean> supportedVariant = new hydra.util.Lazy<>(() -> hydra.lib.sets.Member.apply(
        hydra.Reflect.typeVariant(typ),
        (constraints).typeVariants));
      return hydra.lib.logic.IfElse.lazy(
        supportedVariant.get(),
        () -> (forSupported).apply(typ),
        () -> forUnsupported.get().apply(typ));
    }));
    java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>> rewrite = (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>) (typ -> hydra.lib.eithers.Bind.apply(
      (recurse).apply(typ),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>) (type1 -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Type>left(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          "no alternatives for type: ",
          hydra.show.Core.type(typ))))),
        (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>) (type2 -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Type>right(type2)),
        tryType.get().apply(type1))))));
    return hydra.Rewriting.rewriteTypeM(
      rewrite,
      type0);
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.Type> adaptTypeForLanguage(hydra.coders.Language lang, hydra.core.Type typ) {
    hydra.coders.LanguageConstraints constraints = (lang).constraints;
    java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap = hydra.Adapt.adaptLiteralTypesMap(constraints);
    return hydra.Adapt.adaptType(
      constraints,
      litmap,
      typ);
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.core.TypeScheme> adaptTypeScheme(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, hydra.core.TypeScheme ts0) {
    hydra.core.Type t0 = (ts0).type;
    java.util.List<hydra.core.Name> vars0 = (ts0).variables;
    return hydra.lib.eithers.Bind.apply(
      hydra.Adapt.adaptType(
        constraints,
        litmap,
        t0),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.core.TypeScheme>>) (t1 -> hydra.util.Either.<hydra.errors.Error_, hydra.core.TypeScheme>right(new hydra.core.TypeScheme(vars0, t1, (ts0).constraints))));
  }

  static <T0, T1, T2> hydra.coders.Coder<T0, T2> composeCoders(hydra.coders.Coder<T0, T1> c1, hydra.coders.Coder<T1, T2> c2) {
    return (hydra.coders.Coder<T0, T2>) ((hydra.coders.Coder<T0, T2>) (new hydra.coders.Coder<T0, T2>((java.util.function.Function<hydra.context.Context, java.util.function.Function<T0, hydra.util.Either<hydra.errors.Error_, T2>>>) (cx -> (java.util.function.Function<T0, hydra.util.Either<hydra.errors.Error_, T2>>) (a -> hydra.lib.eithers.Bind.apply(
      ((java.util.function.Function<hydra.coders.Coder<T0, T1>, java.util.function.Function<hydra.context.Context, java.util.function.Function<T0, hydra.util.Either<hydra.errors.Error_, T1>>>>) ((java.util.function.Function<hydra.coders.Coder<T0, T1>, java.util.function.Function<hydra.context.Context, java.util.function.Function<T0, hydra.util.Either<hydra.errors.Error_, T1>>>>) (projected -> projected.encode))).apply(c1).apply(cx).apply(a),
      (java.util.function.Function<T1, hydra.util.Either<hydra.errors.Error_, T2>>) (b1 -> ((java.util.function.Function<hydra.coders.Coder<T1, T2>, java.util.function.Function<hydra.context.Context, java.util.function.Function<T1, hydra.util.Either<hydra.errors.Error_, T2>>>>) ((java.util.function.Function<hydra.coders.Coder<T1, T2>, java.util.function.Function<hydra.context.Context, java.util.function.Function<T1, hydra.util.Either<hydra.errors.Error_, T2>>>>) (projected -> projected.encode))).apply(c2).apply(cx).apply(b1))))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<T2, hydra.util.Either<hydra.errors.Error_, T0>>>) (cx -> (java.util.function.Function<T2, hydra.util.Either<hydra.errors.Error_, T0>>) (c -> hydra.lib.eithers.Bind.apply(
      ((java.util.function.Function<hydra.coders.Coder<T1, T2>, java.util.function.Function<hydra.context.Context, java.util.function.Function<T2, hydra.util.Either<hydra.errors.Error_, T1>>>>) ((java.util.function.Function<hydra.coders.Coder<T1, T2>, java.util.function.Function<hydra.context.Context, java.util.function.Function<T2, hydra.util.Either<hydra.errors.Error_, T1>>>>) (projected -> projected.decode))).apply(c2).apply(cx).apply(c),
      (java.util.function.Function<T1, hydra.util.Either<hydra.errors.Error_, T0>>) (b2 -> ((java.util.function.Function<hydra.coders.Coder<T0, T1>, java.util.function.Function<hydra.context.Context, java.util.function.Function<T1, hydra.util.Either<hydra.errors.Error_, T0>>>>) ((java.util.function.Function<hydra.coders.Coder<T0, T1>, java.util.function.Function<hydra.context.Context, java.util.function.Function<T1, hydra.util.Either<hydra.errors.Error_, T0>>>>) (projected -> projected.decode))).apply(c1).apply(cx).apply(b2))))))));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.graph.Graph, java.util.List<java.util.List<hydra.packaging.TermDefinition>>>> dataGraphToDefinitions(hydra.coders.LanguageConstraints constraints, Boolean doInfer, Boolean doExpand, Boolean doHoistCaseStatements, Boolean doHoistPolymorphicLetBindings, java.util.List<hydra.core.Binding> originalBindings, hydra.graph.Graph graph0, java.util.List<hydra.packaging.Namespace> namespaces, hydra.context.Context cx) {
    java.util.List<hydra.core.Binding> bins0 = originalBindings;
    java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>> hoistCases = (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>>) (bindings -> {
      hydra.util.Lazy<java.util.List<hydra.core.Binding>> stripped = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, hydra.Strip.stripTypeLambdas((b).term), (b).type)),
        bindings));
      hydra.core.Term term0 = new hydra.core.Term.Let(new hydra.core.Let(stripped.get(), new hydra.core.Term.Unit()));
      java.util.List<hydra.core.Binding> unshadowed0 = hydra.Environment.termAsBindings(hydra.Variables.unshadowVariables(term0));
      java.util.List<hydra.core.Binding> hoisted = hydra.Hoisting.hoistCaseStatementsInGraph(unshadowed0);
      hydra.core.Term term1 = new hydra.core.Term.Let(new hydra.core.Let(hoisted, new hydra.core.Term.Unit()));
      return hydra.Environment.termAsBindings(hydra.Variables.unshadowVariables(term1));
    });
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> bins1 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      doHoistCaseStatements,
      () -> (hoistCases).apply(bins0),
      () -> bins0));
    java.util.function.Function<String, java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Binding>>>> checkBindingsTyped = (java.util.function.Function<String, java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Binding>>>>) (debugLabel -> (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Binding>>>) (bindings -> {
      hydra.util.Lazy<java.util.List<String>> untypedBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Binding, String>) (b -> (b).name.value),
        hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.logic.Not.apply(hydra.lib.maybes.IsJust.apply((b).type))),
          bindings)));
      return hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(untypedBindings.get()),
        () -> hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.Binding>>right(bindings),
        () -> hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.Binding>>left(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "Found untyped bindings (",
          debugLabel,
          "): ",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            untypedBindings.get())))))));
    }));
    hydra.util.Lazy<java.util.Set<hydra.packaging.Namespace>> namespacesSet = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(namespaces));
    java.util.function.Function<hydra.core.Binding, Boolean> isParentBinding = (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.maybes.Maybe.applyLazy(
      () -> false,
      (java.util.function.Function<hydra.packaging.Namespace, Boolean>) (ns -> hydra.lib.sets.Member.apply(
        ns,
        namespacesSet.get())),
      hydra.Names.namespaceOf((b).name)));
    java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>> hoistPoly = (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>>) (bindings -> {
      hydra.core.Let letBefore = new hydra.core.Let(bindings, new hydra.core.Term.Unit());
      hydra.core.Let letAfter = hydra.Hoisting.hoistPolymorphicLetBindings(
        isParentBinding,
        letBefore);
      return (letAfter).bindings;
    });
    java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>> normalizeBindings = (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>>) (bindings -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, hydra.Adapt.pushTypeAppsInward((b).term), (b).type)),
      bindings));
    java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.graph.Graph> rebuildGraph = (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.graph.Graph>) (bindings -> {
      hydra.util.Lazy<hydra.graph.Graph> g = new hydra.util.Lazy<>(() -> hydra.Lexical.buildGraph(
        bindings,
        (java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) ((java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>apply())),
        (graph0).primitives));
      return new hydra.graph.Graph(g.get().boundTerms, g.get().boundTypes, g.get().classConstraints, g.get().lambdaVariables, g.get().metadata, g.get().primitives, (graph0).schemaTypes, g.get().typeVariables);
    });
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.logic.IfElse.lazy(
        doInfer,
        () -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>, java.util.List<hydra.core.Binding>>) (result -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))),
          hydra.Inference.inferGraphTypes(
            cx,
            bins1.get(),
            (rebuildGraph).apply(bins1.get()))),
        () -> (checkBindingsTyped).apply("after case hoisting").apply(bins1.get())),
      (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.graph.Graph, java.util.List<java.util.List<hydra.packaging.TermDefinition>>>>>) (bins2 -> hydra.lib.eithers.Bind.apply(
        hydra.lib.logic.IfElse.lazy(
          doHoistPolymorphicLetBindings,
          () -> (checkBindingsTyped).apply("after let hoisting").apply((hoistPoly).apply(bins2)),
          () -> hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.Binding>>right(bins2)),
        (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.graph.Graph, java.util.List<java.util.List<hydra.packaging.TermDefinition>>>>>) (bins3 -> hydra.lib.eithers.Bind.apply(
          hydra.Adapt.adaptDataGraph(
            constraints,
            doExpand,
            bins3,
            cx,
            (rebuildGraph).apply(bins3)),
          (java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.graph.Graph, java.util.List<java.util.List<hydra.packaging.TermDefinition>>>>>) (adaptResult -> {
            hydra.util.Lazy<hydra.graph.Graph> adapted = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(adaptResult));
            hydra.util.Lazy<java.util.List<hydra.core.Binding>> adaptedBindings = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(adaptResult));
            return hydra.lib.eithers.Bind.apply(
              (checkBindingsTyped).apply("after adaptation").apply(adaptedBindings.get()),
              (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.graph.Graph, java.util.List<java.util.List<hydra.packaging.TermDefinition>>>>>) (bins4 -> {
                java.util.List<hydra.core.Binding> bins5 = (normalizeBindings).apply(bins4);
                hydra.util.Lazy<java.util.List<hydra.core.Binding>> selectedElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
                  (java.util.function.Function<hydra.core.Binding, Boolean>) (el -> hydra.lib.maybes.Maybe.applyLazy(
                    () -> false,
                    (java.util.function.Function<hydra.packaging.Namespace, Boolean>) (ns -> hydra.lib.sets.Member.apply(
                      ns,
                      namespacesSet.get())),
                    hydra.Names.namespaceOf((el).name))),
                  bins5));
                hydra.util.Lazy<java.util.Map<hydra.packaging.Namespace, java.util.List<hydra.core.Binding>>> elementsByNamespace = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                  (java.util.function.Function<java.util.Map<hydra.packaging.Namespace, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Binding, java.util.Map<hydra.packaging.Namespace, java.util.List<hydra.core.Binding>>>>) (acc -> (java.util.function.Function<hydra.core.Binding, java.util.Map<hydra.packaging.Namespace, java.util.List<hydra.core.Binding>>>) (el -> hydra.lib.maybes.Maybe.applyLazy(
                    () -> acc,
                    (java.util.function.Function<hydra.packaging.Namespace, java.util.Map<hydra.packaging.Namespace, java.util.List<hydra.core.Binding>>>) (ns -> {
                      hydra.util.Lazy<java.util.List<hydra.core.Binding>> existing = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
                        () -> (java.util.List<hydra.core.Binding>) (java.util.Collections.<hydra.core.Binding>emptyList()),
                        (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>>) (hydra.lib.equality.Identity::apply),
                        hydra.lib.maps.Lookup.apply(
                          ns,
                          acc)));
                      return hydra.lib.maps.Insert.apply(
                        ns,
                        hydra.lib.lists.Concat2.apply(
                          existing.get(),
                          java.util.Arrays.asList(el)),
                        acc);
                    }),
                    hydra.Names.namespaceOf((el).name)))),
                  (java.util.Map<hydra.packaging.Namespace, java.util.List<hydra.core.Binding>>) ((java.util.Map<hydra.packaging.Namespace, java.util.List<hydra.core.Binding>>) (hydra.lib.maps.Empty.<hydra.packaging.Namespace, java.util.List<hydra.core.Binding>>apply())),
                  selectedElements.get()));
                java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.packaging.TermDefinition>> toDef = (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.packaging.TermDefinition>>) (el -> hydra.lib.maybes.Map.apply(
                  (java.util.function.Function<hydra.core.TypeScheme, hydra.packaging.TermDefinition>) (ts -> new hydra.packaging.TermDefinition((el).name, (el).term, hydra.util.Maybe.just(ts))),
                  (el).type));
                hydra.util.Lazy<java.util.List<java.util.List<hydra.packaging.TermDefinition>>> defsGrouped = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.packaging.Namespace, java.util.List<hydra.packaging.TermDefinition>>) (ns -> {
                    hydra.util.Lazy<java.util.List<hydra.core.Binding>> elsForNs = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
                      () -> (java.util.List<hydra.core.Binding>) (java.util.Collections.<hydra.core.Binding>emptyList()),
                      (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>>) (hydra.lib.equality.Identity::apply),
                      hydra.lib.maps.Lookup.apply(
                        ns,
                        elementsByNamespace.get())));
                    return hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
                      toDef,
                      elsForNs.get()));
                  }),
                  namespaces));
                hydra.util.Lazy<hydra.graph.Graph> g = new hydra.util.Lazy<>(() -> hydra.Lexical.buildGraph(
                  bins5,
                  (java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) ((java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>apply())),
                  adapted.get().primitives));
                return hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<hydra.graph.Graph, java.util.List<java.util.List<hydra.packaging.TermDefinition>>>>right((hydra.util.Pair<hydra.graph.Graph, java.util.List<java.util.List<hydra.packaging.TermDefinition>>>) ((hydra.util.Pair<hydra.graph.Graph, java.util.List<java.util.List<hydra.packaging.TermDefinition>>>) (new hydra.util.Pair<hydra.graph.Graph, java.util.List<java.util.List<hydra.packaging.TermDefinition>>>(new hydra.graph.Graph(g.get().boundTerms, g.get().boundTypes, g.get().classConstraints, g.get().lambdaVariables, g.get().metadata, g.get().primitives, adapted.get().schemaTypes, g.get().typeVariables), defsGrouped.get()))));
              }));
          }))))));
  }

  static Boolean literalTypeSupported(hydra.coders.LanguageConstraints constraints, hydra.core.LiteralType lt) {
    java.util.function.Function<hydra.core.LiteralType, Boolean> forType = (java.util.function.Function<hydra.core.LiteralType, Boolean>) (lt2 -> (lt2).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.LiteralType instance) {
        return true;
      }

      @Override
      public Boolean visit(hydra.core.LiteralType.Float_ ft) {
        return hydra.lib.sets.Member.apply(
          (ft).value,
          (constraints).floatTypes);
      }

      @Override
      public Boolean visit(hydra.core.LiteralType.Integer_ it) {
        return hydra.lib.sets.Member.apply(
          (it).value,
          (constraints).integerTypes);
      }
    }));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        hydra.Reflect.literalTypeVariant(lt),
        (constraints).literalVariants),
      () -> (forType).apply(lt),
      () -> false);
  }

  static hydra.util.Pair<hydra.core.FloatType, hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, java.util.Set<String>>> prepareFloatType(hydra.core.FloatType ft) {
    return (ft).accept(new hydra.core.FloatType.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.core.FloatType, hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, java.util.Set<String>>> otherwise(hydra.core.FloatType instance) {
        return hydra.Adapt.prepareSame(ft);
      }

      @Override
      public hydra.util.Pair<hydra.core.FloatType, hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, java.util.Set<String>>> visit(hydra.core.FloatType.Bigfloat ignored) {
        return (hydra.util.Pair<hydra.core.FloatType, hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, java.util.Set<String>>>) ((hydra.util.Pair<hydra.core.FloatType, hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, java.util.Set<String>>>) (new hydra.util.Pair<hydra.core.FloatType, hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, java.util.Set<String>>>(new hydra.core.FloatType.Float64(), (hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, java.util.Set<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, java.util.Set<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, java.util.Set<String>>((java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>) (v -> (v).accept(new hydra.core.FloatValue.PartialVisitor<>() {
          @Override
          public hydra.core.FloatValue otherwise(hydra.core.FloatValue instance) {
            return v;
          }

          @Override
          public hydra.core.FloatValue visit(hydra.core.FloatValue.Bigfloat d) {
            return new hydra.core.FloatValue.Float64(hydra.lib.literals.BigfloatToFloat64.apply((d).value));
          }
        })), hydra.lib.sets.FromList.apply(java.util.Arrays.asList("replace arbitrary-precision floating-point numbers with 64-bit floating-point numbers (doubles)"))))))));
      }
    });
  }

  static hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>> prepareIntegerType(hydra.core.IntegerType it) {
    return (it).accept(new hydra.core.IntegerType.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>> otherwise(hydra.core.IntegerType instance) {
        return hydra.Adapt.prepareSame(it);
      }

      @Override
      public hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>> visit(hydra.core.IntegerType.Bigint ignored) {
        return (hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>>) ((hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>>) (new hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>>(new hydra.core.IntegerType.Int64(), (hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>((java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>) (v -> (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public hydra.core.IntegerValue otherwise(hydra.core.IntegerValue instance) {
            return v;
          }

          @Override
          public hydra.core.IntegerValue visit(hydra.core.IntegerValue.Bigint i) {
            return new hydra.core.IntegerValue.Int64(hydra.lib.literals.BigintToInt64.apply((i).value));
          }
        })), hydra.lib.sets.FromList.apply(java.util.Arrays.asList("replace arbitrary-precision integers with 64-bit integers"))))))));
      }

      @Override
      public hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>> visit(hydra.core.IntegerType.Uint8 ignored) {
        return (hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>>) ((hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>>) (new hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>>(new hydra.core.IntegerType.Int8(), (hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>((java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>) (v -> (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public hydra.core.IntegerValue otherwise(hydra.core.IntegerValue instance) {
            return v;
          }

          @Override
          public hydra.core.IntegerValue visit(hydra.core.IntegerValue.Uint8 i) {
            return new hydra.core.IntegerValue.Int8(hydra.lib.literals.BigintToInt8.apply(hydra.lib.literals.Uint8ToBigint.apply((i).value)));
          }
        })), hydra.lib.sets.FromList.apply(java.util.Arrays.asList("replace unsigned 8-bit integers with signed 8-bit integers"))))))));
      }

      @Override
      public hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>> visit(hydra.core.IntegerType.Uint32 ignored) {
        return (hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>>) ((hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>>) (new hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>>(new hydra.core.IntegerType.Int32(), (hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>((java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>) (v -> (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public hydra.core.IntegerValue otherwise(hydra.core.IntegerValue instance) {
            return v;
          }

          @Override
          public hydra.core.IntegerValue visit(hydra.core.IntegerValue.Uint32 i) {
            return new hydra.core.IntegerValue.Int32(hydra.lib.literals.BigintToInt32.apply(hydra.lib.literals.Uint32ToBigint.apply((i).value)));
          }
        })), hydra.lib.sets.FromList.apply(java.util.Arrays.asList("replace unsigned 32-bit integers with signed 32-bit integers"))))))));
      }

      @Override
      public hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>> visit(hydra.core.IntegerType.Uint64 ignored) {
        return (hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>>) ((hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>>) (new hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>>(new hydra.core.IntegerType.Int64(), (hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>((java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>) (v -> (v).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public hydra.core.IntegerValue otherwise(hydra.core.IntegerValue instance) {
            return v;
          }

          @Override
          public hydra.core.IntegerValue visit(hydra.core.IntegerValue.Uint64 i) {
            return new hydra.core.IntegerValue.Int64(hydra.lib.literals.BigintToInt64.apply(hydra.lib.literals.Uint64ToBigint.apply((i).value)));
          }
        })), hydra.lib.sets.FromList.apply(java.util.Arrays.asList("replace unsigned 64-bit integers with signed 64-bit integers"))))))));
      }
    });
  }

  static hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>> prepareLiteralType(hydra.core.LiteralType at) {
    return (at).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>> otherwise(hydra.core.LiteralType instance) {
        return hydra.Adapt.prepareSame(at);
      }

      @Override
      public hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>> visit(hydra.core.LiteralType.Binary ignored) {
        return (hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>>) ((hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>>) (new hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>>(new hydra.core.LiteralType.String_(), (hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>((java.util.function.Function<hydra.core.Literal, hydra.core.Literal>) (v -> (v).accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public hydra.core.Literal otherwise(hydra.core.Literal instance) {
            return v;
          }

          @Override
          public hydra.core.Literal visit(hydra.core.Literal.Binary b) {
            return new hydra.core.Literal.String_(hydra.lib.literals.BinaryToString.apply((b).value));
          }
        })), hydra.lib.sets.FromList.apply(java.util.Arrays.asList("replace binary strings with character strings"))))))));
      }

      @Override
      public hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>> visit(hydra.core.LiteralType.Float_ ft) {
        hydra.util.Pair<hydra.core.FloatType, hydra.util.Pair<java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>, java.util.Set<String>>> result = hydra.Adapt.prepareFloatType((ft).value);
        hydra.util.Lazy<java.util.Set<String>> msgs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(result)));
        java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue> rep = hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(result));
        hydra.util.Lazy<hydra.core.FloatType> rtyp = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
        return (hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>>) ((hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>>) (new hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>>(new hydra.core.LiteralType.Float_(rtyp.get()), (hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>((java.util.function.Function<hydra.core.Literal, hydra.core.Literal>) (v -> (v).accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public hydra.core.Literal otherwise(hydra.core.Literal instance) {
            return v;
          }

          @Override
          public hydra.core.Literal visit(hydra.core.Literal.Float_ fv) {
            return new hydra.core.Literal.Float_((rep).apply((fv).value));
          }
        })), msgs.get()))))));
      }

      @Override
      public hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>> visit(hydra.core.LiteralType.Integer_ it) {
        hydra.util.Pair<hydra.core.IntegerType, hydra.util.Pair<java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.Set<String>>> result = hydra.Adapt.prepareIntegerType((it).value);
        hydra.util.Lazy<java.util.Set<String>> msgs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(result)));
        java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue> rep = hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(result));
        hydra.util.Lazy<hydra.core.IntegerType> rtyp = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
        return (hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>>) ((hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>>) (new hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>>(new hydra.core.LiteralType.Integer_(rtyp.get()), (hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>((java.util.function.Function<hydra.core.Literal, hydra.core.Literal>) (v -> (v).accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public hydra.core.Literal otherwise(hydra.core.Literal instance) {
            return v;
          }

          @Override
          public hydra.core.Literal visit(hydra.core.Literal.Integer_ iv) {
            return new hydra.core.Literal.Integer_((rep).apply((iv).value));
          }
        })), msgs.get()))))));
      }
    });
  }

  static <T0, T1, T2> hydra.util.Pair<T0, hydra.util.Pair<java.util.function.Function<T1, T1>, java.util.Set<T2>>> prepareSame(T0 x) {
    return (hydra.util.Pair<T0, hydra.util.Pair<java.util.function.Function<T1, T1>, java.util.Set<T2>>>) ((hydra.util.Pair<T0, hydra.util.Pair<java.util.function.Function<T1, T1>, java.util.Set<T2>>>) (new hydra.util.Pair<T0, hydra.util.Pair<java.util.function.Function<T1, T1>, java.util.Set<T2>>>(x, (hydra.util.Pair<java.util.function.Function<T1, T1>, java.util.Set<T2>>) ((hydra.util.Pair<java.util.function.Function<T1, T1>, java.util.Set<T2>>) (new hydra.util.Pair<java.util.function.Function<T1, T1>, java.util.Set<T2>>((java.util.function.Function<T1, T1>) (y -> y), (java.util.Set<T2>) (hydra.lib.sets.Empty.<T2>apply())))))));
  }

  static <T0> hydra.util.Pair<hydra.core.Type, hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.Set<String>>> prepareType(T0 cx, hydra.core.Type typ) {
    return hydra.Strip.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.core.Type, hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.Set<String>>> otherwise(hydra.core.Type instance) {
        return hydra.Adapt.prepareSame(typ);
      }

      @Override
      public hydra.util.Pair<hydra.core.Type, hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.Set<String>>> visit(hydra.core.Type.Literal at) {
        hydra.util.Pair<hydra.core.LiteralType, hydra.util.Pair<java.util.function.Function<hydra.core.Literal, hydra.core.Literal>, java.util.Set<String>>> result = hydra.Adapt.prepareLiteralType((at).value);
        hydra.util.Lazy<java.util.Set<String>> msgs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(result)));
        java.util.function.Function<hydra.core.Literal, hydra.core.Literal> rep = hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(result));
        hydra.util.Lazy<hydra.core.LiteralType> rtyp = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
        return (hydra.util.Pair<hydra.core.Type, hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.Set<String>>>) ((hydra.util.Pair<hydra.core.Type, hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.Set<String>>>) (new hydra.util.Pair<hydra.core.Type, hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.Set<String>>>(new hydra.core.Type.Literal(rtyp.get()), (hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.Set<String>>) ((hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.Set<String>>) (new hydra.util.Pair<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.Set<String>>((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v -> (v).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Term instance) {
            return v;
          }

          @Override
          public hydra.core.Term visit(hydra.core.Term.Literal av) {
            return new hydra.core.Term.Literal((rep).apply((av).value));
          }
        })), msgs.get()))))));
      }
    });
  }

  static hydra.core.Term pushTypeAppsInward(hydra.core.Term term) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, hydra.core.Term>> go = new java.util.concurrent.atomic.AtomicReference<>();
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Type, hydra.core.Term>>> push = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> {
      java.util.function.Function<hydra.core.Field, hydra.core.Field> forField = (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (fld -> new hydra.core.Field((fld).name, go.get().apply((fld).term)));
      java.util.function.Function<hydra.core.Elimination, hydra.core.Elimination> forElimination = (java.util.function.Function<hydra.core.Elimination, hydra.core.Elimination>) (elm -> (elm).accept(new hydra.core.Elimination.PartialVisitor<>() {
        @Override
        public hydra.core.Elimination visit(hydra.core.Elimination.Record p) {
          return new hydra.core.Elimination.Record((p).value);
        }

        @Override
        public hydra.core.Elimination visit(hydra.core.Elimination.Union cs) {
          return new hydra.core.Elimination.Union(new hydra.core.CaseStatement((cs).value.typeName, hydra.lib.maybes.Map.apply(
            go.get(),
            (cs).value.default_), hydra.lib.lists.Map.apply(
            forField,
            (cs).value.cases)));
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
          return new hydra.core.Function.Lambda(new hydra.core.Lambda((l).value.parameter, (l).value.domain, go.get().apply((l).value.body)));
        }
      }));
      java.util.function.Function<hydra.core.Let, hydra.core.Let> forLet = (java.util.function.Function<hydra.core.Let, hydra.core.Let>) (lt -> {
        java.util.function.Function<hydra.core.Binding, hydra.core.Binding> mapBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, go.get().apply((b).term), (b).type));
        return new hydra.core.Let(hydra.lib.lists.Map.apply(
          mapBinding,
          (lt).bindings), go.get().apply((lt).body));
      });
      java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, java.util.Map<hydra.core.Term, hydra.core.Term>> forMap = (java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, java.util.Map<hydra.core.Term, hydra.core.Term>>) (m -> {
        java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>> forPair = (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) (p -> (hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(go.get().apply(hydra.lib.pairs.First.apply(p)), go.get().apply(hydra.lib.pairs.Second.apply(p))))));
        return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          forPair,
          hydra.lib.maps.ToList.apply(m)));
      });
      return (t).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term visit(hydra.core.Term.Annotated at) {
          return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(go.get().apply((at).value.body), (at).value.annotation));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Application a) {
          return new hydra.core.Term.Application(new hydra.core.Application(go.get().apply((a).value.function), go.get().apply((a).value.argument)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Either e) {
          return new hydra.core.Term.Either(hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (l -> hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(go.get().apply(l))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (r -> hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(go.get().apply(r))),
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
            go.get(),
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
            go.get(),
            (m).value));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Pair p) {
          return new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(go.get().apply(hydra.lib.pairs.First.apply((p).value)), go.get().apply(hydra.lib.pairs.Second.apply((p).value))))));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Record r) {
          return new hydra.core.Term.Record(new hydra.core.Record((r).value.typeName, hydra.lib.lists.Map.apply(
            forField,
            (r).value.fields)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Set s) {
          return new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
            go.get(),
            hydra.lib.sets.ToList.apply((s).value))));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
          hydra.core.Term body1 = go.get().apply((tt).value.body);
          return push.get().apply(body1).apply((tt).value.type);
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
          return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda((ta).value.parameter, go.get().apply((ta).value.body)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Union i) {
          return new hydra.core.Term.Union(new hydra.core.Injection((i).value.typeName, (forField).apply((i).value.field)));
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
          return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm((wt).value.typeName, go.get().apply((wt).value.body)));
        }
      });
    }));
    push.set((java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Type, hydra.core.Term>>) (body -> (java.util.function.Function<hydra.core.Type, hydra.core.Term>) (typ -> (body).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(body, typ));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Application a) {
        return go.get().apply(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm((a).value.function, typ)), (a).value.argument)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(new hydra.core.Term.Function((f).value), typ));
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda l) {
            return go.get().apply(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda((l).value.parameter, (l).value.domain, new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm((l).value.body, typ))))));
          }
        });
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        return go.get().apply(new hydra.core.Term.Let(new hydra.core.Let((lt).value.bindings, new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm((lt).value.body, typ)))));
      }
    }))));
    return go.get().apply(term);
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.List<java.util.List<hydra.packaging.TypeDefinition>>>> schemaGraphToDefinitions(hydra.coders.LanguageConstraints constraints, hydra.graph.Graph graph, java.util.List<java.util.List<hydra.core.Name>> nameLists, T0 cx) {
    java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap = hydra.Adapt.adaptLiteralTypesMap(constraints);
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.errors.DecodingError, hydra.errors.Error_>) (e -> new hydra.errors.Error_.Decoding(e)),
        (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.Map<hydra.core.Name, hydra.core.Type>>) (x -> x),
        hydra.Environment.graphAsTypes(
          graph,
          hydra.Lexical.graphToBindings(graph))),
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.List<java.util.List<hydra.packaging.TypeDefinition>>>>>) (tmap0 -> hydra.lib.eithers.Bind.apply(
        hydra.Adapt.adaptGraphSchema(
          constraints,
          litmap,
          tmap0),
        (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.List<java.util.List<hydra.packaging.TypeDefinition>>>>>) (tmap1 -> {
          java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, hydra.packaging.TypeDefinition> toDef = (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, hydra.packaging.TypeDefinition>) (pair -> new hydra.packaging.TypeDefinition(hydra.lib.pairs.First.apply(pair), new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), hydra.lib.pairs.Second.apply(pair), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))));
          return hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.List<java.util.List<hydra.packaging.TypeDefinition>>>>right((hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.List<java.util.List<hydra.packaging.TypeDefinition>>>) ((hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.List<java.util.List<hydra.packaging.TypeDefinition>>>) (new hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.List<java.util.List<hydra.packaging.TypeDefinition>>>(tmap1, hydra.lib.lists.Map.apply(
            (java.util.function.Function<java.util.List<hydra.core.Name>, java.util.List<hydra.packaging.TypeDefinition>>) (names -> hydra.lib.lists.Map.apply(
              toDef,
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(n, hydra.lib.maybes.FromJust.apply(hydra.lib.maps.Lookup.apply(
                  n,
                  tmap1)))))),
                names))),
            nameLists)))));
        }))));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.coders.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> simpleLanguageAdapter(hydra.coders.Language lang, T0 cx, hydra.graph.Graph g, hydra.core.Type typ) {
    hydra.coders.LanguageConstraints constraints = (lang).constraints;
    java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap = hydra.Adapt.adaptLiteralTypesMap(constraints);
    return hydra.lib.eithers.Bind.apply(
      hydra.Adapt.adaptType(
        constraints,
        litmap,
        typ),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.coders.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (adaptedType -> hydra.util.Either.<hydra.errors.Error_, hydra.coders.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.coders.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.coders.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.coders.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.coders.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.coders.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(false, typ, adaptedType, (hydra.coders.Coder<hydra.core.Term, hydra.core.Term>) ((hydra.coders.Coder<hydra.core.Term, hydra.core.Term>) (new hydra.coders.Coder<hydra.core.Term, hydra.core.Term>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (term -> hydra.Adapt.adaptTerm(
        constraints,
        litmap,
        cx2,
        g,
        term))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (term -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(term))))))))))))));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Term>> termAlternatives(T0 cx, hydra.graph.Graph graph, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Term>> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.Term>>right((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList()));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Term>> visit(hydra.core.Term.Annotated at) {
        hydra.core.Term term2 = (at).value.body;
        return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.Term>>right(java.util.Arrays.asList(term2));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Term>> visit(hydra.core.Term.Maybe ot) {
        return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.Term>>right(java.util.Arrays.asList(new hydra.core.Term.List(hydra.lib.maybes.Maybe.applyLazy(
          () -> (java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList()),
          (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (term2 -> java.util.Arrays.asList(term2)),
          (ot).value))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Term>> visit(hydra.core.Term.TypeLambda abs) {
        hydra.core.Term term2 = (abs).value.body;
        return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.Term>>right(java.util.Arrays.asList(term2));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Term>> visit(hydra.core.Term.TypeApplication ta) {
        hydra.core.Term term2 = (ta).value.body;
        return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.Term>>right(java.util.Arrays.asList(term2));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Term>> visit(hydra.core.Term.Union inj) {
        hydra.core.Field field = (inj).value.field;
        hydra.core.Name fname = (field).name;
        hydra.core.Term fterm = (field).term;
        java.util.function.Function<hydra.core.FieldType, hydra.core.Field> forFieldType = (java.util.function.Function<hydra.core.FieldType, hydra.core.Field>) (ft -> {
          hydra.core.Name ftname = (ft).name;
          return new hydra.core.Field(fname, new hydra.core.Term.Maybe(hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              ftname,
              fname),
            () -> hydra.util.Maybe.just(fterm),
            () -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))));
        });
        hydra.core.Name tname = (inj).value.typeName;
        return hydra.lib.eithers.Bind.apply(
          hydra.Resolution.<T0>requireUnionType(
            cx,
            graph,
            tname),
          (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Term>>>) (rt -> hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.Term>>right(java.util.Arrays.asList(new hydra.core.Term.Record(new hydra.core.Record(tname, hydra.lib.lists.Map.apply(
            forFieldType,
            rt)))))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Term>> visit(hydra.core.Term.Unit ignored) {
        return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.Term>>right(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Term>> visit(hydra.core.Term.Wrap wt) {
        hydra.core.Term term2 = (wt).value.body;
        return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.core.Term>>right(java.util.Arrays.asList(term2));
      }
    });
  }

  static java.util.List<hydra.core.Type> typeAlternatives(hydra.core.Type type) {
    return (type).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (java.util.List<hydra.core.Type>) (java.util.Collections.<hydra.core.Type>emptyList());
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Annotated at) {
        hydra.core.Type type2 = (at).value.body;
        return java.util.Arrays.asList(type2);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Maybe ot) {
        return java.util.Arrays.asList(new hydra.core.Type.List((ot).value));
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Union rt) {
        java.util.function.Function<hydra.core.FieldType, hydra.core.FieldType> toOptField = (java.util.function.Function<hydra.core.FieldType, hydra.core.FieldType>) (f -> new hydra.core.FieldType((f).name, new hydra.core.Type.Maybe((f).type)));
        hydra.util.Lazy<java.util.List<hydra.core.FieldType>> optFields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          toOptField,
          (rt).value));
        return java.util.Arrays.asList(new hydra.core.Type.Record(optFields.get()));
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Unit ignored) {
        return java.util.Arrays.asList(new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()));
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Void_ ignored) {
        return java.util.Arrays.asList(new hydra.core.Type.Unit());
      }
    });
  }
}
