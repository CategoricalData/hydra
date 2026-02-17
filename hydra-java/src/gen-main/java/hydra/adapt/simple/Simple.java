// Note: this is an automatically generated file. Do not edit.

package hydra.adapt.simple;

/**
 * Simple, one-way adapters for types and terms
 */
public interface Simple {
  static hydra.util.Maybe<hydra.core.FloatType> adaptFloatType(hydra.coders.LanguageConstraints constraints, hydra.core.FloatType ft) {
    java.util.function.Function<hydra.core.FloatType, hydra.util.Maybe<hydra.core.FloatType>> alt = (java.util.function.Function<hydra.core.FloatType, hydra.util.Maybe<hydra.core.FloatType>>) (v1 -> hydra.adapt.simple.Simple.adaptFloatType(
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
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Graph> adaptDataGraph(hydra.coders.LanguageConstraints constraints, Boolean doExpand, hydra.graph.Graph graph0) {
    hydra.core.Term body0 = (graph0).body;
    java.util.List<hydra.core.Binding> els0 = (graph0).elements;
    java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>> env0 = (graph0).environment;
    java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap = hydra.adapt.simple.Simple.adaptLiteralTypesMap(constraints);
    java.util.Map<hydra.core.Name, hydra.graph.Primitive> prims0 = (graph0).primitives;
    hydra.util.Maybe<hydra.graph.Graph> schema0 = (graph0).schema;
    return hydra.lib.flows.Bind.apply(
      hydra.lib.maybes.Maybe.apply(
        hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.graph.Graph>) (hydra.util.Maybe.<hydra.graph.Graph>nothing())),
        (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.graph.Graph>>>) (sg -> hydra.lib.flows.Bind.apply(
          hydra.schemas.Schemas.graphAsTypes(sg),
          (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.graph.Graph>>>) (tmap0 -> hydra.lib.flows.Bind.apply(
            hydra.adapt.simple.Simple.adaptGraphSchema(
              constraints,
              litmap,
              tmap0),
            (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.graph.Graph>>>) (tmap1 -> {
              java.util.List<hydra.core.Binding> emap = hydra.schemas.Schemas.typesToElements(tmap1);
              return hydra.lib.flows.Pure.apply(hydra.util.Maybe.just(new hydra.graph.Graph(emap, (sg).environment, (sg).types, (sg).body, (sg).primitives, (sg).schema)));
            }))))),
        schema0),
      (java.util.function.Function<hydra.util.Maybe<hydra.graph.Graph>, hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Graph>>) (schema1 -> {
        hydra.core.Term gterm0 = hydra.schemas.Schemas.graphAsTerm(graph0);
        return hydra.lib.flows.Bind.apply(
          hydra.lib.logic.IfElse.lazy(
            doExpand,
            () -> hydra.adapt.simple.Simple.adaptDataGraph_transform(
              doExpand,
              hydra.adapt.simple.Simple::pushTypeAppsInward,
              (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (p0 -> p1 -> hydra.reduction.Reduction.etaExpandTermNew(
                p0,
                p1)),
              hydra.rewriting.Rewriting::liftLambdaAboveLet,
              hydra.rewriting.Rewriting::unshadowVariables,
              graph0,
              gterm0),
            () -> hydra.lib.flows.Pure.apply(gterm0)),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Graph>>) (gterm1 -> hydra.lib.flows.Bind.apply(
            hydra.adapt.simple.Simple.adaptTerm(
              constraints,
              litmap,
              gterm1),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Graph>>) (gterm2 -> hydra.lib.flows.Bind.apply(
              hydra.rewriting.Rewriting.rewriteTermM(
                (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (v2 -> hydra.adapt.simple.Simple.adaptDataGraph_adaptLambdaDomains(
                  constraints,
                  litmap,
                  v1,
                  v2))),
                gterm2),
              (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Graph>>) (gterm3 -> {
                java.util.List<hydra.core.Binding> els1Raw = hydra.schemas.Schemas.termAsGraph(gterm3);
                return hydra.lib.flows.Bind.apply(
                  hydra.lib.flows.MapList.apply(
                    (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Binding>>) (v1 -> hydra.adapt.simple.Simple.adaptDataGraph_processBinding(
                      constraints,
                      litmap,
                      v1)),
                    els1Raw),
                  (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Graph>>) (els1 -> hydra.lib.flows.Bind.apply(
                    hydra.lib.flows.MapElems.apply(
                      (java.util.function.Function<hydra.graph.Primitive, hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Primitive>>) (v1 -> hydra.adapt.simple.Simple.adaptPrimitive(
                        constraints,
                        litmap,
                        v1)),
                      prims0),
                    (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.graph.Primitive>, hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Graph>>) (prims1 -> hydra.lib.flows.Pure.apply(new hydra.graph.Graph(els1, env0, (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), new hydra.core.Term.Unit(), prims1, schema1))))));
              }))))));
      }));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T3, T2> adaptDataGraph_transform(Boolean doExpand, java.util.function.Function<T0, T1> hydra_adapt_simple_pushTypeAppsInward2, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<T1, T0>> hydra_reduction_etaExpandTermNew2, java.util.function.Function<T1, T2> hydra_rewriting_liftLambdaAboveLet2, java.util.function.Function<T1, T1> hydra_rewriting_unshadowVariables2, hydra.graph.Graph graph, T0 gterm) {
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T3>graphToTypeContext(graph),
      (java.util.function.Function<hydra.typing.TypeContext, hydra.compute.Flow<T3, T2>>) (tx -> {
        T1 gterm1 = (hydra_rewriting_unshadowVariables2).apply((hydra_adapt_simple_pushTypeAppsInward2).apply(gterm));
        hydra.util.Lazy<T1> gterm2 = new hydra.util.Lazy<>(() -> (hydra_rewriting_unshadowVariables2).apply(hydra.lib.logic.IfElse.lazy(
          doExpand,
          () -> (hydra_adapt_simple_pushTypeAppsInward2).apply(((hydra_reduction_etaExpandTermNew2).apply(tx)).apply(gterm1)),
          () -> gterm1)));
        return hydra.lib.flows.Pure.apply((hydra_rewriting_liftLambdaAboveLet2).apply(gterm2.get()));
      }));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.core.Term> adaptDataGraph_adaptLambdaDomains(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, java.util.function.Function<T0, hydra.compute.Flow<T1, hydra.core.Term>> recurse, T0 term) {
    return hydra.lib.flows.Bind.apply(
      (recurse).apply(term),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>) (rewritten -> (rewritten).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.compute.Flow<T1, hydra.core.Term> otherwise(hydra.core.Term instance) {
          return hydra.lib.flows.Pure.apply(rewritten);
        }
        
        @Override
        public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Function f) {
          return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.compute.Flow<T1, hydra.core.Term> otherwise(hydra.core.Function instance) {
              return hydra.lib.flows.Pure.apply(new hydra.core.Term.Function((f).value));
            }
            
            @Override
            public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Function.Lambda l) {
              return hydra.lib.flows.Bind.apply(
                hydra.lib.maybes.Maybe.apply(
                  hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
                  (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T1, hydra.util.Maybe<hydra.core.Type>>>) (dom -> hydra.lib.flows.Bind.apply(
                    hydra.adapt.simple.Simple.<T1>adaptType(
                      constraints,
                      litmap,
                      dom),
                    (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T1, hydra.util.Maybe<hydra.core.Type>>>) (dom1 -> hydra.lib.flows.Pure.apply(hydra.util.Maybe.just(dom1))))),
                  ((l).value).domain),
                (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.compute.Flow<T1, hydra.core.Term>>) (adaptedDomain -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(((l).value).parameter, adaptedDomain, ((l).value).body))))));
            }
          });
        }
      })));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.core.Term> adaptDataGraph_adaptNestedTypes(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, java.util.function.Function<T0, hydra.compute.Flow<T1, hydra.core.Term>> recurse, T0 term) {
    return hydra.lib.flows.Bind.apply(
      (recurse).apply(term),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>) (rewritten -> (rewritten).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.compute.Flow<T1, hydra.core.Term> otherwise(hydra.core.Term instance) {
          return hydra.lib.flows.Pure.apply(rewritten);
        }
        
        @Override
        public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.core.Term.Let lt) {
          return hydra.lib.flows.Bind.apply(
            hydra.lib.flows.MapList.apply(
              (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<T1, hydra.core.Binding>>) (v1 -> hydra.adapt.simple.Simple.<T1>adaptDataGraph_adaptB(
                constraints,
                litmap,
                v1)),
              ((lt).value).bindings),
            (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.compute.Flow<T1, hydra.core.Term>>) (adaptedBindings -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Let(new hydra.core.Let(adaptedBindings, ((lt).value).body)))));
        }
      })));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Binding> adaptDataGraph_processBinding(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, hydra.core.Binding el) {
    return hydra.lib.flows.Bind.apply(
      hydra.rewriting.Rewriting.<T0>rewriteTermM(
        (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>) (v2 -> hydra.adapt.simple.Simple.adaptDataGraph_adaptNestedTypes(
          constraints,
          litmap,
          v1,
          v2))),
        (el).term),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Binding>>) (newTerm -> hydra.lib.flows.Bind.apply(
        hydra.lib.maybes.Maybe.apply(
          hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.TypeScheme>>>) (ts -> hydra.lib.flows.Bind.apply(
            hydra.adapt.simple.Simple.<T0>adaptTypeScheme(
              constraints,
              litmap,
              ts),
            (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.TypeScheme>>>) (ts1 -> hydra.lib.flows.Pure.apply(hydra.util.Maybe.just(ts1))))),
          (el).type),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.TypeScheme>, hydra.compute.Flow<T0, hydra.core.Binding>>) (adaptedType -> hydra.lib.flows.Pure.apply(new hydra.core.Binding((el).name, newTerm, adaptedType))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Binding> adaptDataGraph_adaptB(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, hydra.core.Binding b) {
    return hydra.lib.flows.Bind.apply(
      hydra.lib.maybes.Maybe.apply(
        hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
        (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.TypeScheme>>>) (ts -> hydra.lib.flows.Bind.apply(
          hydra.adapt.simple.Simple.<T0>adaptTypeScheme(
            constraints,
            litmap,
            ts),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.TypeScheme>>>) (ts1 -> hydra.lib.flows.Pure.apply(hydra.util.Maybe.just(ts1))))),
        (b).type),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.TypeScheme>, hydra.compute.Flow<T0, hydra.core.Binding>>) (adaptedBType -> hydra.lib.flows.Pure.apply(new hydra.core.Binding((b).name, (b).term, adaptedBType))));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, java.util.Map<T0, hydra.core.Type>> adaptGraphSchema(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, java.util.Map<T0, hydra.core.Type> types0) {
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapList.apply(
        (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.Type>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Type>>>) (v1 -> hydra.adapt.simple.Simple.<T0, T1>adaptGraphSchema_mapPair(
          constraints,
          litmap,
          v1)),
        hydra.lib.maps.ToList.apply(types0)),
      (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<T0, hydra.core.Type>>, hydra.compute.Flow<T1, java.util.Map<T0, hydra.core.Type>>>) (pairs -> hydra.lib.flows.Pure.apply(hydra.lib.maps.FromList.apply(pairs))));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Type>> adaptGraphSchema_mapPair(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, hydra.util.Tuple.Tuple2<T0, hydra.core.Type> pair) {
    hydra.util.Lazy<hydra.core.Type> typ = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
    return hydra.lib.flows.Bind.apply(
      hydra.adapt.simple.Simple.<T1>adaptType(
        constraints,
        litmap,
        typ.get()),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T0, hydra.core.Type>>>) (typ1 -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T0, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Type>(hydra.adapt.simple.Simple.adaptGraphSchema_name(pair), typ1))))));
  }
  
  static <T0, T1> T0 adaptGraphSchema_name(hydra.util.Tuple.Tuple2<T0, T1> pair) {
    return hydra.lib.pairs.First.apply(pair);
  }
  
  static hydra.util.Maybe<hydra.core.IntegerType> adaptIntegerType(hydra.coders.LanguageConstraints constraints, hydra.core.IntegerType it) {
    java.util.function.Function<hydra.core.IntegerType, hydra.util.Maybe<hydra.core.IntegerType>> alt = (java.util.function.Function<hydra.core.IntegerType, hydra.util.Maybe<hydra.core.IntegerType>>) (v1 -> hydra.adapt.simple.Simple.adaptIntegerType(
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
            return new hydra.core.Literal.Integer_(hydra.literals.Literals.bigintToIntegerValue(
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
            return new hydra.core.Literal.Float_(hydra.literals.Literals.bigfloatToFloatValue(
              (ft).value,
              hydra.literals.Literals.floatValueToBigfloat((f).value)));
          }
        });
      }
      
      @Override
      public hydra.core.Literal visit(hydra.core.Literal.Integer_ i) {
        return (lt).accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public hydra.core.Literal visit(hydra.core.LiteralType.Integer_ it) {
            return new hydra.core.Literal.Integer_(hydra.literals.Literals.bigintToIntegerValue(
              (it).value,
              hydra.literals.Literals.integerValueToBigint((i).value)));
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
          hydra.adapt.simple.Simple.adaptIntegerType(
            constraints,
            new hydra.core.IntegerType.Int8()));
      }
      
      @Override
      public hydra.util.Maybe<hydra.core.LiteralType> visit(hydra.core.LiteralType.Float_ ft) {
        return hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.FloatType, hydra.core.LiteralType>) (x -> new hydra.core.LiteralType.Float_(x)),
          hydra.adapt.simple.Simple.adaptFloatType(
            constraints,
            (ft).value));
      }
      
      @Override
      public hydra.util.Maybe<hydra.core.LiteralType> visit(hydra.core.LiteralType.Integer_ it) {
        return hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.IntegerType, hydra.core.LiteralType>) (x -> new hydra.core.LiteralType.Integer_(x)),
          hydra.adapt.simple.Simple.adaptIntegerType(
            constraints,
            (it).value));
      }
    }));
    return hydra.lib.logic.IfElse.lazy(
      hydra.adapt.simple.Simple.literalTypeSupported(
        constraints,
        lt),
      () -> (hydra.util.Maybe<hydra.core.LiteralType>) (hydra.util.Maybe.<hydra.core.LiteralType>nothing()),
      () -> (forUnsupported).apply(lt));
  }
  
  static java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> adaptLiteralTypesMap(hydra.coders.LanguageConstraints constraints) {
    java.util.function.Function<hydra.core.LiteralType, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.LiteralType, hydra.core.LiteralType>>> tryType = (java.util.function.Function<hydra.core.LiteralType, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.LiteralType, hydra.core.LiteralType>>>) (lt -> hydra.lib.maybes.Maybe.apply(
      (hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.LiteralType, hydra.core.LiteralType>>) (hydra.util.Maybe.<hydra.util.Tuple.Tuple2<hydra.core.LiteralType, hydra.core.LiteralType>>nothing()),
      (java.util.function.Function<hydra.core.LiteralType, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.LiteralType, hydra.core.LiteralType>>>) (lt2 -> hydra.util.Maybe.just((hydra.util.Tuple.Tuple2<hydra.core.LiteralType, hydra.core.LiteralType>) ((hydra.util.Tuple.Tuple2<hydra.core.LiteralType, hydra.core.LiteralType>) (new hydra.util.Tuple.Tuple2<hydra.core.LiteralType, hydra.core.LiteralType>(lt, lt2))))),
      hydra.adapt.simple.Simple.adaptLiteralType(
        constraints,
        lt)));
    return hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      tryType,
      hydra.reflect.Reflect.literalTypes())));
  }
  
  static <T0> hydra.core.Literal adaptLiteralValue(java.util.Map<T0, hydra.core.LiteralType> litmap, T0 lt, hydra.core.Literal l) {
    return hydra.lib.maybes.Maybe.apply(
      new hydra.core.Literal.String_(hydra.show.core.Core.literal(l)),
      (java.util.function.Function<hydra.core.LiteralType, hydra.core.Literal>) (lt2 -> hydra.adapt.simple.Simple.adaptLiteral(
        lt2,
        l)),
      hydra.lib.maps.Lookup.apply(
        lt,
        litmap));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.graph.Primitive> adaptPrimitive(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, hydra.graph.Primitive prim0) {
    hydra.core.TypeScheme ts0 = (prim0).type;
    return hydra.lib.flows.Bind.apply(
      hydra.adapt.simple.Simple.<T0>adaptTypeScheme(
        constraints,
        litmap,
        ts0),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.graph.Primitive>>) (ts1 -> hydra.lib.flows.Pure.apply(new hydra.graph.Primitive((prim0).name, ts1, (prim0).implementation))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> adaptTerm(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, hydra.core.Term term0) {
    return hydra.rewriting.Rewriting.rewriteTermM(
      (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (v2 -> hydra.adapt.simple.Simple.adaptTerm_rewrite(
        constraints,
        (java.util.function.Function<hydra.coders.LanguageConstraints, java.util.function.Function<hydra.core.LiteralType, Boolean>>) (p0 -> p1 -> hydra.adapt.simple.Simple.literalTypeSupported(
          p0,
          p1)),
        hydra.adapt.simple.Simple::termAlternatives,
        hydra.reflect.Reflect::literalType,
        hydra.reflect.Reflect::termVariant,
        hydra.show.core.Core::term,
        litmap,
        v1,
        v2))),
      term0);
  }
  
  static <T0, T1> hydra.compute.Flow<T0, hydra.core.Term> adaptTerm_rewrite(hydra.coders.LanguageConstraints constraints, java.util.function.Function<hydra.coders.LanguageConstraints, java.util.function.Function<hydra.core.LiteralType, Boolean>> hydra_adapt_simple_literalTypeSupported2, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, java.util.List<hydra.core.Term>>> hydra_adapt_simple_termAlternatives2, java.util.function.Function<hydra.core.Literal, hydra.core.LiteralType> hydra_reflect_literalType2, java.util.function.Function<hydra.core.Term, hydra.variants.TermVariant> hydra_reflect_termVariant2, java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, java.util.function.Function<T1, hydra.compute.Flow<T0, hydra.core.Term>> recurse, T1 term0) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.Term>>>> forUnsupported = new java.util.concurrent.atomic.AtomicReference<>();
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.Term>>>> tryTerm = new java.util.concurrent.atomic.AtomicReference<>();
    forUnsupported.set((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.Term>>>) (term -> {
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.Term>>>> forNonNull = new java.util.concurrent.atomic.AtomicReference<>();
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.Term>>>> tryAlts = new java.util.concurrent.atomic.AtomicReference<>();
      forNonNull.set((java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.Term>>>) (alts -> hydra.lib.flows.Bind.apply(
        (tryTerm.get()).apply(hydra.lib.lists.Head.apply(alts)),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.Term>>>) (mterm -> hydra.lib.maybes.Maybe.apply(
          (tryAlts.get()).apply(hydra.lib.lists.Tail.apply(alts)),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.Term>>>) (t -> hydra.lib.flows.Pure.apply(hydra.util.Maybe.just(t))),
          mterm)))));
      tryAlts.set((java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.Term>>>) (alts -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(alts),
        () -> hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())),
        () -> (forNonNull.get()).apply(alts))));
      return hydra.lib.flows.Bind.apply(
        (hydra_adapt_simple_termAlternatives2).apply(term),
        (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.Term>>>) (alts -> (tryAlts.get()).apply(alts)));
    }));
    tryTerm.set((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.Term>>>) (term -> {
      hydra.util.Lazy<Boolean> supportedVariant = new hydra.util.Lazy<>(() -> hydra.lib.sets.Member.apply(
        (hydra_reflect_termVariant2).apply(term),
        (constraints).termVariants));
      return hydra.lib.logic.IfElse.lazy(
        supportedVariant.get(),
        () -> hydra.adapt.simple.Simple.adaptTerm_forSupported(
          constraints,
          hydra_adapt_simple_literalTypeSupported2,
          hydra_reflect_literalType2,
          litmap,
          term),
        () -> (forUnsupported.get()).apply(term));
    }));
    return hydra.lib.flows.Bind.apply(
      (recurse).apply(term0),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>) (term1 -> (term1).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
          return hydra.lib.flows.Bind.apply(
            (tryTerm.get()).apply(term1),
            (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.compute.Flow<T0, hydra.core.Term>>) (mterm -> hydra.lib.maybes.Maybe.apply(
              hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
                "no alternatives for term: ",
                (hydra_show_core_term2).apply(term1))),
              (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>) (term2 -> hydra.lib.flows.Pure.apply(term2)),
              mterm)));
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.TypeApplication ta) {
          return hydra.lib.flows.Bind.apply(
            hydra.adapt.simple.Simple.<T0>adaptType(
              constraints,
              litmap,
              ((ta).value).type),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Term>>) (atyp -> hydra.lib.flows.Pure.apply(new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(((ta).value).body, atyp)))));
        }
        
        @Override
        public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.TypeLambda ignored) {
          return hydra.lib.flows.Pure.apply(term1);
        }
      })));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T2, hydra.util.Maybe<hydra.core.Term>> adaptTerm_forSupported(T0 constraints, java.util.function.Function<T0, java.util.function.Function<T1, Boolean>> hydra_adapt_simple_literalTypeSupported2, java.util.function.Function<hydra.core.Literal, T1> hydra_reflect_literalType2, java.util.Map<T1, hydra.core.LiteralType> litmap, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T2, hydra.util.Maybe<hydra.core.Term>> otherwise(hydra.core.Term instance) {
        return hydra.lib.flows.Pure.apply(hydra.util.Maybe.just(term));
      }
      
      @Override
      public hydra.compute.Flow<T2, hydra.util.Maybe<hydra.core.Term>> visit(hydra.core.Term.Literal l) {
        T1 lt = (hydra_reflect_literalType2).apply((l).value);
        return hydra.lib.flows.Pure.apply(hydra.util.Maybe.just(hydra.lib.logic.IfElse.lazy(
          ((hydra_adapt_simple_literalTypeSupported2).apply(constraints)).apply(lt),
          () -> term,
          () -> new hydra.core.Term.Literal(hydra.adapt.simple.Simple.<T1>adaptLiteralValue(
            litmap,
            lt,
            (l).value)))));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> adaptType(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, hydra.core.Type type0) {
    java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>> forSupported = (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (typ -> (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.util.Maybe.just(typ);
      }
      
      @Override
      public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Type.Literal lt) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.adapt.simple.Simple.literalTypeSupported(
            constraints,
            (lt).value),
          () -> hydra.util.Maybe.just(typ),
          () -> hydra.lib.maybes.Maybe.apply(
            hydra.util.Maybe.just(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            (java.util.function.Function<hydra.core.LiteralType, hydra.util.Maybe<hydra.core.Type>>) (lt2 -> hydra.util.Maybe.just(new hydra.core.Type.Literal(lt2))),
            hydra.lib.maps.Lookup.apply(
              (lt).value,
              litmap)));
      }
    }));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>> forUnsupported = new java.util.concurrent.atomic.AtomicReference<>();
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>> tryType = new java.util.concurrent.atomic.AtomicReference<>();
    forUnsupported.set((java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (typ -> {
      java.util.List<hydra.core.Type> alts = hydra.adapt.simple.Simple.typeAlternatives(typ);
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>> tryAlts = new java.util.concurrent.atomic.AtomicReference<>();
      tryAlts.set((java.util.function.Function<java.util.List<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (alts2 -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(alts2),
        () -> (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()),
        () -> hydra.lib.maybes.Maybe.apply(
          (tryAlts.get()).apply(hydra.lib.lists.Tail.apply(alts2)),
          (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (t -> hydra.util.Maybe.just(t)),
          (tryType.get()).apply(hydra.lib.lists.Head.apply(alts2))))));
      return (tryAlts.get()).apply(alts);
    }));
    tryType.set((java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (typ -> {
      hydra.util.Lazy<Boolean> supportedVariant = new hydra.util.Lazy<>(() -> hydra.lib.sets.Member.apply(
        hydra.reflect.Reflect.typeVariant(typ),
        (constraints).typeVariants));
      return hydra.lib.logic.IfElse.lazy(
        supportedVariant.get(),
        () -> (forSupported).apply(typ),
        () -> (forUnsupported.get()).apply(typ));
    }));
    return hydra.rewriting.Rewriting.<T0>rewriteTypeM(
      (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>>) (v1 -> (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (v2 -> hydra.adapt.simple.Simple.adaptType_rewrite(
        hydra.show.core.Core::type,
        tryType.get(),
        v1,
        v2))),
      type0);
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T3, T2> adaptType_rewrite(java.util.function.Function<T0, String> hydra_show_core_type2, java.util.function.Function<T1, hydra.util.Maybe<T2>> tryType, java.util.function.Function<T0, hydra.compute.Flow<T3, T1>> recurse, T0 typ) {
    return hydra.lib.flows.Bind.apply(
      (recurse).apply(typ),
      (java.util.function.Function<T1, hydra.compute.Flow<T3, T2>>) (type1 -> hydra.lib.maybes.Maybe.apply(
        hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
          "no alternatives for type: ",
          (hydra_show_core_type2).apply(typ))),
        (java.util.function.Function<T2, hydra.compute.Flow<T3, T2>>) (type2 -> hydra.lib.flows.Pure.apply(type2)),
        (tryType).apply(type1))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.TypeScheme> adaptTypeScheme(hydra.coders.LanguageConstraints constraints, java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap, hydra.core.TypeScheme ts0) {
    hydra.core.Type t0 = (ts0).type;
    java.util.List<hydra.core.Name> vars0 = (ts0).variables;
    return hydra.lib.flows.Bind.apply(
      hydra.adapt.simple.Simple.<T0>adaptType(
        constraints,
        litmap,
        t0),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.TypeScheme>>) (t1 -> hydra.lib.flows.Pure.apply(new hydra.core.TypeScheme(vars0, t1, (ts0).constraints))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<hydra.graph.Graph, java.util.List<java.util.List<hydra.module.TermDefinition>>>> dataGraphToDefinitions(hydra.coders.LanguageConstraints constraints, Boolean doExpand, Boolean doHoistCaseStatements, Boolean doHoistPolymorphicLetBindings, hydra.graph.Graph graph, java.util.List<hydra.module.Namespace> namespaces) {
    hydra.util.Lazy<java.util.Set<hydra.module.Namespace>> namespacesSet = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(namespaces));
    java.util.function.Function<hydra.core.Binding, Boolean> isParentBinding = (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.maybes.Maybe.apply(
      false,
      (java.util.function.Function<hydra.module.Namespace, Boolean>) (ns -> hydra.lib.sets.Member.apply(
        ns,
        namespacesSet.get())),
      hydra.names.Names.namespaceOf((b).name)));
    java.util.function.Function<hydra.graph.Graph, hydra.graph.Graph> hoistPoly = (java.util.function.Function<hydra.graph.Graph, hydra.graph.Graph>) (graphBefore -> {
      hydra.core.Let letBefore = hydra.schemas.Schemas.graphAsLet(graphBefore);
      hydra.core.Let letAfter = hydra.hoisting.Hoisting.hoistPolymorphicLetBindings(
        isParentBinding,
        letBefore);
      return new hydra.graph.Graph((letAfter).bindings, (graphBefore).environment, (graphBefore).types, (graphBefore).body, (graphBefore).primitives, (graphBefore).schema);
    });
    return hydra.lib.flows.Bind.apply(
      hydra.lib.logic.IfElse.lazy(
        doHoistCaseStatements,
        () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Graph>>) (() -> {
          hydra.core.Term gterm0 = hydra.schemas.Schemas.graphAsTerm(graph);
          return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Graph>>) (() -> {
            hydra.core.Term gterm1 = hydra.rewriting.Rewriting.unshadowVariables(gterm0);
            return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Graph>>) (() -> {
              java.util.List<hydra.core.Binding> newElements = hydra.schemas.Schemas.termAsGraph(gterm1);
              return hydra.lib.flows.Pure.apply(new hydra.graph.Graph(newElements, (graph).environment, (graph).types, (graph).body, (graph).primitives, (graph).schema));
            })).get();
          })).get();
        })).get(),
        () -> hydra.lib.flows.Pure.apply(graph)),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<hydra.graph.Graph, java.util.List<java.util.List<hydra.module.TermDefinition>>>>>) (graphu0 -> hydra.lib.flows.Bind.apply(
        hydra.lib.logic.IfElse.lazy(
          doHoistCaseStatements,
          () -> hydra.hoisting.Hoisting.hoistCaseStatementsInGraph(graphu0),
          () -> hydra.lib.flows.Pure.apply(graphu0)),
        (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<hydra.graph.Graph, java.util.List<java.util.List<hydra.module.TermDefinition>>>>>) (graphh1 -> hydra.lib.flows.Bind.apply(
          hydra.lib.logic.IfElse.lazy(
            doHoistCaseStatements,
            () -> ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Graph>>) (() -> {
              hydra.core.Term gterm2 = hydra.schemas.Schemas.graphAsTerm(graphh1);
              return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Graph>>) (() -> {
                hydra.core.Term gterm3 = hydra.rewriting.Rewriting.unshadowVariables(gterm2);
                return ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Graph>>) (() -> {
                  java.util.List<hydra.core.Binding> newElements2 = hydra.schemas.Schemas.termAsGraph(gterm3);
                  return hydra.lib.flows.Pure.apply(new hydra.graph.Graph(newElements2, (graphh1).environment, (graphh1).types, (graphh1).body, (graphh1).primitives, (graphh1).schema));
                })).get();
              })).get();
            })).get(),
            () -> hydra.lib.flows.Pure.apply(graphh1)),
          (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<hydra.graph.Graph, java.util.List<java.util.List<hydra.module.TermDefinition>>>>>) (graphu1 -> {
            hydra.util.Lazy<Boolean> allHaveTypes = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
              (java.util.function.Function<Boolean, java.util.function.Function<Boolean, Boolean>>) (p0 -> p1 -> hydra.lib.logic.And.apply(
                p0,
                p1)),
              true,
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.maybes.IsJust.apply((b).type)),
                (graphu1).elements)));
            return hydra.lib.flows.Bind.apply(
              hydra.lib.logic.IfElse.lazy(
                allHaveTypes.get(),
                () -> hydra.lib.flows.Pure.apply(graphu1),
                () -> hydra.inference.Inference.inferGraphTypes(graphu1)),
              (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<hydra.graph.Graph, java.util.List<java.util.List<hydra.module.TermDefinition>>>>>) (graphi1 -> {
                hydra.util.Lazy<hydra.graph.Graph> graphh = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                  doHoistPolymorphicLetBindings,
                  () -> (hoistPoly).apply(graphi1),
                  () -> graphi1));
                return hydra.lib.flows.Bind.apply(
                  hydra.inference.Inference.inferGraphTypes(graphh.get()),
                  (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<hydra.graph.Graph, java.util.List<java.util.List<hydra.module.TermDefinition>>>>>) (graphi2 -> hydra.lib.flows.Bind.apply(
                    hydra.adapt.simple.Simple.adaptDataGraph(
                      constraints,
                      doExpand,
                      graphi2),
                    (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<hydra.graph.Graph, java.util.List<java.util.List<hydra.module.TermDefinition>>>>>) (graph1raw -> {
                      java.util.function.Function<hydra.graph.Graph, hydra.graph.Graph> normalizeGraph = (java.util.function.Function<hydra.graph.Graph, hydra.graph.Graph>) (g -> new hydra.graph.Graph(hydra.lib.lists.Map.apply(
                        (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, hydra.adapt.simple.Simple.pushTypeAppsInward((b).term), (b).type)),
                        (g).elements), (g).environment, (g).types, (g).body, (g).primitives, (g).schema));
                      hydra.graph.Graph graph1 = (normalizeGraph).apply(graph1raw);
                      hydra.util.Lazy<Boolean> allHaveTypesAfterAdapt = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                        (java.util.function.Function<Boolean, java.util.function.Function<Boolean, Boolean>>) (p0 -> p1 -> hydra.lib.logic.And.apply(
                          p0,
                          p1)),
                        true,
                        hydra.lib.lists.Map.apply(
                          (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.maybes.IsJust.apply((b).type)),
                          (graph1).elements)));
                      return hydra.lib.flows.Bind.apply(
                        hydra.lib.logic.IfElse.lazy(
                          allHaveTypesAfterAdapt.get(),
                          () -> hydra.lib.flows.Pure.apply(graph1),
                          () -> hydra.inference.Inference.inferGraphTypes(graph1)),
                        (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<hydra.graph.Graph, java.util.List<java.util.List<hydra.module.TermDefinition>>>>>) (graph2raw -> {
                          hydra.graph.Graph graph2 = (normalizeGraph).apply(graph2raw);
                          hydra.util.Lazy<java.util.List<hydra.core.Binding>> selectedElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
                            (java.util.function.Function<hydra.core.Binding, Boolean>) (el -> hydra.lib.maybes.Maybe.apply(
                              false,
                              (java.util.function.Function<hydra.module.Namespace, Boolean>) (ns -> hydra.lib.sets.Member.apply(
                                ns,
                                namespacesSet.get())),
                              hydra.names.Names.namespaceOf((el).name))),
                            (graph2).elements));
                          hydra.util.Lazy<java.util.Map<hydra.module.Namespace, java.util.List<hydra.core.Binding>>> elementsByNamespace = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                            (java.util.function.Function<java.util.Map<hydra.module.Namespace, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Binding, java.util.Map<hydra.module.Namespace, java.util.List<hydra.core.Binding>>>>) (acc -> (java.util.function.Function<hydra.core.Binding, java.util.Map<hydra.module.Namespace, java.util.List<hydra.core.Binding>>>) (el -> hydra.lib.maybes.Maybe.apply(
                              acc,
                              (java.util.function.Function<hydra.module.Namespace, java.util.Map<hydra.module.Namespace, java.util.List<hydra.core.Binding>>>) (ns -> {
                                hydra.util.Lazy<java.util.List<hydra.core.Binding>> existing = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.apply(
                                  (java.util.List<hydra.core.Binding>) (java.util.List.<hydra.core.Binding>of()),
                                  (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>>) (hydra.lib.equality.Identity::apply),
                                  hydra.lib.maps.Lookup.apply(
                                    ns,
                                    acc)));
                                return hydra.lib.maps.Insert.apply(
                                  ns,
                                  hydra.lib.lists.Concat2.apply(
                                    existing.get(),
                                    java.util.List.of(el)),
                                  acc);
                              }),
                              hydra.names.Names.namespaceOf((el).name)))),
                            (java.util.Map<hydra.module.Namespace, java.util.List<hydra.core.Binding>>) ((java.util.Map<hydra.module.Namespace, java.util.List<hydra.core.Binding>>) (hydra.lib.maps.Empty.<hydra.module.Namespace, java.util.List<hydra.core.Binding>>apply())),
                            selectedElements.get()));
                          java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.module.TermDefinition>> toDef = (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.module.TermDefinition>>) (el -> hydra.lib.maybes.Map.apply(
                            (java.util.function.Function<hydra.core.TypeScheme, hydra.module.TermDefinition>) (ts -> new hydra.module.TermDefinition((el).name, (el).term, ts)),
                            (el).type));
                          hydra.util.Lazy<java.util.List<java.util.List<hydra.module.TermDefinition>>> defsGrouped = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                            (java.util.function.Function<hydra.module.Namespace, java.util.List<hydra.module.TermDefinition>>) (ns -> {
                              hydra.util.Lazy<java.util.List<hydra.core.Binding>> elsForNs = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.apply(
                                (java.util.List<hydra.core.Binding>) (java.util.List.<hydra.core.Binding>of()),
                                (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>>) (hydra.lib.equality.Identity::apply),
                                hydra.lib.maps.Lookup.apply(
                                  ns,
                                  elementsByNamespace.get())));
                              return hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
                                toDef,
                                elsForNs.get()));
                            }),
                            namespaces));
                          return hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.graph.Graph, java.util.List<java.util.List<hydra.module.TermDefinition>>>) ((hydra.util.Tuple.Tuple2<hydra.graph.Graph, java.util.List<java.util.List<hydra.module.TermDefinition>>>) (new hydra.util.Tuple.Tuple2<hydra.graph.Graph, java.util.List<java.util.List<hydra.module.TermDefinition>>>(graph2, defsGrouped.get()))));
                        }));
                    }))));
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
        hydra.reflect.Reflect.literalTypeVariant(lt),
        (constraints).literalVariants),
      () -> (forType).apply(lt),
      () -> false);
  }
  
  static hydra.core.Term pushTypeAppsInward(hydra.core.Term term) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, hydra.core.Term>> go = new java.util.concurrent.atomic.AtomicReference<>();
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Type, hydra.core.Term>>> push = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> {
      java.util.function.Function<hydra.core.Field, hydra.core.Field> forField = (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (fld -> new hydra.core.Field((fld).name, (go.get()).apply((fld).term)));
      java.util.function.Function<hydra.core.Elimination, hydra.core.Elimination> forElimination = (java.util.function.Function<hydra.core.Elimination, hydra.core.Elimination>) (elm -> (elm).accept(new hydra.core.Elimination.PartialVisitor<>() {
        @Override
        public hydra.core.Elimination visit(hydra.core.Elimination.Record p) {
          return new hydra.core.Elimination.Record((p).value);
        }
        
        @Override
        public hydra.core.Elimination visit(hydra.core.Elimination.Union cs) {
          return new hydra.core.Elimination.Union(new hydra.core.CaseStatement(((cs).value).typeName, hydra.lib.maybes.Map.apply(
            go.get(),
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
          return new hydra.core.Function.Lambda(new hydra.core.Lambda(((l).value).parameter, ((l).value).domain, (go.get()).apply(((l).value).body)));
        }
        
        @Override
        public hydra.core.Function visit(hydra.core.Function.Primitive name) {
          return new hydra.core.Function.Primitive((name).value);
        }
      }));
      java.util.function.Function<hydra.core.Let, hydra.core.Let> forLet = (java.util.function.Function<hydra.core.Let, hydra.core.Let>) (lt -> {
        java.util.function.Function<hydra.core.Binding, hydra.core.Binding> mapBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, (go.get()).apply((b).term), (b).type));
        return new hydra.core.Let(hydra.lib.lists.Map.apply(
          mapBinding,
          (lt).bindings), (go.get()).apply((lt).body));
      });
      java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, java.util.Map<hydra.core.Term, hydra.core.Term>> forMap = (java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, java.util.Map<hydra.core.Term, hydra.core.Term>>) (m -> {
        java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>> forPair = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>) (p -> (hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>((go.get()).apply(hydra.lib.pairs.First.apply(p)), (go.get()).apply(hydra.lib.pairs.Second.apply(p))))));
        return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          forPair,
          hydra.lib.maps.ToList.apply(m)));
      });
      return (t).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term visit(hydra.core.Term.Annotated at) {
          return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm((go.get()).apply(((at).value).body), ((at).value).annotation));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Application a) {
          return new hydra.core.Term.Application(new hydra.core.Application((go.get()).apply(((a).value).function), (go.get()).apply(((a).value).argument)));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Either e) {
          return new hydra.core.Term.Either(hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (l -> (hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left((go.get()).apply(l))))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (r -> (hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right((go.get()).apply(r))))),
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
          return new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>((go.get()).apply(hydra.lib.pairs.First.apply((p).value)), (go.get()).apply(hydra.lib.pairs.Second.apply((p).value))))));
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
            go.get(),
            hydra.lib.sets.ToList.apply((s).value))));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
          hydra.core.Term body1 = (go.get()).apply(((tt).value).body);
          return ((push.get()).apply(body1)).apply(((tt).value).type);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
          return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(((ta).value).parameter, (go.get()).apply(((ta).value).body)));
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
          return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(((wt).value).typeName, (go.get()).apply(((wt).value).body)));
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
        return (go.get()).apply(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(((a).value).function, typ)), ((a).value).argument)));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(new hydra.core.Term.Function((f).value), typ));
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda l) {
            return (go.get()).apply(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(((l).value).parameter, ((l).value).domain, new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(((l).value).body, typ))))));
          }
        });
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        return (go.get()).apply(new hydra.core.Term.Let(new hydra.core.Let(((lt).value).bindings, new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(((lt).value).body, typ)))));
      }
    }))));
    return (go.get()).apply(term);
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.List<java.util.List<hydra.module.TypeDefinition>>>> schemaGraphToDefinitions(hydra.coders.LanguageConstraints constraints, hydra.graph.Graph graph, java.util.List<java.util.List<hydra.core.Name>> nameLists) {
    java.util.Map<hydra.core.LiteralType, hydra.core.LiteralType> litmap = hydra.adapt.simple.Simple.adaptLiteralTypesMap(constraints);
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.graphAsTypes(graph),
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.List<java.util.List<hydra.module.TypeDefinition>>>>>) (tmap0 -> hydra.lib.flows.Bind.apply(
        hydra.adapt.simple.Simple.adaptGraphSchema(
          constraints,
          litmap,
          tmap0),
        (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.List<java.util.List<hydra.module.TypeDefinition>>>>>) (tmap1 -> {
          java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>, hydra.module.TypeDefinition> toDef = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>, hydra.module.TypeDefinition>) (pair -> new hydra.module.TypeDefinition(hydra.lib.pairs.First.apply(pair), hydra.lib.pairs.Second.apply(pair)));
          return hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.List<java.util.List<hydra.module.TypeDefinition>>>) ((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.List<java.util.List<hydra.module.TypeDefinition>>>) (new hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.List<java.util.List<hydra.module.TypeDefinition>>>(tmap1, hydra.lib.lists.Map.apply(
            (java.util.function.Function<java.util.List<hydra.core.Name>, java.util.List<hydra.module.TypeDefinition>>) (names -> hydra.lib.lists.Map.apply(
              toDef,
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>) (n -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>(n, hydra.lib.maybes.FromJust.apply(hydra.lib.maps.Lookup.apply(
                  n,
                  tmap1)))))),
                names))),
            nameLists)))));
        }))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>> termAlternatives(hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>> otherwise(hydra.core.Term instance) {
        return hydra.lib.flows.Pure.apply((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>> visit(hydra.core.Term.Annotated at) {
        hydra.core.Term term2 = ((at).value).body;
        return hydra.lib.flows.Pure.apply(java.util.List.of(term2));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>> visit(hydra.core.Term.Maybe ot) {
        return hydra.lib.flows.Pure.apply(java.util.List.of(new hydra.core.Term.List(hydra.lib.maybes.Maybe.apply(
          (java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()),
          (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (term2 -> java.util.List.of(term2)),
          (ot).value))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>> visit(hydra.core.Term.TypeLambda abs) {
        hydra.core.Term term2 = ((abs).value).body;
        return hydra.lib.flows.Pure.apply(java.util.List.of(term2));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>> visit(hydra.core.Term.TypeApplication ta) {
        hydra.core.Term term2 = ((ta).value).body;
        return hydra.lib.flows.Pure.apply(java.util.List.of(term2));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>> visit(hydra.core.Term.Union inj) {
        hydra.core.Field field = ((inj).value).field;
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
        hydra.core.Name tname = ((inj).value).typeName;
        return hydra.lib.flows.Bind.apply(
          hydra.schemas.Schemas.requireUnionType(tname),
          (java.util.function.Function<hydra.core.RowType, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>>>) (rt -> hydra.lib.flows.Pure.apply(java.util.List.of(new hydra.core.Term.Record(new hydra.core.Record(tname, hydra.lib.lists.Map.apply(
            forFieldType,
            (rt).fields)))))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>> visit(hydra.core.Term.Unit ignored) {
        return hydra.lib.flows.Pure.apply(java.util.List.of(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Term>> visit(hydra.core.Term.Wrap wt) {
        hydra.core.Term term2 = ((wt).value).body;
        return hydra.lib.flows.Pure.apply(java.util.List.of(term2));
      }
    });
  }
  
  static java.util.List<hydra.core.Type> typeAlternatives(hydra.core.Type type) {
    return (type).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of());
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Annotated at) {
        hydra.core.Type type2 = ((at).value).body;
        return java.util.List.of(type2);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Maybe ot) {
        return java.util.List.of(new hydra.core.Type.List((ot).value));
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Union rt) {
        java.util.List<hydra.core.FieldType> fields = ((rt).value).fields;
        java.util.function.Function<hydra.core.FieldType, hydra.core.FieldType> toOptField = (java.util.function.Function<hydra.core.FieldType, hydra.core.FieldType>) (f -> new hydra.core.FieldType((f).name, new hydra.core.Type.Maybe((f).type)));
        hydra.util.Lazy<java.util.List<hydra.core.FieldType>> optFields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          toOptField,
          fields));
        hydra.core.Name tname = ((rt).value).typeName;
        return java.util.List.of(new hydra.core.Type.Record(new hydra.core.RowType(tname, optFields.get())));
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Unit ignored) {
        return java.util.List.of(new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()));
      }
    });
  }
}
