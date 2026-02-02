// Note: this is an automatically generated file. Do not edit.

package hydra.schemas;

/**
 * Various functions for dereferencing and decoding schema types.
 */
public interface Schemas {
  static <T0> hydra.module.Namespaces<T0> addNamesToNamespaces(java.util.function.Function<hydra.module.Namespace, T0> encodeNamespace, java.util.Set<hydra.core.Name> names, hydra.module.Namespaces<T0> ns0) {
    java.util.Set<hydra.module.Namespace> nss = hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (hydra.names.Names::namespaceOf),
      hydra.lib.sets.ToList.apply((names)))));
    return (hydra.module.Namespaces<T0>) (new hydra.module.Namespaces<T0>(((java.util.function.Function<hydra.module.Namespaces<T0>, hydra.util.Tuple.Tuple2<hydra.module.Namespace, T0>>) (projected -> projected.focus)).apply((ns0)), hydra.lib.maps.Union.apply(
      ((java.util.function.Function<hydra.module.Namespaces<T0>, java.util.Map<hydra.module.Namespace, T0>>) (projected -> projected.mapping)).apply((ns0)),
      hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Namespace, hydra.util.Tuple.Tuple2<hydra.module.Namespace, T0>>) (v1 -> hydra.schemas.Schemas.addNamesToNamespaces_toPair(
          (encodeNamespace),
          (v1))),
        hydra.lib.sets.ToList.apply((nss)))))));
  }
  
  static <T0, T1> hydra.util.Tuple.Tuple2<T0, T1> addNamesToNamespaces_toPair(java.util.function.Function<T0, T1> encodeNamespace, T0 ns) {
    return (hydra.util.Tuple.Tuple2<T0, T1>) ((hydra.util.Tuple.Tuple2<T0, T1>) (new hydra.util.Tuple.Tuple2<T0, T1>((ns), ((encodeNamespace)).apply((ns)))));
  }
  
  static java.util.Set<hydra.module.Namespace> definitionDependencyNamespaces(java.util.List<hydra.module.Definition> defs) {
    java.util.function.Function<hydra.module.Definition, java.util.Set<hydra.core.Name>> defNames = (java.util.function.Function<hydra.module.Definition, java.util.Set<hydra.core.Name>>) (def -> ((def)).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.module.Definition.Type typeDef) {
        return hydra.rewriting.Rewriting.typeDependencyNames(
          true,
          (((typeDef)).value).type);
      }
      
      @Override
      public java.util.Set<hydra.core.Name> visit(hydra.module.Definition.Term termDef) {
        return hydra.rewriting.Rewriting.termDependencyNames(
          true,
          true,
          true,
          (((termDef)).value).term);
      }
    }));
    java.util.Set<hydra.core.Name> allNames = hydra.lib.sets.Unions.apply(hydra.lib.lists.Map.apply(
      (defNames),
      (defs)));
    return hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (hydra.names.Names::namespaceOf),
      hydra.lib.sets.ToList.apply((allNames)))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.Set<hydra.module.Namespace>> dependencyNamespaces(Boolean binds, Boolean withPrims, Boolean withNoms, Boolean withSchema, java.util.List<hydra.core.Binding> els) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, java.util.Set<hydra.module.Namespace>>>) (cx -> hydra.lib.flows.Bind.apply(
        hydra.lib.flows.MapList.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, java.util.Set<hydra.core.Name>>>) (v1 -> hydra.schemas.Schemas.dependencyNamespaces_depNames(
            (binds),
            (cx),
            (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
              (p0),
              (p1))),
            (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
              (p0),
              (p1))),
            (hydra.rewriting.Rewriting::deannotateTerm),
            (java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>>>>>) (p0 -> p1 -> p2 -> p3 -> hydra.rewriting.Rewriting.termDependencyNames(
              (p0),
              (p1),
              (p2),
              (p3))),
            (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>>) (p0 -> p1 -> hydra.rewriting.Rewriting.typeDependencyNames(
              (p0),
              (p1))),
            (hydra.schemas.Schemas::isEncodedTerm),
            (hydra.schemas.Schemas::isEncodedType),
            (withNoms),
            (withPrims),
            (withSchema),
            (v1))),
          (els)),
        (java.util.function.Function<java.util.List<java.util.Set<hydra.core.Name>>, hydra.compute.Flow<hydra.graph.Graph, java.util.Set<hydra.module.Namespace>>>) (namesList -> hydra.lib.flows.Pure.apply(hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
          (hydra.names.Names::namespaceOf),
          hydra.lib.sets.ToList.apply(hydra.lib.sets.Delete.apply(
            (hydra.constants.Constants.placeholderName),
            hydra.lib.sets.Unions.apply((namesList))))))))))));
  }
  
  static <T0, T1, T2, T3, T4, T5, T6> hydra.compute.Flow<T6, java.util.Set<T5>> dependencyNamespaces_depNames(T0 binds, T1 cx, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>> hydra_decode_core_term2, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>> hydra_decode_core_type2, java.util.function.Function<hydra.core.Term, T2> hydra_rewriting_deannotateTerm2, java.util.function.Function<T0, java.util.function.Function<T3, java.util.function.Function<T4, java.util.function.Function<hydra.core.Term, java.util.Set<T5>>>>> hydra_rewriting_termDependencyNames2, java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Type, java.util.Set<T5>>> hydra_rewriting_typeDependencyNames2, java.util.function.Function<T2, Boolean> hydra_schemas_isEncodedTerm2, java.util.function.Function<T2, Boolean> hydra_schemas_isEncodedType2, T4 withNoms, T3 withPrims, Boolean withSchema, hydra.core.Binding el) {
    hydra.core.Term term = ((el)).term;
    java.util.Set<T5> dataNames = (((((hydra_rewriting_termDependencyNames2)).apply((binds))).apply((withPrims))).apply((withNoms))).apply((term));
    T2 deannotatedTerm = ((hydra_rewriting_deannotateTerm2)).apply((term));
    java.util.Set<T5> schemaNames = hydra.lib.logic.IfElse.apply(
      (withSchema),
      hydra.lib.maybes.Maybe.apply(
        (java.util.Set<T5>) (hydra.lib.sets.Empty.<T5>apply()),
        (java.util.function.Function<hydra.core.TypeScheme, java.util.Set<T5>>) (ts -> (((hydra_rewriting_typeDependencyNames2)).apply(true)).apply(((ts)).type)),
        ((el)).type),
      (java.util.Set<T5>) (hydra.lib.sets.Empty.<T5>apply()));
    return hydra.lib.logic.IfElse.apply(
      ((hydra_schemas_isEncodedType2)).apply((deannotatedTerm)),
      hydra.lib.flows.Bind.apply(
        hydra.monads.Monads.withTrace(
          "dependency namespace (type)",
          hydra.monads.Monads.eitherToFlow(
            wrapped -> ((wrapped)).value,
            (((hydra_decode_core_type2)).apply((cx))).apply((term)))),
        (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T6, java.util.Set<T5>>>) (typ -> hydra.lib.flows.Pure.apply(hydra.lib.sets.Unions.apply(java.util.List.of(
          (dataNames),
          (schemaNames),
          (((hydra_rewriting_typeDependencyNames2)).apply(true)).apply((typ))))))),
      hydra.lib.logic.IfElse.apply(
        ((hydra_schemas_isEncodedTerm2)).apply((deannotatedTerm)),
        hydra.lib.flows.Bind.apply(
          hydra.monads.Monads.withTrace(
            "dependency namespace (term)",
            hydra.monads.Monads.eitherToFlow(
              wrapped -> ((wrapped)).value,
              (((hydra_decode_core_term2)).apply((cx))).apply((term)))),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T6, java.util.Set<T5>>>) (decodedTerm -> hydra.lib.flows.Pure.apply(hydra.lib.sets.Unions.apply(java.util.List.of(
            (dataNames),
            (schemaNames),
            (((((hydra_rewriting_termDependencyNames2)).apply((binds))).apply((withPrims))).apply((withNoms))).apply((decodedTerm))))))),
        hydra.lib.flows.Pure.apply(hydra.lib.sets.Unions.apply(java.util.List.of(
          (dataNames),
          (schemaNames))))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>> dereferenceType(hydra.core.Name name) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>>>) (cx -> hydra.lib.flows.Bind.apply(
        hydra.lexical.Lexical.dereferenceElement((name)),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>>>) (mel -> hydra.lib.maybes.Maybe.apply(
          hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
          (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>>>) (el -> hydra.lib.flows.Map.apply(
            (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) ((hydra.lib.maybes.Pure::apply)),
            hydra.monads.Monads.withTrace(
              "dereference type",
              hydra.monads.Monads.eitherToFlow(
                wrapped -> ((wrapped)).value,
                hydra.decode.core.Core.type(
                  (cx),
                  ((el)).term))))),
          (mel))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.TypeApplicationTerm> elementAsTypeApplicationTerm(hydra.core.Binding el) {
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.flows.Fail.apply("missing element type"),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.core.TypeApplicationTerm>>) (ts -> hydra.lib.flows.Pure.apply(new hydra.core.TypeApplicationTerm(((el)).term, ((ts)).type))),
      ((el)).type);
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Binding>> elementsWithDependencies(java.util.List<hydra.core.Binding> original) {
    java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Name>> depNames = (java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Name>>) (el -> hydra.lib.sets.ToList.apply(hydra.rewriting.Rewriting.termDependencyNames(
      true,
      false,
      false,
      ((el)).term)));
    java.util.List<hydra.core.Name> allDepNames = hydra.lib.lists.Nub.apply(hydra.lib.lists.Concat2.apply(
      hydra.lib.lists.Map.apply(
        projected -> projected.name,
        (original)),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (depNames),
        (original)))));
    return hydra.lib.flows.MapList.apply(
      (hydra.lexical.Lexical::requireElement),
      (allDepNames));
  }
  
  static hydra.typing.TypeContext extendTypeContextForLambda(hydra.typing.TypeContext tcontext, hydra.core.Lambda lam) {
    hydra.core.Name var = ((lam)).parameter;
    return new hydra.typing.TypeContext(hydra.lib.maybes.Maybe.apply(
      ((tcontext)).types,
      (java.util.function.Function<hydra.core.Type, java.util.Map<hydra.core.Name, hydra.core.Type>>) (dom -> hydra.lib.maps.Insert.apply(
        (var),
        (dom),
        ((tcontext)).types)),
      ((lam)).domain), hydra.lib.maps.Delete.apply(
      (var),
      ((tcontext)).metadata), ((tcontext)).typeVariables, hydra.lib.sets.Insert.apply(
      (var),
      ((tcontext)).lambdaVariables), hydra.lib.sets.Delete.apply(
      (var),
      ((tcontext)).letVariables), ((tcontext)).inferenceContext);
  }
  
  static hydra.typing.TypeContext extendTypeContextForLet(java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>> forBinding, hydra.typing.TypeContext tcontext, hydra.core.Let letrec) {
    java.util.List<hydra.core.Binding> bindings = ((letrec)).bindings;
    return new hydra.typing.TypeContext(hydra.lib.maps.Union.apply(
      hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>>) (b -> hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>) (ts -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>(((b)).name, hydra.schemas.Schemas.typeSchemeToFType((ts)))))),
          ((b)).type)),
        (bindings)))),
      ((tcontext)).types), hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, java.util.function.Function<hydra.core.Binding, java.util.Map<hydra.core.Name, hydra.core.Term>>>) (m -> (java.util.function.Function<hydra.core.Binding, java.util.Map<hydra.core.Name, hydra.core.Term>>) (b -> hydra.lib.maybes.Maybe.apply(
        hydra.lib.maps.Delete.apply(
          ((b)).name,
          (m)),
        (java.util.function.Function<hydra.core.Term, java.util.Map<hydra.core.Name, hydra.core.Term>>) (t -> hydra.lib.maps.Insert.apply(
          ((b)).name,
          (t),
          (m))),
        (((forBinding)).apply((tcontext))).apply((b))))),
      ((tcontext)).metadata,
      (bindings)), ((tcontext)).typeVariables, hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Binding, java.util.Set<hydra.core.Name>>>) (s -> (java.util.function.Function<hydra.core.Binding, java.util.Set<hydra.core.Name>>) (b -> hydra.lib.sets.Delete.apply(
        ((b)).name,
        (s)))),
      ((tcontext)).lambdaVariables,
      (bindings)), hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Binding, java.util.Set<hydra.core.Name>>>) (s -> (java.util.function.Function<hydra.core.Binding, java.util.Set<hydra.core.Name>>) (b -> hydra.lib.sets.Insert.apply(
        ((b)).name,
        (s)))),
      ((tcontext)).letVariables,
      (bindings)), ((tcontext)).inferenceContext);
  }
  
  static hydra.typing.TypeContext extendTypeContextForTypeLambda(hydra.typing.TypeContext tcontext, hydra.core.TypeLambda tlam) {
    hydra.core.Name name = ((tlam)).parameter;
    return new hydra.typing.TypeContext(((tcontext)).types, ((tcontext)).metadata, hydra.lib.sets.Insert.apply(
      (name),
      ((tcontext)).typeVariables), ((tcontext)).lambdaVariables, ((tcontext)).letVariables, ((tcontext)).inferenceContext);
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap(java.util.List<hydra.core.Field> fields) {
    java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>> toPair = (java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>) (f -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>(((f)).name, ((f)).term))));
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (toPair),
      (fields)));
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.Type> fieldTypeMap(java.util.List<hydra.core.FieldType> fields) {
    java.util.function.Function<hydra.core.FieldType, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>> toPair = (java.util.function.Function<hydra.core.FieldType, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>) (f -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>(((f)).name, ((f)).type))));
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (toPair),
      (fields)));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>> fieldTypes(hydra.core.Type t) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>>>) (cx -> {
        java.util.function.Function<java.util.List<hydra.core.FieldType>, java.util.Map<hydra.core.Name, hydra.core.Type>> toMap = (java.util.function.Function<java.util.List<hydra.core.FieldType>, java.util.Map<hydra.core.Name, hydra.core.Type>>) (fields -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.FieldType, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>) (ft -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>(((ft)).name, ((ft)).type)))),
          (fields))));
        return (hydra.rewriting.Rewriting.deannotateType((t))).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>> otherwise(hydra.core.Type instance) {
            return hydra.monads.Monads.unexpected(
              "record or union type",
              hydra.show.core.Core.type((t)));
          }
          
          @Override
          public hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>> visit(hydra.core.Type.Forall ft) {
            return hydra.schemas.Schemas.fieldTypes((((ft)).value).body);
          }
          
          @Override
          public hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>> visit(hydra.core.Type.Record rt) {
            return hydra.lib.flows.Pure.apply(((toMap)).apply((((rt)).value).fields));
          }
          
          @Override
          public hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>> visit(hydra.core.Type.Union rt) {
            return hydra.lib.flows.Pure.apply(((toMap)).apply((((rt)).value).fields));
          }
          
          @Override
          public hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>> visit(hydra.core.Type.Variable name) {
            return hydra.monads.Monads.withTrace(
              hydra.lib.strings.Cat2.apply(
                "field types of ",
                (((name)).value).value),
              hydra.lib.flows.Bind.apply(
                hydra.lexical.Lexical.requireElement(((name)).value),
                (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>>>) (el -> hydra.lib.flows.Bind.apply(
                  hydra.monads.Monads.withTrace(
                    "field types",
                    hydra.monads.Monads.eitherToFlow(
                      wrapped -> ((wrapped)).value,
                      hydra.decode.core.Core.type(
                        (cx),
                        ((el)).term))),
                  (hydra.schemas.Schemas::fieldTypes)))));
          }
        });
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> findFieldType(hydra.core.Name fname, java.util.List<hydra.core.FieldType> fields) {
    java.util.List<hydra.core.FieldType> matchingFields = hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.lib.equality.Equal.apply(
        (((ft)).name).value,
        ((fname)).value)),
      (fields));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((matchingFields)),
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
        "No such field: ",
        ((fname)).value)),
      hydra.lib.logic.IfElse.apply(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply((matchingFields)),
          1),
        hydra.lib.flows.Pure.apply((hydra.lib.lists.Head.apply((matchingFields))).type),
        hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
          "Multiple fields named ",
          ((fname)).value))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Name> freshName() {
    return hydra.lib.flows.Map.apply(
      (hydra.schemas.Schemas::normalTypeVariable),
      hydra.annotations.Annotations.<T0>nextCount((hydra.constants.Constants.key_freshTypeVariableCount)));
  }
  
  static <T0> hydra.compute.Flow<T0, java.util.List<hydra.core.Name>> freshNames(Integer n) {
    return hydra.lib.flows.Sequence.apply(hydra.lib.lists.Replicate.apply(
      (n),
      hydra.schemas.Schemas.<T0>freshName()));
  }
  
  static Boolean fTypeIsPolymorphic(hydra.core.Type typ) {
    return ((typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Annotated at) {
        return hydra.schemas.Schemas.fTypeIsPolymorphic((((at)).value).body);
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Forall ft) {
        return true;
      }
    });
  }
  
  static hydra.core.TypeScheme fTypeToTypeScheme(hydra.core.Type typ) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>>> gatherForall = new java.util.concurrent.atomic.AtomicReference<>();
    gatherForall.set((java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>>) (vars -> (java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>) (typ2 -> (hydra.rewriting.Rewriting.deannotateType((typ2))).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.TypeScheme otherwise(hydra.core.Type instance) {
        return new hydra.core.TypeScheme(hydra.lib.lists.Reverse.apply((vars)), (typ2), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()));
      }
      
      @Override
      public hydra.core.TypeScheme visit(hydra.core.Type.Forall ft) {
        return ((gatherForall.get()).apply(hydra.lib.lists.Cons.apply(
          (((ft)).value).parameter,
          (vars)))).apply((((ft)).value).body);
      }
    }))));
    return ((gatherForall.get()).apply((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()))).apply((typ));
  }
  
  static hydra.core.Type fullyStripAndNormalizeType(hydra.core.Type typ) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Integer, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type>>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<Integer, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type>>>>) (depth -> (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type>>>) (subst2 -> (java.util.function.Function<hydra.core.Type, hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type>>) (t -> (hydra.rewriting.Rewriting.deannotateType((t))).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type>((subst2), (t))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type> visit(hydra.core.Type.Forall ft) {
        hydra.core.Name newVar = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
          "_",
          hydra.lib.literals.ShowInt32.apply((depth))));
        hydra.core.Name oldVar = (((ft)).value).parameter;
        return (((go.get()).apply(hydra.lib.math.Add.apply(
          (depth),
          1))).apply(hydra.lib.maps.Insert.apply(
          (oldVar),
          (newVar),
          (subst2)))).apply((((ft)).value).body);
      }
    })))));
    hydra.util.Tuple.Tuple2<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type> result = (((go.get()).apply(0)).apply((java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())))).apply((typ));
    hydra.core.Type body = hydra.lib.pairs.Second.apply((result));
    java.util.Map<hydra.core.Name, hydra.core.Name> subst = hydra.lib.pairs.First.apply((result));
    return hydra.rewriting.Rewriting.substituteTypeVariables(
      (subst),
      (body));
  }
  
  static hydra.core.Type fullyStripType(hydra.core.Type typ) {
    return (hydra.rewriting.Rewriting.deannotateType((typ))).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return (typ);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return hydra.schemas.Schemas.fullyStripType((((ft)).value).body);
      }
    });
  }
  
  static hydra.core.Let graphAsLet(hydra.graph.Graph g) {
    return new hydra.core.Let(((g)).elements, ((g)).body);
  }
  
  static hydra.core.Term graphAsTerm(hydra.graph.Graph g) {
    return new hydra.core.Term.Let(hydra.schemas.Schemas.graphAsLet((g)));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>> graphAsTypes(hydra.graph.Graph sg) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>>>) (cx -> {
        java.util.List<hydra.core.Binding> els = ((sg)).elements;
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>>) (v1 -> hydra.schemas.Schemas.graphAsTypes_toPair(
              (cx),
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                (p0),
                (p1))),
              (v1))),
            (els)),
          (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>>>) (pairs -> hydra.lib.flows.Pure.apply(hydra.lib.maps.FromList.apply((pairs)))));
      }));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<hydra.core.Name, T1>> graphAsTypes_toPair(T0 cx, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T1>>> hydra_decode_core_type2, hydra.core.Binding el) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<T2, T1>withTrace(
        hydra.lib.strings.Cat2.apply(
          "graph as types: ",
          (((el)).name).value),
        hydra.monads.Monads.eitherToFlow(
          wrapped -> ((wrapped)).value,
          (((hydra_decode_core_type2)).apply((cx))).apply(((el)).term))),
      (java.util.function.Function<T1, hydra.compute.Flow<T2, hydra.util.Tuple.Tuple2<hydra.core.Name, T1>>>) (typ -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.core.Name, T1>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, T1>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, T1>(((el)).name, (typ)))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceContext> graphToInferenceContext(hydra.graph.Graph graph) {
    java.util.Map<hydra.core.Name, hydra.core.TypeScheme> primTypes = hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.graph.Primitive, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>>) (p -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>(((p)).name, ((p)).type)))),
      hydra.lib.maps.Elems.apply(((graph)).primitives)));
    hydra.graph.Graph schema = hydra.lib.maybes.FromMaybe.apply(
      (graph),
      ((graph)).schema);
    java.util.Map<hydra.core.Name, hydra.core.TypeScheme> varTypes = hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>>>) (b -> hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>>) (ts -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>(((b)).name, (ts))))),
        ((b)).type)),
      ((graph)).elements)));
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>schemaGraphToTypingEnvironment((schema)),
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>, hydra.compute.Flow<T0, hydra.typing.InferenceContext>>) (schemaTypes -> hydra.lib.flows.Pure.apply(new hydra.typing.InferenceContext((schemaTypes), (primTypes), (varTypes), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), false))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.TypeContext> graphToTypeContext(hydra.graph.Graph graph) {
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>graphToInferenceContext((graph)),
      (java.util.function.Function<hydra.typing.InferenceContext, hydra.compute.Flow<T0, hydra.typing.TypeContext>>) (ix -> {
        java.util.Map<hydra.core.Name, hydra.core.Type> elementTypes = hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>>) (b -> hydra.lib.maybes.Map.apply(
            (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>) (ts -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>(((b)).name, hydra.schemas.Schemas.typeSchemeToFType((ts)))))),
            ((b)).type)),
          ((graph)).elements)));
        return hydra.lib.flows.Pure.apply(new hydra.typing.TypeContext((elementTypes), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (ix)));
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> instantiateType(hydra.core.Type typ) {
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>instantiateTypeScheme(hydra.schemas.Schemas.typeToTypeScheme((typ))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.core.Type>>) (ts -> hydra.lib.flows.Pure.apply(hydra.schemas.Schemas.typeSchemeToFType((ts)))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.TypeScheme> instantiateTypeScheme(hydra.core.TypeScheme scheme) {
    java.util.List<hydra.core.Name> oldVars = ((scheme)).variables;
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>freshNames(hydra.lib.lists.Length.apply((oldVars))),
      (java.util.function.Function<java.util.List<hydra.core.Name>, hydra.compute.Flow<T0, hydra.core.TypeScheme>>) (newVars -> {
        java.util.Map<hydra.core.Name, hydra.core.Name> nameSubst = hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
          (oldVars),
          (newVars)));
        hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> renamedConstraints = hydra.lib.maybes.Map.apply(
          (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (oldConstraints -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeVariableMetadata>, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (kv -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeVariableMetadata>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeVariableMetadata>(hydra.lib.maybes.FromMaybe.apply(
              hydra.lib.pairs.First.apply((kv)),
              hydra.lib.maps.Lookup.apply(
                hydra.lib.pairs.First.apply((kv)),
                (nameSubst))), hydra.lib.pairs.Second.apply((kv)))))),
            hydra.lib.maps.ToList.apply((oldConstraints))))),
          ((scheme)).constraints);
        hydra.typing.TypeSubst subst = new hydra.typing.TypeSubst(hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
          (oldVars),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable((x))),
            (newVars)))));
        return hydra.lib.flows.Pure.apply(new hydra.core.TypeScheme((newVars), hydra.substitution.Substitution.substInType(
          (subst),
          ((scheme)).type), (renamedConstraints)));
      }));
  }
  
  static Boolean isEncodedTerm(hydra.core.Term t) {
    return (hydra.rewriting.Rewriting.deannotateTerm((t))).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Application a) {
        return hydra.schemas.Schemas.isEncodedTerm((((a)).value).function);
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Union i) {
        return hydra.lib.equality.Equal.apply(
          "hydra.core.Term",
          ((((i)).value).typeName).value);
      }
    });
  }
  
  static Boolean isEncodedType(hydra.core.Term t) {
    return (hydra.rewriting.Rewriting.deannotateTerm((t))).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Application a) {
        return hydra.schemas.Schemas.isEncodedType((((a)).value).function);
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Union i) {
        return hydra.lib.equality.Equal.apply(
          "hydra.core.Type",
          ((((i)).value).typeName).value);
      }
    });
  }
  
  static Boolean isEnumRowType(hydra.core.RowType rt) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<Boolean, java.util.function.Function<Boolean, Boolean>>) (p0 -> p1 -> hydra.lib.logic.And.apply(
        (p0),
        (p1))),
      true,
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.FieldType, Boolean>) (f -> hydra.schemas.Schemas.isUnitType(hydra.rewriting.Rewriting.deannotateType(((f)).type))),
        ((rt)).fields));
  }
  
  static Boolean isEnumType(hydra.core.Type typ) {
    return (hydra.rewriting.Rewriting.deannotateType((typ))).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Union rt) {
        return hydra.schemas.Schemas.isEnumRowType(((rt)).value);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, Boolean> isSerializable(hydra.core.Binding el) {
    java.util.function.Function<hydra.core.Type, java.util.List<hydra.variants.TypeVariant>> variants = (java.util.function.Function<hydra.core.Type, java.util.List<hydra.variants.TypeVariant>>) (typ -> hydra.lib.lists.Map.apply(
      (hydra.reflect.Reflect::typeVariant),
      hydra.rewriting.Rewriting.foldOverType(
        new hydra.coders.TraversalOrder.Pre(true),
        (java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>>) (m -> (java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>) (t -> hydra.lib.lists.Cons.apply(
          (t),
          (m)))),
        (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
        (typ))));
    return hydra.lib.flows.Map.apply(
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, Boolean>) (deps -> {
        java.util.Set<hydra.variants.TypeVariant> allVariants = hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (variants),
          hydra.lib.maps.Elems.apply((deps)))));
        return hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
          new hydra.variants.TypeVariant.Function(true),
          (allVariants)));
      }),
      hydra.schemas.Schemas.typeDependencies(
        false,
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) ((hydra.lib.equality.Identity::apply)),
        ((el)).name));
  }
  
  static Boolean isSerializableType(hydra.core.Type typ) {
    java.util.Set<hydra.variants.TypeVariant> allVariants = hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
      (hydra.reflect.Reflect::typeVariant),
      hydra.rewriting.Rewriting.foldOverType(
        new hydra.coders.TraversalOrder.Pre(true),
        (java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>>) (m -> (java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>) (t -> hydra.lib.lists.Cons.apply(
          (t),
          (m)))),
        (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
        (typ))));
    return hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
      new hydra.variants.TypeVariant.Function(true),
      (allVariants)));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, Boolean> isSerializableByName(hydra.core.Name name) {
    java.util.function.Function<hydra.core.Type, java.util.List<hydra.variants.TypeVariant>> variants = (java.util.function.Function<hydra.core.Type, java.util.List<hydra.variants.TypeVariant>>) (typ -> hydra.lib.lists.Map.apply(
      (hydra.reflect.Reflect::typeVariant),
      hydra.rewriting.Rewriting.foldOverType(
        new hydra.coders.TraversalOrder.Pre(true),
        (java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>>) (m -> (java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>) (t -> hydra.lib.lists.Cons.apply(
          (t),
          (m)))),
        (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
        (typ))));
    return hydra.lib.flows.Map.apply(
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, Boolean>) (deps -> {
        java.util.Set<hydra.variants.TypeVariant> allVariants = hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (variants),
          hydra.lib.maps.Elems.apply((deps)))));
        return hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
          new hydra.variants.TypeVariant.Function(true),
          (allVariants)));
      }),
      hydra.schemas.Schemas.typeDependencies(
        false,
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) ((hydra.lib.equality.Identity::apply)),
        (name)));
  }
  
  static Boolean isType(hydra.core.Type t) {
    return (hydra.rewriting.Rewriting.deannotateType((t))).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Application a) {
        return hydra.schemas.Schemas.isType((((a)).value).function);
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Forall l) {
        return hydra.schemas.Schemas.isType((((l)).value).body);
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Union rt) {
        return hydra.lib.equality.Equal.apply(
          "hydra.core.Type",
          ((((rt)).value).typeName).value);
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Variable v) {
        return hydra.lib.equality.Equal.apply(
          ((v)).value,
          new hydra.core.Name("hydra.core.Type"));
      }
    });
  }
  
  static Boolean isUnitTerm(hydra.core.Term v1) {
    return ((v1)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Unit ignored) {
        return true;
      }
    });
  }
  
  static Boolean isUnitType(hydra.core.Type v1) {
    return ((v1)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Unit ignored) {
        return true;
      }
    });
  }
  
  static Boolean moduleContainsBinaryLiterals(hydra.module.Module mod) {
    java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>> checkTerm = (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (found -> (java.util.function.Function<hydra.core.Term, Boolean>) (term -> hydra.lib.logic.Or.apply(
      (found),
      ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public Boolean otherwise(hydra.core.Term instance) {
          return false;
        }
        
        @Override
        public Boolean visit(hydra.core.Term.Literal lit) {
          return (((lit)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
            @Override
            public Boolean otherwise(hydra.core.Literal instance) {
              return false;
            }
            
            @Override
            public Boolean visit(hydra.core.Literal.Binary ignored) {
              return true;
            }
          });
        }
      }))));
    java.util.function.Function<hydra.core.Term, Boolean> termContainsBinary = (java.util.function.Function<hydra.core.Term, Boolean>) (term -> hydra.rewriting.Rewriting.foldOverTerm(
      new hydra.coders.TraversalOrder.Pre(true),
      (checkTerm),
      false,
      (term)));
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Binding, Boolean>>) (acc -> (java.util.function.Function<hydra.core.Binding, Boolean>) (el -> hydra.lib.logic.Or.apply(
        (acc),
        ((termContainsBinary)).apply(((el)).term)))),
      false,
      ((mod)).elements);
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.Set<hydra.module.Namespace>> moduleDependencyNamespaces(Boolean binds, Boolean withPrims, Boolean withNoms, Boolean withSchema, hydra.module.Module mod) {
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.dependencyNamespaces(
        (binds),
        (withPrims),
        (withNoms),
        (withSchema),
        ((mod)).elements),
      (java.util.function.Function<java.util.Set<hydra.module.Namespace>, hydra.compute.Flow<hydra.graph.Graph, java.util.Set<hydra.module.Namespace>>>) (deps -> hydra.lib.flows.Pure.apply(hydra.lib.sets.Delete.apply(
        ((mod)).namespace,
        (deps)))));
  }
  
  static <T0> hydra.module.Namespaces<T0> namespacesForDefinitions(java.util.function.Function<hydra.module.Namespace, T0> encodeNamespace, hydra.module.Namespace focusNs, java.util.List<hydra.module.Definition> defs) {
    java.util.Set<hydra.module.Namespace> nss = hydra.lib.sets.Delete.apply(
      (focusNs),
      hydra.schemas.Schemas.definitionDependencyNamespaces((defs)));
    return (hydra.module.Namespaces<T0>) (new hydra.module.Namespaces<T0>(hydra.schemas.Schemas.namespacesForDefinitions_toPair(
      (encodeNamespace),
      (focusNs)), hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Namespace, hydra.util.Tuple.Tuple2<hydra.module.Namespace, T0>>) (v1 -> hydra.schemas.Schemas.namespacesForDefinitions_toPair(
        (encodeNamespace),
        (v1))),
      hydra.lib.sets.ToList.apply((nss))))));
  }
  
  static <T0, T1> hydra.util.Tuple.Tuple2<T0, T1> namespacesForDefinitions_toPair(java.util.function.Function<T0, T1> encodeNamespace, T0 ns) {
    return (hydra.util.Tuple.Tuple2<T0, T1>) ((hydra.util.Tuple.Tuple2<T0, T1>) (new hydra.util.Tuple.Tuple2<T0, T1>((ns), ((encodeNamespace)).apply((ns)))));
  }
  
  static hydra.core.Type nominalApplication(hydra.core.Name tname, java.util.List<hydra.core.Type> args) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (t -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (a -> new hydra.core.Type.Application(new hydra.core.ApplicationType((t), (a))))),
      new hydra.core.Type.Variable((tname)),
      (args));
  }
  
  static hydra.core.Name normalTypeVariable(Integer i) {
    return new hydra.core.Name(hydra.lib.strings.Cat2.apply(
      "t",
      hydra.lib.literals.ShowInt32.apply((i))));
  }
  
  static hydra.util.Tuple.Tuple2<java.util.List<hydra.module.TypeDefinition>, java.util.List<hydra.module.TermDefinition>> partitionDefinitions(java.util.List<hydra.module.Definition> defs) {
    java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.module.TermDefinition>> getTerm = (java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.module.TermDefinition>>) (def -> ((def)).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.module.TermDefinition> visit(hydra.module.Definition.Type ignored) {
        return (hydra.util.Maybe<hydra.module.TermDefinition>) (hydra.util.Maybe.<hydra.module.TermDefinition>nothing());
      }
      
      @Override
      public hydra.util.Maybe<hydra.module.TermDefinition> visit(hydra.module.Definition.Term td) {
        return hydra.util.Maybe.just(((td)).value);
      }
    }));
    java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.module.TypeDefinition>> getType = (java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.module.TypeDefinition>>) (def -> ((def)).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.module.TypeDefinition> visit(hydra.module.Definition.Type td) {
        return hydra.util.Maybe.just(((td)).value);
      }
      
      @Override
      public hydra.util.Maybe<hydra.module.TypeDefinition> visit(hydra.module.Definition.Term ignored) {
        return (hydra.util.Maybe<hydra.module.TypeDefinition>) (hydra.util.Maybe.<hydra.module.TypeDefinition>nothing());
      }
    }));
    return (hydra.util.Tuple.Tuple2<java.util.List<hydra.module.TypeDefinition>, java.util.List<hydra.module.TermDefinition>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.module.TypeDefinition>, java.util.List<hydra.module.TermDefinition>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.module.TypeDefinition>, java.util.List<hydra.module.TermDefinition>>(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (getType),
      (defs))), hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (getTerm),
      (defs))))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.RowType> requireRecordType(hydra.core.Name name) {
    java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.RowType>> toRecord = (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.RowType>>) (t -> ((t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.RowType> otherwise(hydra.core.Type instance) {
        return (hydra.util.Maybe<hydra.core.RowType>) (hydra.util.Maybe.<hydra.core.RowType>nothing());
      }
      
      @Override
      public hydra.util.Maybe<hydra.core.RowType> visit(hydra.core.Type.Record rt) {
        return hydra.util.Maybe.just(((rt)).value);
      }
    }));
    return hydra.schemas.Schemas.requireRowType(
      "record type",
      (toRecord),
      (name));
  }
  
  static <T0> hydra.compute.Flow<hydra.graph.Graph, T0> requireRowType(String label, java.util.function.Function<hydra.core.Type, hydra.util.Maybe<T0>> getter, hydra.core.Name name) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, hydra.core.Type>> rawType = new java.util.concurrent.atomic.AtomicReference<>();
    rawType.set((java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> ((t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return (t);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return (rawType.get()).apply((((at)).value).body);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return (rawType.get()).apply((((ft)).value).body);
      }
    })));
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.requireType((name)),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, T0>>) (t -> hydra.lib.maybes.Maybe.apply(
        hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
          ((name)).value,
          " does not resolve to a ",
          (label),
          " type: ",
          hydra.show.core.Core.type((t))))),
        (java.util.function.Function<T0, hydra.compute.Flow<hydra.graph.Graph, T0>>) ((java.util.function.Function<T0, hydra.compute.Flow<hydra.graph.Graph, T0>>) ((hydra.lib.flows.Pure::apply))),
        ((getter)).apply((rawType.get()).apply((t))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.TypeScheme> requireSchemaType(hydra.typing.InferenceContext cx, hydra.core.Name tname) {
    java.util.Map<hydra.core.Name, hydra.core.TypeScheme> types = ((cx)).schemaTypes;
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
        "No such schema type: ",
        ((tname)).value,
        ". Available types are: ",
        hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.lists.Map.apply(
            wrapped -> ((wrapped)).value,
            hydra.lib.maps.Keys.apply((types))))))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.core.TypeScheme>>) (ts -> hydra.schemas.Schemas.<T0>instantiateTypeScheme(hydra.rewriting.Rewriting.deannotateTypeSchemeRecursive((ts)))),
      hydra.lib.maps.Lookup.apply(
        (tname),
        (types)));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type> requireType(hydra.core.Name name) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (cx -> hydra.monads.Monads.withTrace(
        hydra.lib.strings.Cat2.apply(
          "require type ",
          ((name)).value),
        hydra.lib.flows.Bind.apply(
          hydra.lexical.Lexical.withSchemaContext(hydra.lexical.Lexical.requireElement((name))),
          (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (el -> hydra.monads.Monads.eitherToFlow(
            wrapped -> ((wrapped)).value,
            hydra.decode.core.Core.type(
              (cx),
              ((el)).term)))))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type> requireUnionField(hydra.core.Name tname, hydra.core.Name fname) {
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.requireUnionType((tname)),
      (java.util.function.Function<hydra.core.RowType, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (v1 -> hydra.schemas.Schemas.requireUnionField_withRowType(
        (fname),
        (tname),
        (v1))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> requireUnionField_withRowType(hydra.core.Name fname, hydra.core.Name tname, hydra.core.RowType rt) {
    java.util.List<hydra.core.FieldType> matches = hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.lib.equality.Equal.apply(
        ((ft)).name,
        (fname))),
      ((rt)).fields);
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((matches)),
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
        "no field \"",
        ((fname)).value,
        "\" in union type \"",
        ((tname)).value))),
      hydra.lib.flows.Pure.apply((hydra.lib.lists.Head.apply((matches))).type));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.RowType> requireUnionType(hydra.core.Name name) {
    java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.RowType>> toUnion = (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.RowType>>) (t -> ((t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.RowType> otherwise(hydra.core.Type instance) {
        return (hydra.util.Maybe<hydra.core.RowType>) (hydra.util.Maybe.<hydra.core.RowType>nothing());
      }
      
      @Override
      public hydra.util.Maybe<hydra.core.RowType> visit(hydra.core.Type.Union rt) {
        return hydra.util.Maybe.just(((rt)).value);
      }
    }));
    return hydra.schemas.Schemas.requireRowType(
      "union",
      (toUnion),
      (name));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>> resolveType(hydra.core.Type typ) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>>>) (cx -> (hydra.rewriting.Rewriting.deannotateType((typ))).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>> otherwise(hydra.core.Type instance) {
          return hydra.lib.flows.Pure.apply(hydra.util.Maybe.just((typ)));
        }
        
        @Override
        public hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>> visit(hydra.core.Type.Variable name) {
          return hydra.lexical.Lexical.withSchemaContext(hydra.lib.flows.Bind.apply(
            hydra.lexical.Lexical.resolveTerm(((name)).value),
            (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>>>) (mterm -> hydra.lib.maybes.Maybe.apply(
              hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
              (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>>>) (t -> hydra.lib.flows.Map.apply(
                (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) ((hydra.lib.maybes.Pure::apply)),
                hydra.monads.Monads.eitherToFlow(
                  wrapped -> ((wrapped)).value,
                  hydra.decode.core.Core.type(
                    (cx),
                    (t))))),
              (mterm)))));
        }
      })));
  }
  
  static <T0> hydra.compute.Flow<T0, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> schemaGraphToTypingEnvironment(hydra.graph.Graph g) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>>> toTypeScheme = new java.util.concurrent.atomic.AtomicReference<>();
    toTypeScheme.set((java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>>) (vars -> (java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>) (typ -> (hydra.rewriting.Rewriting.deannotateType((typ))).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.TypeScheme otherwise(hydra.core.Type instance) {
        return new hydra.core.TypeScheme(hydra.lib.lists.Reverse.apply((vars)), (typ), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()));
      }
      
      @Override
      public hydra.core.TypeScheme visit(hydra.core.Type.Forall ft) {
        return ((toTypeScheme.get()).apply(hydra.lib.lists.Cons.apply(
          (((ft)).value).parameter,
          (vars)))).apply((((ft)).value).body);
      }
    }))));
    java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>>>> toPair = (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>>>>) (el -> hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>>>>) (cx -> hydra.lib.flows.Bind.apply(
        hydra.lib.maybes.Maybe.apply(
          hydra.lib.flows.Map.apply(
            (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.TypeScheme>>) (typ -> hydra.util.Maybe.just(hydra.schemas.Schemas.fTypeToTypeScheme((typ)))),
            hydra.monads.Monads.eitherToFlow(
              wrapped -> ((wrapped)).value,
              hydra.decode.core.Core.type(
                (cx),
                ((el)).term))),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.TypeScheme>>>) (ts -> hydra.lib.logic.IfElse.apply(
            hydra.lib.equality.Equal.apply(
              (ts),
              new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.TypeScheme")), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))),
            hydra.lib.flows.Map.apply(
              (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Maybe<hydra.core.TypeScheme>>) ((hydra.lib.maybes.Pure::apply)),
              hydra.monads.Monads.eitherToFlow(
                wrapped -> ((wrapped)).value,
                hydra.decode.core.Core.typeScheme(
                  (cx),
                  ((el)).term))),
            hydra.lib.logic.IfElse.apply(
              hydra.lib.equality.Equal.apply(
                (ts),
                new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))),
              hydra.lib.flows.Map.apply(
                (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.TypeScheme>>) (decoded -> hydra.util.Maybe.just(((toTypeScheme.get()).apply((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()))).apply((decoded)))),
                hydra.monads.Monads.eitherToFlow(
                  wrapped -> ((wrapped)).value,
                  hydra.decode.core.Core.type(
                    (cx),
                    ((el)).term))),
              ((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.TypeScheme>>>) (v1 -> hydra.schemas.Schemas.schemaGraphToTypingEnvironment_forTerm(
                (cx),
                (el),
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                  (p0),
                  (p1))),
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.TypeScheme>>>) (p0 -> p1 -> hydra.decode.core.Core.typeScheme(
                  (p0),
                  (p1))),
                toTypeScheme.get(),
                (v1)))).apply(hydra.rewriting.Rewriting.deannotateTerm(((el)).term))))),
          ((el)).type),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.TypeScheme>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>>>>) (mts -> hydra.lib.flows.Pure.apply(hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>>) (ts -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>(((el)).name, (ts))))),
          (mts))))))));
    return hydra.monads.Monads.withTrace(
      "schema graph to typing environment",
      hydra.monads.Monads.withState(
        (g),
        hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (toPair),
            ((g)).elements),
          (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>>>, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>>) (mpairs -> hydra.lib.flows.Pure.apply(hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply((mpairs))))))));
  }
  
  static <T0, T1, T2, T3, T4> hydra.compute.Flow<T4, hydra.util.Maybe<T2>> schemaGraphToTypingEnvironment_forTerm(T0 cx, hydra.core.Binding el, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T1>>> hydra_decode_core_type2, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T2>>> hydra_decode_core_typeScheme2, java.util.function.Function<java.util.List<T3>, java.util.function.Function<T1, T2>> toTypeScheme, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T4, hydra.util.Maybe<T2>> otherwise(hydra.core.Term instance) {
        return hydra.lib.flows.Pure.apply((hydra.util.Maybe<T2>) (hydra.util.Maybe.<T2>nothing()));
      }
      
      @Override
      public hydra.compute.Flow<T4, hydra.util.Maybe<T2>> visit(hydra.core.Term.Record r) {
        return hydra.lib.logic.IfElse.apply(
          hydra.lib.equality.Equal.apply(
            (((r)).value).typeName,
            new hydra.core.Name("hydra.core.TypeScheme")),
          hydra.lib.flows.Map.apply(
            (java.util.function.Function<T2, hydra.util.Maybe<T2>>) ((hydra.lib.maybes.Pure::apply)),
            hydra.monads.Monads.eitherToFlow(
              wrapped -> ((wrapped)).value,
              (((hydra_decode_core_typeScheme2)).apply((cx))).apply(((el)).term))),
          hydra.lib.flows.Pure.apply((hydra.util.Maybe<T2>) (hydra.util.Maybe.<T2>nothing())));
      }
      
      @Override
      public hydra.compute.Flow<T4, hydra.util.Maybe<T2>> visit(hydra.core.Term.Union i) {
        return hydra.lib.logic.IfElse.apply(
          hydra.lib.equality.Equal.apply(
            (((i)).value).typeName,
            new hydra.core.Name("hydra.core.Type")),
          hydra.lib.flows.Map.apply(
            (java.util.function.Function<T1, hydra.util.Maybe<T2>>) (decoded -> hydra.util.Maybe.just((((toTypeScheme)).apply((java.util.List<T3>) (java.util.List.<T3>of()))).apply((decoded)))),
            hydra.monads.Monads.eitherToFlow(
              wrapped -> ((wrapped)).value,
              (((hydra_decode_core_type2)).apply((cx))).apply(((el)).term))),
          hydra.lib.flows.Pure.apply((hydra.util.Maybe<T2>) (hydra.util.Maybe.<T2>nothing())));
      }
    });
  }
  
  static java.util.List<hydra.core.Binding> termAsGraph(hydra.core.Term term) {
    return (hydra.rewriting.Rewriting.deannotateTerm((term))).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Binding> otherwise(hydra.core.Term instance) {
        return (java.util.List<hydra.core.Binding>) (java.util.List.<hydra.core.Binding>of());
      }
      
      @Override
      public java.util.List<hydra.core.Binding> visit(hydra.core.Term.Let lt) {
        return (((lt)).value).bindings;
      }
    });
  }
  
  static java.util.List<java.util.List<hydra.module.TypeDefinition>> topologicalSortTypeDefinitions(java.util.List<hydra.module.TypeDefinition> defs) {
    java.util.Map<hydra.core.Name, hydra.module.TypeDefinition> nameToDef = hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.TypeDefinition, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.module.TypeDefinition>>) (d -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.module.TypeDefinition>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.module.TypeDefinition>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.module.TypeDefinition>(((d)).name, (d))))),
      (defs)));
    java.util.function.Function<hydra.module.TypeDefinition, hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>> toPair = (java.util.function.Function<hydra.module.TypeDefinition, hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>>) (def -> (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>(((def)).name, hydra.lib.sets.ToList.apply(hydra.rewriting.Rewriting.typeDependencyNames(
      false,
      ((def)).type))))));
    java.util.List<java.util.List<hydra.core.Name>> sorted = hydra.sorting.Sorting.topologicalSortComponents(hydra.lib.lists.Map.apply(
      (toPair),
      (defs)));
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<hydra.core.Name>, java.util.List<hydra.module.TypeDefinition>>) (names -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.module.TypeDefinition>>) (n -> hydra.lib.maps.Lookup.apply(
          (n),
          (nameToDef))),
        (names)))),
      (sorted));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>> typeDependencies(Boolean withSchema, java.util.function.Function<hydra.core.Type, hydra.core.Type> transform, hydra.core.Name name) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>>>) (cx -> {
        java.util.function.Function<hydra.core.Name, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>> requireType = (java.util.function.Function<hydra.core.Name, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (name2 -> hydra.monads.Monads.withTrace(
          hydra.lib.strings.Cat2.apply(
            "type dependencies of ",
            ((name2)).value),
          hydra.lib.flows.Bind.apply(
            hydra.lexical.Lexical.requireElement((name2)),
            (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (el -> hydra.monads.Monads.eitherToFlow(
              wrapped -> ((wrapped)).value,
              hydra.decode.core.Core.type(
                (cx),
                ((el)).term))))));
        java.util.function.Function<hydra.core.Name, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>> toPair = (java.util.function.Function<hydra.core.Name, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>>) (name2 -> hydra.lib.flows.Bind.apply(
          ((requireType)).apply((name2)),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>>) (typ -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>((name2), ((transform)).apply((typ)))))))));
        java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>>>>> deps = new java.util.concurrent.atomic.AtomicReference<>();
        deps.set((java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>>>>) (seeds -> (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>>>) (names -> hydra.lib.logic.IfElse.apply(
          hydra.lib.sets.Null.apply((seeds)),
          hydra.lib.flows.Pure.apply((names)),
          hydra.lib.flows.Bind.apply(
            hydra.lib.flows.MapList.apply(
              (toPair),
              hydra.lib.sets.ToList.apply((seeds))),
            (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, hydra.core.Type>>>) (pairs -> {
              java.util.Map<hydra.core.Name, hydra.core.Type> newNames = hydra.lib.maps.Union.apply(
                (names),
                hydra.lib.maps.FromList.apply((pairs)));
              java.util.Set<hydra.core.Name> refs = hydra.lib.lists.Foldl.apply(
                (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.Set<hydra.core.Name>>>) ((java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.Set<hydra.core.Name>>>) (p0 -> p1 -> hydra.lib.sets.Union.apply(
                  (p0),
                  (p1)))),
                (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>, java.util.Set<hydra.core.Name>>) (pair -> hydra.rewriting.Rewriting.typeDependencyNames(
                    (withSchema),
                    hydra.lib.pairs.Second.apply((pair)))),
                  (pairs)));
              java.util.Set<hydra.core.Name> visited = hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply((names)));
              java.util.Set<hydra.core.Name> newSeeds = hydra.lib.sets.Difference.apply(
                (refs),
                (visited));
              return ((deps.get()).apply((newSeeds))).apply((newNames));
            }))))));
        return hydra.monads.Monads.withTrace(
          "type dependencies",
          ((deps.get()).apply(hydra.lib.sets.Singleton.apply((name)))).apply((java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()))));
      }));
  }
  
  static hydra.core.Type typeSchemeToFType(hydra.core.TypeScheme ts) {
    hydra.core.Type body = ((ts)).type;
    java.util.List<hydra.core.Name> vars = ((ts)).variables;
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Name, hydra.core.Type>>) (t -> (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (v -> new hydra.core.Type.Forall(new hydra.core.ForallType((v), (t))))),
      (body),
      hydra.lib.lists.Reverse.apply((vars)));
  }
  
  static hydra.core.TypeScheme typeToTypeScheme(hydra.core.Type t0) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>>> helper = new java.util.concurrent.atomic.AtomicReference<>();
    helper.set((java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>>) (vars -> (java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>) (t -> (hydra.rewriting.Rewriting.deannotateType((t))).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.TypeScheme otherwise(hydra.core.Type instance) {
        return new hydra.core.TypeScheme(hydra.lib.lists.Reverse.apply((vars)), (t), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()));
      }
      
      @Override
      public hydra.core.TypeScheme visit(hydra.core.Type.Forall ft) {
        return ((helper.get()).apply(hydra.lib.lists.Cons.apply(
          (((ft)).value).parameter,
          (vars)))).apply((((ft)).value).body);
      }
    }))));
    return ((helper.get()).apply((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()))).apply((t0));
  }
  
  static java.util.List<hydra.core.Binding> typesToElements(java.util.Map<hydra.core.Name, hydra.core.Type> typeMap) {
    java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>, hydra.core.Binding> toElement = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>, hydra.core.Binding>) (pair -> {
      hydra.core.Name name = hydra.lib.pairs.First.apply((pair));
      return new hydra.core.Binding((name), hydra.encode.core.Core.type(hydra.lib.pairs.Second.apply((pair))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()));
    });
    return hydra.lib.lists.Map.apply(
      (toElement),
      hydra.lib.maps.ToList.apply((typeMap)));
  }
  
  static <T0, T1, T2> T2 withLambdaContext(java.util.function.Function<T0, hydra.typing.TypeContext> getContext, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<T0, T1>> setContext, T0 env, hydra.core.Lambda lam, java.util.function.Function<T1, T2> body) {
    hydra.typing.TypeContext newContext = hydra.schemas.Schemas.extendTypeContextForLambda(
      ((getContext)).apply((env)),
      (lam));
    return ((body)).apply((((setContext)).apply((newContext))).apply((env)));
  }
  
  static <T0, T1, T2> T2 withLetContext(java.util.function.Function<T0, hydra.typing.TypeContext> getContext, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<T0, T1>> setContext, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>> forBinding, T0 env, hydra.core.Let letrec, java.util.function.Function<T1, T2> body) {
    hydra.typing.TypeContext newContext = hydra.schemas.Schemas.extendTypeContextForLet(
      (forBinding),
      ((getContext)).apply((env)),
      (letrec));
    return ((body)).apply((((setContext)).apply((newContext))).apply((env)));
  }
  
  static <T0, T1, T2> T2 withTypeLambdaContext(java.util.function.Function<T0, hydra.typing.TypeContext> getContext, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<T0, T1>> setContext, T0 env, hydra.core.TypeLambda tlam, java.util.function.Function<T1, T2> body) {
    hydra.typing.TypeContext newContext = hydra.schemas.Schemas.extendTypeContextForTypeLambda(
      ((getContext)).apply((env)),
      (tlam));
    return ((body)).apply((((setContext)).apply((newContext))).apply((env)));
  }
}
