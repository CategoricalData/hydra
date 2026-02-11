// Note: this is an automatically generated file. Do not edit.

package hydra.adapt.modules;

/**
 * Entry point for Hydra's adapter (type/term rewriting) framework
 */
public interface Modules {
  static <T0> hydra.compute.Flow<hydra.graph.Graph, T0> adaptTypeToLanguageAndEncode(hydra.coders.Language lang, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, T0>> enc, hydra.core.Type typ) {
    return (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, T0> otherwise(hydra.core.Type instance) {
        return hydra.adapt.modules.Modules.<T0>adaptTypeToLanguageAndEncode_dflt(
          enc,
          lang,
          typ);
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, T0> visit(hydra.core.Type.Variable ignored) {
        return (enc).apply(typ);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<hydra.graph.Graph, T0> adaptTypeToLanguageAndEncode_dflt(java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, T0>> enc, hydra.coders.Language lang, hydra.core.Type typ) {
    return hydra.lib.flows.Bind.apply(
      hydra.adapt.modules.Modules.adaptTypeToLanguage(
        lang,
        typ),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, T0>>) (adaptedType -> (enc).apply(adaptedType)));
  }
  
  static <T1, T0> hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type> adaptTypeToLanguage(hydra.coders.Language lang, hydra.core.Type typ) {
    return hydra.lib.flows.Bind.apply(
      hydra.adapt.modules.Modules.languageAdapter(
        lang,
        typ),
      (java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (adapter -> hydra.lib.flows.Pure.apply(((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))))).apply(adapter))));
  }
  
  static <T0> hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.module.Definition>> adaptedModuleDefinitions(hydra.coders.Language lang, hydra.module.Module mod) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.module.Definition>>>) (cx -> {
        java.util.List<hydra.core.Binding> els = (mod).elements;
        return hydra.lib.flows.Bind.apply(
          hydra.lexical.Lexical.withSchemaContext(hydra.lib.flows.MapList.apply(
            p0 -> hydra.schemas.Schemas.<hydra.graph.Graph>elementAsTypeApplicationTerm(p0),
            els)),
          (java.util.function.Function<java.util.List<hydra.core.TypeApplicationTerm>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.module.Definition>>>) (tterms -> {
            hydra.util.Lazy<java.util.List<hydra.core.Type>> types = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply(hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.TypeApplicationTerm, hydra.core.Type>) (arg_ -> hydra.rewriting.Rewriting.deannotateType((arg_).type)),
              tterms))));
            return hydra.lib.flows.Bind.apply(
              ((java.util.function.Function<java.util.List<hydra.core.Type>, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Type, hydra.compute.Adapter<hydra.graph.Graph, T0, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (v1 -> hydra.adapt.modules.Modules.adaptedModuleDefinitions_adaptersFor(
                lang,
                v1))).apply(types.get()),
              (java.util.function.Function<java.util.Map<hydra.core.Type, hydra.compute.Adapter<hydra.graph.Graph, T0, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.module.Definition>>>) (adapters -> hydra.lib.flows.MapList.apply(
                ((java.util.function.Function<java.util.Map<hydra.core.Type, hydra.compute.Adapter<hydra.graph.Graph, T0, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.TypeApplicationTerm>, hydra.compute.Flow<hydra.graph.Graph, hydra.module.Definition>>>) (v1 -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.TypeApplicationTerm>, hydra.compute.Flow<hydra.graph.Graph, hydra.module.Definition>>) (v2 -> hydra.adapt.modules.Modules.adaptedModuleDefinitions_classify(
                  cx,
                  hydra.annotations.Annotations::isNativeType,
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                    p0,
                    p1)),
                  hydra.schemas.Schemas::typeToTypeScheme,
                  lang,
                  v1,
                  v2)))).apply(adapters),
                hydra.lib.lists.Zip.apply(
                  els,
                  tterms))));
          }));
      }));
  }
  
  static <T0, T1> hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Type, hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> adaptedModuleDefinitions_adaptersFor(hydra.coders.Language lang, java.util.List<hydra.core.Type> types) {
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapList.apply(
        (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (v1 -> hydra.adapt.modules.Modules.<T0, T1>languageAdapter(
          lang,
          v1)),
        types),
      (java.util.function.Function<java.util.List<hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Type, hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (adapters -> hydra.lib.flows.Pure.apply(hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
        types,
        adapters)))));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<hydra.graph.Graph, hydra.module.Definition> adaptedModuleDefinitions_classify(T0 cx, java.util.function.Function<hydra.core.Binding, Boolean> hydra_annotations_isNativeType2, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>> hydra_decode_core_type2, java.util.function.Function<T1, hydra.core.TypeScheme> hydra_schemas_typeToTypeScheme2, hydra.coders.Language lang, java.util.Map<hydra.core.Type, hydra.compute.Adapter<hydra.graph.Graph, T2, T3, T1, hydra.core.Term, hydra.core.Term>> adapters, hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.TypeApplicationTerm> pair) {
    hydra.util.Lazy<hydra.core.Binding> el = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
    hydra.core.Name name = (el.get()).name;
    hydra.util.Lazy<hydra.core.TypeApplicationTerm> tt = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
    hydra.core.Term term = (tt.get()).body;
    hydra.core.Type typ = (tt.get()).type;
    return hydra.lib.logic.IfElse.lazy(
      (hydra_annotations_isNativeType2).apply(el.get()),
      () -> hydra.lib.flows.Bind.apply(
        hydra.lib.flows.Bind.apply(
          hydra.monads.Monads.withTrace(
            "adapt module definitions",
            hydra.monads.Monads.eitherToFlow(
              wrapped -> (wrapped).value,
              ((hydra_decode_core_type2).apply(cx)).apply(term))),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (coreTyp -> hydra.adapt.modules.Modules.adaptTypeToLanguage(
            lang,
            coreTyp))),
        (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.module.Definition>>) (adaptedTyp -> hydra.lib.flows.Pure.apply(new hydra.module.Definition.Type(new hydra.module.TypeDefinition(name, adaptedTyp))))),
      () -> hydra.lib.maybes.Maybe.apply(
        hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
          "no adapter for element ",
          (name).value)),
        (java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T2, T3, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.module.Definition>>) (adapter -> hydra.lib.flows.Bind.apply(
          (((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T2, hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T2, hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T2, hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T2, hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>) (projected -> projected.encode))))).apply(((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T2, T3, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.graph.Graph, T2, hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T2, T3, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.graph.Graph, T2, hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T2, T3, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.graph.Graph, T2, hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T2, T3, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.graph.Graph, T2, hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T2, T3, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.graph.Graph, T2, hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T2, T3, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.graph.Graph, T2, hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))))).apply(adapter))).apply(term),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.module.Definition>>) (adapted -> hydra.lib.flows.Pure.apply(new hydra.module.Definition.Term(new hydra.module.TermDefinition(name, adapted, (hydra_schemas_typeToTypeScheme2).apply(((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T2, T3, T1, hydra.core.Term, hydra.core.Term>, T1>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T2, T3, T1, hydra.core.Term, hydra.core.Term>, T1>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T2, T3, T1, hydra.core.Term, hydra.core.Term>, T1>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T2, T3, T1, hydra.core.Term, hydra.core.Term>, T1>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T2, T3, T1, hydra.core.Term, hydra.core.Term>, T1>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T2, T3, T1, hydra.core.Term, hydra.core.Term>, T1>) (projected -> projected.target))))))).apply(adapter)))))))),
        hydra.lib.maps.Lookup.apply(
          typ,
          adapters)));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<hydra.graph.Graph, hydra.compute.Coder<T0, T2, hydra.core.Term, T1>> constructCoder(hydra.coders.Language lang, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, T1>> encodeTerm, hydra.core.Type typ) {
    return hydra.monads.Monads.withTrace(
      hydra.lib.strings.Cat2.apply(
        "coder for ",
        hydra.show.core.Core.type(typ)),
      hydra.lib.flows.Bind.apply(
        hydra.adapt.modules.Modules.<T0, T2>languageAdapter(
          lang,
          typ),
        (java.util.function.Function<hydra.compute.Adapter<T0, T2, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.compute.Coder<T0, T2, hydra.core.Term, T1>>>) (adapter -> hydra.lib.flows.Pure.apply(hydra.adapt.utils.Utils.composeCoders(
          ((java.util.function.Function<hydra.compute.Adapter<T0, T2, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<T0, T2, hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T2, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<T0, T2, hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T2, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<T0, T2, hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T2, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<T0, T2, hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T2, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<T0, T2, hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T2, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<T0, T2, hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))))).apply(adapter),
          hydra.adapt.utils.Utils.unidirectionalCoder(encodeTerm))))));
  }
  
  static <T0, T1> hydra.compute.Flow<hydra.graph.Graph, hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> languageAdapter(hydra.coders.Language lang, hydra.core.Type typ) {
    java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.coders.AdapterContext, hydra.util.Tuple.Tuple2<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.coders.AdapterContext>>> getPair = (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.coders.AdapterContext, hydra.util.Tuple.Tuple2<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.coders.AdapterContext>>>) (typ2 -> hydra.lib.flows.Bind.apply(
      hydra.adapt.terms.Terms.termAdapter(typ2),
      (java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<hydra.coders.AdapterContext, hydra.util.Tuple.Tuple2<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.coders.AdapterContext>>>) (ad -> hydra.lib.flows.Bind.apply(
        hydra.monads.Monads.<hydra.coders.AdapterContext>getState(),
        (java.util.function.Function<hydra.coders.AdapterContext, hydra.compute.Flow<hydra.coders.AdapterContext, hydra.util.Tuple.Tuple2<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.coders.AdapterContext>>>) (cx -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.coders.AdapterContext>) ((hydra.util.Tuple.Tuple2<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.coders.AdapterContext>) (new hydra.util.Tuple.Tuple2<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.coders.AdapterContext>(ad, cx)))))))));
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (g -> {
        hydra.util.Lazy<hydra.coders.AdapterContext> cx0 = new hydra.util.Lazy<>(() -> new hydra.coders.AdapterContext(g, lang, (java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>apply()))));
        return hydra.lib.flows.Bind.apply(
          hydra.monads.Monads.withState(
            cx0.get(),
            (getPair).apply(typ)),
          (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.coders.AdapterContext>, hydra.compute.Flow<hydra.graph.Graph, hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (result -> {
            hydra.util.Lazy<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> adapter = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
            hydra.util.Lazy<hydra.coders.AdapterContext> cx = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result));
            return hydra.lib.flows.Pure.apply((hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))))).apply(adapter.get()), ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.source))))))).apply(adapter.get()), ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))))).apply(adapter.get()), (hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.core.Term>((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>) (v1 -> hydra.adapt.modules.Modules.languageAdapter_encode(
              adapter.get(),
              cx.get(),
              v1)), (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>) (v1 -> hydra.adapt.modules.Modules.languageAdapter_decode(
              adapter.get(),
              cx.get(),
              v1)))))))))))))));
          }));
      }));
  }
  
  static <T0, T1, T2, T3, T4, T5, T6> hydra.compute.Flow<T6, T5> languageAdapter_encode(hydra.compute.Adapter<T0, T1, T2, T3, T4, T5> adapter, T0 cx, T4 term) {
    return hydra.monads.Monads.<T0, T5, T6>withState(
      cx,
      (((java.util.function.Function<hydra.compute.Coder<T0, T1, T4, T5>, java.util.function.Function<T4, hydra.compute.Flow<T0, T5>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T4, T5>, java.util.function.Function<T4, hydra.compute.Flow<T0, T5>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T4, T5>, java.util.function.Function<T4, hydra.compute.Flow<T0, T5>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T4, T5>, java.util.function.Function<T4, hydra.compute.Flow<T0, T5>>>) (projected -> projected.encode))))).apply(((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) (projected -> projected.coder))))))).apply(adapter))).apply(term));
  }
  
  static <T0, T1, T2, T3, T4, T5, T6> hydra.compute.Flow<T6, T4> languageAdapter_decode(hydra.compute.Adapter<T0, T1, T2, T3, T4, T5> adapter, T1 cx, T5 term) {
    return hydra.monads.Monads.<T1, T4, T6>withState(
      cx,
      (((java.util.function.Function<hydra.compute.Coder<T0, T1, T4, T5>, java.util.function.Function<T5, hydra.compute.Flow<T1, T4>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T4, T5>, java.util.function.Function<T5, hydra.compute.Flow<T1, T4>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T4, T5>, java.util.function.Function<T5, hydra.compute.Flow<T1, T4>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T4, T5>, java.util.function.Function<T5, hydra.compute.Flow<T1, T4>>>) (projected -> projected.decode))))).apply(((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) (projected -> projected.coder))))))).apply(adapter))).apply(term));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<hydra.graph.Graph, T3> transformModule(hydra.coders.Language lang, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, T1>> encodeTerm, java.util.function.Function<hydra.module.Module, java.util.function.Function<java.util.Map<hydra.core.Type, hydra.compute.Coder<T0, T2, hydra.core.Term, T1>>, java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.TypeApplicationTerm>>, hydra.compute.Flow<hydra.graph.Graph, T3>>>> createModule, hydra.module.Module mod) {
    java.util.List<hydra.core.Binding> els = (mod).elements;
    return hydra.monads.Monads.withTrace(
      hydra.lib.strings.Cat2.apply(
        "transform module ",
        ((mod).namespace).value),
      hydra.adapt.modules.Modules.transformModule_transform(
        createModule,
        els,
        encodeTerm,
        lang,
        mod));
  }
  
  static <T0, T1, T2, T3, T4> hydra.compute.Flow<hydra.graph.Graph, T4> transformModule_transform(java.util.function.Function<T0, java.util.function.Function<java.util.Map<hydra.core.Type, hydra.compute.Coder<T1, T2, hydra.core.Term, T3>>, java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.TypeApplicationTerm>>, hydra.compute.Flow<hydra.graph.Graph, T4>>>> createModule, java.util.List<hydra.core.Binding> els, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, T3>> encodeTerm, hydra.coders.Language lang, T0 mod) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.withSchemaContext(hydra.lib.flows.MapList.apply(
        p0 -> hydra.schemas.Schemas.<hydra.graph.Graph>elementAsTypeApplicationTerm(p0),
        els)),
      (java.util.function.Function<java.util.List<hydra.core.TypeApplicationTerm>, hydra.compute.Flow<hydra.graph.Graph, T4>>) (tterms -> {
        hydra.util.Lazy<java.util.List<hydra.core.Type>> types = new hydra.util.Lazy<>(() -> hydra.lib.lists.Nub.apply(hydra.lib.lists.Map.apply(
          projected -> projected.type,
          tterms)));
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.compute.Coder<T1, T2, hydra.core.Term, T3>>>) (v1 -> hydra.adapt.modules.Modules.<T1, T3, T2>constructCoder(
              lang,
              encodeTerm,
              v1)),
            types.get()),
          (java.util.function.Function<java.util.List<hydra.compute.Coder<T1, T2, hydra.core.Term, T3>>, hydra.compute.Flow<hydra.graph.Graph, T4>>) (cdrs -> (((createModule).apply(mod)).apply(hydra.adapt.modules.Modules.transformModule_coders(
            cdrs,
            types.get()))).apply(hydra.lib.lists.Zip.apply(
            els,
            tterms))));
      }));
  }
  
  static <T0, T1> java.util.Map<T1, T0> transformModule_coders(java.util.List<T0> cdrs, java.util.List<T1> types) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      types,
      cdrs));
  }
}
