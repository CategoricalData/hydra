// Note: this is an automatically generated file. Do not edit.

package hydra.adapt.modules;

/**
 * Entry point for Hydra's adapter (type/term rewriting) framework
 */
public interface Modules {
  static <T0, T1> hydra.util.Either<String, T0> adaptTypeToLanguageAndEncode(hydra.coders.Language lang, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, T0>> enc, T1 cx, hydra.graph.Graph g, hydra.core.Type typ) {
    return (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, T0> otherwise(hydra.core.Type instance) {
        return hydra.adapt.modules.Modules.<T0, T1>adaptTypeToLanguageAndEncode_dflt(
          cx,
          enc,
          g,
          lang,
          typ);
      }
      
      @Override
      public hydra.util.Either<String, T0> visit(hydra.core.Type.Variable ignored) {
        return (enc).apply(typ);
      }
    });
  }
  
  static <T0, T1> hydra.util.Either<String, T0> adaptTypeToLanguageAndEncode_dflt(T1 cx, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, T0>> enc, hydra.graph.Graph g, hydra.coders.Language lang, hydra.core.Type typ) {
    return hydra.lib.eithers.Bind.apply(
      hydra.adapt.modules.Modules.<T1>adaptTypeToLanguage(
        lang,
        cx,
        g,
        typ),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, T0>>) (adaptedType -> (enc).apply(adaptedType)));
  }
  
  static <T0> hydra.util.Either<String, hydra.core.Type> adaptTypeToLanguage(hydra.coders.Language lang, T0 cx, hydra.graph.Graph g, hydra.core.Type typ) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target)))),
      hydra.adapt.modules.Modules.<T0>languageAdapter(
        lang,
        cx,
        g,
        typ));
  }
  
  static hydra.util.Either<String, java.util.List<hydra.module.Definition>> adaptedModuleDefinitions(hydra.coders.Language lang, hydra.context.Context cx, hydra.graph.Graph graph, hydra.module.Module mod) {
    java.util.function.Function<java.util.List<hydra.core.Type>, hydra.util.Either<String, java.util.Map<hydra.core.Type, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>> adaptersFor = (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.util.Either<String, java.util.Map<hydra.core.Type, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (types -> hydra.lib.eithers.Map.apply(
      (java.util.function.Function<java.util.List<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>, java.util.Map<hydra.core.Type, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (adapters -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
        types,
        adapters))),
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (v1 -> hydra.adapt.modules.Modules.languageAdapter(
          lang,
          cx,
          graph,
          v1)),
        types)));
    java.util.List<hydra.core.Binding> els = (mod).elements;
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.util.Either<String, hydra.core.TypeApplicationTerm>>) (_el -> hydra.lib.eithers.Bimap.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.error.OtherError>, String>) (ic -> (((java.util.function.Function<hydra.context.InContext<hydra.error.OtherError>, hydra.error.OtherError>) (projected -> projected.object)).apply(ic)).value),
          (java.util.function.Function<hydra.core.TypeApplicationTerm, hydra.core.TypeApplicationTerm>) (x -> x),
          hydra.schemas.Schemas.elementAsTypeApplicationTerm(
            cx,
            _el))),
        els),
      (java.util.function.Function<java.util.List<hydra.core.TypeApplicationTerm>, hydra.util.Either<String, java.util.List<hydra.module.Definition>>>) (tterms -> {
        hydra.util.Lazy<java.util.List<hydra.core.Type>> types = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply(hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.TypeApplicationTerm, hydra.core.Type>) (arg_ -> hydra.rewriting.Rewriting.deannotateType((arg_).type)),
          tterms))));
        return hydra.lib.eithers.Bind.apply(
          (adaptersFor).apply(types.get()),
          (java.util.function.Function<java.util.Map<hydra.core.Type, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>, hydra.util.Either<String, java.util.List<hydra.module.Definition>>>) (adapters -> hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.util.Pair<hydra.core.Binding, hydra.core.TypeApplicationTerm>, hydra.util.Either<String, hydra.module.Definition>>) (v1 -> hydra.adapt.modules.Modules.adaptedModuleDefinitions_classify(
              cx,
              graph,
              hydra.annotations.Annotations::isNativeType,
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                p0,
                p1)),
              hydra.schemas.Schemas::typeToTypeScheme,
              lang,
              adapters,
              v1)),
            hydra.lib.lists.Zip.apply(
              els,
              tterms))));
      }));
  }
  
  static <T0> hydra.util.Either<String, hydra.module.Definition> adaptedModuleDefinitions_classify(hydra.context.Context cx, hydra.graph.Graph graph, java.util.function.Function<hydra.core.Binding, Boolean> hydra_annotations_isNativeType2, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.core.Type>>> hydra_decode_core_type2, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme> hydra_schemas_typeToTypeScheme2, hydra.coders.Language lang, java.util.Map<hydra.core.Type, hydra.compute.Adapter<T0, hydra.core.Type, hydra.core.Term, hydra.core.Term>> adapters, hydra.util.Pair<hydra.core.Binding, hydra.core.TypeApplicationTerm> pair) {
    hydra.util.Lazy<hydra.core.Binding> el = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
    hydra.core.Name name = (el.get()).name;
    hydra.util.Lazy<hydra.core.TypeApplicationTerm> tt = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
    hydra.core.Term term = (tt.get()).body;
    hydra.core.Type typ = (tt.get()).type;
    return hydra.lib.logic.IfElse.lazy(
      (hydra_annotations_isNativeType2).apply(el.get()),
      () -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.Bimap.apply(
          (java.util.function.Function<hydra.error.DecodingError, String>) (e -> (e).value),
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> x),
          ((hydra_decode_core_type2).apply(graph)).apply(term)),
        (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.module.Definition>>) (coreTyp -> hydra.lib.eithers.Bind.apply(
          hydra.adapt.modules.Modules.adaptTypeToLanguage(
            lang,
            cx,
            graph,
            coreTyp),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.module.Definition>>) (adaptedTyp -> (hydra.util.Either<String, hydra.module.Definition>) ((hydra.util.Either<String, hydra.module.Definition>) (hydra.util.Either.<String, hydra.module.Definition>right(new hydra.module.Definition.Type(new hydra.module.TypeDefinition(name, adaptedTyp))))))))),
      () -> hydra.lib.maybes.Maybe.apply(
        (hydra.util.Either<String, hydra.module.Definition>) ((hydra.util.Either<String, hydra.module.Definition>) (hydra.util.Either.<String, hydra.module.Definition>left(hydra.lib.strings.Cat2.apply(
          "no adapter for element ",
          (name).value)))),
        (java.util.function.Function<hydra.compute.Adapter<T0, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.module.Definition>>) (adapter -> hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Bimap.apply(
            (java.util.function.Function<hydra.context.InContext<hydra.error.OtherError>, String>) (ic -> (((java.util.function.Function<hydra.context.InContext<hydra.error.OtherError>, hydra.error.OtherError>) (projected -> projected.object)).apply(ic)).value),
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (x -> x),
            ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (projected -> projected.encode))).apply(((java.util.function.Function<hydra.compute.Adapter<T0, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(adapter))).apply(cx)).apply(term)),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.module.Definition>>) (adapted -> (hydra.util.Either<String, hydra.module.Definition>) ((hydra.util.Either<String, hydra.module.Definition>) (hydra.util.Either.<String, hydra.module.Definition>right(new hydra.module.Definition.Term(new hydra.module.TermDefinition(name, adapted, (hydra_schemas_typeToTypeScheme2).apply(((java.util.function.Function<hydra.compute.Adapter<T0, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<T0, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<T0, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<T0, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(adapter)))))))))),
        hydra.lib.maps.Lookup.apply(
          typ,
          adapters)));
  }
  
  static <T0, T1> hydra.util.Either<String, hydra.compute.Coder<hydra.core.Term, T0>> constructCoder(hydra.coders.Language lang, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T0>>> encodeTerm, T1 cx, hydra.graph.Graph g, hydra.core.Type typ) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, T0>>) (adapter -> hydra.adapt.utils.Utils.composeCoders(
        ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(adapter),
        hydra.adapt.utils.Utils.unidirectionalCoder(encodeTerm))),
      hydra.adapt.modules.Modules.<T1>languageAdapter(
        lang,
        cx,
        g,
        typ));
  }
  
  static <T0> hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> languageAdapter(hydra.coders.Language lang, T0 _cx, hydra.graph.Graph g, hydra.core.Type typ) {
    hydra.util.Lazy<hydra.coders.AdapterContext> cx0 = new hydra.util.Lazy<>(() -> new hydra.coders.AdapterContext(g, lang, (java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>apply()))));
    return hydra.adapt.terms.Terms.termAdapter(
      cx0.get(),
      typ);
  }
  
  static <T0, T1> hydra.util.Either<String, T1> transformModule(hydra.coders.Language lang, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T0>>> encodeTerm, java.util.function.Function<hydra.module.Module, java.util.function.Function<java.util.Map<hydra.core.Type, hydra.compute.Coder<hydra.core.Term, T0>>, java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.TypeApplicationTerm>>, hydra.util.Either<String, T1>>>> createModule, hydra.context.Context cx, hydra.graph.Graph g, hydra.module.Module mod) {
    java.util.List<hydra.core.Binding> els = (mod).elements;
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.util.Either<String, hydra.core.TypeApplicationTerm>>) (_el -> hydra.lib.eithers.Bimap.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.error.OtherError>, String>) (ic -> (((java.util.function.Function<hydra.context.InContext<hydra.error.OtherError>, hydra.error.OtherError>) (projected -> projected.object)).apply(ic)).value),
          (java.util.function.Function<hydra.core.TypeApplicationTerm, hydra.core.TypeApplicationTerm>) (x -> x),
          hydra.schemas.Schemas.elementAsTypeApplicationTerm(
            cx,
            _el))),
        els),
      (java.util.function.Function<java.util.List<hydra.core.TypeApplicationTerm>, hydra.util.Either<String, T1>>) (tterms -> {
        hydra.util.Lazy<java.util.List<hydra.core.Type>> types = new hydra.util.Lazy<>(() -> hydra.lib.lists.Nub.apply(hydra.lib.lists.Map.apply(
          projected -> projected.type,
          tterms)));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Coder<hydra.core.Term, T0>>>) (v1 -> hydra.adapt.modules.Modules.constructCoder(
              lang,
              encodeTerm,
              cx,
              g,
              v1)),
            types.get()),
          (java.util.function.Function<java.util.List<hydra.compute.Coder<hydra.core.Term, T0>>, hydra.util.Either<String, T1>>) (cdrs -> (((createModule).apply(mod)).apply(hydra.adapt.modules.Modules.<T0>transformModule_coders(
            cdrs,
            types.get()))).apply(hydra.lib.lists.Zip.apply(
            els,
            tterms))));
      }));
  }
  
  static <T0> java.util.Map<hydra.core.Type, hydra.compute.Coder<hydra.core.Term, T0>> transformModule_coders(java.util.List<hydra.compute.Coder<hydra.core.Term, T0>> cdrs, java.util.List<hydra.core.Type> types) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      types,
      cdrs));
  }
}
