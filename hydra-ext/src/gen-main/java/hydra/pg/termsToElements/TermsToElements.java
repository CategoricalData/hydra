// Note: this is an automatically generated file. Do not edit.

package hydra.pg.termsToElements;

/**
 * Functions for mapping Hydra terms to property graph elements using mapping specifications
 */
public interface TermsToElements {
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>> applyPattern(hydra.context.Context cx, String firstLit, java.util.List<hydra.util.Pair<java.util.List<String>, String>> pairs, hydra.core.Term term) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(pairs),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>right(java.util.List.of(new hydra.core.Term.Literal(new hydra.core.Literal.String_(firstLit)))),
      () -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.util.Pair<java.util.List<String>, String>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<java.util.List<String>, String>>>) (pp -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<String>, String>>) (terms -> (hydra.util.Pair<java.util.List<String>, String>) ((hydra.util.Pair<java.util.List<String>, String>) (new hydra.util.Pair<java.util.List<String>, String>(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.pg.termsToElements.TermsToElements.termToString(t)),
              terms), hydra.lib.pairs.Second.apply(pp))))),
            hydra.pg.termsToElements.TermsToElements.evalPath(
              cx,
              hydra.lib.pairs.First.apply(pp),
              term))),
          pairs),
        (java.util.function.Function<java.util.List<hydra.util.Pair<java.util.List<String>, String>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>) (evaluated -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>right(hydra.lib.lists.Map.apply(
          (java.util.function.Function<String, hydra.core.Term>) (s -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(s))),
          hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<java.util.List<String>, java.util.function.Function<hydra.util.Pair<java.util.List<String>, String>, java.util.List<String>>>) (accum -> (java.util.function.Function<hydra.util.Pair<java.util.List<String>, String>, java.util.List<String>>) (ep -> {
              hydra.util.Lazy<String> litP = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(ep));
              hydra.util.Lazy<java.util.List<String>> pStrs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(ep));
              return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
                (java.util.function.Function<String, java.util.List<String>>) (pStr -> hydra.lib.lists.Map.apply(
                  (java.util.function.Function<String, String>) (a -> hydra.lib.strings.Cat2.apply(
                    hydra.lib.strings.Cat2.apply(
                      a,
                      pStr),
                    litP.get())),
                  accum)),
                pStrs.get()));
            })),
            java.util.List.of(firstLit),
            evaluated))))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.EdgeLabel> decodeEdgeLabel(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term t) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<String, hydra.pg.model.EdgeLabel>) (_x -> new hydra.pg.model.EdgeLabel(_x)),
      hydra.extract.core.Core.string(
        cx,
        g,
        t));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.EdgeSpec> decodeEdgeSpec(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.pg.termsToElements.TermsToElements.readRecord(
      cx,
      g,
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.EdgeSpec>>) (fields -> hydra.lib.eithers.Bind.apply(
        hydra.pg.termsToElements.TermsToElements.readField(
          cx,
          fields,
          new hydra.core.Name("label"),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.EdgeLabel>>) (v1 -> hydra.pg.termsToElements.TermsToElements.decodeEdgeLabel(
            cx,
            g,
            v1))),
        (java.util.function.Function<hydra.pg.model.EdgeLabel, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.EdgeSpec>>) (_a -> hydra.lib.eithers.Bind.apply(
          hydra.pg.termsToElements.TermsToElements.readField(
            cx,
            fields,
            new hydra.core.Name("id"),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>) (v1 -> hydra.pg.termsToElements.TermsToElements.decodeValueSpec(
              cx,
              g,
              v1))),
          (java.util.function.Function<hydra.pg.mapping.ValueSpec, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.EdgeSpec>>) (_b -> hydra.lib.eithers.Bind.apply(
            hydra.pg.termsToElements.TermsToElements.readField(
              cx,
              fields,
              new hydra.core.Name("out"),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>) (v1 -> hydra.pg.termsToElements.TermsToElements.decodeValueSpec(
                cx,
                g,
                v1))),
            (java.util.function.Function<hydra.pg.mapping.ValueSpec, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.EdgeSpec>>) (_c -> hydra.lib.eithers.Bind.apply(
              hydra.pg.termsToElements.TermsToElements.readField(
                cx,
                fields,
                new hydra.core.Name("in"),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>) (v1 -> hydra.pg.termsToElements.TermsToElements.decodeValueSpec(
                  cx,
                  g,
                  v1))),
              (java.util.function.Function<hydra.pg.mapping.ValueSpec, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.EdgeSpec>>) (_d -> hydra.lib.eithers.Map.apply(
                (java.util.function.Function<java.util.List<hydra.pg.mapping.PropertySpec>, hydra.pg.mapping.EdgeSpec>) (_e -> new hydra.pg.mapping.EdgeSpec(_a, _b, _c, _d, _e)),
                hydra.pg.termsToElements.TermsToElements.readField(
                  cx,
                  fields,
                  new hydra.core.Name("properties"),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.mapping.PropertySpec>>>) (v1 -> hydra.pg.termsToElements.TermsToElements.expectList(
                    cx,
                    g,
                    (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.PropertySpec>>>>) (p0 -> p1 -> p2 -> hydra.pg.termsToElements.TermsToElements.decodePropertySpec(
                      p0,
                      p1,
                      p2)),
                    v1))))))))))))),
      term);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ElementSpec> decodeElementSpec(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.pg.termsToElements.TermsToElements.readInjection(
      cx,
      g,
      java.util.List.of(
        (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ElementSpec>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ElementSpec>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ElementSpec>>>(new hydra.core.Name("vertex"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ElementSpec>>) (t -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.pg.mapping.VertexSpec, hydra.pg.mapping.ElementSpec>) (_x -> new hydra.pg.mapping.ElementSpec.Vertex(_x)),
          hydra.pg.termsToElements.TermsToElements.decodeVertexSpec(
            cx,
            g,
            t)))))),
        (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ElementSpec>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ElementSpec>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ElementSpec>>>(new hydra.core.Name("edge"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ElementSpec>>) (t -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.pg.mapping.EdgeSpec, hydra.pg.mapping.ElementSpec>) (_x -> new hydra.pg.mapping.ElementSpec.Edge(_x)),
          hydra.pg.termsToElements.TermsToElements.decodeEdgeSpec(
            cx,
            g,
            t))))))),
      term);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.PropertyKey> decodePropertyKey(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term t) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<String, hydra.pg.model.PropertyKey>) (_x -> new hydra.pg.model.PropertyKey(_x)),
      hydra.extract.core.Core.string(
        cx,
        g,
        t));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.PropertySpec> decodePropertySpec(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.pg.termsToElements.TermsToElements.readRecord(
      cx,
      g,
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.PropertySpec>>) (fields -> hydra.lib.eithers.Bind.apply(
        hydra.pg.termsToElements.TermsToElements.readField(
          cx,
          fields,
          new hydra.core.Name("key"),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.PropertyKey>>) (v1 -> hydra.pg.termsToElements.TermsToElements.decodePropertyKey(
            cx,
            g,
            v1))),
        (java.util.function.Function<hydra.pg.model.PropertyKey, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.PropertySpec>>) (_a -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.pg.mapping.ValueSpec, hydra.pg.mapping.PropertySpec>) (_b -> new hydra.pg.mapping.PropertySpec(_a, _b)),
          hydra.pg.termsToElements.TermsToElements.readField(
            cx,
            fields,
            new hydra.core.Name("value"),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>) (v1 -> hydra.pg.termsToElements.TermsToElements.decodeValueSpec(
              cx,
              g,
              v1))))))),
      term);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec> decodeValueSpec(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term term) {
    return (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec> otherwise(hydra.core.Term instance) {
        return hydra.pg.termsToElements.TermsToElements.readInjection(
          cx,
          g,
          java.util.List.of(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>>(new hydra.core.Name("value"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>) (ignored -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>right(new hydra.pg.mapping.ValueSpec.Value()))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>>(new hydra.core.Name("pattern"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>) (t -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<String, hydra.pg.mapping.ValueSpec>) (_x -> new hydra.pg.mapping.ValueSpec.Pattern(_x)),
              hydra.extract.core.Core.string(
                cx,
                g,
                t))))))),
          term);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec> visit(hydra.core.Term.Literal lit) {
        return ((lit).value).accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec> otherwise(hydra.core.Literal instance) {
            return hydra.pg.termsToElements.TermsToElements.readInjection(
              cx,
              g,
              java.util.List.of(
                (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>>(new hydra.core.Name("value"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>) (ignored -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>right(new hydra.pg.mapping.ValueSpec.Value()))))),
                (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>>(new hydra.core.Name("pattern"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>) (t -> hydra.lib.eithers.Map.apply(
                  (java.util.function.Function<String, hydra.pg.mapping.ValueSpec>) (_x -> new hydra.pg.mapping.ValueSpec.Pattern(_x)),
                  hydra.extract.core.Core.string(
                    cx,
                    g,
                    t))))))),
              term);
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec> visit(hydra.core.Literal.String_ s) {
            return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>right(new hydra.pg.mapping.ValueSpec.Pattern((s).value));
          }
        });
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.VertexLabel> decodeVertexLabel(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term t) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<String, hydra.pg.model.VertexLabel>) (_x -> new hydra.pg.model.VertexLabel(_x)),
      hydra.extract.core.Core.string(
        cx,
        g,
        t));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.VertexSpec> decodeVertexSpec(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.pg.termsToElements.TermsToElements.readRecord(
      cx,
      g,
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.VertexSpec>>) (fields -> hydra.lib.eithers.Bind.apply(
        hydra.pg.termsToElements.TermsToElements.readField(
          cx,
          fields,
          new hydra.core.Name("label"),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.VertexLabel>>) (v1 -> hydra.pg.termsToElements.TermsToElements.decodeVertexLabel(
            cx,
            g,
            v1))),
        (java.util.function.Function<hydra.pg.model.VertexLabel, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.VertexSpec>>) (_a -> hydra.lib.eithers.Bind.apply(
          hydra.pg.termsToElements.TermsToElements.readField(
            cx,
            fields,
            new hydra.core.Name("id"),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>) (v1 -> hydra.pg.termsToElements.TermsToElements.decodeValueSpec(
              cx,
              g,
              v1))),
          (java.util.function.Function<hydra.pg.mapping.ValueSpec, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.VertexSpec>>) (_b -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.List<hydra.pg.mapping.PropertySpec>, hydra.pg.mapping.VertexSpec>) (_c -> new hydra.pg.mapping.VertexSpec(_a, _b, _c)),
            hydra.pg.termsToElements.TermsToElements.readField(
              cx,
              fields,
              new hydra.core.Name("properties"),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.mapping.PropertySpec>>>) (v1 -> hydra.pg.termsToElements.TermsToElements.expectList(
                cx,
                g,
                (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.PropertySpec>>>>) (p0 -> p1 -> p2 -> hydra.pg.termsToElements.TermsToElements.decodePropertySpec(
                  p0,
                  p1,
                  p2)),
                v1))))))))),
      term);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>> evalPath(hydra.context.Context cx, java.util.List<String> path, hydra.core.Term term) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(path),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>right(java.util.List.of(term)),
      () -> hydra.lib.eithers.Bind.apply(
        hydra.pg.termsToElements.TermsToElements.evalStep(
          cx,
          hydra.lib.lists.Head.apply(path),
          term),
        (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>) (results -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.util.List<java.util.List<hydra.core.Term>>, java.util.List<hydra.core.Term>>) (xs -> hydra.lib.lists.Concat.apply(xs)),
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>) (v1 -> hydra.pg.termsToElements.TermsToElements.evalPath(
              cx,
              hydra.lib.lists.Tail.apply(path),
              v1)),
            results)))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>> evalStep(hydra.context.Context cx, String step, hydra.core.Term term) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.strings.Null.apply(step),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>right(java.util.List.of(term)),
      () -> (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            "Can't traverse through term for step ",
            step))), cx)));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>> visit(hydra.core.Term.List terms) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.List<java.util.List<hydra.core.Term>>, java.util.List<hydra.core.Term>>) (xs -> hydra.lib.lists.Concat.apply(xs)),
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>) (v1 -> hydra.pg.termsToElements.TermsToElements.evalStep(
                cx,
                step,
                v1)),
              (terms).value));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>> visit(hydra.core.Term.Maybe mt) {
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>right((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of())),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>) (t -> hydra.pg.termsToElements.TermsToElements.evalStep(
              cx,
              step,
              t)),
            (mt).value);
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>> visit(hydra.core.Term.Record rec) {
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "No such field ",
                step),
              " in record"))), cx))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>) (t -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>right(java.util.List.of(t))),
            hydra.lib.maps.Lookup.apply(
              new hydra.core.Name(step),
              hydra.schemas.Schemas.fieldMap(((rec).value).fields)));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>> visit(hydra.core.Term.Union inj) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              ((((inj).value).field).name).value,
              step),
            () -> hydra.pg.termsToElements.TermsToElements.evalStep(
              cx,
              step,
              (((inj).value).field).term),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>right((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of())));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>> visit(hydra.core.Term.Wrap wt) {
          return hydra.pg.termsToElements.TermsToElements.evalStep(
            cx,
            step,
            ((wt).value).body);
        }
      }));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T0>> expectList(hydra.context.Context cx, hydra.graph.Graph g, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>>> f, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.list(
        cx,
        g,
        term),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T0>>>) (elems -> hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>) (v1 -> (((f).apply(cx)).apply(g)).apply(v1)),
        elems)));
  }
  
  static <T0, T1, T2, T3, T4, T5> hydra.util.Either<T5, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>>> parseEdgeIdPattern(T0 cx, T1 g, hydra.pg.mapping.Schema<T2, T3, T4> schema, hydra.pg.mapping.ValueSpec spec) {
    return hydra.lib.eithers.Bind.apply(
      hydra.pg.termsToElements.TermsToElements.<T0, T1, T5>parseValueSpec(
        cx,
        g,
        spec),
      (java.util.function.Function<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>>, hydra.util.Either<T5, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>>>>) (fun -> hydra.util.Either.<T5, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>>>right((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>>) (cx_ -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>) (term -> hydra.lib.eithers.Bind.apply(
        ((fun).apply(cx_)).apply(term),
        (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>) (terms -> hydra.lib.eithers.MapList.apply(
          (((java.util.function.Function<hydra.util.Coder<hydra.core.Term, T4>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T4>>>>) ((java.util.function.Function<hydra.util.Coder<hydra.core.Term, T4>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T4>>>>) (projected -> projected.encode))).apply(((java.util.function.Function<hydra.pg.mapping.Schema<T2, T3, T4>, hydra.util.Coder<hydra.core.Term, T4>>) ((java.util.function.Function<hydra.pg.mapping.Schema<T2, T3, T4>, hydra.util.Coder<hydra.core.Term, T4>>) ((java.util.function.Function<hydra.pg.mapping.Schema<T2, T3, T4>, hydra.util.Coder<hydra.core.Term, T4>>) (projected -> projected.edgeIds)))).apply(schema))).apply(cx_),
          terms))))))));
  }
  
  static <T0, T1, T2, T3, T4, T5> hydra.util.Either<T5, hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>> parseEdgeSpec(T0 cx, T1 g, hydra.pg.mapping.Schema<T2, T3, T4> schema, hydra.pg.mapping.EdgeSpec spec) {
    hydra.pg.mapping.ValueSpec id = (spec).id;
    hydra.pg.mapping.ValueSpec inV = (spec).in;
    hydra.pg.model.EdgeLabel label = (spec).label;
    hydra.pg.mapping.ValueSpec outV = (spec).out;
    java.util.List<hydra.pg.mapping.PropertySpec> props = (spec).properties;
    return hydra.lib.eithers.Bind.apply(
      hydra.pg.termsToElements.TermsToElements.<T0, T1, T2, T3, T4, T5>parseEdgeIdPattern(
        cx,
        g,
        schema,
        id),
      (java.util.function.Function<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>>, hydra.util.Either<T5, hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>>>) (getId -> hydra.lib.eithers.Bind.apply(
        hydra.pg.termsToElements.TermsToElements.<T0, T1, T2, T3, T4, T5>parseVertexIdPattern(
          cx,
          g,
          schema,
          outV),
        (java.util.function.Function<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>>, hydra.util.Either<T5, hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>>>) (getOut -> hydra.lib.eithers.Bind.apply(
          hydra.pg.termsToElements.TermsToElements.<T0, T1, T2, T3, T4, T5>parseVertexIdPattern(
            cx,
            g,
            schema,
            inV),
          (java.util.function.Function<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>>, hydra.util.Either<T5, hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>>>) (getIn -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.pg.mapping.PropertySpec, hydra.util.Either<T5, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>>>>>) (v1 -> hydra.pg.termsToElements.TermsToElements.<T0, T1, T2, T3, T4, T5>parsePropertySpec(
                cx,
                g,
                schema,
                v1)),
              props),
            (java.util.function.Function<java.util.List<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>>>>, hydra.util.Either<T5, hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>>>) (getProps -> hydra.util.Either.<T5, hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>>right((hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>) ((hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>) (new hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>(new hydra.pg.model.Label.Edge(label), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>) (cx_ -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>) (term -> hydra.lib.eithers.Bind.apply(
              hydra.pg.termsToElements.TermsToElements.requireUnique(
                cx_,
                "edge id",
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>) (v1 -> ((getId).apply(cx_)).apply(v1)),
                term),
              (java.util.function.Function<T4, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>) (tid -> hydra.lib.eithers.Bind.apply(
                hydra.pg.termsToElements.TermsToElements.requireUnique(
                  cx_,
                  "vertex id",
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>) (v1 -> ((getOut).apply(cx_)).apply(v1)),
                  term),
                (java.util.function.Function<T4, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>) (tout -> hydra.lib.eithers.Bind.apply(
                  hydra.pg.termsToElements.TermsToElements.requireUnique(
                    cx_,
                    "edge id",
                    (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>) (v1 -> ((getIn).apply(cx_)).apply(v1)),
                    term),
                  (java.util.function.Function<T4, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>) (tin -> hydra.lib.eithers.Bind.apply(
                    hydra.lib.eithers.Map.apply(
                      (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>, java.util.Map<hydra.pg.model.PropertyKey, T4>>) (_xs -> hydra.lib.maps.FromList.apply(_xs)),
                      hydra.lib.eithers.MapList.apply(
                        (java.util.function.Function<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>) (gf -> hydra.pg.termsToElements.TermsToElements.requireUnique(
                          cx_,
                          "property key",
                          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>>) (v1 -> ((gf).apply(cx_)).apply(v1)),
                          term)),
                        getProps)),
                    (java.util.function.Function<java.util.Map<hydra.pg.model.PropertyKey, T4>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>) (tprops -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>right(java.util.List.of((hydra.pg.model.Element<T4>) (new hydra.pg.model.Element.Edge((hydra.pg.model.Edge<T4>) (new hydra.pg.model.Edge<T4>(label, tid, tout, tin, tprops))))))))))))))))))))))))))));
  }
  
  static <T0, T1, T2, T3, T4, T5> hydra.util.Either<T5, hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>> parseElementSpec(T0 cx, T1 g, hydra.pg.mapping.Schema<T2, T3, T4> schema, hydra.pg.mapping.ElementSpec spec) {
    return (spec).accept(new hydra.pg.mapping.ElementSpec.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T5, hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>> visit(hydra.pg.mapping.ElementSpec.Vertex vspec) {
        return hydra.pg.termsToElements.TermsToElements.<T0, T1, T2, T3, T4, T5>parseVertexSpec(
          cx,
          g,
          schema,
          (vspec).value);
      }
      
      @Override
      public hydra.util.Either<T5, hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>> visit(hydra.pg.mapping.ElementSpec.Edge espec) {
        return hydra.pg.termsToElements.TermsToElements.<T0, T1, T2, T3, T4, T5>parseEdgeSpec(
          cx,
          g,
          schema,
          (espec).value);
      }
    });
  }
  
  static <T0, T1, T2> hydra.util.Either<T2, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>>> parsePattern(T0 cx, T1 _g, String pat) {
    java.util.List<String> segments = hydra.lib.strings.SplitOn.apply(
      "${",
      pat);
    hydra.util.Lazy<String> firstLit = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(segments));
    hydra.util.Lazy<java.util.List<String>> rest = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(segments));
    hydra.util.Lazy<java.util.List<hydra.util.Pair<java.util.List<String>, String>>> parsed = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<String, hydra.util.Pair<java.util.List<String>, String>>) (seg -> {
        java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
          "}",
          seg);
        hydra.util.Lazy<String> litPart = new hydra.util.Lazy<>(() -> hydra.lib.strings.Intercalate.apply(
          "}",
          hydra.lib.lists.Tail.apply(parts)));
        hydra.util.Lazy<String> pathStr = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(parts));
        java.util.List<String> pathSteps = hydra.lib.strings.SplitOn.apply(
          "/",
          pathStr.get());
        return (hydra.util.Pair<java.util.List<String>, String>) ((hydra.util.Pair<java.util.List<String>, String>) (new hydra.util.Pair<java.util.List<String>, String>(pathSteps, litPart.get())));
      }),
      rest.get()));
    return hydra.util.Either.<T2, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>>>right((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>>) (cx_ -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>) (term -> hydra.pg.termsToElements.TermsToElements.applyPattern(
      cx_,
      firstLit.get(),
      parsed.get(),
      term))));
  }
  
  static <T0, T1, T2, T3, T4, T5> hydra.util.Either<T5, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>>>> parsePropertySpec(T0 cx, T1 g, hydra.pg.mapping.Schema<T2, T3, T4> schema, hydra.pg.mapping.PropertySpec spec) {
    hydra.pg.model.PropertyKey key = (spec).key;
    hydra.pg.mapping.ValueSpec value = (spec).value;
    return hydra.lib.eithers.Bind.apply(
      hydra.pg.termsToElements.TermsToElements.<T0, T1, T5>parseValueSpec(
        cx,
        g,
        value),
      (java.util.function.Function<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>>, hydra.util.Either<T5, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>>>>>) (fun -> hydra.util.Either.<T5, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>>>>right((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>>>) (cx_ -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>>) (term -> hydra.lib.eithers.Bind.apply(
        ((fun).apply(cx_)).apply(term),
        (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>>) (results -> hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (((java.util.function.Function<hydra.util.Coder<hydra.core.Term, T4>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T4>>>>) ((java.util.function.Function<hydra.util.Coder<hydra.core.Term, T4>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T4>>>>) (projected -> projected.encode))).apply(((java.util.function.Function<hydra.pg.mapping.Schema<T2, T3, T4>, hydra.util.Coder<hydra.core.Term, T4>>) ((java.util.function.Function<hydra.pg.mapping.Schema<T2, T3, T4>, hydra.util.Coder<hydra.core.Term, T4>>) ((java.util.function.Function<hydra.pg.mapping.Schema<T2, T3, T4>, hydra.util.Coder<hydra.core.Term, T4>>) (projected -> projected.propertyValues)))).apply(schema))).apply(cx_),
            results),
          (java.util.function.Function<java.util.List<T4>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>>) (values -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>right(hydra.lib.lists.Map.apply(
            (java.util.function.Function<T4, hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>) (v -> (hydra.util.Pair<hydra.pg.model.PropertyKey, T4>) ((hydra.util.Pair<hydra.pg.model.PropertyKey, T4>) (new hydra.util.Pair<hydra.pg.model.PropertyKey, T4>(key, v)))),
            values)))))))))));
  }
  
  static <T0, T1, T2> hydra.util.Either<T2, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>>> parseValueSpec(T0 cx, T1 g, hydra.pg.mapping.ValueSpec spec) {
    return (spec).accept(new hydra.pg.mapping.ValueSpec.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T2, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>>> visit(hydra.pg.mapping.ValueSpec.Value ignored) {
        return hydra.util.Either.<T2, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>>>right((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>>) (_cx -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>) (term -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>right(java.util.List.of(term)))));
      }
      
      @Override
      public hydra.util.Either<T2, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>>> visit(hydra.pg.mapping.ValueSpec.Pattern pat) {
        return hydra.pg.termsToElements.TermsToElements.<T0, T1, T2>parsePattern(
          cx,
          g,
          (pat).value);
      }
    });
  }
  
  static <T0, T1, T2, T3, T4, T5> hydra.util.Either<T5, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>>> parseVertexIdPattern(T0 cx, T1 g, hydra.pg.mapping.Schema<T2, T3, T4> schema, hydra.pg.mapping.ValueSpec spec) {
    return hydra.lib.eithers.Bind.apply(
      hydra.pg.termsToElements.TermsToElements.<T0, T1, T5>parseValueSpec(
        cx,
        g,
        spec),
      (java.util.function.Function<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>>, hydra.util.Either<T5, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>>>>) (fun -> hydra.util.Either.<T5, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>>>right((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>>) (cx_ -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>) (term -> hydra.lib.eithers.Bind.apply(
        ((fun).apply(cx_)).apply(term),
        (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>) (terms -> hydra.lib.eithers.MapList.apply(
          (((java.util.function.Function<hydra.util.Coder<hydra.core.Term, T4>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T4>>>>) ((java.util.function.Function<hydra.util.Coder<hydra.core.Term, T4>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T4>>>>) (projected -> projected.encode))).apply(((java.util.function.Function<hydra.pg.mapping.Schema<T2, T3, T4>, hydra.util.Coder<hydra.core.Term, T4>>) ((java.util.function.Function<hydra.pg.mapping.Schema<T2, T3, T4>, hydra.util.Coder<hydra.core.Term, T4>>) ((java.util.function.Function<hydra.pg.mapping.Schema<T2, T3, T4>, hydra.util.Coder<hydra.core.Term, T4>>) (projected -> projected.vertexIds)))).apply(schema))).apply(cx_),
          terms))))))));
  }
  
  static <T0, T1, T2, T3, T4, T5> hydra.util.Either<T5, hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>> parseVertexSpec(T0 cx, T1 g, hydra.pg.mapping.Schema<T2, T3, T4> schema, hydra.pg.mapping.VertexSpec spec) {
    hydra.pg.mapping.ValueSpec id = (spec).id;
    hydra.pg.model.VertexLabel label = (spec).label;
    java.util.List<hydra.pg.mapping.PropertySpec> props = (spec).properties;
    return hydra.lib.eithers.Bind.apply(
      hydra.pg.termsToElements.TermsToElements.<T0, T1, T2, T3, T4, T5>parseVertexIdPattern(
        cx,
        g,
        schema,
        id),
      (java.util.function.Function<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>>, hydra.util.Either<T5, hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>>>) (getId -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.pg.mapping.PropertySpec, hydra.util.Either<T5, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>>>>>) (v1 -> hydra.pg.termsToElements.TermsToElements.<T0, T1, T2, T3, T4, T5>parsePropertySpec(
            cx,
            g,
            schema,
            v1)),
          props),
        (java.util.function.Function<java.util.List<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>>>>, hydra.util.Either<T5, hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>>>) (getProps -> hydra.util.Either.<T5, hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>>right((hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>) ((hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>) (new hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>>(new hydra.pg.model.Label.Vertex(label), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>>) (cx_ -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>) (term -> hydra.lib.eithers.Bind.apply(
          hydra.pg.termsToElements.TermsToElements.requireUnique(
            cx_,
            "vertex id",
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T4>>>) (v1 -> ((getId).apply(cx_)).apply(v1)),
            term),
          (java.util.function.Function<T4, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>) (tid -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>, java.util.Map<hydra.pg.model.PropertyKey, T4>>) (_xs -> hydra.lib.maps.FromList.apply(_xs)),
              hydra.lib.eithers.MapList.apply(
                (java.util.function.Function<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>) (gf -> hydra.pg.termsToElements.TermsToElements.requireUnique(
                  cx_,
                  "property key",
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.PropertyKey, T4>>>>) (v1 -> ((gf).apply(cx_)).apply(v1)),
                  term)),
                getProps)),
            (java.util.function.Function<java.util.Map<hydra.pg.model.PropertyKey, T4>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>>) (tprops -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T4>>>right(java.util.List.of((hydra.pg.model.Element<T4>) (new hydra.pg.model.Element.Vertex((hydra.pg.model.Vertex<T4>) (new hydra.pg.model.Vertex<T4>(label, tid, tprops))))))))))))))))))));
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T1> readField(hydra.context.Context cx, java.util.Map<hydra.core.Name, T0> fields, hydra.core.Name fname, java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T1>> fun) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T1>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
        "no such field: ",
        (fname).value))), cx))),
      fun,
      hydra.lib.maps.Lookup.apply(
        fname,
        fields));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0> readInjection(hydra.context.Context cx, hydra.graph.Graph g, java.util.List<hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>>> cases, hydra.core.Term encoded) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.map(
        cx,
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Name>>) (k -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<String, hydra.core.Name>) (_n -> new hydra.core.Name(_n)),
          hydra.extract.core.Core.string(
            cx,
            g,
            k))),
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (_v -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(_v)),
        g,
        encoded),
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>) (mp -> {
        hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>> entries = new hydra.util.Lazy<>(() -> hydra.lib.maps.ToList.apply(mp));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(entries.get()),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T0>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError("empty injection")), cx))),
          () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>) (() -> {
            hydra.util.Lazy<hydra.util.Pair<hydra.core.Name, hydra.core.Term>> f = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(entries.get()));
            hydra.util.Lazy<hydra.core.Name> key = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(f.get()));
            hydra.util.Lazy<hydra.core.Term> val = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(f.get()));
            return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>) (() -> {
              hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>>>> matching = new hydra.util.Lazy<>(() -> hydra.pg.termsToElements.TermsToElements.<T0>readInjection_matching(
                cases,
                key.get()));
              return hydra.lib.logic.IfElse.lazy(
                hydra.lib.lists.Null.apply(matching.get()),
                () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T0>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
                  "unexpected field: ",
                  (key.get()).value))), cx))),
                () -> (hydra.lib.pairs.Second.apply(hydra.lib.lists.Head.apply(matching.get()))).apply(val.get()));
            })).get();
          })).get());
      }));
  }
  
  static <T0> java.util.List<hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>>> readInjection_matching(java.util.List<hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>>> cases, hydra.core.Name key) {
    return hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>>, Boolean>) (c -> hydra.lib.equality.Equal.apply(
        hydra.lib.pairs.First.apply(c),
        key)),
      cases);
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0> readRecord(hydra.context.Context cx, hydra.graph.Graph g, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>> cons, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.map(
        cx,
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Name>>) (k -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<String, hydra.core.Name>) (_n -> new hydra.core.Name(_n)),
          hydra.extract.core.Core.string(
            cx,
            g,
            k))),
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (_v -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(_v)),
        g,
        term),
      cons);
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T1> requireUnique(hydra.context.Context cx, String context, java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T1>>> fun, T0 term) {
    return hydra.lib.eithers.Bind.apply(
      (fun).apply(term),
      (java.util.function.Function<java.util.List<T1>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T1>>) (results -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(results),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T1>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          "No value found: ",
          context))), cx))),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply(results),
            1),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T1>right(hydra.lib.lists.Head.apply(results)),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T1>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            "Multiple values found: ",
            context))), cx)))))));
  }
  
  static <T0, T1, T2> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, java.util.List<hydra.pg.model.Label>, hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>> termToElementsAdapter(hydra.context.Context cx, hydra.graph.Graph g, hydra.pg.mapping.Schema<T0, T1, T2> schema, hydra.core.Type typ) {
    hydra.core.Name key_elements = new hydra.core.Name("elements");
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, java.util.List<hydra.pg.model.Label>, hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>>right((hydra.util.Adapter<hydra.core.Type, java.util.List<hydra.pg.model.Label>, hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>) ((hydra.util.Adapter<hydra.core.Type, java.util.List<hydra.pg.model.Label>, hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>) ((hydra.util.Adapter<hydra.core.Type, java.util.List<hydra.pg.model.Label>, hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>) ((hydra.util.Adapter<hydra.core.Type, java.util.List<hydra.pg.model.Label>, hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>) (new hydra.util.Adapter<hydra.core.Type, java.util.List<hydra.pg.model.Label>, hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>(false, typ, (java.util.List<hydra.pg.model.Label>) (java.util.List.<hydra.pg.model.Label>of()), (hydra.util.Coder<hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>) ((hydra.util.Coder<hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>) (new hydra.util.Coder<hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T2>>>>>) (_cx -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T2>>>>) (_t -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T2>>>right((java.util.List<hydra.pg.model.Element<T2>>) (java.util.List.<hydra.pg.model.Element<T2>>of())))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<java.util.List<hydra.pg.model.Element<T2>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>) (cx_ -> (java.util.function.Function<java.util.List<hydra.pg.model.Element<T2>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (_els -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError("no corresponding element type")), cx_)))))))))))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, java.util.List<hydra.pg.model.Label>, hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>>>) (term -> hydra.lib.eithers.Bind.apply(
        hydra.pg.termsToElements.TermsToElements.expectList(
          cx,
          g,
          (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ElementSpec>>>>) (p0 -> p1 -> p2 -> hydra.pg.termsToElements.TermsToElements.decodeElementSpec(
            p0,
            p1,
            p2)),
          term),
        (java.util.function.Function<java.util.List<hydra.pg.mapping.ElementSpec>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, java.util.List<hydra.pg.model.Label>, hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>>>) (specTerms -> hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.pg.mapping.ElementSpec, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T2>>>>>>>>) (v1 -> hydra.pg.termsToElements.TermsToElements.parseElementSpec(
              cx,
              g,
              schema,
              v1)),
            specTerms),
          (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T2>>>>>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, java.util.List<hydra.pg.model.Label>, hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>>>) (specs -> {
            hydra.util.Lazy<java.util.List<hydra.pg.model.Label>> labels = new hydra.util.Lazy<>(() -> hydra.lib.lists.Nub.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T2>>>>>>, hydra.pg.model.Label>) (_p -> hydra.lib.pairs.First.apply(_p)),
              specs)));
            return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, java.util.List<hydra.pg.model.Label>, hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>>right((hydra.util.Adapter<hydra.core.Type, java.util.List<hydra.pg.model.Label>, hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>) ((hydra.util.Adapter<hydra.core.Type, java.util.List<hydra.pg.model.Label>, hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>) ((hydra.util.Adapter<hydra.core.Type, java.util.List<hydra.pg.model.Label>, hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>) ((hydra.util.Adapter<hydra.core.Type, java.util.List<hydra.pg.model.Label>, hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>) (new hydra.util.Adapter<hydra.core.Type, java.util.List<hydra.pg.model.Label>, hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>(false, typ, labels.get(), (hydra.util.Coder<hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>) ((hydra.util.Coder<hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>) (new hydra.util.Coder<hydra.core.Term, java.util.List<hydra.pg.model.Element<T2>>>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T2>>>>>) (cx_ -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T2>>>>) (t -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<java.util.List<hydra.pg.model.Element<T2>>>, java.util.List<hydra.pg.model.Element<T2>>>) (_xs -> hydra.lib.lists.Concat.apply(_xs)),
              hydra.lib.eithers.MapList.apply(
                (java.util.function.Function<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T2>>>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T2>>>>) (e -> ((e).apply(cx_)).apply(t)),
                hydra.pg.termsToElements.TermsToElements.<T2>termToElementsAdapter_encoders(specs))))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<java.util.List<hydra.pg.model.Element<T2>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>) (cx_ -> (java.util.function.Function<java.util.List<hydra.pg.model.Element<T2>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (_els -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError("element decoding is not yet supported")), cx_))))))))))))));
          }))))),
      hydra.annotations.Annotations.getTypeAnnotation(
        key_elements,
        typ));
  }
  
  static <T2> java.util.List<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T2>>>>>> termToElementsAdapter_encoders(java.util.List<hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T2>>>>>>> specs) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.pg.model.Label, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T2>>>>>>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.Element<T2>>>>>>) (_p -> hydra.lib.pairs.Second.apply(_p)),
      specs);
  }
  
  static String termToString(hydra.core.Term term) {
    return (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public String otherwise(hydra.core.Term instance) {
        return hydra.show.core.Core.term(term);
      }
      
      @Override
      public String visit(hydra.core.Term.Literal lit) {
        return ((lit).value).accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public String otherwise(hydra.core.Literal instance) {
            return hydra.show.core.Core.term(term);
          }
          
          @Override
          public String visit(hydra.core.Literal.String_ s) {
            return (s).value;
          }
          
          @Override
          public String visit(hydra.core.Literal.Boolean_ b) {
            return hydra.lib.logic.IfElse.lazy(
              (b).value,
              () -> "true",
              () -> "false");
          }
          
          @Override
          public String visit(hydra.core.Literal.Integer_ i) {
            return ((i).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
              @Override
              public String otherwise(hydra.core.IntegerValue instance) {
                return hydra.show.core.Core.term(term);
              }
              
              @Override
              public String visit(hydra.core.IntegerValue.Int32 n) {
                return hydra.lib.literals.ShowInt32.apply((n).value);
              }
            });
          }
          
          @Override
          public String visit(hydra.core.Literal.Float_ f) {
            return ((f).value).accept(new hydra.core.FloatValue.PartialVisitor<>() {
              @Override
              public String otherwise(hydra.core.FloatValue instance) {
                return hydra.show.core.Core.term(term);
              }
              
              @Override
              public String visit(hydra.core.FloatValue.Float64 n) {
                return hydra.lib.literals.ShowFloat64.apply((n).value);
              }
            });
          }
        });
      }
      
      @Override
      public String visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> "nothing",
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.pg.termsToElements.TermsToElements.termToString(t)),
          (mt).value);
      }
    });
  }
}
