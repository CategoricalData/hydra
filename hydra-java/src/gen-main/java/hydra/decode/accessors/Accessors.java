// Note: this is an automatically generated file. Do not edit.

package hydra.decode.accessors;

/**
 * Term decoders for hydra.accessors
 */
public interface Accessors {
  static hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorEdge> accessorEdge(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorEdge>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorEdge>) ((hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorEdge>) (hydra.util.Either.<hydra.util.DecodingError, hydra.accessors.AccessorEdge>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorEdge>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorEdge> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorEdge>) ((hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorEdge>) (hydra.util.Either.<hydra.util.DecodingError, hydra.accessors.AccessorEdge>left(new hydra.util.DecodingError("expected record of type hydra.accessors.AccessorEdge"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorEdge> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "source",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode>>>) (p0 -> p1 -> hydra.decode.accessors.Accessors.accessorNode(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.accessors.AccessorNode, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorEdge>>) (field_source -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "path",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorPath>>>) (p0 -> p1 -> hydra.decode.accessors.Accessors.accessorPath(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.accessors.AccessorPath, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorEdge>>) (field_path -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "target",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode>>>) (p0 -> p1 -> hydra.decode.accessors.Accessors.accessorNode(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.accessors.AccessorNode, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorEdge>>) (field_target -> (hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorEdge>) ((hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorEdge>) (hydra.util.Either.<hydra.util.DecodingError, hydra.accessors.AccessorEdge>right(new hydra.accessors.AccessorEdge(field_source, field_path, field_target))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorGraph> accessorGraph(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorGraph>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorGraph>) ((hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorGraph>) (hydra.util.Either.<hydra.util.DecodingError, hydra.accessors.AccessorGraph>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorGraph>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorGraph> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorGraph>) ((hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorGraph>) (hydra.util.Either.<hydra.util.DecodingError, hydra.accessors.AccessorGraph>left(new hydra.util.DecodingError("expected record of type hydra.accessors.AccessorGraph"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorGraph> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "nodes",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.accessors.AccessorNode>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.accessors.AccessorNode>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode>>>) (p0 -> p1 -> hydra.decode.accessors.Accessors.accessorNode(
                  p0,
                  p1)),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<java.util.List<hydra.accessors.AccessorNode>, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorGraph>>) (field_nodes -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "edges",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.accessors.AccessorEdge>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.accessors.AccessorEdge>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorEdge>>>) (p0 -> p1 -> hydra.decode.accessors.Accessors.accessorEdge(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<java.util.List<hydra.accessors.AccessorEdge>, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorGraph>>) (field_edges -> (hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorGraph>) ((hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorGraph>) (hydra.util.Either.<hydra.util.DecodingError, hydra.accessors.AccessorGraph>right(new hydra.accessors.AccessorGraph(field_nodes, field_edges))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode> accessorNode(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode>) ((hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode>) (hydra.util.Either.<hydra.util.DecodingError, hydra.accessors.AccessorNode>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode>) ((hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode>) (hydra.util.Either.<hydra.util.DecodingError, hydra.accessors.AccessorNode>left(new hydra.util.DecodingError("expected record of type hydra.accessors.AccessorNode"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "label",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
                  (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError(err))))),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                    }
                    
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                      return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                          return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                        }
                        
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                          return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right((s).value)));
                        }
                      });
                    }
                  })),
                  hydra.lexical.Lexical.stripAndDereferenceTermEither(
                    cx2,
                    raw2)))),
                fieldMap,
                cx),
              (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode>>) (field_label -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "id",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
                    (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError(err))))),
                    (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                        return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                            return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                            return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right((s).value)));
                          }
                        });
                      }
                    })),
                    hydra.lexical.Lexical.stripAndDereferenceTermEither(
                      cx2,
                      raw2)))),
                  fieldMap,
                  cx),
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode>>) (field_id -> (hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode>) ((hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorNode>) (hydra.util.Either.<hydra.util.DecodingError, hydra.accessors.AccessorNode>right(new hydra.accessors.AccessorNode(field_name, field_label, field_id))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorPath> accessorPath(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorPath>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorPath>) ((hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorPath>) (hydra.util.Either.<hydra.util.DecodingError, hydra.accessors.AccessorPath>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorPath>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorPath> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorPath>) ((hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorPath>) (hydra.util.Either.<hydra.util.DecodingError, hydra.accessors.AccessorPath>left(new hydra.util.DecodingError("expected wrapped type hydra.accessors.AccessorPath"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.accessors.AccessorPath> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, hydra.accessors.AccessorPath>) (b -> new hydra.accessors.AccessorPath(b)),
            hydra.extract.helpers.Helpers.decodeList(
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (p0 -> p1 -> hydra.decode.accessors.Accessors.termAccessor(
                p0,
                p1)),
              cx,
              ((wrappedTerm).value).body));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor> termAccessor(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>) ((hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>) (hydra.util.Either.<hydra.util.DecodingError, hydra.accessors.TermAccessor>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>) ((hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>) (hydra.util.Either.<hydra.util.DecodingError, hydra.accessors.TermAccessor>left(new hydra.util.DecodingError("expected union of type hydra.accessors.TermAccessor"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = ((inj).value).field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.core.Name tname = ((inj).value).typeName;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("annotatedBody"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.AnnotatedBody()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("applicationFunction"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.ApplicationFunction()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("applicationArgument"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.ApplicationArgument()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("lambdaBody"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.LambdaBody()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("unionCasesDefault"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.UnionCasesDefault()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("unionCasesBranch"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.UnionCasesBranch(t)),
              hydra.decode.core.Core.name(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("letBody"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.LetBody()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("letBinding"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.LetBinding(t)),
              hydra.decode.core.Core.name(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("listElement"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Integer, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.ListElement(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Integer>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError(err))))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                    return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected literal"))));
                  }
                  
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                    return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v1) {
                        return ((v1).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                            return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 value"))));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                            return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>right((i).value)));
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.lexical.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("mapKey"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Integer, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.MapKey(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Integer>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError(err))))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                    return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected literal"))));
                  }
                  
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                    return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v1) {
                        return ((v1).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                            return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 value"))));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                            return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>right((i).value)));
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.lexical.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("mapValue"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Integer, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.MapValue(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Integer>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError(err))))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                    return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected literal"))));
                  }
                  
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                    return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v1) {
                        return ((v1).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                            return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 value"))));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                            return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>right((i).value)));
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.lexical.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("maybeTerm"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.MaybeTerm()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("productTerm"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Integer, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.ProductTerm(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Integer>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError(err))))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                    return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected literal"))));
                  }
                  
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                    return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v1) {
                        return ((v1).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                            return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 value"))));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                            return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>right((i).value)));
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.lexical.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("recordField"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.RecordField(t)),
              hydra.decode.core.Core.name(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("setElement"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Integer, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.SetElement(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Integer>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError(err))))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                    return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected literal"))));
                  }
                  
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                    return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v1) {
                        return ((v1).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                            return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 value"))));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                            return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>right((i).value)));
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.lexical.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("sumTerm"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.SumTerm()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("typeLambdaBody"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.TypeLambdaBody()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("typeApplicationTerm"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.TypeApplicationTerm()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("injectionTerm"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.InjectionTerm()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>>(new hydra.core.Name("wrappedTerm"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.accessors.TermAccessor>) (t -> new hydra.accessors.TermAccessor.WrappedTerm()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>) ((hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>) (hydra.util.Either.<hydra.util.DecodingError, hydra.accessors.TermAccessor>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              (fname).value,
              " in union type ",
              (tname).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>, hydra.util.Either<hydra.util.DecodingError, hydra.accessors.TermAccessor>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
