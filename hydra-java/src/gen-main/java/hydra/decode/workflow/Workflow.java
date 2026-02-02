// Note: this is an automatically generated file. Do not edit.

package hydra.decode.workflow;

/**
 * Term decoders for hydra.workflow
 */
public interface Workflow {
  static hydra.util.Either<hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec> hydraSchemaSpec(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec>) ((hydra.util.Either<hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec>) (hydra.util.Either.<hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec>) ((hydra.util.Either<hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec>) (hydra.util.Either.<hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec>left(new hydra.util.DecodingError("expected record of type hydra.workflow.HydraSchemaSpec"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap(((record)).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "modules",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.module.Module>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.module.Module>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.module.Module>>>) (p0 -> p1 -> hydra.decode.module.Module.module(
                  (p0),
                  (p1))),
                (v1),
                (v2)))),
              (fieldMap),
              (cx)),
            (java.util.function.Function<java.util.List<hydra.module.Module>, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec>>) (field_modules -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "typeName",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                  (p0),
                  (p1))),
                (fieldMap),
                (cx)),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec>>) (field_typeName -> (hydra.util.Either<hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec>) ((hydra.util.Either<hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec>) (hydra.util.Either.<hydra.util.DecodingError, hydra.workflow.HydraSchemaSpec>right(new hydra.workflow.HydraSchemaSpec((field_modules), (field_typeName)))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec> schemaSpec(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>) ((hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>) (hydra.util.Either.<hydra.util.DecodingError, hydra.workflow.SchemaSpec>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>) ((hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>) (hydra.util.Either.<hydra.util.DecodingError, hydra.workflow.SchemaSpec>left(new hydra.util.DecodingError("expected union of type hydra.workflow.SchemaSpec"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (((inj)).value).field;
          hydra.core.Name fname = ((field)).name;
          hydra.core.Term fterm = ((field)).term;
          hydra.core.Name tname = (((inj)).value).typeName;
          java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>> variantMap = hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>>(new hydra.core.Name("hydra"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.workflow.HydraSchemaSpec, hydra.workflow.SchemaSpec>) (t -> new hydra.workflow.SchemaSpec.Hydra((t))),
              hydra.decode.workflow.Workflow.hydraSchemaSpec(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>>(new hydra.core.Name("file"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<String, hydra.workflow.SchemaSpec>) (t -> new hydra.workflow.SchemaSpec.File((t))),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError((err)))))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                    return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                  }
                  
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                    return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                        return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right(((s)).value)));
                      }
                    });
                  }
                })),
                hydra.lexical.Lexical.stripAndDereferenceTermEither(
                  (cx),
                  (input)))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>>(new hydra.core.Name("provided"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.workflow.SchemaSpec>) (t -> new hydra.workflow.SchemaSpec.Provided((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>) ((hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>) (hydra.util.Either.<hydra.util.DecodingError, hydra.workflow.SchemaSpec>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              ((fname)).value,
              " in union type ",
              ((tname)).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>) (f -> ((f)).apply((fterm))),
            hydra.lib.maps.Lookup.apply(
              (fname),
              (variantMap)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.workflow.TransformWorkflow> transformWorkflow(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.TransformWorkflow>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.workflow.TransformWorkflow>) ((hydra.util.Either<hydra.util.DecodingError, hydra.workflow.TransformWorkflow>) (hydra.util.Either.<hydra.util.DecodingError, hydra.workflow.TransformWorkflow>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.TransformWorkflow>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.workflow.TransformWorkflow> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.workflow.TransformWorkflow>) ((hydra.util.Either<hydra.util.DecodingError, hydra.workflow.TransformWorkflow>) (hydra.util.Either.<hydra.util.DecodingError, hydra.workflow.TransformWorkflow>left(new hydra.util.DecodingError("expected record of type hydra.workflow.TransformWorkflow"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.workflow.TransformWorkflow> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap(((record)).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError((err)))))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                    return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                  }
                  
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                    return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                        return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right(((s)).value)));
                      }
                    });
                  }
                })),
                hydra.lexical.Lexical.stripAndDereferenceTermEither(
                  (cx2),
                  (raw2))))),
              (fieldMap),
              (cx)),
            (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.TransformWorkflow>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "schemaSpec",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.SchemaSpec>>>) (p0 -> p1 -> hydra.decode.workflow.Workflow.schemaSpec(
                  (p0),
                  (p1))),
                (fieldMap),
                (cx)),
              (java.util.function.Function<hydra.workflow.SchemaSpec, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.TransformWorkflow>>) (field_schemaSpec -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "srcDir",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
                    (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError((err)))))),
                    (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                        return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                            return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                            return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right(((s)).value)));
                          }
                        });
                      }
                    })),
                    hydra.lexical.Lexical.stripAndDereferenceTermEither(
                      (cx2),
                      (raw2))))),
                  (fieldMap),
                  (cx)),
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.TransformWorkflow>>) (field_srcDir -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.helpers.Helpers.requireField(
                    "destDir",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
                      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError((err)))))),
                      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                          return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                        }
                        
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                          return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                            @Override
                            public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                              return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                            }
                            
                            @Override
                            public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                              return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right(((s)).value)));
                            }
                          });
                        }
                      })),
                      hydra.lexical.Lexical.stripAndDereferenceTermEither(
                        (cx2),
                        (raw2))))),
                    (fieldMap),
                    (cx)),
                  (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.workflow.TransformWorkflow>>) (field_destDir -> (hydra.util.Either<hydra.util.DecodingError, hydra.workflow.TransformWorkflow>) ((hydra.util.Either<hydra.util.DecodingError, hydra.workflow.TransformWorkflow>) (hydra.util.Either.<hydra.util.DecodingError, hydra.workflow.TransformWorkflow>right(new hydra.workflow.TransformWorkflow((field_name), (field_schemaSpec), (field_srcDir), (field_destDir)))))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
}
