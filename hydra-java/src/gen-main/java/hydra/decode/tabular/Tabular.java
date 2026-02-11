// Note: this is an automatically generated file. Do not edit.

package hydra.decode.tabular;

/**
 * Term decoders for hydra.tabular
 */
public interface Tabular {
  static hydra.util.Either<hydra.util.DecodingError, hydra.tabular.ColumnType> columnType(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.ColumnType>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.tabular.ColumnType>) ((hydra.util.Either<hydra.util.DecodingError, hydra.tabular.ColumnType>) (hydra.util.Either.<hydra.util.DecodingError, hydra.tabular.ColumnType>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.ColumnType>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.tabular.ColumnType> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.tabular.ColumnType>) ((hydra.util.Either<hydra.util.DecodingError, hydra.tabular.ColumnType>) (hydra.util.Either.<hydra.util.DecodingError, hydra.tabular.ColumnType>left(new hydra.util.DecodingError("expected record of type hydra.tabular.ColumnType"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.tabular.ColumnType> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.relational.ColumnName>>>) (p0 -> p1 -> hydra.decode.relational.Relational.columnName(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.relational.ColumnName, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.ColumnType>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "type",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.ColumnType>>) (field_type -> (hydra.util.Either<hydra.util.DecodingError, hydra.tabular.ColumnType>) ((hydra.util.Either<hydra.util.DecodingError, hydra.tabular.ColumnType>) (hydra.util.Either.<hydra.util.DecodingError, hydra.tabular.ColumnType>right(new hydra.tabular.ColumnType(field_name, field_type))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static <T0> hydra.util.Either<hydra.util.DecodingError, hydra.tabular.DataRow<T0>> dataRow(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>> v, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.DataRow<T0>>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.tabular.DataRow<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.tabular.DataRow<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.tabular.DataRow<T0>>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.DataRow<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.tabular.DataRow<T0>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.tabular.DataRow<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.tabular.DataRow<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.tabular.DataRow<T0>>left(new hydra.util.DecodingError("expected wrapped type hydra.tabular.DataRow"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.tabular.DataRow<T0>> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.List<hydra.util.Maybe<T0>>, hydra.tabular.DataRow<T0>>) (b -> (hydra.tabular.DataRow<T0>) (new hydra.tabular.DataRow(b))),
            hydra.extract.helpers.Helpers.<hydra.util.Maybe<T0>>decodeList(
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<T0>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<T0>>>) (v2 -> hydra.extract.helpers.Helpers.<T0>decodeMaybe(
                v,
                v1,
                v2))),
              cx,
              ((wrappedTerm).value).body));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.tabular.HeaderRow> headerRow(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.HeaderRow>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.tabular.HeaderRow>) ((hydra.util.Either<hydra.util.DecodingError, hydra.tabular.HeaderRow>) (hydra.util.Either.<hydra.util.DecodingError, hydra.tabular.HeaderRow>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.HeaderRow>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.tabular.HeaderRow> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.tabular.HeaderRow>) ((hydra.util.Either<hydra.util.DecodingError, hydra.tabular.HeaderRow>) (hydra.util.Either.<hydra.util.DecodingError, hydra.tabular.HeaderRow>left(new hydra.util.DecodingError("expected wrapped type hydra.tabular.HeaderRow"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.tabular.HeaderRow> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.List<String>, hydra.tabular.HeaderRow>) (b -> new hydra.tabular.HeaderRow(b)),
            hydra.extract.helpers.Helpers.decodeList(
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
              cx,
              ((wrappedTerm).value).body));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static <T0> hydra.util.Either<hydra.util.DecodingError, hydra.tabular.Table<T0>> table(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>> v, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.Table<T0>>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.tabular.Table<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.tabular.Table<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.tabular.Table<T0>>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.Table<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.tabular.Table<T0>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.tabular.Table<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.tabular.Table<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.tabular.Table<T0>>left(new hydra.util.DecodingError("expected record of type hydra.tabular.Table"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.tabular.Table<T0>> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "header",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<hydra.tabular.HeaderRow>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<hydra.tabular.HeaderRow>>>) (v2 -> hydra.extract.helpers.Helpers.decodeMaybe(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.HeaderRow>>>) (p0 -> p1 -> hydra.decode.tabular.Tabular.headerRow(
                  p0,
                  p1)),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.util.Maybe<hydra.tabular.HeaderRow>, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.Table<T0>>>) (field_header -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "data",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.tabular.DataRow<T0>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.tabular.DataRow<T0>>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.DataRow<T0>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.DataRow<T0>>>) (v22 -> hydra.decode.tabular.Tabular.<T0>dataRow(
                    v,
                    v12,
                    v22))),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<java.util.List<hydra.tabular.DataRow<T0>>, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.Table<T0>>>) (field_data -> (hydra.util.Either<hydra.util.DecodingError, hydra.tabular.Table<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.tabular.Table<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.tabular.Table<T0>>right((hydra.tabular.Table<T0>) (new hydra.tabular.Table<T0>(field_header, field_data)))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.tabular.TableType> tableType(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.TableType>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.tabular.TableType>) ((hydra.util.Either<hydra.util.DecodingError, hydra.tabular.TableType>) (hydra.util.Either.<hydra.util.DecodingError, hydra.tabular.TableType>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.TableType>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.tabular.TableType> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.tabular.TableType>) ((hydra.util.Either<hydra.util.DecodingError, hydra.tabular.TableType>) (hydra.util.Either.<hydra.util.DecodingError, hydra.tabular.TableType>left(new hydra.util.DecodingError("expected record of type hydra.tabular.TableType"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.tabular.TableType> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.relational.RelationName>>>) (p0 -> p1 -> hydra.decode.relational.Relational.relationName(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.relational.RelationName, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.TableType>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "columns",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.tabular.ColumnType>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.tabular.ColumnType>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.ColumnType>>>) (p0 -> p1 -> hydra.decode.tabular.Tabular.columnType(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<java.util.List<hydra.tabular.ColumnType>, hydra.util.Either<hydra.util.DecodingError, hydra.tabular.TableType>>) (field_columns -> (hydra.util.Either<hydra.util.DecodingError, hydra.tabular.TableType>) ((hydra.util.Either<hydra.util.DecodingError, hydra.tabular.TableType>) (hydra.util.Either.<hydra.util.DecodingError, hydra.tabular.TableType>right(new hydra.tabular.TableType(field_name, field_columns))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
