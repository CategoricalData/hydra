// Note: this is an automatically generated file. Do not edit.

package hydra.decode;

/**
 * Term decoders for hydra.relational
 */
public interface Relational {
  static hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnName> columnName(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnName>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.ColumnName>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnName>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnName> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.ColumnName>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnName> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.relational.ColumnName>) (b -> new hydra.relational.ColumnName(b)),
            hydra.lib.eithers.Either.apply(
              (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, String>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError(err))),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.errors.DecodingError, String> otherwise(hydra.core.Term instance) {
                  return hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError("expected literal"));
                }

                @Override
                public hydra.util.Either<hydra.errors.DecodingError, String> visit(hydra.core.Term.Literal v) {
                  return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.errors.DecodingError, String> otherwise(hydra.core.Literal instance) {
                      return hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError("expected string literal"));
                    }

                    @Override
                    public hydra.util.Either<hydra.errors.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                      return hydra.util.Either.<hydra.errors.DecodingError, String>right((s).value);
                    }
                  });
                }
              })),
              hydra.Lexical.stripAndDereferenceTermEither(
                cx,
                (wrappedTerm).value.body)));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnSchema<T0>> columnSchema(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> t, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnSchema<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.ColumnSchema<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnSchema<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnSchema<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.ColumnSchema<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnSchema<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnName>>>) (p0 -> p1 -> hydra.decode.Relational.columnName(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.relational.ColumnName, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnSchema<T0>>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "domain",
                t,
                fieldMap,
                cx),
              (java.util.function.Function<T0, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnSchema<T0>>>) (field_domain -> hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.ColumnSchema<T0>>right((hydra.relational.ColumnSchema<T0>) (new hydra.relational.ColumnSchema<T0>(field_name, field_domain)))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ForeignKey> foreignKey(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ForeignKey>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.ForeignKey>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ForeignKey>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ForeignKey> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.ForeignKey>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ForeignKey> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "foreignRelation",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.RelationName>>>) (p0 -> p1 -> hydra.decode.Relational.relationName(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.relational.RelationName, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ForeignKey>>) (field_foreignRelation -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "keys",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.relational.ColumnName, hydra.relational.ColumnName>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.relational.ColumnName, hydra.relational.ColumnName>>>) (v2 -> hydra.extract.Core.decodeMap(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnName>>>) (p0 -> p1 -> hydra.decode.Relational.columnName(
                    p0,
                    p1)),
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnName>>>) (p0 -> p1 -> hydra.decode.Relational.columnName(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.PersistentMap<hydra.relational.ColumnName, hydra.relational.ColumnName>, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ForeignKey>>) (field_keys -> hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.ForeignKey>right(new hydra.relational.ForeignKey(field_foreignRelation, field_keys))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.relational.PrimaryKey> primaryKey(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.PrimaryKey>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.PrimaryKey>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.PrimaryKey>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.PrimaryKey> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.PrimaryKey>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.PrimaryKey> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.util.ConsList<hydra.relational.ColumnName>, hydra.relational.PrimaryKey>) (b -> new hydra.relational.PrimaryKey(b)),
            hydra.extract.Core.decodeList(
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnName>>>) (p0 -> p1 -> hydra.decode.Relational.columnName(
                p0,
                p1)),
              cx,
              (wrappedTerm).value.body));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Relation<T0>> relation(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> v, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Relation<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.Relation<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Relation<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Relation<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.Relation<T0>>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Relation<T0>> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.util.ConsList<hydra.relational.Row<T0>>, hydra.relational.Relation<T0>>) (b -> (hydra.relational.Relation<T0>) (new hydra.relational.Relation(b))),
            hydra.extract.Core.decodeList(
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Row<T0>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Row<T0>>>) (v2 -> hydra.decode.Relational.<T0>row(
                v,
                v1,
                v2))),
              cx,
              (wrappedTerm).value.body));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.relational.RelationName> relationName(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.RelationName>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.RelationName>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.RelationName>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.RelationName> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.RelationName>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.RelationName> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.relational.RelationName>) (b -> new hydra.relational.RelationName(b)),
            hydra.lib.eithers.Either.apply(
              (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, String>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError(err))),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.errors.DecodingError, String> otherwise(hydra.core.Term instance) {
                  return hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError("expected literal"));
                }

                @Override
                public hydra.util.Either<hydra.errors.DecodingError, String> visit(hydra.core.Term.Literal v) {
                  return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.errors.DecodingError, String> otherwise(hydra.core.Literal instance) {
                      return hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError("expected string literal"));
                    }

                    @Override
                    public hydra.util.Either<hydra.errors.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                      return hydra.util.Either.<hydra.errors.DecodingError, String>right((s).value);
                    }
                  });
                }
              })),
              hydra.Lexical.stripAndDereferenceTermEither(
                cx,
                (wrappedTerm).value.body)));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.relational.RelationSchema<T0>> relationSchema(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> t, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.RelationSchema<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.RelationSchema<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.RelationSchema<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.RelationSchema<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.RelationSchema<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.RelationSchema<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.RelationName>>>) (p0 -> p1 -> hydra.decode.Relational.relationName(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.relational.RelationName, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.RelationSchema<T0>>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "columns",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.relational.ColumnSchema<T0>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.relational.ColumnSchema<T0>>>>) (v2 -> hydra.extract.Core.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnSchema<T0>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnSchema<T0>>>) (v22 -> hydra.decode.Relational.<T0>columnSchema(
                    t,
                    v12,
                    v22))),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.ConsList<hydra.relational.ColumnSchema<T0>>, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.RelationSchema<T0>>>) (field_columns -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "primaryKeys",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.relational.PrimaryKey>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.relational.PrimaryKey>>>) (v2 -> hydra.extract.Core.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.PrimaryKey>>>) (p0 -> p1 -> hydra.decode.Relational.primaryKey(
                      p0,
                      p1)),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.util.ConsList<hydra.relational.PrimaryKey>, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.RelationSchema<T0>>>) (field_primaryKeys -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.Core.requireField(
                    "foreignKeys",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.relational.ForeignKey>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.relational.ForeignKey>>>) (v2 -> hydra.extract.Core.decodeList(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ForeignKey>>>) (p0 -> p1 -> hydra.decode.Relational.foreignKey(
                        p0,
                        p1)),
                      v1,
                      v2))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<hydra.util.ConsList<hydra.relational.ForeignKey>, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.RelationSchema<T0>>>) (field_foreignKeys -> hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.RelationSchema<T0>>right((hydra.relational.RelationSchema<T0>) (new hydra.relational.RelationSchema<T0>(field_name, field_columns, field_primaryKeys, field_foreignKeys)))))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Relationship<T0>> relationship(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> v, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Relationship<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.Relationship<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Relationship<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Relationship<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.Relationship<T0>>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Relationship<T0>> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.util.PersistentSet<hydra.util.PersistentMap<hydra.relational.ColumnName, T0>>, hydra.relational.Relationship<T0>>) (b -> (hydra.relational.Relationship<T0>) (new hydra.relational.Relationship(b))),
            hydra.extract.Core.decodeSet(
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.relational.ColumnName, T0>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.relational.ColumnName, T0>>>) (v2 -> hydra.extract.Core.decodeMap(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.ColumnName>>>) (p0 -> p1 -> hydra.decode.Relational.columnName(
                  p0,
                  p1)),
                v,
                v1,
                v2))),
              cx,
              (wrappedTerm).value.body));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Row<T0>> row(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> v, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Row<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.Row<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Row<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Row<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.relational.Row<T0>>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.relational.Row<T0>> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.util.ConsList<T0>, hydra.relational.Row<T0>>) (b -> (hydra.relational.Row<T0>) (new hydra.relational.Row(b))),
            hydra.extract.Core.<T0>decodeList(
              v,
              cx,
              (wrappedTerm).value.body));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
