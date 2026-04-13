// Note: this is an automatically generated file. Do not edit.

package hydra.demos.genpg;

/**
 * Functions for transforming property graph mappings into property graph elements.
 */
public interface Transform {
  static <T0, T1> hydra.util.Pair<java.util.List<T0>, java.util.List<T1>> concatPairs(hydra.util.Pair<java.util.List<T0>, java.util.List<T1>> acc, hydra.util.Pair<java.util.List<T0>, java.util.List<T1>> p) {
    return (hydra.util.Pair<java.util.List<T0>, java.util.List<T1>>) ((hydra.util.Pair<java.util.List<T0>, java.util.List<T1>>) (new hydra.util.Pair<java.util.List<T0>, java.util.List<T1>>(hydra.lib.lists.Concat2.apply(
      hydra.lib.pairs.First.apply(acc),
      hydra.lib.pairs.First.apply(p)), hydra.lib.lists.Concat2.apply(
      hydra.lib.pairs.Second.apply(acc),
      hydra.lib.pairs.Second.apply(p)))));
  }

  static hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>> decodeCell(hydra.tabular.ColumnType colType, hydra.util.Maybe<String> mvalue) {
    String cname = (colType).name.value;
    hydra.core.Type typ = (colType).type;
    java.util.function.Function<String, hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>>> decodeValue = (java.util.function.Function<String, hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>>>) (value -> {
      String parseError = hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        "Invalid value for column ",
        cname,
        ": ",
        value));
      return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>> otherwise(hydra.core.Type instance) {
          return hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>left(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "Unsupported type for column ",
            cname)));
        }

        @Override
        public hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>> visit(hydra.core.Type.Literal lt) {
          return (lt).value.accept(new hydra.core.LiteralType.PartialVisitor<>() {
            @Override
            public hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>> otherwise(hydra.core.LiteralType instance) {
              return hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>left(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                "Unsupported literal type for column ",
                cname)));
            }

            @Override
            public hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>> visit(hydra.core.LiteralType.Boolean_ ignored) {
              return hydra.lib.maybes.Maybe.applyLazy(
                () -> hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>left(parseError),
                (java.util.function.Function<Boolean, hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>>>) (parsed -> hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>right(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(parsed))))),
                hydra.lib.literals.ReadBoolean.apply(value));
            }

            @Override
            public hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>> visit(hydra.core.LiteralType.Float_ ft) {
              return (ft).value.accept(new hydra.core.FloatType.PartialVisitor<>() {
                @Override
                public hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>> otherwise(hydra.core.FloatType instance) {
                  return hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>left(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                    "Unsupported float type for column ",
                    cname)));
                }

                @Override
                public hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>> visit(hydra.core.FloatType.Bigfloat ignored) {
                  return hydra.lib.maybes.Maybe.applyLazy(
                    () -> hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>left(parseError),
                    (java.util.function.Function<java.math.BigDecimal, hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>>>) (parsed -> hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>right(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat(parsed)))))),
                    hydra.lib.literals.ReadBigfloat.apply(value));
                }

                @Override
                public hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>> visit(hydra.core.FloatType.Float32 ignored) {
                  return hydra.lib.maybes.Maybe.applyLazy(
                    () -> hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>left(parseError),
                    (java.util.function.Function<Float, hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>>>) (parsed -> hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>right(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32(parsed)))))),
                    hydra.lib.literals.ReadFloat32.apply(value));
                }

                @Override
                public hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>> visit(hydra.core.FloatType.Float64 ignored) {
                  return hydra.lib.maybes.Maybe.applyLazy(
                    () -> hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>left(parseError),
                    (java.util.function.Function<Double, hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>>>) (parsed -> hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>right(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(parsed)))))),
                    hydra.lib.literals.ReadFloat64.apply(value));
                }
              });
            }

            @Override
            public hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>> visit(hydra.core.LiteralType.Integer_ it) {
              return (it).value.accept(new hydra.core.IntegerType.PartialVisitor<>() {
                @Override
                public hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>> otherwise(hydra.core.IntegerType instance) {
                  return hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>left(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                    "Unsupported integer type for column ",
                    cname)));
                }

                @Override
                public hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>> visit(hydra.core.IntegerType.Int32 ignored) {
                  return hydra.lib.maybes.Maybe.applyLazy(
                    () -> hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>left(parseError),
                    (java.util.function.Function<Integer, hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>>>) (parsed -> hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>right(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(parsed)))))),
                    hydra.lib.literals.ReadInt32.apply(value));
                }

                @Override
                public hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>> visit(hydra.core.IntegerType.Int64 ignored) {
                  return hydra.lib.maybes.Maybe.applyLazy(
                    () -> hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>left(parseError),
                    (java.util.function.Function<Long, hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>>>) (parsed -> hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>right(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64(parsed)))))),
                    hydra.lib.literals.ReadInt64.apply(value));
                }
              });
            }

            @Override
            public hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>> visit(hydra.core.LiteralType.String_ ignored) {
              return hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>right(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_(value))));
            }
          });
        }
      });
    });
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<String, hydra.util.Maybe<hydra.core.Term>>right((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())),
      decodeValue,
      mvalue);
  }

  static hydra.util.Either<String, hydra.tabular.DataRow<hydra.core.Term>> decodeRow(java.util.List<hydra.tabular.ColumnType> colTypes, hydra.tabular.DataRow<String> row) {
    hydra.util.Lazy<java.util.List<hydra.util.Maybe<String>>> cells = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.tabular.DataRow<String>, java.util.List<hydra.util.Maybe<String>>>) (wrapped -> (wrapped).value)).apply(row));
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.core.Term>>, hydra.tabular.DataRow<hydra.core.Term>>) (decodedCells -> (hydra.tabular.DataRow<hydra.core.Term>) (new hydra.tabular.DataRow(decodedCells))),
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.tabular.ColumnType, hydra.util.Maybe<String>>, hydra.util.Either<String, hydra.util.Maybe<hydra.core.Term>>>) (pair -> {
          hydra.util.Lazy<hydra.tabular.ColumnType> colType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
          hydra.util.Lazy<hydra.util.Maybe<String>> mvalue = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
          return hydra.demos.genpg.Transform.decodeCell(
            colType.get(),
            mvalue.get());
        }),
        hydra.lib.lists.Zip.apply(
          colTypes,
          cells.get())));
  }

  static hydra.util.Either<String, hydra.tabular.Table<hydra.core.Term>> decodeTable(hydra.tabular.TableType tableType, hydra.tabular.Table<String> table) {
    java.util.List<hydra.tabular.ColumnType> colTypes = (tableType).columns;
    hydra.util.Lazy<hydra.util.Maybe<hydra.tabular.HeaderRow>> header = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.tabular.Table<String>, hydra.util.Maybe<hydra.tabular.HeaderRow>>) (projected -> projected.header)).apply(table));
    hydra.util.Lazy<java.util.List<hydra.tabular.DataRow<String>>> rows = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.tabular.Table<String>, java.util.List<hydra.tabular.DataRow<String>>>) (projected -> projected.data)).apply(table));
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<java.util.List<hydra.tabular.DataRow<hydra.core.Term>>, hydra.tabular.Table<hydra.core.Term>>) (decodedRows -> (hydra.tabular.Table<hydra.core.Term>) (new hydra.tabular.Table<hydra.core.Term>(header.get(), decodedRows))),
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.tabular.DataRow<String>, hydra.util.Either<String, hydra.tabular.DataRow<hydra.core.Term>>>) (row -> hydra.demos.genpg.Transform.decodeRow(
          colTypes,
          row)),
        rows.get()));
  }

  static <T0> Boolean elementIsEdge(hydra.pg.model.Element<T0> el) {
    return ((java.util.function.Function<hydra.pg.model.Element, Boolean>) (v1 -> ((java.util.function.Function<hydra.pg.model.Element<T0>, Boolean>) ((java.util.function.Function<hydra.pg.model.Element<T0>, Boolean>) (u -> (u).accept(new hydra.pg.model.Element.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.pg.model.Element<T0> instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.pg.model.Element.Edge<T0> ignored) {
        return true;
      }
    })))).apply(v1))).apply(el);
  }

  static <T0> Boolean elementIsVertex(hydra.pg.model.Element<T0> el) {
    return ((java.util.function.Function<hydra.pg.model.Element, Boolean>) (v1 -> ((java.util.function.Function<hydra.pg.model.Element<T0>, Boolean>) ((java.util.function.Function<hydra.pg.model.Element<T0>, Boolean>) (u -> (u).accept(new hydra.pg.model.Element.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.pg.model.Element<T0> instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.pg.model.Element.Vertex<T0> ignored) {
        return true;
      }
    })))).apply(v1))).apply(el);
  }

  static hydra.util.Either<String, java.util.Map<String, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>>> elementSpecsByTable(hydra.pg.model.LazyGraph<hydra.core.Term> graph) {
    hydra.util.Lazy<java.util.List<hydra.pg.model.Edge<hydra.core.Term>>> edges = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.LazyGraph<hydra.core.Term>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>) (projected -> projected.edges)).apply(graph));
    hydra.util.Lazy<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>> vertices = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.LazyGraph<hydra.core.Term>, java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>>) (projected -> projected.vertices)).apply(graph));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.pg.model.Vertex<hydra.core.Term>, hydra.util.Either<String, hydra.util.Pair<String, hydra.pg.model.Vertex<hydra.core.Term>>>>) (v -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<String, hydra.util.Pair<String, hydra.pg.model.Vertex<hydra.core.Term>>>) (t -> (hydra.util.Pair<String, hydra.pg.model.Vertex<hydra.core.Term>>) ((hydra.util.Pair<String, hydra.pg.model.Vertex<hydra.core.Term>>) (new hydra.util.Pair<String, hydra.pg.model.Vertex<hydra.core.Term>>(t, v)))),
          hydra.demos.genpg.Transform.tableForVertex(v))),
        vertices.get()),
      (java.util.function.Function<java.util.List<hydra.util.Pair<String, hydra.pg.model.Vertex<hydra.core.Term>>>, hydra.util.Either<String, java.util.Map<String, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>>>>) (vertexPairs -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.pg.model.Edge<hydra.core.Term>, hydra.util.Either<String, hydra.util.Pair<String, hydra.pg.model.Edge<hydra.core.Term>>>>) (e -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.util.Pair<String, hydra.pg.model.Edge<hydra.core.Term>>>) (t -> (hydra.util.Pair<String, hydra.pg.model.Edge<hydra.core.Term>>) ((hydra.util.Pair<String, hydra.pg.model.Edge<hydra.core.Term>>) (new hydra.util.Pair<String, hydra.pg.model.Edge<hydra.core.Term>>(t, e)))),
            hydra.demos.genpg.Transform.tableForEdge(e))),
          edges.get()),
        (java.util.function.Function<java.util.List<hydra.util.Pair<String, hydra.pg.model.Edge<hydra.core.Term>>>, hydra.util.Either<String, java.util.Map<String, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>>>>) (edgePairs -> hydra.util.Either.<String, java.util.Map<String, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>>>right(hydra.lib.lists.Foldl.apply(
          p0 -> p1 -> hydra.demos.genpg.Transform.<String, hydra.pg.model.Vertex<hydra.core.Term>, hydra.pg.model.Edge<hydra.core.Term>>elementSpecsByTable_addEdge(
            p0,
            p1),
          hydra.demos.genpg.Transform.elementSpecsByTable_vertexMap(vertexPairs),
          edgePairs))))));
  }

  static <T0, T1, T2> java.util.Map<T0, hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>> elementSpecsByTable_addEdge(java.util.Map<T0, hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>> m, hydra.util.Pair<T0, T2> p) {
    hydra.util.Lazy<T0> table = new hydra.util.Lazy<>(() -> hydra.demos.genpg.Transform.<T0, T2>elementSpecsByTable_table(p));
    hydra.util.Lazy<hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>> current = new hydra.util.Lazy<>(() -> hydra.demos.genpg.Transform.<T1, T2>elementSpecsByTable_current(hydra.demos.genpg.Transform.<T0, T1, T2>elementSpecsByTable_existing(
      m,
      table.get())));
    return hydra.lib.maps.Insert.apply(
      table.get(),
      (hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>) ((hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>) (new hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>(hydra.lib.pairs.First.apply(current.get()), hydra.lib.lists.Cons.apply(
        hydra.demos.genpg.Transform.<T0, T2>elementSpecsByTable_e(p),
        hydra.lib.pairs.Second.apply(current.get()))))),
      m);
  }

  static <T0, T1, T2> java.util.Map<T0, hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>> elementSpecsByTable_addVertex(java.util.Map<T0, hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>> m, hydra.util.Pair<T0, T1> p) {
    hydra.util.Lazy<T0> table = new hydra.util.Lazy<>(() -> hydra.demos.genpg.Transform.<T0, T1>elementSpecsByTable_table2(p));
    hydra.util.Lazy<hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>> current = new hydra.util.Lazy<>(() -> hydra.demos.genpg.Transform.<T1, T2>elementSpecsByTable_current2(hydra.demos.genpg.Transform.<T0, T1, T2>elementSpecsByTable_existing2(
      m,
      table.get())));
    return hydra.lib.maps.Insert.apply(
      table.get(),
      (hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>) ((hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>) (new hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>(hydra.lib.lists.Cons.apply(
        hydra.demos.genpg.Transform.<T0, T1>elementSpecsByTable_v(p),
        hydra.lib.pairs.First.apply(current.get())), hydra.lib.pairs.Second.apply(current.get())))),
      m);
  }

  static <T1, T2> hydra.util.Pair<java.util.List<T1>, java.util.List<T2>> elementSpecsByTable_current(hydra.util.Maybe<hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>> existing) {
    return hydra.lib.maybes.FromMaybe.applyLazy(
      () -> (hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>) ((hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>) (new hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>((java.util.List<T1>) (java.util.Collections.<T1>emptyList()), (java.util.List<T2>) (java.util.Collections.<T2>emptyList())))),
      existing);
  }

  static <T1, T2> hydra.util.Pair<java.util.List<T1>, java.util.List<T2>> elementSpecsByTable_current2(hydra.util.Maybe<hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>> existing) {
    return hydra.lib.maybes.FromMaybe.applyLazy(
      () -> (hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>) ((hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>) (new hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>((java.util.List<T1>) (java.util.Collections.<T1>emptyList()), (java.util.List<T2>) (java.util.Collections.<T2>emptyList())))),
      existing);
  }

  static <T0, T2> T2 elementSpecsByTable_e(hydra.util.Pair<T0, T2> p) {
    return hydra.lib.pairs.Second.apply(p);
  }

  static <T0, T1, T2> hydra.util.Maybe<hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>> elementSpecsByTable_existing(java.util.Map<T0, hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>> m, T0 table) {
    return hydra.lib.maps.Lookup.apply(
      table,
      m);
  }

  static <T0, T1, T2> hydra.util.Maybe<hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>> elementSpecsByTable_existing2(java.util.Map<T0, hydra.util.Pair<java.util.List<T1>, java.util.List<T2>>> m, T0 table) {
    return hydra.lib.maps.Lookup.apply(
      table,
      m);
  }

  static <T0, T2> T0 elementSpecsByTable_table(hydra.util.Pair<T0, T2> p) {
    return hydra.lib.pairs.First.apply(p);
  }

  static <T0, T1> T0 elementSpecsByTable_table2(hydra.util.Pair<T0, T1> p) {
    return hydra.lib.pairs.First.apply(p);
  }

  static <T0, T1> T1 elementSpecsByTable_v(hydra.util.Pair<T0, T1> p) {
    return hydra.lib.pairs.Second.apply(p);
  }

  static <T0> java.util.Map<String, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<T0>>> elementSpecsByTable_vertexMap(java.util.List<hydra.util.Pair<String, hydra.pg.model.Vertex<hydra.core.Term>>> vertexPairs) {
    return hydra.lib.lists.Foldl.apply(
      p0 -> p1 -> hydra.demos.genpg.Transform.<String, hydra.pg.model.Vertex<hydra.core.Term>, T0>elementSpecsByTable_addVertex(
        p0,
        p1),
      (java.util.Map<String, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<T0>>>) ((java.util.Map<String, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<T0>>>) (hydra.lib.maps.Empty.<String, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<T0>>>apply())),
      vertexPairs);
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.pg.model.Edge<hydra.core.Term>>> evaluateEdge(hydra.context.Context cx, hydra.graph.Graph g, hydra.pg.model.Edge<hydra.core.Term> edgeSpec, hydra.core.Term record) {
    hydra.util.Lazy<hydra.core.Term> idSpec = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Edge<hydra.core.Term>, hydra.core.Term>) (projected -> projected.id)).apply(edgeSpec));
    hydra.util.Lazy<hydra.core.Term> inSpec = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Edge<hydra.core.Term>, hydra.core.Term>) (projected -> projected.in)).apply(edgeSpec));
    hydra.util.Lazy<hydra.pg.model.EdgeLabel> label = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Edge<hydra.core.Term>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(edgeSpec));
    hydra.util.Lazy<hydra.core.Term> outSpec = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Edge<hydra.core.Term>, hydra.core.Term>) (projected -> projected.out)).apply(edgeSpec));
    hydra.util.Lazy<java.util.Map<hydra.pg.model.PropertyKey, hydra.core.Term>> propSpecs = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Edge<hydra.core.Term>, java.util.Map<hydra.pg.model.PropertyKey, hydra.core.Term>>) (projected -> projected.properties)).apply(edgeSpec));
    return hydra.lib.eithers.Bind.apply(
      hydra.Reduction.reduceTerm(
        cx,
        g,
        true,
        new hydra.core.Term.Application(new hydra.core.Application(idSpec.get(), record))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.pg.model.Edge<hydra.core.Term>>>>) (id -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.Bind.apply(
          hydra.Reduction.reduceTerm(
            cx,
            g,
            true,
            new hydra.core.Term.Application(new hydra.core.Application(outSpec.get(), record))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Term>>>) (_term -> hydra.extract.Core.maybeTerm(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (t -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(t)),
            g,
            _term))),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.pg.model.Edge<hydra.core.Term>>>>) (mOutId -> hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Bind.apply(
            hydra.Reduction.reduceTerm(
              cx,
              g,
              true,
              new hydra.core.Term.Application(new hydra.core.Application(inSpec.get(), record))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Term>>>) (_term -> hydra.extract.Core.maybeTerm(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (t -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(t)),
              g,
              _term))),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.pg.model.Edge<hydra.core.Term>>>>) (mInId -> hydra.lib.eithers.Bind.apply(
            hydra.demos.genpg.Transform.evaluateProperties(
              cx,
              g,
              propSpecs.get(),
              record),
            (java.util.function.Function<java.util.Map<hydra.pg.model.PropertyKey, hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.pg.model.Edge<hydra.core.Term>>>>) (props -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.pg.model.Edge<hydra.core.Term>>>right(hydra.lib.maybes.Bind.apply(
              mOutId,
              (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.pg.model.Edge<hydra.core.Term>>>) (outId -> hydra.lib.maybes.Map.apply(
                (java.util.function.Function<hydra.core.Term, hydra.pg.model.Edge<hydra.core.Term>>) (inId -> (hydra.pg.model.Edge<hydra.core.Term>) (new hydra.pg.model.Edge<hydra.core.Term>(label.get(), id, outId, inId, props))),
                mInId))))))))))));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, java.util.Map<T0, hydra.core.Term>> evaluateProperties(hydra.context.Context cx, hydra.graph.Graph g, java.util.Map<T0, hydra.core.Term> specs, hydra.core.Term record) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.util.Pair<T0, hydra.core.Term>>>, java.util.Map<T0, hydra.core.Term>>) (pairs -> hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(pairs))),
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.util.Pair<T0, hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.util.Pair<T0, hydra.core.Term>>>>) (pair -> {
          hydra.util.Lazy<hydra.core.Term> spec = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
          return hydra.lib.eithers.Bind.apply(
            hydra.Reduction.reduceTerm(
              cx,
              g,
              true,
              new hydra.core.Term.Application(new hydra.core.Application(spec.get(), record))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.util.Pair<T0, hydra.core.Term>>>>) (value -> hydra.demos.genpg.Transform.evaluateProperties_extractMaybe(
              hydra.demos.genpg.Transform.<T0>evaluateProperties_k(pair),
              hydra.Strip.deannotateTerm(value))));
        }),
        hydra.lib.maps.ToList.apply(specs)));
  }

  static <T1, T2> hydra.util.Either<T2, hydra.util.Maybe<hydra.util.Pair<T1, hydra.core.Term>>> evaluateProperties_extractMaybe(T1 k, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T2, hydra.util.Maybe<hydra.util.Pair<T1, hydra.core.Term>>> visit(hydra.core.Term.Maybe mv) {
        return hydra.util.Either.<T2, hydra.util.Maybe<hydra.util.Pair<T1, hydra.core.Term>>>right(hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>) (v -> (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(k, v)))),
          (mv).value));
      }
    });
  }

  static <T0> T0 evaluateProperties_k(hydra.util.Pair<T0, hydra.core.Term> pair) {
    return hydra.lib.pairs.First.apply(pair);
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.pg.model.Vertex<hydra.core.Term>>> evaluateVertex(hydra.context.Context cx, hydra.graph.Graph g, hydra.pg.model.Vertex<hydra.core.Term> vertexSpec, hydra.core.Term record) {
    hydra.util.Lazy<hydra.core.Term> idSpec = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Vertex<hydra.core.Term>, hydra.core.Term>) (projected -> projected.id)).apply(vertexSpec));
    hydra.util.Lazy<hydra.pg.model.VertexLabel> label = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Vertex<hydra.core.Term>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(vertexSpec));
    hydra.util.Lazy<java.util.Map<hydra.pg.model.PropertyKey, hydra.core.Term>> propSpecs = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Vertex<hydra.core.Term>, java.util.Map<hydra.pg.model.PropertyKey, hydra.core.Term>>) (projected -> projected.properties)).apply(vertexSpec));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bind.apply(
        hydra.Reduction.reduceTerm(
          cx,
          g,
          true,
          new hydra.core.Term.Application(new hydra.core.Application(idSpec.get(), record))),
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Term>>>) (_term -> hydra.extract.Core.maybeTerm(
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (t -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(t)),
          g,
          _term))),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.pg.model.Vertex<hydra.core.Term>>>>) (mId -> hydra.lib.eithers.Bind.apply(
        hydra.demos.genpg.Transform.evaluateProperties(
          cx,
          g,
          propSpecs.get(),
          record),
        (java.util.function.Function<java.util.Map<hydra.pg.model.PropertyKey, hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.pg.model.Vertex<hydra.core.Term>>>>) (props -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.pg.model.Vertex<hydra.core.Term>>>right(hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.Term, hydra.pg.model.Vertex<hydra.core.Term>>) (id -> (hydra.pg.model.Vertex<hydra.core.Term>) (new hydra.pg.model.Vertex<hydra.core.Term>(label.get(), id, props))),
          mId))))));
  }

  static java.util.Set<String> findTablesInTerm(hydra.core.Term term) {
    return hydra.Rewriting.foldOverTerm(
      new hydra.coders.TraversalOrder.Pre(),
      (java.util.function.Function<java.util.Set<String>, java.util.function.Function<hydra.core.Term, java.util.Set<String>>>) (names -> (java.util.function.Function<hydra.core.Term, java.util.Set<String>>) (t -> (t).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public java.util.Set<String> otherwise(hydra.core.Term instance) {
          return names;
        }

        @Override
        public java.util.Set<String> visit(hydra.core.Term.Project proj) {
          return hydra.lib.sets.Insert.apply(
            (proj).value.typeName.value,
            names);
        }
      }))),
      (java.util.Set<String>) (hydra.lib.sets.Empty.<String>apply()),
      term);
  }

  static java.util.Set<String> findTablesInTerms(java.util.List<hydra.core.Term> terms) {
    return hydra.lib.sets.Unions.apply(hydra.lib.lists.Map.apply(
      hydra.demos.genpg.Transform::findTablesInTerm,
      terms));
  }

  static <T0> Boolean listAny(java.util.function.Function<T0, Boolean> pred, java.util.List<T0> xs) {
    return hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.lists.Filter.apply(
      pred,
      xs)));
  }

  static <T0> hydra.pg.model.LazyGraph<T0> makeLazyGraph(java.util.List<hydra.pg.model.Vertex<T0>> vertices, java.util.List<hydra.pg.model.Edge<T0>> edges) {
    return (hydra.pg.model.LazyGraph<T0>) (new hydra.pg.model.LazyGraph<T0>(vertices, edges));
  }

  static hydra.util.Maybe<String> normalizeField(String s) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.strings.Null.apply(s),
      () -> (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      () -> hydra.util.Maybe.just(s));
  }

  static hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean> parseCsvChar(hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean> state, Integer c) {
    hydra.util.Lazy<java.util.List<hydra.util.Maybe<String>>> acc = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(state)));
    hydra.util.Lazy<String> field = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(state)));
    hydra.util.Lazy<Boolean> inQuotes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(state));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        c,
        34),
      () -> hydra.lib.logic.IfElse.lazy(
        inQuotes.get(),
        () -> (hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>) ((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>) (new hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>((hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>) ((hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>) (new hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>(acc.get(), field.get()))), false))),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.strings.Null.apply(field.get()),
          () -> (hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>) ((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>) (new hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>((hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>) ((hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>) (new hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>(acc.get(), field.get()))), true))),
          () -> (hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>) ((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>) (new hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>((hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>) ((hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>) (new hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>(acc.get(), hydra.lib.strings.Cat2.apply(
            field.get(),
            "\"")))), inQuotes.get()))))),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.logic.And.apply(
          hydra.lib.equality.Equal.apply(
            c,
            44),
          hydra.lib.logic.Not.apply(inQuotes.get())),
        () -> (hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>) ((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>) (new hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>((hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>) ((hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>) (new hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>(hydra.lib.lists.Cons.apply(
          hydra.demos.genpg.Transform.normalizeField(field.get()),
          acc.get()), ""))), false))),
        () -> (hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>) ((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>) (new hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>((hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>) ((hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>) (new hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>(acc.get(), hydra.lib.strings.Cat2.apply(
          field.get(),
          hydra.lib.strings.FromList.apply(java.util.Arrays.asList(c)))))), inQuotes.get())))));
  }

  static hydra.util.Either<String, java.util.List<hydra.util.Maybe<String>>> parseCsvLine(String line) {
    java.util.List<Integer> chars = hydra.lib.strings.ToList.apply(line);
    hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>> finalState = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>, java.util.function.Function<Integer, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.util.Maybe<String>>, String>, Boolean>>>) (p0 -> p1 -> hydra.demos.genpg.Transform.parseCsvChar(
        p0,
        p1)),
      hydra.demos.genpg.Transform.<hydra.util.Maybe<String>>parseCsvLine_initState(),
      chars));
    hydra.util.Lazy<java.util.List<hydra.util.Maybe<String>>> acc = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(finalState.get())));
    hydra.util.Lazy<String> field = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(finalState.get())));
    hydra.util.Lazy<Boolean> inQuotes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(finalState.get()));
    return hydra.lib.logic.IfElse.lazy(
      inQuotes.get(),
      () -> hydra.util.Either.<String, java.util.List<hydra.util.Maybe<String>>>left("Unclosed quoted field"),
      () -> hydra.util.Either.<String, java.util.List<hydra.util.Maybe<String>>>right(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Cons.apply(
        hydra.demos.genpg.Transform.normalizeField(field.get()),
        acc.get()))));
  }

  static <T0> hydra.util.Pair<hydra.util.Pair<java.util.List<T0>, String>, Boolean> parseCsvLine_initState() {
    return (hydra.util.Pair<hydra.util.Pair<java.util.List<T0>, String>, Boolean>) ((hydra.util.Pair<hydra.util.Pair<java.util.List<T0>, String>, Boolean>) (new hydra.util.Pair<hydra.util.Pair<java.util.List<T0>, String>, Boolean>((hydra.util.Pair<java.util.List<T0>, String>) ((hydra.util.Pair<java.util.List<T0>, String>) (new hydra.util.Pair<java.util.List<T0>, String>((java.util.List<T0>) (java.util.Collections.<T0>emptyList()), ""))), false)));
  }

  static hydra.util.Either<String, hydra.util.Maybe<java.util.List<hydra.util.Maybe<String>>>> parseSingleLine(String line) {
    String trimmed = hydra.demos.genpg.Transform.stripWhitespace(line);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.strings.Null.apply(trimmed),
      () -> hydra.util.Either.<String, hydra.util.Maybe<java.util.List<hydra.util.Maybe<String>>>>right((hydra.util.Maybe<java.util.List<hydra.util.Maybe<String>>>) (hydra.util.Maybe.<java.util.List<hydra.util.Maybe<String>>>nothing())),
      () -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<java.util.List<hydra.util.Maybe<String>>, hydra.util.Maybe<java.util.List<hydra.util.Maybe<String>>>>) (x -> hydra.util.Maybe.just(x)),
        hydra.demos.genpg.Transform.parseCsvLine(trimmed)));
  }

  static hydra.util.Either<String, hydra.tabular.Table<String>> parseTableLines(Boolean hasHeader, java.util.List<String> rawLines) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<String, hydra.util.Either<String, hydra.util.Maybe<java.util.List<hydra.util.Maybe<String>>>>>) (ln -> hydra.demos.genpg.Transform.parseSingleLine(ln)),
        rawLines),
      (java.util.function.Function<java.util.List<hydra.util.Maybe<java.util.List<hydra.util.Maybe<String>>>>, hydra.util.Either<String, hydra.tabular.Table<String>>>) (parsedRows -> {
        hydra.util.Lazy<java.util.List<java.util.List<hydra.util.Maybe<String>>>> rows = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(parsedRows));
        return hydra.lib.logic.IfElse.lazy(
          hasHeader,
          () -> ((java.util.function.Supplier<hydra.util.Either<String, hydra.tabular.Table<String>>>) (() -> {
            hydra.util.Lazy<java.util.List<hydra.util.Maybe<String>>> headerRow = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(rows.get()));
            return ((java.util.function.Supplier<hydra.util.Either<String, hydra.tabular.Table<String>>>) (() -> {
              hydra.util.Lazy<java.util.List<java.util.List<hydra.util.Maybe<String>>>> dataRows = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(rows.get()));
              return hydra.lib.logic.IfElse.lazy(
                hydra.demos.genpg.Transform.listAny(
                  (java.util.function.Function<hydra.util.Maybe<String>, Boolean>) (m -> hydra.lib.maybes.IsNothing.apply(m)),
                  headerRow.get()),
                () -> hydra.util.Either.<String, hydra.tabular.Table<String>>left("null header column(s)"),
                () -> hydra.util.Either.<String, hydra.tabular.Table<String>>right((hydra.tabular.Table<String>) (new hydra.tabular.Table<String>(hydra.util.Maybe.just(new hydra.tabular.HeaderRow(hydra.lib.maybes.Cat.apply(headerRow.get()))), hydra.lib.lists.Map.apply(
                  (java.util.function.Function<java.util.List<hydra.util.Maybe<String>>, hydra.tabular.DataRow<String>>) (r -> (hydra.tabular.DataRow<String>) (new hydra.tabular.DataRow(r))),
                  dataRows.get())))));
            })).get();
          })).get(),
          () -> hydra.util.Either.<String, hydra.tabular.Table<String>>right((hydra.tabular.Table<String>) (new hydra.tabular.Table<String>((hydra.util.Maybe<hydra.tabular.HeaderRow>) (hydra.util.Maybe.<hydra.tabular.HeaderRow>nothing()), hydra.lib.lists.Map.apply(
            (java.util.function.Function<java.util.List<hydra.util.Maybe<String>>, hydra.tabular.DataRow<String>>) (r -> (hydra.tabular.DataRow<String>) (new hydra.tabular.DataRow(r))),
            rows.get())))));
      }));
  }

  static String stripWhitespace(String s) {
    java.util.List<Integer> chars = hydra.lib.strings.ToList.apply(s);
    java.util.function.Function<Integer, Boolean> isSpaceChar = (java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.chars.IsSpace.apply(c));
    hydra.util.Lazy<java.util.List<Integer>> trimLeft = new hydra.util.Lazy<>(() -> hydra.lib.lists.DropWhile.apply(
      isSpaceChar,
      chars));
    hydra.util.Lazy<java.util.List<Integer>> trimRight = new hydra.util.Lazy<>(() -> hydra.lib.lists.Reverse.apply(hydra.lib.lists.DropWhile.apply(
      isSpaceChar,
      hydra.lib.lists.Reverse.apply(trimLeft.get()))));
    return hydra.lib.strings.FromList.apply(trimRight.get());
  }

  static hydra.util.Either<String, String> tableForEdge(hydra.pg.model.Edge<hydra.core.Term> edge) {
    hydra.util.Lazy<hydra.core.Term> id = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Edge<hydra.core.Term>, hydra.core.Term>) (projected -> projected.id)).apply(edge));
    hydra.util.Lazy<hydra.core.Term> inId = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Edge<hydra.core.Term>, hydra.core.Term>) (projected -> projected.in)).apply(edge));
    hydra.util.Lazy<hydra.pg.model.EdgeLabel> label = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Edge<hydra.core.Term>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(edge));
    hydra.util.Lazy<hydra.core.Term> outId = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Edge<hydra.core.Term>, hydra.core.Term>) (projected -> projected.out)).apply(edge));
    hydra.util.Lazy<java.util.Map<hydra.pg.model.PropertyKey, hydra.core.Term>> props = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Edge<hydra.core.Term>, java.util.Map<hydra.pg.model.PropertyKey, hydra.core.Term>>) (projected -> projected.properties)).apply(edge));
    hydra.util.Lazy<java.util.Set<String>> tables = new hydra.util.Lazy<>(() -> hydra.demos.genpg.Transform.findTablesInTerms(hydra.lib.lists.Concat2.apply(
      java.util.Arrays.asList(
        id.get(),
        outId.get(),
        inId.get()),
      hydra.lib.maps.Elems.apply(props.get()))));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.sets.Size.apply(tables.get()),
        1),
      () -> hydra.util.Either.<String, String>right(hydra.lib.lists.Head.apply(hydra.lib.sets.ToList.apply(tables.get()))),
      () -> hydra.util.Either.<String, String>left(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        "Specification for ",
        label.get().value,
        " edges has wrong number of tables"))));
  }

  static hydra.util.Either<String, String> tableForVertex(hydra.pg.model.Vertex<hydra.core.Term> vertex) {
    hydra.util.Lazy<hydra.core.Term> id = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Vertex<hydra.core.Term>, hydra.core.Term>) (projected -> projected.id)).apply(vertex));
    hydra.util.Lazy<hydra.pg.model.VertexLabel> label = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Vertex<hydra.core.Term>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(vertex));
    hydra.util.Lazy<java.util.Map<hydra.pg.model.PropertyKey, hydra.core.Term>> props = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Vertex<hydra.core.Term>, java.util.Map<hydra.pg.model.PropertyKey, hydra.core.Term>>) (projected -> projected.properties)).apply(vertex));
    hydra.util.Lazy<java.util.Set<String>> tables = new hydra.util.Lazy<>(() -> hydra.demos.genpg.Transform.findTablesInTerms(hydra.lib.lists.Cons.apply(
      id.get(),
      hydra.lib.maps.Elems.apply(props.get()))));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.sets.Size.apply(tables.get()),
        1),
      () -> hydra.util.Either.<String, String>right(hydra.lib.lists.Head.apply(hydra.lib.sets.ToList.apply(tables.get()))),
      () -> hydra.util.Either.<String, String>left(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        "Specification for ",
        label.get().value,
        " vertices has wrong number of tables"))));
  }

  static java.util.Map<hydra.relational.RelationName, hydra.tabular.TableType> tableTypesByName(java.util.List<hydra.tabular.TableType> tableTypes) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.tabular.TableType, hydra.util.Pair<hydra.relational.RelationName, hydra.tabular.TableType>>) (t -> (hydra.util.Pair<hydra.relational.RelationName, hydra.tabular.TableType>) ((hydra.util.Pair<hydra.relational.RelationName, hydra.tabular.TableType>) (new hydra.util.Pair<hydra.relational.RelationName, hydra.tabular.TableType>((t).name, t)))),
      tableTypes));
  }

  static hydra.core.Term termRowToRecord(hydra.tabular.TableType tableType, hydra.tabular.DataRow<hydra.core.Term> row) {
    hydra.util.Lazy<java.util.List<hydra.util.Maybe<hydra.core.Term>>> cells = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.tabular.DataRow<hydra.core.Term>, java.util.List<hydra.util.Maybe<hydra.core.Term>>>) (wrapped -> (wrapped).value)).apply(row));
    java.util.List<hydra.tabular.ColumnType> colTypes = (tableType).columns;
    String tname = (tableType).name.value;
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name(tname), hydra.lib.lists.ZipWith.apply(
      (java.util.function.Function<hydra.tabular.ColumnType, java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.core.Field>>) (colType -> (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.core.Field>) (mvalue -> {
        String cname = (colType).name.value;
        return new hydra.core.Field(new hydra.core.Name(cname), new hydra.core.Term.Maybe(mvalue));
      })),
      colTypes,
      cells.get())));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>> transformRecord(hydra.context.Context cx, hydra.graph.Graph g, java.util.List<hydra.pg.model.Vertex<hydra.core.Term>> vspecs, java.util.List<hydra.pg.model.Edge<hydra.core.Term>> especs, hydra.core.Term record) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.pg.model.Vertex<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.pg.model.Vertex<hydra.core.Term>>>>) (spec -> hydra.demos.genpg.Transform.evaluateVertex(
          cx,
          g,
          spec,
          record)),
        vspecs),
      (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.pg.model.Vertex<hydra.core.Term>>>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>>>) (mVertices -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.pg.model.Edge<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.pg.model.Edge<hydra.core.Term>>>>) (spec -> hydra.demos.genpg.Transform.evaluateEdge(
            cx,
            g,
            spec,
            record)),
          especs),
        (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.pg.model.Edge<hydra.core.Term>>>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>>>) (mEdges -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>>right((hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>) ((hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>) (new hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>(hydra.lib.maybes.Cat.apply(mVertices), hydra.lib.maybes.Cat.apply(mEdges)))))))));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>> transformTableRows(hydra.context.Context cx, hydra.graph.Graph g, java.util.List<hydra.pg.model.Vertex<hydra.core.Term>> vspecs, java.util.List<hydra.pg.model.Edge<hydra.core.Term>> especs, hydra.tabular.TableType tableType, java.util.List<hydra.tabular.DataRow<hydra.core.Term>> rows) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<java.util.List<hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>>, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>>) (pairs -> hydra.lib.lists.Foldl.apply(
        p0 -> p1 -> hydra.demos.genpg.Transform.<hydra.pg.model.Vertex<hydra.core.Term>, hydra.pg.model.Edge<hydra.core.Term>>concatPairs(
          p0,
          p1),
        (hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>) ((hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>) (new hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>((java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>) (java.util.Collections.<hydra.pg.model.Vertex<hydra.core.Term>>emptyList()), (java.util.List<hydra.pg.model.Edge<hydra.core.Term>>) (java.util.Collections.<hydra.pg.model.Edge<hydra.core.Term>>emptyList())))),
        pairs)),
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.tabular.DataRow<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<hydra.core.Term>>, java.util.List<hydra.pg.model.Edge<hydra.core.Term>>>>>) (row -> hydra.demos.genpg.Transform.transformRecord(
          cx,
          g,
          vspecs,
          especs,
          hydra.demos.genpg.Transform.termRowToRecord(
            tableType,
            row))),
        rows));
  }
}
