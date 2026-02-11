// Note: this is an automatically generated file. Do not edit.

package hydra.encode.tabular;

/**
 * Term encoders for hydra.tabular
 */
public interface Tabular {
  static hydra.core.Term columnType(hydra.tabular.ColumnType x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.tabular.ColumnType"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.relational.Relational.columnName((x).name)),
      new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.core.Core.type((x).type)))));
  }
  
  static <T0> hydra.core.Term dataRow(java.util.function.Function<T0, hydra.core.Term> v, hydra.tabular.DataRow<T0> x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.tabular.DataRow"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Maybe<T0>, hydra.core.Term>) (opt -> new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        v,
        opt))),
      ((java.util.function.Function<hydra.tabular.DataRow<T0>, java.util.List<hydra.util.Maybe<T0>>>) (wrapped -> (wrapped).value)).apply(x)))));
  }
  
  static hydra.core.Term headerRow(hydra.tabular.HeaderRow x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.tabular.HeaderRow"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
      (java.util.function.Function<String, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(x2))),
      (x).value))));
  }
  
  static <T0> hydra.core.Term table(java.util.function.Function<T0, hydra.core.Term> v, hydra.tabular.Table<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.tabular.Table"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("header"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        hydra.encode.tabular.Tabular::headerRow,
        ((java.util.function.Function<hydra.tabular.Table<T0>, hydra.util.Maybe<hydra.tabular.HeaderRow>>) (projected -> projected.header)).apply(x)))),
      new hydra.core.Field(new hydra.core.Name("data"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.tabular.DataRow<T0>, hydra.core.Term>) (v1 -> hydra.encode.tabular.Tabular.<T0>dataRow(
          v,
          v1)),
        ((java.util.function.Function<hydra.tabular.Table<T0>, java.util.List<hydra.tabular.DataRow<T0>>>) (projected -> projected.data)).apply(x)))))));
  }
  
  static hydra.core.Term tableType(hydra.tabular.TableType x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.tabular.TableType"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.relational.Relational.relationName((x).name)),
      new hydra.core.Field(new hydra.core.Name("columns"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.tabular.Tabular::columnType,
        (x).columns))))));
  }
}
