// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.tabular
 */
public interface Tabular {
  static hydra.phantoms.TTerm<hydra.tabular.ColumnType> columnType(hydra.phantoms.TTerm<hydra.relational.ColumnName> name, hydra.phantoms.TTerm<hydra.core.Type> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.tabular.ColumnType"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.relational.ColumnName> columnTypeName(hydra.phantoms.TTerm<hydra.tabular.ColumnType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.tabular.ColumnType"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> columnTypeType(hydra.phantoms.TTerm<hydra.tabular.ColumnType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.tabular.ColumnType"), new hydra.core.Name("type"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.tabular.ColumnType> columnTypeWithName(hydra.phantoms.TTerm<hydra.tabular.ColumnType> original, hydra.phantoms.TTerm<hydra.relational.ColumnName> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.tabular.ColumnType"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.tabular.ColumnType"), new hydra.core.Name("type"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.tabular.ColumnType> columnTypeWithType(hydra.phantoms.TTerm<hydra.tabular.ColumnType> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.tabular.ColumnType"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.tabular.ColumnType"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static <V> hydra.phantoms.TTerm<hydra.tabular.DataRow<V>> dataRow(hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.Maybe<V>>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.tabular.DataRow"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.tabular.HeaderRow> headerRow(hydra.phantoms.TTerm<hydra.util.ConsList<String>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.tabular.HeaderRow"), (x).value)));
  }

  static <V> hydra.phantoms.TTerm<hydra.tabular.Table<V>> table(hydra.phantoms.TTerm<hydra.util.Maybe<hydra.tabular.HeaderRow>> header, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.tabular.DataRow<V>>> data) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.tabular.Table"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("header"), (header).value),
      new hydra.core.Field(new hydra.core.Name("data"), (data).value)))));
  }

  static <V> hydra.phantoms.TTerm<hydra.util.ConsList<hydra.tabular.DataRow<V>>> tableData(hydra.phantoms.TTerm<hydra.tabular.Table<V>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.tabular.Table"), new hydra.core.Name("data"))))), (x).value)));
  }

  static <V> hydra.phantoms.TTerm<hydra.util.Maybe<hydra.tabular.HeaderRow>> tableHeader(hydra.phantoms.TTerm<hydra.tabular.Table<V>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.tabular.Table"), new hydra.core.Name("header"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.tabular.TableType> tableType(hydra.phantoms.TTerm<hydra.relational.RelationName> name, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.tabular.ColumnType>> columns) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.tabular.TableType"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("columns"), (columns).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.tabular.ColumnType>> tableTypeColumns(hydra.phantoms.TTerm<hydra.tabular.TableType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.tabular.TableType"), new hydra.core.Name("columns"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.relational.RelationName> tableTypeName(hydra.phantoms.TTerm<hydra.tabular.TableType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.tabular.TableType"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.tabular.TableType> tableTypeWithColumns(hydra.phantoms.TTerm<hydra.tabular.TableType> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.tabular.ColumnType>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.tabular.TableType"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.tabular.TableType"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("columns"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.tabular.TableType> tableTypeWithName(hydra.phantoms.TTerm<hydra.tabular.TableType> original, hydra.phantoms.TTerm<hydra.relational.RelationName> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.tabular.TableType"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("columns"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.tabular.TableType"), new hydra.core.Name("columns"))))), (original).value)))))));
  }

  static <V> hydra.phantoms.TTerm<hydra.tabular.Table<V>> tableWithData(hydra.phantoms.TTerm<hydra.tabular.Table<V>> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.tabular.DataRow<V>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.tabular.Table"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("header"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.tabular.Table"), new hydra.core.Name("header"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("data"), (newVal).value)))));
  }

  static <V> hydra.phantoms.TTerm<hydra.tabular.Table<V>> tableWithHeader(hydra.phantoms.TTerm<hydra.tabular.Table<V>> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.tabular.HeaderRow>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.tabular.Table"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("header"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("data"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.tabular.Table"), new hydra.core.Name("data"))))), (original).value)))))));
  }

  static <V> hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.Maybe<V>>> unDataRow(hydra.phantoms.TTerm<hydra.tabular.DataRow<V>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.tabular.DataRow")))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<String>> unHeaderRow(hydra.phantoms.TTerm<hydra.tabular.HeaderRow> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.tabular.HeaderRow")))), (x).value)));
  }
}
