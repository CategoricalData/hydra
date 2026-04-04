// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.relational
 */
public interface Relational {
  static hydra.phantoms.TTerm<hydra.relational.ColumnName> columnName(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.relational.ColumnName"), (x).value)));
  }

  static <T> hydra.phantoms.TTerm<hydra.relational.ColumnSchema<T>> columnSchema(hydra.phantoms.TTerm<hydra.relational.ColumnName> name, hydra.phantoms.TTerm<T> domain) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.relational.ColumnSchema"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("domain"), (domain).value)))));
  }

  static <T> hydra.phantoms.TTerm<T> columnSchemaDomain(hydra.phantoms.TTerm<hydra.relational.ColumnSchema<T>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.ColumnSchema"), new hydra.core.Name("domain"))))), (x).value)));
  }

  static <T> hydra.phantoms.TTerm<hydra.relational.ColumnName> columnSchemaName(hydra.phantoms.TTerm<hydra.relational.ColumnSchema<T>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.ColumnSchema"), new hydra.core.Name("name"))))), (x).value)));
  }

  static <T> hydra.phantoms.TTerm<hydra.relational.ColumnSchema<T>> columnSchemaWithDomain(hydra.phantoms.TTerm<hydra.relational.ColumnSchema<T>> original, hydra.phantoms.TTerm<T> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.relational.ColumnSchema"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.ColumnSchema"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("domain"), (newVal).value)))));
  }

  static <T> hydra.phantoms.TTerm<hydra.relational.ColumnSchema<T>> columnSchemaWithName(hydra.phantoms.TTerm<hydra.relational.ColumnSchema<T>> original, hydra.phantoms.TTerm<hydra.relational.ColumnName> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.relational.ColumnSchema"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("domain"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.ColumnSchema"), new hydra.core.Name("domain"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.relational.ForeignKey> foreignKey(hydra.phantoms.TTerm<hydra.relational.RelationName> foreignRelation, hydra.phantoms.TTerm<java.util.Map<hydra.relational.ColumnName, hydra.relational.ColumnName>> keys) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.relational.ForeignKey"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("foreignRelation"), (foreignRelation).value),
      new hydra.core.Field(new hydra.core.Name("keys"), (keys).value)))));
  }

  static hydra.phantoms.TTerm<hydra.relational.RelationName> foreignKeyForeignRelation(hydra.phantoms.TTerm<hydra.relational.ForeignKey> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.ForeignKey"), new hydra.core.Name("foreignRelation"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Map<hydra.relational.ColumnName, hydra.relational.ColumnName>> foreignKeyKeys(hydra.phantoms.TTerm<hydra.relational.ForeignKey> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.ForeignKey"), new hydra.core.Name("keys"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.relational.ForeignKey> foreignKeyWithForeignRelation(hydra.phantoms.TTerm<hydra.relational.ForeignKey> original, hydra.phantoms.TTerm<hydra.relational.RelationName> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.relational.ForeignKey"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("foreignRelation"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("keys"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.ForeignKey"), new hydra.core.Name("keys"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.relational.ForeignKey> foreignKeyWithKeys(hydra.phantoms.TTerm<hydra.relational.ForeignKey> original, hydra.phantoms.TTerm<java.util.Map<hydra.relational.ColumnName, hydra.relational.ColumnName>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.relational.ForeignKey"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("foreignRelation"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.ForeignKey"), new hydra.core.Name("foreignRelation"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("keys"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.relational.PrimaryKey> primaryKey(hydra.phantoms.TTerm<java.util.List<hydra.relational.ColumnName>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.relational.PrimaryKey"), (x).value)));
  }

  static <V> hydra.phantoms.TTerm<hydra.relational.Relation<V>> relation(hydra.phantoms.TTerm<java.util.List<hydra.relational.Row<V>>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.relational.Relation"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.relational.RelationName> relationName(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.relational.RelationName"), (x).value)));
  }

  static <T> hydra.phantoms.TTerm<hydra.relational.RelationSchema<T>> relationSchema(hydra.phantoms.TTerm<hydra.relational.RelationName> name, hydra.phantoms.TTerm<java.util.List<hydra.relational.ColumnSchema<T>>> columns, hydra.phantoms.TTerm<java.util.List<hydra.relational.PrimaryKey>> primaryKeys, hydra.phantoms.TTerm<java.util.List<hydra.relational.ForeignKey>> foreignKeys) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.relational.RelationSchema"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("columns"), (columns).value),
      new hydra.core.Field(new hydra.core.Name("primaryKeys"), (primaryKeys).value),
      new hydra.core.Field(new hydra.core.Name("foreignKeys"), (foreignKeys).value)))));
  }

  static <T> hydra.phantoms.TTerm<java.util.List<hydra.relational.ColumnSchema<T>>> relationSchemaColumns(hydra.phantoms.TTerm<hydra.relational.RelationSchema<T>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.RelationSchema"), new hydra.core.Name("columns"))))), (x).value)));
  }

  static <T> hydra.phantoms.TTerm<java.util.List<hydra.relational.ForeignKey>> relationSchemaForeignKeys(hydra.phantoms.TTerm<hydra.relational.RelationSchema<T>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.RelationSchema"), new hydra.core.Name("foreignKeys"))))), (x).value)));
  }

  static <T> hydra.phantoms.TTerm<hydra.relational.RelationName> relationSchemaName(hydra.phantoms.TTerm<hydra.relational.RelationSchema<T>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.RelationSchema"), new hydra.core.Name("name"))))), (x).value)));
  }

  static <T> hydra.phantoms.TTerm<java.util.List<hydra.relational.PrimaryKey>> relationSchemaPrimaryKeys(hydra.phantoms.TTerm<hydra.relational.RelationSchema<T>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.RelationSchema"), new hydra.core.Name("primaryKeys"))))), (x).value)));
  }

  static <T> hydra.phantoms.TTerm<hydra.relational.RelationSchema<T>> relationSchemaWithColumns(hydra.phantoms.TTerm<hydra.relational.RelationSchema<T>> original, hydra.phantoms.TTerm<java.util.List<hydra.relational.ColumnSchema<T>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.relational.RelationSchema"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.RelationSchema"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("columns"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("primaryKeys"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.RelationSchema"), new hydra.core.Name("primaryKeys"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("foreignKeys"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.RelationSchema"), new hydra.core.Name("foreignKeys"))))), (original).value)))))));
  }

  static <T> hydra.phantoms.TTerm<hydra.relational.RelationSchema<T>> relationSchemaWithForeignKeys(hydra.phantoms.TTerm<hydra.relational.RelationSchema<T>> original, hydra.phantoms.TTerm<java.util.List<hydra.relational.ForeignKey>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.relational.RelationSchema"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.RelationSchema"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("columns"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.RelationSchema"), new hydra.core.Name("columns"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("primaryKeys"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.RelationSchema"), new hydra.core.Name("primaryKeys"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("foreignKeys"), (newVal).value)))));
  }

  static <T> hydra.phantoms.TTerm<hydra.relational.RelationSchema<T>> relationSchemaWithName(hydra.phantoms.TTerm<hydra.relational.RelationSchema<T>> original, hydra.phantoms.TTerm<hydra.relational.RelationName> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.relational.RelationSchema"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("columns"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.RelationSchema"), new hydra.core.Name("columns"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("primaryKeys"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.RelationSchema"), new hydra.core.Name("primaryKeys"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("foreignKeys"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.RelationSchema"), new hydra.core.Name("foreignKeys"))))), (original).value)))))));
  }

  static <T> hydra.phantoms.TTerm<hydra.relational.RelationSchema<T>> relationSchemaWithPrimaryKeys(hydra.phantoms.TTerm<hydra.relational.RelationSchema<T>> original, hydra.phantoms.TTerm<java.util.List<hydra.relational.PrimaryKey>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.relational.RelationSchema"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.RelationSchema"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("columns"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.RelationSchema"), new hydra.core.Name("columns"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("primaryKeys"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("foreignKeys"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.relational.RelationSchema"), new hydra.core.Name("foreignKeys"))))), (original).value)))))));
  }

  static <V> hydra.phantoms.TTerm<hydra.relational.Relationship<V>> relationship(hydra.phantoms.TTerm<java.util.Set<java.util.Map<hydra.relational.ColumnName, V>>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.relational.Relationship"), (x).value)));
  }

  static <V> hydra.phantoms.TTerm<hydra.relational.Row<V>> row(hydra.phantoms.TTerm<java.util.List<V>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.relational.Row"), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unColumnName(hydra.phantoms.TTerm<hydra.relational.ColumnName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.relational.ColumnName")))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.relational.ColumnName>> unPrimaryKey(hydra.phantoms.TTerm<hydra.relational.PrimaryKey> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.relational.PrimaryKey")))), (x).value)));
  }

  static <V> hydra.phantoms.TTerm<java.util.List<hydra.relational.Row<V>>> unRelation(hydra.phantoms.TTerm<hydra.relational.Relation<V>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.relational.Relation")))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unRelationName(hydra.phantoms.TTerm<hydra.relational.RelationName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.relational.RelationName")))), (x).value)));
  }

  static <V> hydra.phantoms.TTerm<java.util.Set<java.util.Map<hydra.relational.ColumnName, V>>> unRelationship(hydra.phantoms.TTerm<hydra.relational.Relationship<V>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.relational.Relationship")))), (x).value)));
  }

  static <V> hydra.phantoms.TTerm<java.util.List<V>> unRow(hydra.phantoms.TTerm<hydra.relational.Row<V>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.relational.Row")))), (x).value)));
  }
}
