// Note: this is an automatically generated file. Do not edit.

package hydra.encode.relational;

/**
 * Term encoders for hydra.relational
 */
public interface Relational {
  static hydra.core.Term columnName(hydra.relational.ColumnName x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.relational.ColumnName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).value))));
  }
  
  static <T0> hydra.core.Term columnSchema(java.util.function.Function<T0, hydra.core.Term> t, hydra.relational.ColumnSchema<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.relational.ColumnSchema"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.relational.Relational.columnName(((java.util.function.Function<hydra.relational.ColumnSchema<T0>, hydra.relational.ColumnName>) (projected -> projected.name)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("domain"), (t).apply(((java.util.function.Function<hydra.relational.ColumnSchema<T0>, T0>) (projected -> projected.domain)).apply(x))))));
  }
  
  static hydra.core.Term foreignKey(hydra.relational.ForeignKey x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.relational.ForeignKey"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("foreignRelation"), hydra.encode.relational.Relational.relationName((x).foreignRelation)),
      new hydra.core.Field(new hydra.core.Name("keys"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.relational.Relational::columnName,
        hydra.encode.relational.Relational::columnName,
        (x).keys))))));
  }
  
  static hydra.core.Term primaryKey(hydra.relational.PrimaryKey x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.relational.PrimaryKey"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
      hydra.encode.relational.Relational::columnName,
      (x).value))));
  }
  
  static <T0> hydra.core.Term relation(java.util.function.Function<T0, hydra.core.Term> v, hydra.relational.Relation<T0> x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.relational.Relation"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.relational.Row<T0>, hydra.core.Term>) (v1 -> hydra.encode.relational.Relational.<T0>row(
        v,
        v1)),
      ((java.util.function.Function<hydra.relational.Relation<T0>, java.util.List<hydra.relational.Row<T0>>>) (wrapped -> (wrapped).value)).apply(x)))));
  }
  
  static hydra.core.Term relationName(hydra.relational.RelationName x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.relational.RelationName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).value))));
  }
  
  static <T0> hydra.core.Term relationSchema(java.util.function.Function<T0, hydra.core.Term> t, hydra.relational.RelationSchema<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.relational.RelationSchema"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.relational.Relational.relationName(((java.util.function.Function<hydra.relational.RelationSchema<T0>, hydra.relational.RelationName>) (projected -> projected.name)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("columns"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.relational.ColumnSchema<T0>, hydra.core.Term>) (v1 -> hydra.encode.relational.Relational.<T0>columnSchema(
          t,
          v1)),
        ((java.util.function.Function<hydra.relational.RelationSchema<T0>, java.util.List<hydra.relational.ColumnSchema<T0>>>) (projected -> projected.columns)).apply(x)))),
      new hydra.core.Field(new hydra.core.Name("primaryKeys"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.relational.Relational::primaryKey,
        ((java.util.function.Function<hydra.relational.RelationSchema<T0>, java.util.List<hydra.relational.PrimaryKey>>) (projected -> projected.primaryKeys)).apply(x)))),
      new hydra.core.Field(new hydra.core.Name("foreignKeys"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.relational.Relational::foreignKey,
        ((java.util.function.Function<hydra.relational.RelationSchema<T0>, java.util.List<hydra.relational.ForeignKey>>) (projected -> projected.foreignKeys)).apply(x)))))));
  }
  
  static <T0> hydra.core.Term relationship(java.util.function.Function<T0, hydra.core.Term> v, hydra.relational.Relationship<T0> x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.relational.Relationship"), new hydra.core.Term.Set(hydra.lib.sets.Map.apply(
      (java.util.function.Function<java.util.Map<hydra.relational.ColumnName, T0>, hydra.core.Term>) (m -> new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.relational.Relational::columnName,
        v,
        m))),
      ((java.util.function.Function<hydra.relational.Relationship<T0>, java.util.Set<java.util.Map<hydra.relational.ColumnName, T0>>>) (wrapped -> (wrapped).value)).apply(x)))));
  }
  
  static <T0> hydra.core.Term row(java.util.function.Function<T0, hydra.core.Term> v, hydra.relational.Row<T0> x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.relational.Row"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
      v,
      ((java.util.function.Function<hydra.relational.Row<T0>, java.util.List<T0>>) (wrapped -> (wrapped).value)).apply(x)))));
  }
}
