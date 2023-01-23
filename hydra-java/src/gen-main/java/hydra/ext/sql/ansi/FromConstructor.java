package hydra.ext.sql.ansi;

public class FromConstructor {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.FromConstructor");
  
  public final java.util.Optional<hydra.ext.sql.ansi.InsertColumnList> columns;
  
  public final java.util.Optional<hydra.ext.sql.ansi.OverrideClause> override;
  
  public final hydra.ext.sql.ansi.ContextuallyTypedTableValueConstructor values;
  
  public FromConstructor (java.util.Optional<hydra.ext.sql.ansi.InsertColumnList> columns, java.util.Optional<hydra.ext.sql.ansi.OverrideClause> override, hydra.ext.sql.ansi.ContextuallyTypedTableValueConstructor values) {
    this.columns = columns;
    this.override = override;
    this.values = values;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FromConstructor)) {
      return false;
    }
    FromConstructor o = (FromConstructor) (other);
    return columns.equals(o.columns) && override.equals(o.override) && values.equals(o.values);
  }
  
  @Override
  public int hashCode() {
    return 2 * columns.hashCode() + 3 * override.hashCode() + 5 * values.hashCode();
  }
  
  public FromConstructor withColumns(java.util.Optional<hydra.ext.sql.ansi.InsertColumnList> columns) {
    return new FromConstructor(columns, override, values);
  }
  
  public FromConstructor withOverride(java.util.Optional<hydra.ext.sql.ansi.OverrideClause> override) {
    return new FromConstructor(columns, override, values);
  }
  
  public FromConstructor withValues(hydra.ext.sql.ansi.ContextuallyTypedTableValueConstructor values) {
    return new FromConstructor(columns, override, values);
  }
}