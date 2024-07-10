// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class FromConstructor implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.FromConstructor");
  
  public final java.util.Optional<hydra.langs.sql.ansi.InsertColumnList> columns;
  
  public final java.util.Optional<hydra.langs.sql.ansi.OverrideClause> override;
  
  public final hydra.langs.sql.ansi.ContextuallyTypedTableValueConstructor values;
  
  public FromConstructor (java.util.Optional<hydra.langs.sql.ansi.InsertColumnList> columns, java.util.Optional<hydra.langs.sql.ansi.OverrideClause> override, hydra.langs.sql.ansi.ContextuallyTypedTableValueConstructor values) {
    if (columns == null) {
      throw new IllegalArgumentException("null value for 'columns' argument");
    }
    if (override == null) {
      throw new IllegalArgumentException("null value for 'override' argument");
    }
    if (values == null) {
      throw new IllegalArgumentException("null value for 'values' argument");
    }
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
  
  public FromConstructor withColumns(java.util.Optional<hydra.langs.sql.ansi.InsertColumnList> columns) {
    if (columns == null) {
      throw new IllegalArgumentException("null value for 'columns' argument");
    }
    return new FromConstructor(columns, override, values);
  }
  
  public FromConstructor withOverride(java.util.Optional<hydra.langs.sql.ansi.OverrideClause> override) {
    if (override == null) {
      throw new IllegalArgumentException("null value for 'override' argument");
    }
    return new FromConstructor(columns, override, values);
  }
  
  public FromConstructor withValues(hydra.langs.sql.ansi.ContextuallyTypedTableValueConstructor values) {
    if (values == null) {
      throw new IllegalArgumentException("null value for 'values' argument");
    }
    return new FromConstructor(columns, override, values);
  }
}