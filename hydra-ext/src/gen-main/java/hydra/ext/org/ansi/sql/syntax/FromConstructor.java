// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public class FromConstructor implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/ansi/sql/syntax.FromConstructor");
  
  public static final hydra.core.Name FIELD_NAME_COLUMNS = new hydra.core.Name("columns");
  
  public static final hydra.core.Name FIELD_NAME_OVERRIDE = new hydra.core.Name("override");
  
  public static final hydra.core.Name FIELD_NAME_VALUES = new hydra.core.Name("values");
  
  public final hydra.util.Opt<hydra.ext.org.ansi.sql.syntax.InsertColumnList> columns;
  
  public final hydra.util.Opt<hydra.ext.org.ansi.sql.syntax.OverrideClause> override;
  
  public final hydra.ext.org.ansi.sql.syntax.ContextuallyTypedTableValueConstructor values;
  
  public FromConstructor (hydra.util.Opt<hydra.ext.org.ansi.sql.syntax.InsertColumnList> columns, hydra.util.Opt<hydra.ext.org.ansi.sql.syntax.OverrideClause> override, hydra.ext.org.ansi.sql.syntax.ContextuallyTypedTableValueConstructor values) {
    java.util.Objects.requireNonNull((columns));
    java.util.Objects.requireNonNull((override));
    java.util.Objects.requireNonNull((values));
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
  
  public FromConstructor withColumns(hydra.util.Opt<hydra.ext.org.ansi.sql.syntax.InsertColumnList> columns) {
    java.util.Objects.requireNonNull((columns));
    return new FromConstructor(columns, override, values);
  }
  
  public FromConstructor withOverride(hydra.util.Opt<hydra.ext.org.ansi.sql.syntax.OverrideClause> override) {
    java.util.Objects.requireNonNull((override));
    return new FromConstructor(columns, override, values);
  }
  
  public FromConstructor withValues(hydra.ext.org.ansi.sql.syntax.ContextuallyTypedTableValueConstructor values) {
    java.util.Objects.requireNonNull((values));
    return new FromConstructor(columns, override, values);
  }
}