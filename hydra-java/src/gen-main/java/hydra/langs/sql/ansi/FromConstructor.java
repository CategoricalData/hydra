// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class FromConstructor implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.FromConstructor");
  
  public final hydra.util.Opt<hydra.langs.sql.ansi.InsertColumnList> columns;
  
  public final hydra.util.Opt<hydra.langs.sql.ansi.OverrideClause> override;
  
  public final hydra.langs.sql.ansi.ContextuallyTypedTableValueConstructor values;
  
  public FromConstructor (hydra.util.Opt<hydra.langs.sql.ansi.InsertColumnList> columns, hydra.util.Opt<hydra.langs.sql.ansi.OverrideClause> override, hydra.langs.sql.ansi.ContextuallyTypedTableValueConstructor values) {
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
  
  public FromConstructor withColumns(hydra.util.Opt<hydra.langs.sql.ansi.InsertColumnList> columns) {
    java.util.Objects.requireNonNull((columns));
    return new FromConstructor(columns, override, values);
  }
  
  public FromConstructor withOverride(hydra.util.Opt<hydra.langs.sql.ansi.OverrideClause> override) {
    java.util.Objects.requireNonNull((override));
    return new FromConstructor(columns, override, values);
  }
  
  public FromConstructor withValues(hydra.langs.sql.ansi.ContextuallyTypedTableValueConstructor values) {
    java.util.Objects.requireNonNull((values));
    return new FromConstructor(columns, override, values);
  }
}