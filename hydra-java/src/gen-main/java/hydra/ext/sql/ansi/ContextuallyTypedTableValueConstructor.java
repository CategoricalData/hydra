package hydra.ext.sql.ansi;

public class ContextuallyTypedTableValueConstructor {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.ContextuallyTypedTableValueConstructor");
  
  public final hydra.ext.sql.ansi.ContextuallyTypedRowValueExpressionList value;
  
  public ContextuallyTypedTableValueConstructor (hydra.ext.sql.ansi.ContextuallyTypedRowValueExpressionList value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ContextuallyTypedTableValueConstructor)) {
      return false;
    }
    ContextuallyTypedTableValueConstructor o = (ContextuallyTypedTableValueConstructor) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}