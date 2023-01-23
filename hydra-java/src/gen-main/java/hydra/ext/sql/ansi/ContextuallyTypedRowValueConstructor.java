package hydra.ext.sql.ansi;

public class ContextuallyTypedRowValueConstructor {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.ContextuallyTypedRowValueConstructor");
  
  public ContextuallyTypedRowValueConstructor () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ContextuallyTypedRowValueConstructor)) {
      return false;
    }
    ContextuallyTypedRowValueConstructor o = (ContextuallyTypedRowValueConstructor) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}