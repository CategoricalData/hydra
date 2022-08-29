package hydra.ext.java.syntax;

public class ReturnStatement {
  public final java.util.Optional<hydra.ext.java.syntax.Expression> value;
  
  public ReturnStatement (java.util.Optional<hydra.ext.java.syntax.Expression> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReturnStatement)) {
      return false;
    }
    ReturnStatement o = (ReturnStatement) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}