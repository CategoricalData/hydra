package hydra.langs.java.syntax;

public class ContinueStatement {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.ContinueStatement");
  
  public final java.util.Optional<hydra.langs.java.syntax.Identifier> value;
  
  public ContinueStatement (java.util.Optional<hydra.langs.java.syntax.Identifier> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ContinueStatement)) {
      return false;
    }
    ContinueStatement o = (ContinueStatement) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}