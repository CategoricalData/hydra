package hydra.ext.coq.syntax;

public class ScopeKey {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.ScopeKey");
  
  public final hydra.ext.coq.syntax.Ident value;
  
  public ScopeKey (hydra.ext.coq.syntax.Ident value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ScopeKey)) {
      return false;
    }
    ScopeKey o = (ScopeKey) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}