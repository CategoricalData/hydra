package hydra.ext.coq.syntax;

public class FieldIdent {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.FieldIdent");
  
  public final hydra.ext.coq.syntax.Ident value;
  
  public FieldIdent (hydra.ext.coq.syntax.Ident value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldIdent)) {
      return false;
    }
    FieldIdent o = (FieldIdent) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}