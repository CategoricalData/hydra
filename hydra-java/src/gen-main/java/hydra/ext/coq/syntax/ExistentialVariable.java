package hydra.ext.coq.syntax;

public class ExistentialVariable {
  public final hydra.ext.coq.syntax.Ident ident;
  
  public final hydra.ext.coq.syntax.ExistentialVariableVariant variant;
  
  public ExistentialVariable (hydra.ext.coq.syntax.Ident ident, hydra.ext.coq.syntax.ExistentialVariableVariant variant) {
    this.ident = ident;
    this.variant = variant;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExistentialVariable)) {
      return false;
    }
    ExistentialVariable o = (ExistentialVariable) (other);
    return ident.equals(o.ident) && variant.equals(o.variant);
  }
  
  @Override
  public int hashCode() {
    return 2 * ident.hashCode() + 3 * variant.hashCode();
  }
  
  public ExistentialVariable withIdent(hydra.ext.coq.syntax.Ident ident) {
    return new ExistentialVariable(ident, variant);
  }
  
  public ExistentialVariable withVariant(hydra.ext.coq.syntax.ExistentialVariableVariant variant) {
    return new ExistentialVariable(ident, variant);
  }
}