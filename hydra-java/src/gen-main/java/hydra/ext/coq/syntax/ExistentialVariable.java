package hydra.ext.coq.syntax;

public class ExistentialVariable {
  public final Ident ident;
  
  public final ExistentialVariableVariant variant;
  
  public ExistentialVariable (Ident ident, ExistentialVariableVariant variant) {
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
  
  public ExistentialVariable withIdent(Ident ident) {
    return new ExistentialVariable(ident, variant);
  }
  
  public ExistentialVariable withVariant(ExistentialVariableVariant variant) {
    return new ExistentialVariable(ident, variant);
  }
}