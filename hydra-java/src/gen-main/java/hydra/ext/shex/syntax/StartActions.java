package hydra.ext.shex.syntax;

public class StartActions {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.StartActions");
  
  public final java.util.List<hydra.ext.shex.syntax.CodeDecl> value;
  
  public StartActions (java.util.List<hydra.ext.shex.syntax.CodeDecl> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StartActions)) {
      return false;
    }
    StartActions o = (StartActions) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}