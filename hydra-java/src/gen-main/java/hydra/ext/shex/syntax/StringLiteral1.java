package hydra.ext.shex.syntax;

public class StringLiteral1 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.StringLiteral1");
  
  public final java.util.List<hydra.ext.shex.syntax.StringLiteral1_Elmt> value;
  
  public StringLiteral1 (java.util.List<hydra.ext.shex.syntax.StringLiteral1_Elmt> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringLiteral1)) {
      return false;
    }
    StringLiteral1 o = (StringLiteral1) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}