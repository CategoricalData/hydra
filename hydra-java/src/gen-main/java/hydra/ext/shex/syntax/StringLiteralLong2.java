package hydra.ext.shex.syntax;

public class StringLiteralLong2 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.StringLiteralLong2");
  
  public final java.util.List<hydra.ext.shex.syntax.StringLiteralLong2_Elmt> value;
  
  public StringLiteralLong2 (java.util.List<hydra.ext.shex.syntax.StringLiteralLong2_Elmt> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringLiteralLong2)) {
      return false;
    }
    StringLiteralLong2 o = (StringLiteralLong2) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}