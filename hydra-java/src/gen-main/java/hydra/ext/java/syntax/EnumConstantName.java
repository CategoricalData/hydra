package hydra.ext.java.syntax;

public class EnumConstantName {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.EnumConstantName");
  
  public final hydra.ext.java.syntax.Identifier value;
  
  public EnumConstantName (hydra.ext.java.syntax.Identifier value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumConstantName)) {
      return false;
    }
    EnumConstantName o = (EnumConstantName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}