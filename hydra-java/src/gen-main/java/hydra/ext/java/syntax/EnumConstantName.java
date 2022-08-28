package hydra.ext.java.syntax;

public class EnumConstantName {
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