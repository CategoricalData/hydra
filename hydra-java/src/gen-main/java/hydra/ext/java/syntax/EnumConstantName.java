package hydra.ext.java.syntax;

public class EnumConstantName {
  public final Identifier value;
  
  public EnumConstantName (Identifier value) {
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