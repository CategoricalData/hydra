package hydra.ext.java.syntax;

public class EnumBody {
  public final java.util.List<hydra.ext.java.syntax.EnumBody_Element> value;
  
  public EnumBody (java.util.List<hydra.ext.java.syntax.EnumBody_Element> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumBody)) {
      return false;
    }
    EnumBody o = (EnumBody) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}