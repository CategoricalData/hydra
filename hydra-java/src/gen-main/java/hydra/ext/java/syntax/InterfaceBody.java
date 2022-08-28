package hydra.ext.java.syntax;

public class InterfaceBody {
  public final java.util.List<hydra.ext.java.syntax.InterfaceMemberDeclaration> value;
  
  public InterfaceBody (java.util.List<hydra.ext.java.syntax.InterfaceMemberDeclaration> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InterfaceBody)) {
      return false;
    }
    InterfaceBody o = (InterfaceBody) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}