package hydra.ext.java.syntax;

public class ClassBody {
  public final java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments> value;
  
  public ClassBody (java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClassBody)) {
      return false;
    }
    ClassBody o = (ClassBody) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}