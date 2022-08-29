package hydra.ext.java.syntax;

public class LocalVariableDeclarationStatement {
  public final hydra.ext.java.syntax.LocalVariableDeclaration value;
  
  public LocalVariableDeclarationStatement (hydra.ext.java.syntax.LocalVariableDeclaration value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LocalVariableDeclarationStatement)) {
      return false;
    }
    LocalVariableDeclarationStatement o = (LocalVariableDeclarationStatement) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}