// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class LocalVariableDeclarationStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.LocalVariableDeclarationStatement");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.java.syntax.LocalVariableDeclaration value;
  
  public LocalVariableDeclarationStatement (hydra.ext.java.syntax.LocalVariableDeclaration value) {
    java.util.Objects.requireNonNull((value));
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