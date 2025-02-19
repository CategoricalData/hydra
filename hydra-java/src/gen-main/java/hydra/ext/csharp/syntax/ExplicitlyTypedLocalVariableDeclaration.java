// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class ExplicitlyTypedLocalVariableDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ExplicitlyTypedLocalVariableDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATORS = new hydra.core.Name("declarators");
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final java.util.List<hydra.ext.csharp.syntax.ExplicitlyTypedLocalVariableDeclarator> declarators;
  
  public ExplicitlyTypedLocalVariableDeclaration (hydra.ext.csharp.syntax.Type type, java.util.List<hydra.ext.csharp.syntax.ExplicitlyTypedLocalVariableDeclarator> declarators) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((declarators));
    this.type = type;
    this.declarators = declarators;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExplicitlyTypedLocalVariableDeclaration)) {
      return false;
    }
    ExplicitlyTypedLocalVariableDeclaration o = (ExplicitlyTypedLocalVariableDeclaration) (other);
    return type.equals(o.type) && declarators.equals(o.declarators);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * declarators.hashCode();
  }
  
  public ExplicitlyTypedLocalVariableDeclaration withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new ExplicitlyTypedLocalVariableDeclaration(type, declarators);
  }
  
  public ExplicitlyTypedLocalVariableDeclaration withDeclarators(java.util.List<hydra.ext.csharp.syntax.ExplicitlyTypedLocalVariableDeclarator> declarators) {
    java.util.Objects.requireNonNull((declarators));
    return new ExplicitlyTypedLocalVariableDeclaration(type, declarators);
  }
}