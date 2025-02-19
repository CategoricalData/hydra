// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class LocalConstantDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.LocalConstantDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATORS = new hydra.core.Name("declarators");
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final java.util.List<hydra.ext.csharp.syntax.ConstantDeclarator> declarators;
  
  public LocalConstantDeclaration (hydra.ext.csharp.syntax.Type type, java.util.List<hydra.ext.csharp.syntax.ConstantDeclarator> declarators) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((declarators));
    this.type = type;
    this.declarators = declarators;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LocalConstantDeclaration)) {
      return false;
    }
    LocalConstantDeclaration o = (LocalConstantDeclaration) (other);
    return type.equals(o.type) && declarators.equals(o.declarators);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * declarators.hashCode();
  }
  
  public LocalConstantDeclaration withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new LocalConstantDeclaration(type, declarators);
  }
  
  public LocalConstantDeclaration withDeclarators(java.util.List<hydra.ext.csharp.syntax.ConstantDeclarator> declarators) {
    java.util.Objects.requireNonNull((declarators));
    return new LocalConstantDeclaration(type, declarators);
  }
}