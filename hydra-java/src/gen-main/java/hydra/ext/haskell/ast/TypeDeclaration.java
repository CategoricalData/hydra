// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A type synonym declaration
 */
public class TypeDeclaration implements Serializable, Comparable<TypeDeclaration> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.TypeDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  /**
   * The declaration head
   */
  public final hydra.ext.haskell.ast.DeclarationHead name;
  
  /**
   * The type being defined
   */
  public final hydra.ext.haskell.ast.Type type;
  
  public TypeDeclaration (hydra.ext.haskell.ast.DeclarationHead name, hydra.ext.haskell.ast.Type type) {
    this.name = name;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeDeclaration)) {
      return false;
    }
    TypeDeclaration o = (TypeDeclaration) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.type,
      o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(type);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeDeclaration other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) type).compareTo(other.type);
  }
  
  public TypeDeclaration withName(hydra.ext.haskell.ast.DeclarationHead name) {
    return new TypeDeclaration(name, type);
  }
  
  public TypeDeclaration withType(hydra.ext.haskell.ast.Type type) {
    return new TypeDeclaration(name, type);
  }
}
