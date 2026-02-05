// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A type signature
 */
public class TypeSignature implements Serializable, Comparable<TypeSignature> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.TypeSignature");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  /**
   * The name being typed
   */
  public final hydra.ext.haskell.ast.Name name;
  
  /**
   * The type
   */
  public final hydra.ext.haskell.ast.Type type;
  
  public TypeSignature (hydra.ext.haskell.ast.Name name, hydra.ext.haskell.ast.Type type) {
    this.name = name;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeSignature)) {
      return false;
    }
    TypeSignature o = (TypeSignature) (other);
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
  public int compareTo(TypeSignature other) {
    int cmp = 0;
    cmp = ((Comparable) (name)).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (type)).compareTo(other.type);
  }
  
  public TypeSignature withName(hydra.ext.haskell.ast.Name name) {
    return new TypeSignature(name, type);
  }
  
  public TypeSignature withType(hydra.ext.haskell.ast.Type type) {
    return new TypeSignature(name, type);
  }
}
