// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A term applied to a type; a type application
 */
public class TypeApplicationTerm implements Serializable, Comparable<TypeApplicationTerm> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.TypeApplicationTerm");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  /**
   * The term being applied to a type
   */
  public final hydra.core.Term body;
  
  /**
   * The type argument
   */
  public final hydra.core.Type type;
  
  public TypeApplicationTerm (hydra.core.Term body, hydra.core.Type type) {
    this.body = body;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeApplicationTerm)) {
      return false;
    }
    TypeApplicationTerm o = (TypeApplicationTerm) other;
    return java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.type,
      o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(body) + 3 * java.util.Objects.hashCode(type);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeApplicationTerm other) {
    int cmp = 0;
    cmp = ((Comparable) body).compareTo(other.body);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) type).compareTo(other.type);
  }
  
  public TypeApplicationTerm withBody(hydra.core.Term body) {
    return new TypeApplicationTerm(body, type);
  }
  
  public TypeApplicationTerm withType(hydra.core.Type type) {
    return new TypeApplicationTerm(body, type);
  }
}
