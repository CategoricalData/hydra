// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * An instance of a union type; i.e. a string-indexed generalization of inl() or inr()
 */
public class Injection implements Serializable, Comparable<Injection> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.Injection");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_NAME = new hydra.core.Name("typeName");
  
  public static final hydra.core.Name FIELD_NAME_FIELD = new hydra.core.Name("field");
  
  /**
   * The name of the union type
   */
  public final hydra.core.Name typeName;
  
  /**
   * The field being injected, including its name and value
   */
  public final hydra.core.Field field;
  
  public Injection (hydra.core.Name typeName, hydra.core.Field field) {
    this.typeName = typeName;
    this.field = field;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Injection)) {
      return false;
    }
    Injection o = (Injection) other;
    return java.util.Objects.equals(
      this.typeName,
      o.typeName) && java.util.Objects.equals(
      this.field,
      o.field);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeName) + 3 * java.util.Objects.hashCode(field);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Injection other) {
    int cmp = 0;
    cmp = ((Comparable) typeName).compareTo(other.typeName);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) field).compareTo(other.field);
  }
  
  public Injection withTypeName(hydra.core.Name typeName) {
    return new Injection(typeName, field);
  }
  
  public Injection withField(hydra.core.Field field) {
    return new Injection(typeName, field);
  }
}
