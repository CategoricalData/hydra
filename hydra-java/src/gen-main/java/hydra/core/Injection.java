// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * An instance of a union type; i.e. a string-indexed generalization of inl() or inr()
 */
public class Injection implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/core.Injection");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_NAME = new hydra.core.Name("typeName");
  
  public static final hydra.core.Name FIELD_NAME_FIELD = new hydra.core.Name("field");
  
  public final hydra.core.Name typeName;
  
  public final hydra.core.Field field;
  
  public Injection (hydra.core.Name typeName, hydra.core.Field field) {
    java.util.Objects.requireNonNull((typeName));
    java.util.Objects.requireNonNull((field));
    this.typeName = typeName;
    this.field = field;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Injection)) {
      return false;
    }
    Injection o = (Injection) (other);
    return typeName.equals(o.typeName) && field.equals(o.field);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * field.hashCode();
  }
  
  public Injection withTypeName(hydra.core.Name typeName) {
    java.util.Objects.requireNonNull((typeName));
    return new Injection(typeName, field);
  }
  
  public Injection withField(hydra.core.Field field) {
    java.util.Objects.requireNonNull((field));
    return new Injection(typeName, field);
  }
}