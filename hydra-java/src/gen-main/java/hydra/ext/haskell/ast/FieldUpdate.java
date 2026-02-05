// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A field name and value
 */
public class FieldUpdate implements Serializable, Comparable<FieldUpdate> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.FieldUpdate");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  /**
   * The field name
   */
  public final hydra.ext.haskell.ast.Name name;
  
  /**
   * The field value
   */
  public final hydra.ext.haskell.ast.Expression value;
  
  public FieldUpdate (hydra.ext.haskell.ast.Name name, hydra.ext.haskell.ast.Expression value) {
    this.name = name;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldUpdate)) {
      return false;
    }
    FieldUpdate o = (FieldUpdate) (other);
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FieldUpdate other) {
    int cmp = 0;
    cmp = ((Comparable) (name)).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (value)).compareTo(other.value);
  }
  
  public FieldUpdate withName(hydra.ext.haskell.ast.Name name) {
    return new FieldUpdate(name, value);
  }
  
  public FieldUpdate withValue(hydra.ext.haskell.ast.Expression value) {
    return new FieldUpdate(name, value);
  }
}
