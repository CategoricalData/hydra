// Note: this is an automatically generated file. Do not edit.

package hydra.relational;

import java.io.Serializable;

/**
 * A unique relation (table) name
 */
public class RelationName implements Serializable, Comparable<RelationName> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.relational.RelationName");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public RelationName (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationName)) {
      return false;
    }
    RelationName o = (RelationName) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RelationName other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
