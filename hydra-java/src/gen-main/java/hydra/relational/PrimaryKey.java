// Note: this is an automatically generated file. Do not edit.

package hydra.relational;

import java.io.Serializable;

/**
 * A primary key of a relation, specified either as a single column, or as a list of columns
 */
public class PrimaryKey implements Serializable, Comparable<PrimaryKey> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.relational.PrimaryKey");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.relational.ColumnName> value;
  
  public PrimaryKey (java.util.List<hydra.relational.ColumnName> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrimaryKey)) {
      return false;
    }
    PrimaryKey o = (PrimaryKey) (other);
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
  public int compareTo(PrimaryKey other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
