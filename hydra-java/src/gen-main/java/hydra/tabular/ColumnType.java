// Note: this is an automatically generated file. Do not edit.

package hydra.tabular;

import java.io.Serializable;

/**
 * A column type, consisting of a name and a value type
 */
public class ColumnType implements Serializable, Comparable<ColumnType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.tabular.ColumnType");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public final hydra.relational.ColumnName name;
  
  public final hydra.core.Type type;
  
  public ColumnType (hydra.relational.ColumnName name, hydra.core.Type type) {
    this.name = name;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ColumnType)) {
      return false;
    }
    ColumnType o = (ColumnType) (other);
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
  public int compareTo(ColumnType other) {
    int cmp = 0;
    cmp = ((Comparable) (name)).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (type)).compareTo(other.type);
  }
  
  public ColumnType withName(hydra.relational.ColumnName name) {
    return new ColumnType(name, type);
  }
  
  public ColumnType withType(hydra.core.Type type) {
    return new ColumnType(name, type);
  }
}
