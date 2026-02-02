// Note: this is an automatically generated file. Do not edit.

package hydra.tabular;

import java.io.Serializable;

/**
 * A column type, consisting of a name and a value type
 */
public class ColumnType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.tabular.ColumnType");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public final hydra.relational.ColumnName name;
  
  public final hydra.core.Type type;
  
  public ColumnType (hydra.relational.ColumnName name, hydra.core.Type type) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((type));
    this.name = name;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ColumnType)) {
      return false;
    }
    ColumnType o = (ColumnType) (other);
    return name.equals(o.name) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * type.hashCode();
  }
  
  public ColumnType withName(hydra.relational.ColumnName name) {
    java.util.Objects.requireNonNull((name));
    return new ColumnType(name, type);
  }
  
  public ColumnType withType(hydra.core.Type type) {
    java.util.Objects.requireNonNull((type));
    return new ColumnType(name, type);
  }
}
