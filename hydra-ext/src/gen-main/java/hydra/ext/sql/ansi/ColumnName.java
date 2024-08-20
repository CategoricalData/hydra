// Note: this is an automatically generated file. Do not edit.

package hydra.ext.sql.ansi;

import java.io.Serializable;

public class ColumnName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/sql/ansi.ColumnName");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public ColumnName (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ColumnName)) {
      return false;
    }
    ColumnName o = (ColumnName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
