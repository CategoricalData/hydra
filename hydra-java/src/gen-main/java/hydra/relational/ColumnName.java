// Note: this is an automatically generated file. Do not edit.

package hydra.relational;

import java.io.Serializable;

/**
 * A name for a domain which serves to identify the role played by that domain in the given relation; a 'role name' in Codd
 */
public class ColumnName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.relational.ColumnName");
  
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
