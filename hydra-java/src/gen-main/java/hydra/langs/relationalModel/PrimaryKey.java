// Note: this is an automatically generated file. Do not edit.

package hydra.langs.relationalModel;

import java.io.Serializable;

/**
 * A primary key of a relation, specified either as a single column, or as a list of columns
 */
public class PrimaryKey implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/relationalModel.PrimaryKey");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.langs.relationalModel.ColumnName> value;
  
  public PrimaryKey (java.util.List<hydra.langs.relationalModel.ColumnName> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrimaryKey)) {
      return false;
    }
    PrimaryKey o = (PrimaryKey) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}