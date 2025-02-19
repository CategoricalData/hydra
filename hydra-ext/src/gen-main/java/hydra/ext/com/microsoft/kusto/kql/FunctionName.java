// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.kusto.kql;

import java.io.Serializable;

public class FunctionName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.kusto.kql.FunctionName");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public FunctionName (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionName)) {
      return false;
    }
    FunctionName o = (FunctionName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}