// Note: this is an automatically generated file. Do not edit.

package hydra.ext.kusto.kql;

import java.io.Serializable;

public class Parameter implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/kusto/kql.Parameter");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String key;
  
  public final hydra.ext.kusto.kql.Literal value;
  
  public Parameter (String key, hydra.ext.kusto.kql.Literal value) {
    java.util.Objects.requireNonNull((key));
    java.util.Objects.requireNonNull((value));
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Parameter)) {
      return false;
    }
    Parameter o = (Parameter) (other);
    return key.equals(o.key) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode();
  }
  
  public Parameter withKey(String key) {
    java.util.Objects.requireNonNull((key));
    return new Parameter(key, value);
  }
  
  public Parameter withValue(hydra.ext.kusto.kql.Literal value) {
    java.util.Objects.requireNonNull((value));
    return new Parameter(key, value);
  }
}