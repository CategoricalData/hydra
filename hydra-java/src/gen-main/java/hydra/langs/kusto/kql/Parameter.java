package hydra.langs.kusto.kql;

import java.io.Serializable;

public class Parameter implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.Parameter");
  
  public final String key;
  
  public final hydra.langs.kusto.kql.Literal value;
  
  public Parameter (String key, hydra.langs.kusto.kql.Literal value) {
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
    return new Parameter(key, value);
  }
  
  public Parameter withValue(hydra.langs.kusto.kql.Literal value) {
    return new Parameter(key, value);
  }
}