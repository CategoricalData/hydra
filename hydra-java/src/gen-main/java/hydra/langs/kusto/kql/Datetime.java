package hydra.langs.kusto.kql;

import java.io.Serializable;

public class Datetime implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.Datetime");
  
  public final String value;
  
  public Datetime (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Datetime)) {
      return false;
    }
    Datetime o = (Datetime) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}