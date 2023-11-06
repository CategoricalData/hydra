package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class Namespace implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Namespace");
  
  public final java.util.List<String> value;
  
  public Namespace (java.util.List<String> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Namespace)) {
      return false;
    }
    Namespace o = (Namespace) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}