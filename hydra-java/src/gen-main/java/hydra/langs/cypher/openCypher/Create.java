package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class Create implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Create");
  
  public final java.util.List<hydra.langs.cypher.openCypher.PatternPart> value;
  
  public Create (java.util.List<hydra.langs.cypher.openCypher.PatternPart> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Create)) {
      return false;
    }
    Create o = (Create) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}