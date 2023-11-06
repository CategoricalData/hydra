package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class MapLiteral implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.MapLiteral");
  
  public final java.util.Map<hydra.langs.cypher.openCypher.PropertyKeyName, hydra.langs.cypher.openCypher.Expression> value;
  
  public MapLiteral (java.util.Map<hydra.langs.cypher.openCypher.PropertyKeyName, hydra.langs.cypher.openCypher.Expression> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MapLiteral)) {
      return false;
    }
    MapLiteral o = (MapLiteral) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}