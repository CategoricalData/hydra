// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class MapLiteral implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.MapLiteral");
  
  public final java.util.List<hydra.langs.cypher.openCypher.KeyValuePair> value;
  
  public MapLiteral (java.util.List<hydra.langs.cypher.openCypher.KeyValuePair> value) {
    java.util.Objects.requireNonNull((value));
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