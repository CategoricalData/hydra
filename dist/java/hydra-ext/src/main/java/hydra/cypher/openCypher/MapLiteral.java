// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class MapLiteral implements Serializable, Comparable<MapLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.MapLiteral");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.cypher.openCypher.KeyValuePair> value;

  public MapLiteral (java.util.List<hydra.cypher.openCypher.KeyValuePair> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MapLiteral)) {
      return false;
    }
    MapLiteral o = (MapLiteral) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MapLiteral other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
