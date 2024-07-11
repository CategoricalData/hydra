// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for property map functions.
 */
public class MapFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.MapFeatures");
  
  /**
   * Whether to expect the keys() function.
   */
  public final Boolean keys;
  
  public MapFeatures (Boolean keys) {
    java.util.Objects.requireNonNull((keys));
    this.keys = keys;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MapFeatures)) {
      return false;
    }
    MapFeatures o = (MapFeatures) (other);
    return keys.equals(o.keys);
  }
  
  @Override
  public int hashCode() {
    return 2 * keys.hashCode();
  }
}