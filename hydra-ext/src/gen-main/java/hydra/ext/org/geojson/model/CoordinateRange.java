// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.geojson.model;

import java.io.Serializable;

public class CoordinateRange implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/geojson/model.CoordinateRange");
  
  public static final hydra.core.Name FIELD_NAME_MIN = new hydra.core.Name("min");
  
  public static final hydra.core.Name FIELD_NAME_MAX = new hydra.core.Name("max");
  
  public final Double min;
  
  public final Double max;
  
  public CoordinateRange (Double min, Double max) {
    java.util.Objects.requireNonNull((min));
    java.util.Objects.requireNonNull((max));
    this.min = min;
    this.max = max;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CoordinateRange)) {
      return false;
    }
    CoordinateRange o = (CoordinateRange) (other);
    return min.equals(o.min) && max.equals(o.max);
  }
  
  @Override
  public int hashCode() {
    return 2 * min.hashCode() + 3 * max.hashCode();
  }
  
  public CoordinateRange withMin(Double min) {
    java.util.Objects.requireNonNull((min));
    return new CoordinateRange(min, max);
  }
  
  public CoordinateRange withMax(Double max) {
    java.util.Objects.requireNonNull((max));
    return new CoordinateRange(min, max);
  }
}