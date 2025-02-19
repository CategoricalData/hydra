// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.geojson.model;

import java.io.Serializable;

/**
 * For type "MultiPoint", the "coordinates" member is an array of positions.
 */
public class MultiPoint implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.geojson.model.MultiPoint");
  
  public static final hydra.core.Name FIELD_NAME_COORDINATES = new hydra.core.Name("coordinates");
  
  public static final hydra.core.Name FIELD_NAME_BBOX = new hydra.core.Name("bbox");
  
  public final java.util.List<hydra.ext.org.geojson.model.Point> coordinates;
  
  public final hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox;
  
  public MultiPoint (java.util.List<hydra.ext.org.geojson.model.Point> coordinates, hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox) {
    java.util.Objects.requireNonNull((coordinates));
    java.util.Objects.requireNonNull((bbox));
    this.coordinates = coordinates;
    this.bbox = bbox;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiPoint)) {
      return false;
    }
    MultiPoint o = (MultiPoint) (other);
    return coordinates.equals(o.coordinates) && bbox.equals(o.bbox);
  }
  
  @Override
  public int hashCode() {
    return 2 * coordinates.hashCode() + 3 * bbox.hashCode();
  }
  
  public MultiPoint withCoordinates(java.util.List<hydra.ext.org.geojson.model.Point> coordinates) {
    java.util.Objects.requireNonNull((coordinates));
    return new MultiPoint(coordinates, bbox);
  }
  
  public MultiPoint withBbox(hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox) {
    java.util.Objects.requireNonNull((bbox));
    return new MultiPoint(coordinates, bbox);
  }
}