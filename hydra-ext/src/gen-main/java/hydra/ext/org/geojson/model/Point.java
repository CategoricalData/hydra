// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.geojson.model;

import java.io.Serializable;

/**
 * For type "Point", the "coordinates" member is a single position.
 */
public class Point implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/geojson/model.Point");
  
  public static final hydra.core.Name FIELD_NAME_COORDINATES = new hydra.core.Name("coordinates");
  
  public static final hydra.core.Name FIELD_NAME_BBOX = new hydra.core.Name("bbox");
  
  public final hydra.ext.org.geojson.model.Position coordinates;
  
  public final hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox;
  
  public Point (hydra.ext.org.geojson.model.Position coordinates, hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox) {
    java.util.Objects.requireNonNull((coordinates));
    java.util.Objects.requireNonNull((bbox));
    this.coordinates = coordinates;
    this.bbox = bbox;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Point)) {
      return false;
    }
    Point o = (Point) (other);
    return coordinates.equals(o.coordinates) && bbox.equals(o.bbox);
  }
  
  @Override
  public int hashCode() {
    return 2 * coordinates.hashCode() + 3 * bbox.hashCode();
  }
  
  public Point withCoordinates(hydra.ext.org.geojson.model.Position coordinates) {
    java.util.Objects.requireNonNull((coordinates));
    return new Point(coordinates, bbox);
  }
  
  public Point withBbox(hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox) {
    java.util.Objects.requireNonNull((bbox));
    return new Point(coordinates, bbox);
  }
}