// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.geojson.model;

import java.io.Serializable;

/**
 * For type "LineString", the "coordinates" member is an array of two or more positions.
 */
public class LineString implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/geojson/model.LineString");
  
  public static final hydra.core.Name FIELD_NAME_COORDINATES = new hydra.core.Name("coordinates");
  
  public static final hydra.core.Name FIELD_NAME_BBOX = new hydra.core.Name("bbox");
  
  public final java.util.List<hydra.ext.org.geojson.model.Position> coordinates;
  
  public final hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox;
  
  public LineString (java.util.List<hydra.ext.org.geojson.model.Position> coordinates, hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox) {
    java.util.Objects.requireNonNull((coordinates));
    java.util.Objects.requireNonNull((bbox));
    this.coordinates = coordinates;
    this.bbox = bbox;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LineString)) {
      return false;
    }
    LineString o = (LineString) (other);
    return coordinates.equals(o.coordinates) && bbox.equals(o.bbox);
  }
  
  @Override
  public int hashCode() {
    return 2 * coordinates.hashCode() + 3 * bbox.hashCode();
  }
  
  public LineString withCoordinates(java.util.List<hydra.ext.org.geojson.model.Position> coordinates) {
    java.util.Objects.requireNonNull((coordinates));
    return new LineString(coordinates, bbox);
  }
  
  public LineString withBbox(hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox) {
    java.util.Objects.requireNonNull((bbox));
    return new LineString(coordinates, bbox);
  }
}