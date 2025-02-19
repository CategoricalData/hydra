// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.geojson.model;

import java.io.Serializable;

/**
 * For type "Polygon", the "coordinates" member MUST be an array of linear ring coordinate arrays.
 * For Polygons with more than one of these rings, the first MUST be the exterior ring, and any others MUST be interior rings.  The exterior ring bounds the surface, and the interior rings (if present) bound holes within the surface.
 */
public class Polygon implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.geojson.model.Polygon");
  
  public static final hydra.core.Name FIELD_NAME_COORDINATES = new hydra.core.Name("coordinates");
  
  public static final hydra.core.Name FIELD_NAME_BBOX = new hydra.core.Name("bbox");
  
  public final java.util.List<hydra.ext.org.geojson.model.Position> coordinates;
  
  public final hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox;
  
  public Polygon (java.util.List<hydra.ext.org.geojson.model.Position> coordinates, hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox) {
    java.util.Objects.requireNonNull((coordinates));
    java.util.Objects.requireNonNull((bbox));
    this.coordinates = coordinates;
    this.bbox = bbox;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Polygon)) {
      return false;
    }
    Polygon o = (Polygon) (other);
    return coordinates.equals(o.coordinates) && bbox.equals(o.bbox);
  }
  
  @Override
  public int hashCode() {
    return 2 * coordinates.hashCode() + 3 * bbox.hashCode();
  }
  
  public Polygon withCoordinates(java.util.List<hydra.ext.org.geojson.model.Position> coordinates) {
    java.util.Objects.requireNonNull((coordinates));
    return new Polygon(coordinates, bbox);
  }
  
  public Polygon withBbox(hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox) {
    java.util.Objects.requireNonNull((bbox));
    return new Polygon(coordinates, bbox);
  }
}