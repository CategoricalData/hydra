// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.geojson.model;

import java.io.Serializable;

/**
 * A GeoJSON object with type "GeometryCollection" is a Geometry object. A GeometryCollection has a member with the name "geometries".  The value of "geometries" is an array.  Each element of this array is a GeoJSON Geometry object.  It is possible for this array to be empty.
 */
public class GeometryCollection implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.geojson.model.GeometryCollection");
  
  public static final hydra.core.Name FIELD_NAME_GEOMETRIES = new hydra.core.Name("geometries");
  
  public static final hydra.core.Name FIELD_NAME_BBOX = new hydra.core.Name("bbox");
  
  public final java.util.List<hydra.ext.org.geojson.model.Geometry> geometries;
  
  public final hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox;
  
  public GeometryCollection (java.util.List<hydra.ext.org.geojson.model.Geometry> geometries, hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox) {
    java.util.Objects.requireNonNull((geometries));
    java.util.Objects.requireNonNull((bbox));
    this.geometries = geometries;
    this.bbox = bbox;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GeometryCollection)) {
      return false;
    }
    GeometryCollection o = (GeometryCollection) (other);
    return geometries.equals(o.geometries) && bbox.equals(o.bbox);
  }
  
  @Override
  public int hashCode() {
    return 2 * geometries.hashCode() + 3 * bbox.hashCode();
  }
  
  public GeometryCollection withGeometries(java.util.List<hydra.ext.org.geojson.model.Geometry> geometries) {
    java.util.Objects.requireNonNull((geometries));
    return new GeometryCollection(geometries, bbox);
  }
  
  public GeometryCollection withBbox(hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox) {
    java.util.Objects.requireNonNull((bbox));
    return new GeometryCollection(geometries, bbox);
  }
}