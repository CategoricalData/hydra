// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.geojson.model;

import java.io.Serializable;

/**
 * A Feature object represents a spatially bounded thing.  Every Feature object is a GeoJSON object no matter where it occurs in a GeoJSON text.
 */
public class Feature implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.geojson.model.Feature");
  
  public static final hydra.core.Name FIELD_NAME_GEOMETRY = new hydra.core.Name("geometry");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_BBOX = new hydra.core.Name("bbox");
  
  /**
   * A Feature object has a member with the name "geometry".  The value of the geometry member SHALL be either a Geometry object as defined above or, in the case that the Feature is unlocated, a JSON null value.
   */
  public final hydra.util.Opt<hydra.ext.org.geojson.model.Geometry> geometry;
  
  /**
   * A Feature object has a member with the name "properties".  The value of the properties member is an object (any JSON object or a JSON null value).
   */
  public final hydra.util.Opt<java.util.Map<String, hydra.json.Value>> properties;
  
  /**
   * If a Feature has a commonly used identifier, that identifier SHOULD be included as a member of the Feature object with the name "id", and the value of this member is either a JSON string or number.
   */
  public final hydra.util.Opt<hydra.ext.org.geojson.model.Id> id;
  
  public final hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox;
  
  public Feature (hydra.util.Opt<hydra.ext.org.geojson.model.Geometry> geometry, hydra.util.Opt<java.util.Map<String, hydra.json.Value>> properties, hydra.util.Opt<hydra.ext.org.geojson.model.Id> id, hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox) {
    java.util.Objects.requireNonNull((geometry));
    java.util.Objects.requireNonNull((properties));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((bbox));
    this.geometry = geometry;
    this.properties = properties;
    this.id = id;
    this.bbox = bbox;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Feature)) {
      return false;
    }
    Feature o = (Feature) (other);
    return geometry.equals(o.geometry) && properties.equals(o.properties) && id.equals(o.id) && bbox.equals(o.bbox);
  }
  
  @Override
  public int hashCode() {
    return 2 * geometry.hashCode() + 3 * properties.hashCode() + 5 * id.hashCode() + 7 * bbox.hashCode();
  }
  
  public Feature withGeometry(hydra.util.Opt<hydra.ext.org.geojson.model.Geometry> geometry) {
    java.util.Objects.requireNonNull((geometry));
    return new Feature(geometry, properties, id, bbox);
  }
  
  public Feature withProperties(hydra.util.Opt<java.util.Map<String, hydra.json.Value>> properties) {
    java.util.Objects.requireNonNull((properties));
    return new Feature(geometry, properties, id, bbox);
  }
  
  public Feature withId(hydra.util.Opt<hydra.ext.org.geojson.model.Id> id) {
    java.util.Objects.requireNonNull((id));
    return new Feature(geometry, properties, id, bbox);
  }
  
  public Feature withBbox(hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox) {
    java.util.Objects.requireNonNull((bbox));
    return new Feature(geometry, properties, id, bbox);
  }
}