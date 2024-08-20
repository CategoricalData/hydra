// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.geojson.model;

import java.io.Serializable;

/**
 * A GeoJSON object with the type "FeatureCollection" is a FeatureCollection object.  A FeatureCollection object has a member with the name "features".  The value of "features" is a JSON array. Each element of the array is a Feature object as defined above.  It is possible for this array to be empty.
 */
public class FeatureCollection implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/geojson/model.FeatureCollection");
  
  public static final hydra.core.Name FIELD_NAME_FEATURES = new hydra.core.Name("features");
  
  public static final hydra.core.Name FIELD_NAME_BBOX = new hydra.core.Name("bbox");
  
  public final java.util.List<hydra.ext.org.geojson.model.Feature> features;
  
  public final hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox;
  
  public FeatureCollection (java.util.List<hydra.ext.org.geojson.model.Feature> features, hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox) {
    java.util.Objects.requireNonNull((features));
    java.util.Objects.requireNonNull((bbox));
    this.features = features;
    this.bbox = bbox;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FeatureCollection)) {
      return false;
    }
    FeatureCollection o = (FeatureCollection) (other);
    return features.equals(o.features) && bbox.equals(o.bbox);
  }
  
  @Override
  public int hashCode() {
    return 2 * features.hashCode() + 3 * bbox.hashCode();
  }
  
  public FeatureCollection withFeatures(java.util.List<hydra.ext.org.geojson.model.Feature> features) {
    java.util.Objects.requireNonNull((features));
    return new FeatureCollection(features, bbox);
  }
  
  public FeatureCollection withBbox(hydra.util.Opt<hydra.ext.org.geojson.model.BoundingBox> bbox) {
    java.util.Objects.requireNonNull((bbox));
    return new FeatureCollection(features, bbox);
  }
}