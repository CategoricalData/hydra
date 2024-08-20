// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.geojson.model;

import java.io.Serializable;

/**
 * A GeoJSON object represents a Geometry, Feature, or collection of Features.
 */
public abstract class Object_ implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/geojson/model.Object");
  
  public static final hydra.core.Name FIELD_NAME_GEOMETRY = new hydra.core.Name("geometry");
  
  public static final hydra.core.Name FIELD_NAME_FEATURE = new hydra.core.Name("feature");
  
  public static final hydra.core.Name FIELD_NAME_FEATURE_COLLECTION = new hydra.core.Name("featureCollection");
  
  private Object_ () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Geometry instance) ;
    
    R visit(Feature instance) ;
    
    R visit(FeatureCollection instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Object_ instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Geometry instance) {
      return otherwise((instance));
    }
    
    default R visit(Feature instance) {
      return otherwise((instance));
    }
    
    default R visit(FeatureCollection instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Geometry extends hydra.ext.org.geojson.model.Object_ implements Serializable {
    public final hydra.ext.org.geojson.model.Geometry value;
    
    public Geometry (hydra.ext.org.geojson.model.Geometry value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Geometry)) {
        return false;
      }
      Geometry o = (Geometry) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Feature extends hydra.ext.org.geojson.model.Object_ implements Serializable {
    public final hydra.ext.org.geojson.model.Feature value;
    
    public Feature (hydra.ext.org.geojson.model.Feature value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Feature)) {
        return false;
      }
      Feature o = (Feature) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class FeatureCollection extends hydra.ext.org.geojson.model.Object_ implements Serializable {
    public final hydra.ext.org.geojson.model.FeatureCollection value;
    
    public FeatureCollection (hydra.ext.org.geojson.model.FeatureCollection value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FeatureCollection)) {
        return false;
      }
      FeatureCollection o = (FeatureCollection) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}