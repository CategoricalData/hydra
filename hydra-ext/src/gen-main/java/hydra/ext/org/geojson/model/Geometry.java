// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.geojson.model;

import java.io.Serializable;

/**
 * A Geometry object represents points, curves, and surfaces in coordinate space.  Every Geometry object is a GeoJSON object no matter where it occurs in a GeoJSON text.
 */
public abstract class Geometry implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/geojson/model.Geometry");
  
  public static final hydra.core.Name FIELD_NAME_POINT = new hydra.core.Name("point");
  
  public static final hydra.core.Name FIELD_NAME_MULTI_POINT = new hydra.core.Name("multiPoint");
  
  public static final hydra.core.Name FIELD_NAME_LINE_STRING = new hydra.core.Name("lineString");
  
  public static final hydra.core.Name FIELD_NAME_MULTI_LINE_STRING = new hydra.core.Name("multiLineString");
  
  public static final hydra.core.Name FIELD_NAME_POLYGON = new hydra.core.Name("polygon");
  
  public static final hydra.core.Name FIELD_NAME_MULTI_POLYGON = new hydra.core.Name("multiPolygon");
  
  public static final hydra.core.Name FIELD_NAME_GEOMETRY_COLLECTION = new hydra.core.Name("geometryCollection");
  
  private Geometry () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Point instance) ;
    
    R visit(MultiPoint instance) ;
    
    R visit(LineString instance) ;
    
    R visit(MultiLineString instance) ;
    
    R visit(Polygon instance) ;
    
    R visit(MultiPolygon instance) ;
    
    R visit(GeometryCollection instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Geometry instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Point instance) {
      return otherwise((instance));
    }
    
    default R visit(MultiPoint instance) {
      return otherwise((instance));
    }
    
    default R visit(LineString instance) {
      return otherwise((instance));
    }
    
    default R visit(MultiLineString instance) {
      return otherwise((instance));
    }
    
    default R visit(Polygon instance) {
      return otherwise((instance));
    }
    
    default R visit(MultiPolygon instance) {
      return otherwise((instance));
    }
    
    default R visit(GeometryCollection instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Point extends hydra.ext.org.geojson.model.Geometry implements Serializable {
    public final hydra.ext.org.geojson.model.Point value;
    
    public Point (hydra.ext.org.geojson.model.Point value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Point)) {
        return false;
      }
      Point o = (Point) (other);
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
  
  public static final class MultiPoint extends hydra.ext.org.geojson.model.Geometry implements Serializable {
    public final hydra.ext.org.geojson.model.MultiPoint value;
    
    public MultiPoint (hydra.ext.org.geojson.model.MultiPoint value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultiPoint)) {
        return false;
      }
      MultiPoint o = (MultiPoint) (other);
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
  
  public static final class LineString extends hydra.ext.org.geojson.model.Geometry implements Serializable {
    public final hydra.ext.org.geojson.model.LineString value;
    
    public LineString (hydra.ext.org.geojson.model.LineString value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LineString)) {
        return false;
      }
      LineString o = (LineString) (other);
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
  
  public static final class MultiLineString extends hydra.ext.org.geojson.model.Geometry implements Serializable {
    public final hydra.ext.org.geojson.model.MultiLineString value;
    
    public MultiLineString (hydra.ext.org.geojson.model.MultiLineString value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultiLineString)) {
        return false;
      }
      MultiLineString o = (MultiLineString) (other);
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
  
  public static final class Polygon extends hydra.ext.org.geojson.model.Geometry implements Serializable {
    public final hydra.ext.org.geojson.model.Polygon value;
    
    public Polygon (hydra.ext.org.geojson.model.Polygon value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Polygon)) {
        return false;
      }
      Polygon o = (Polygon) (other);
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
  
  public static final class MultiPolygon extends hydra.ext.org.geojson.model.Geometry implements Serializable {
    public final hydra.ext.org.geojson.model.MultiPolygon value;
    
    public MultiPolygon (hydra.ext.org.geojson.model.MultiPolygon value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultiPolygon)) {
        return false;
      }
      MultiPolygon o = (MultiPolygon) (other);
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
  
  public static final class GeometryCollection extends hydra.ext.org.geojson.model.Geometry implements Serializable {
    public final hydra.ext.org.geojson.model.GeometryCollection value;
    
    public GeometryCollection (hydra.ext.org.geojson.model.GeometryCollection value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GeometryCollection)) {
        return false;
      }
      GeometryCollection o = (GeometryCollection) (other);
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