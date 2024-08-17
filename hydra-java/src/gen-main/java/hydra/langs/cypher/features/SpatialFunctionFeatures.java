// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * Spatial functions
 */
public class SpatialFunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.SpatialFunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_POINT.DISTANCE = new hydra.core.Name("point.distance");
  
  public static final hydra.core.Name FIELD_NAME_POINT = new hydra.core.Name("point");
  
  public static final hydra.core.Name FIELD_NAME_POINT.WITHIN_B_BOX = new hydra.core.Name("point.withinBBox");
  
  /**
   * The point.distance() function. Returns a FLOAT representing the geodesic distance between any two points in the same CRS.
   */
  public final Boolean point_distance;
  
  /**
   * The point() function. Returns a 2D point object, given two coordinate values in the Cartesian coordinate system.; Returns a 3D point object, given three coordinate values in the Cartesian coordinate system.; Returns a 2D point object, given two coordinate values in the WGS 84 geographic coordinate system.; Returns a 3D point object, given three coordinate values in the WGS 84 geographic coordinate system.
   */
  public final Boolean point;
  
  /**
   * The point.withinBBox() function. Returns true if the provided point is within the bounding box defined by the two provided points, lowerLeft and upperRight.
   */
  public final Boolean point_withinBBox;
  
  public SpatialFunctionFeatures (Boolean point_distance, Boolean point, Boolean point_withinBBox) {
    java.util.Objects.requireNonNull((point_distance));
    java.util.Objects.requireNonNull((point));
    java.util.Objects.requireNonNull((point_withinBBox));
    this.point_distance = point_distance;
    this.point = point;
    this.point_withinBBox = point_withinBBox;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SpatialFunctionFeatures)) {
      return false;
    }
    SpatialFunctionFeatures o = (SpatialFunctionFeatures) (other);
    return point_distance.equals(o.point_distance) && point.equals(o.point) && point_withinBBox.equals(o.point_withinBBox);
  }
  
  @Override
  public int hashCode() {
    return 2 * point_distance.hashCode() + 3 * point.hashCode() + 5 * point_withinBBox.hashCode();
  }
  
  public SpatialFunctionFeatures withPoint.distance(Boolean point_distance) {
    java.util.Objects.requireNonNull((point_distance));
    return new SpatialFunctionFeatures(point_distance, point, point_withinBBox);
  }
  
  public SpatialFunctionFeatures withPoint(Boolean point) {
    java.util.Objects.requireNonNull((point));
    return new SpatialFunctionFeatures(point_distance, point, point_withinBBox);
  }
  
  public SpatialFunctionFeatures withPoint.withinBBox(Boolean point_withinBBox) {
    java.util.Objects.requireNonNull((point_withinBBox));
    return new SpatialFunctionFeatures(point_distance, point, point_withinBBox);
  }
}