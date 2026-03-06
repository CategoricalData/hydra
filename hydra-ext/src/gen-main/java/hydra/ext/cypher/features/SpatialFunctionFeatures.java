// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Spatial functions
 */
public class SpatialFunctionFeatures implements Serializable, Comparable<SpatialFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.SpatialFunctionFeatures");
  
  public static final hydra.core.Name POINT_DISTANCE = new hydra.core.Name("point.distance");
  
  public static final hydra.core.Name POINT = new hydra.core.Name("point");
  
  public static final hydra.core.Name POINT_WITHIN_B_BOX = new hydra.core.Name("point.withinBBox");
  
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
    this.point_distance = point_distance;
    this.point = point;
    this.point_withinBBox = point_withinBBox;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SpatialFunctionFeatures)) {
      return false;
    }
    SpatialFunctionFeatures o = (SpatialFunctionFeatures) other;
    return java.util.Objects.equals(
      this.point_distance,
      o.point_distance) && java.util.Objects.equals(
      this.point,
      o.point) && java.util.Objects.equals(
      this.point_withinBBox,
      o.point_withinBBox);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(point_distance) + 3 * java.util.Objects.hashCode(point) + 5 * java.util.Objects.hashCode(point_withinBBox);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SpatialFunctionFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) point_distance).compareTo(other.point_distance);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) point).compareTo(other.point);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) point_withinBBox).compareTo(other.point_withinBBox);
  }
  
  public SpatialFunctionFeatures withPoint_distance(Boolean point_distance) {
    return new SpatialFunctionFeatures(point_distance, point, point_withinBBox);
  }
  
  public SpatialFunctionFeatures withPoint(Boolean point) {
    return new SpatialFunctionFeatures(point_distance, point, point_withinBBox);
  }
  
  public SpatialFunctionFeatures withPoint_withinBBox(Boolean point_withinBBox) {
    return new SpatialFunctionFeatures(point_distance, point, point_withinBBox);
  }
}
