// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Spatial functions
 */
public class SpatialFunctionFeatures implements Serializable, Comparable<SpatialFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.SpatialFunctionFeatures");

  public static final hydra.core.Name POINT_DISTANCE = new hydra.core.Name("pointDistance");

  public static final hydra.core.Name POINT = new hydra.core.Name("point");

  public static final hydra.core.Name POINT_WITHIN_B_BOX = new hydra.core.Name("pointWithinBBox");

  /**
   * The point.distance() function. Returns a FLOAT representing the geodesic distance between any two points in the same CRS.
   */
  public final Boolean pointDistance;

  /**
   * The point() function. Returns a 2D point object, given two coordinate values in the Cartesian coordinate system.; Returns a 3D point object, given three coordinate values in the Cartesian coordinate system.; Returns a 2D point object, given two coordinate values in the WGS 84 geographic coordinate system.; Returns a 3D point object, given three coordinate values in the WGS 84 geographic coordinate system.
   */
  public final Boolean point;

  /**
   * The point.withinBBox() function. Returns true if the provided point is within the bounding box defined by the two provided points, lowerLeft and upperRight.
   */
  public final Boolean pointWithinBBox;

  public SpatialFunctionFeatures (Boolean pointDistance, Boolean point, Boolean pointWithinBBox) {
    this.pointDistance = pointDistance;
    this.point = point;
    this.pointWithinBBox = pointWithinBBox;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SpatialFunctionFeatures)) {
      return false;
    }
    SpatialFunctionFeatures o = (SpatialFunctionFeatures) other;
    return java.util.Objects.equals(
      this.pointDistance,
      o.pointDistance) && java.util.Objects.equals(
      this.point,
      o.point) && java.util.Objects.equals(
      this.pointWithinBBox,
      o.pointWithinBBox);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pointDistance) + 3 * java.util.Objects.hashCode(point) + 5 * java.util.Objects.hashCode(pointWithinBBox);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SpatialFunctionFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pointDistance,
      other.pointDistance);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      point,
      other.point);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      pointWithinBBox,
      other.pointWithinBBox);
  }

  public SpatialFunctionFeatures withPointDistance(Boolean pointDistance) {
    return new SpatialFunctionFeatures(pointDistance, point, pointWithinBBox);
  }

  public SpatialFunctionFeatures withPoint(Boolean point) {
    return new SpatialFunctionFeatures(pointDistance, point, pointWithinBBox);
  }

  public SpatialFunctionFeatures withPointWithinBBox(Boolean pointWithinBBox) {
    return new SpatialFunctionFeatures(pointDistance, point, pointWithinBBox);
  }
}
