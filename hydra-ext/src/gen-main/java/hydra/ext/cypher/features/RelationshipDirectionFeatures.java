// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Relationship directions / arrow patterns
 */
public class RelationshipDirectionFeatures implements Serializable, Comparable<RelationshipDirectionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.RelationshipDirectionFeatures");
  
  public static final hydra.core.Name BOTH = new hydra.core.Name("both");
  
  public static final hydra.core.Name LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name NEITHER = new hydra.core.Name("neither");
  
  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");
  
  /**
   * The two-headed arrow (&lt;-[]-&gt;) relationship direction
   */
  public final Boolean both;
  
  /**
   * The left arrow (&lt;-[]-) relationship direction
   */
  public final Boolean left;
  
  /**
   * The headless arrow (-[]-) relationship direction
   */
  public final Boolean neither;
  
  /**
   * The right arrow (-[]-&gt;) relationship direction
   */
  public final Boolean right;
  
  public RelationshipDirectionFeatures (Boolean both, Boolean left, Boolean neither, Boolean right) {
    this.both = both;
    this.left = left;
    this.neither = neither;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationshipDirectionFeatures)) {
      return false;
    }
    RelationshipDirectionFeatures o = (RelationshipDirectionFeatures) other;
    return java.util.Objects.equals(
      this.both,
      o.both) && java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.neither,
      o.neither) && java.util.Objects.equals(
      this.right,
      o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(both) + 3 * java.util.Objects.hashCode(left) + 5 * java.util.Objects.hashCode(neither) + 7 * java.util.Objects.hashCode(right);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RelationshipDirectionFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) both).compareTo(other.both);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) left).compareTo(other.left);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) neither).compareTo(other.neither);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) right).compareTo(other.right);
  }
  
  public RelationshipDirectionFeatures withBoth(Boolean both) {
    return new RelationshipDirectionFeatures(both, left, neither, right);
  }
  
  public RelationshipDirectionFeatures withLeft(Boolean left) {
    return new RelationshipDirectionFeatures(both, left, neither, right);
  }
  
  public RelationshipDirectionFeatures withNeither(Boolean neither) {
    return new RelationshipDirectionFeatures(both, left, neither, right);
  }
  
  public RelationshipDirectionFeatures withRight(Boolean right) {
    return new RelationshipDirectionFeatures(both, left, neither, right);
  }
}
