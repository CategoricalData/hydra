// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for relationship directions / arrow patterns.
 */
public class RelationshipDirectionFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.RelationshipDirectionFeatures");
  
  /**
   * Whether to expect the two-headed arrow (&lt;-[]-&gt;) relationship direction.
   */
  public final Boolean both;
  
  /**
   * Whether to expect the left arrow (&lt;-[]-) relationship direction.
   */
  public final Boolean left;
  
  /**
   * Whether to expect the headless arrow (-[]-) relationship direction.
   */
  public final Boolean neither;
  
  /**
   * Whether to expect the right arrow (-[]-&gt;) relationship direction.
   */
  public final Boolean right;
  
  public RelationshipDirectionFeatures (Boolean both, Boolean left, Boolean neither, Boolean right) {
    if (both == null) {
      throw new IllegalArgumentException("null value for 'both' argument");
    }
    if (left == null) {
      throw new IllegalArgumentException("null value for 'left' argument");
    }
    if (neither == null) {
      throw new IllegalArgumentException("null value for 'neither' argument");
    }
    if (right == null) {
      throw new IllegalArgumentException("null value for 'right' argument");
    }
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
    RelationshipDirectionFeatures o = (RelationshipDirectionFeatures) (other);
    return both.equals(o.both) && left.equals(o.left) && neither.equals(o.neither) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * both.hashCode() + 3 * left.hashCode() + 5 * neither.hashCode() + 7 * right.hashCode();
  }
  
  public RelationshipDirectionFeatures withBoth(Boolean both) {
    if (both == null) {
      throw new IllegalArgumentException("null value for 'both' argument");
    }
    return new RelationshipDirectionFeatures(both, left, neither, right);
  }
  
  public RelationshipDirectionFeatures withLeft(Boolean left) {
    if (left == null) {
      throw new IllegalArgumentException("null value for 'left' argument");
    }
    return new RelationshipDirectionFeatures(both, left, neither, right);
  }
  
  public RelationshipDirectionFeatures withNeither(Boolean neither) {
    if (neither == null) {
      throw new IllegalArgumentException("null value for 'neither' argument");
    }
    return new RelationshipDirectionFeatures(both, left, neither, right);
  }
  
  public RelationshipDirectionFeatures withRight(Boolean right) {
    if (right == null) {
      throw new IllegalArgumentException("null value for 'right' argument");
    }
    return new RelationshipDirectionFeatures(both, left, neither, right);
  }
}