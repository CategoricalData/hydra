// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for relationship directions / arrow patterns.
 */
public class RelationshipDirectionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.RelationshipDirectionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_BOTH = new hydra.core.Name("both");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_NEITHER = new hydra.core.Name("neither");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
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
    java.util.Objects.requireNonNull((both));
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((neither));
    java.util.Objects.requireNonNull((right));
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
    java.util.Objects.requireNonNull((both));
    return new RelationshipDirectionFeatures(both, left, neither, right);
  }
  
  public RelationshipDirectionFeatures withLeft(Boolean left) {
    java.util.Objects.requireNonNull((left));
    return new RelationshipDirectionFeatures(both, left, neither, right);
  }
  
  public RelationshipDirectionFeatures withNeither(Boolean neither) {
    java.util.Objects.requireNonNull((neither));
    return new RelationshipDirectionFeatures(both, left, neither, right);
  }
  
  public RelationshipDirectionFeatures withRight(Boolean right) {
    java.util.Objects.requireNonNull((right));
    return new RelationshipDirectionFeatures(both, left, neither, right);
  }
}