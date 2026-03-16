// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class RelationshipPattern implements Serializable, Comparable<RelationshipPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.RelationshipPattern");
  
  public static final hydra.core.Name LEFT_ARROW = new hydra.core.Name("leftArrow");
  
  public static final hydra.core.Name DETAIL = new hydra.core.Name("detail");
  
  public static final hydra.core.Name RIGHT_ARROW = new hydra.core.Name("rightArrow");
  
  public final Boolean leftArrow;
  
  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.RelationshipDetail> detail;
  
  public final Boolean rightArrow;
  
  public RelationshipPattern (Boolean leftArrow, hydra.util.Maybe<hydra.ext.cypher.openCypher.RelationshipDetail> detail, Boolean rightArrow) {
    this.leftArrow = leftArrow;
    this.detail = detail;
    this.rightArrow = rightArrow;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationshipPattern)) {
      return false;
    }
    RelationshipPattern o = (RelationshipPattern) other;
    return java.util.Objects.equals(
      this.leftArrow,
      o.leftArrow) && java.util.Objects.equals(
      this.detail,
      o.detail) && java.util.Objects.equals(
      this.rightArrow,
      o.rightArrow);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(leftArrow) + 3 * java.util.Objects.hashCode(detail) + 5 * java.util.Objects.hashCode(rightArrow);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RelationshipPattern other) {
    int cmp = 0;
    cmp = ((Comparable) leftArrow).compareTo(other.leftArrow);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) detail).compareTo(other.detail);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rightArrow).compareTo(other.rightArrow);
  }
  
  public RelationshipPattern withLeftArrow(Boolean leftArrow) {
    return new RelationshipPattern(leftArrow, detail, rightArrow);
  }
  
  public RelationshipPattern withDetail(hydra.util.Maybe<hydra.ext.cypher.openCypher.RelationshipDetail> detail) {
    return new RelationshipPattern(leftArrow, detail, rightArrow);
  }
  
  public RelationshipPattern withRightArrow(Boolean rightArrow) {
    return new RelationshipPattern(leftArrow, detail, rightArrow);
  }
}
