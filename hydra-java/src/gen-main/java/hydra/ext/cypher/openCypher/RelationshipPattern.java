// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class RelationshipPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/openCypher.RelationshipPattern");
  
  public static final hydra.core.Name FIELD_NAME_LEFT_ARROW = new hydra.core.Name("leftArrow");
  
  public static final hydra.core.Name FIELD_NAME_DETAIL = new hydra.core.Name("detail");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT_ARROW = new hydra.core.Name("rightArrow");
  
  public final Boolean leftArrow;
  
  public final hydra.util.Opt<hydra.ext.cypher.openCypher.RelationshipDetail> detail;
  
  public final Boolean rightArrow;
  
  public RelationshipPattern (Boolean leftArrow, hydra.util.Opt<hydra.ext.cypher.openCypher.RelationshipDetail> detail, Boolean rightArrow) {
    java.util.Objects.requireNonNull((leftArrow));
    java.util.Objects.requireNonNull((detail));
    java.util.Objects.requireNonNull((rightArrow));
    this.leftArrow = leftArrow;
    this.detail = detail;
    this.rightArrow = rightArrow;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationshipPattern)) {
      return false;
    }
    RelationshipPattern o = (RelationshipPattern) (other);
    return leftArrow.equals(o.leftArrow) && detail.equals(o.detail) && rightArrow.equals(o.rightArrow);
  }
  
  @Override
  public int hashCode() {
    return 2 * leftArrow.hashCode() + 3 * detail.hashCode() + 5 * rightArrow.hashCode();
  }
  
  public RelationshipPattern withLeftArrow(Boolean leftArrow) {
    java.util.Objects.requireNonNull((leftArrow));
    return new RelationshipPattern(leftArrow, detail, rightArrow);
  }
  
  public RelationshipPattern withDetail(hydra.util.Opt<hydra.ext.cypher.openCypher.RelationshipDetail> detail) {
    java.util.Objects.requireNonNull((detail));
    return new RelationshipPattern(leftArrow, detail, rightArrow);
  }
  
  public RelationshipPattern withRightArrow(Boolean rightArrow) {
    java.util.Objects.requireNonNull((rightArrow));
    return new RelationshipPattern(leftArrow, detail, rightArrow);
  }
}
