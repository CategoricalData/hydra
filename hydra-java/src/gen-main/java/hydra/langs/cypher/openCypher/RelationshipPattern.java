package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class RelationshipPattern implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RelationshipPattern");
  
  public final Boolean leftArrow;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.RelationshipDetail> detail;
  
  public final Boolean rightArrow;
  
  public RelationshipPattern (Boolean leftArrow, java.util.Optional<hydra.langs.cypher.openCypher.RelationshipDetail> detail, Boolean rightArrow) {
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
    return new RelationshipPattern(leftArrow, detail, rightArrow);
  }
  
  public RelationshipPattern withDetail(java.util.Optional<hydra.langs.cypher.openCypher.RelationshipDetail> detail) {
    return new RelationshipPattern(leftArrow, detail, rightArrow);
  }
  
  public RelationshipPattern withRightArrow(Boolean rightArrow) {
    return new RelationshipPattern(leftArrow, detail, rightArrow);
  }
}