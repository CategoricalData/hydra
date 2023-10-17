package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class RelationshipPattern implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RelationshipPattern");
  
  public final hydra.langs.cypher.openCypher.RelationshipDirection direction;
  
  public final hydra.langs.cypher.openCypher.RelationshipDetail detail;
  
  public RelationshipPattern (hydra.langs.cypher.openCypher.RelationshipDirection direction, hydra.langs.cypher.openCypher.RelationshipDetail detail) {
    this.direction = direction;
    this.detail = detail;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationshipPattern)) {
      return false;
    }
    RelationshipPattern o = (RelationshipPattern) (other);
    return direction.equals(o.direction) && detail.equals(o.detail);
  }
  
  @Override
  public int hashCode() {
    return 2 * direction.hashCode() + 3 * detail.hashCode();
  }
  
  public RelationshipPattern withDirection(hydra.langs.cypher.openCypher.RelationshipDirection direction) {
    return new RelationshipPattern(direction, detail);
  }
  
  public RelationshipPattern withDetail(hydra.langs.cypher.openCypher.RelationshipDetail detail) {
    return new RelationshipPattern(direction, detail);
  }
}