package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class RelationshipTypes implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RelationshipTypes");
  
  public final java.util.List<hydra.langs.cypher.openCypher.RelTypeName> value;
  
  public RelationshipTypes (java.util.List<hydra.langs.cypher.openCypher.RelTypeName> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationshipTypes)) {
      return false;
    }
    RelationshipTypes o = (RelationshipTypes) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}