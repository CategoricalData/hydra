package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ListLiteral implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ListLiteral");
  
  public final java.util.List<hydra.langs.cypher.openCypher.Expression> value;
  
  public ListLiteral (java.util.List<hydra.langs.cypher.openCypher.Expression> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListLiteral)) {
      return false;
    }
    ListLiteral o = (ListLiteral) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}