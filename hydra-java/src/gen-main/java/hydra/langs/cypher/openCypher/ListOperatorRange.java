package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ListOperatorRange implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ListOperatorRange");
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Expression> from;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Expression> to;
  
  public ListOperatorRange (java.util.Optional<hydra.langs.cypher.openCypher.Expression> from, java.util.Optional<hydra.langs.cypher.openCypher.Expression> to) {
    this.from = from;
    this.to = to;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListOperatorRange)) {
      return false;
    }
    ListOperatorRange o = (ListOperatorRange) (other);
    return from.equals(o.from) && to.equals(o.to);
  }
  
  @Override
  public int hashCode() {
    return 2 * from.hashCode() + 3 * to.hashCode();
  }
  
  public ListOperatorRange withFrom(java.util.Optional<hydra.langs.cypher.openCypher.Expression> from) {
    return new ListOperatorRange(from, to);
  }
  
  public ListOperatorRange withTo(java.util.Optional<hydra.langs.cypher.openCypher.Expression> to) {
    return new ListOperatorRange(from, to);
  }
}