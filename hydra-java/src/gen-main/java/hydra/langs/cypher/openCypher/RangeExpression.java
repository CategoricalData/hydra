package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class RangeExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RangeExpression");
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Expression> start;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Expression> end;
  
  public RangeExpression (java.util.Optional<hydra.langs.cypher.openCypher.Expression> start, java.util.Optional<hydra.langs.cypher.openCypher.Expression> end) {
    this.start = start;
    this.end = end;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RangeExpression)) {
      return false;
    }
    RangeExpression o = (RangeExpression) (other);
    return start.equals(o.start) && end.equals(o.end);
  }
  
  @Override
  public int hashCode() {
    return 2 * start.hashCode() + 3 * end.hashCode();
  }
  
  public RangeExpression withStart(java.util.Optional<hydra.langs.cypher.openCypher.Expression> start) {
    return new RangeExpression(start, end);
  }
  
  public RangeExpression withEnd(java.util.Optional<hydra.langs.cypher.openCypher.Expression> end) {
    return new RangeExpression(start, end);
  }
}