// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class RangeExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RangeExpression");
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Expression> start;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Expression> end;
  
  public RangeExpression (hydra.util.Opt<hydra.langs.cypher.openCypher.Expression> start, hydra.util.Opt<hydra.langs.cypher.openCypher.Expression> end) {
    java.util.Objects.requireNonNull((start));
    java.util.Objects.requireNonNull((end));
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
  
  public RangeExpression withStart(hydra.util.Opt<hydra.langs.cypher.openCypher.Expression> start) {
    java.util.Objects.requireNonNull((start));
    return new RangeExpression(start, end);
  }
  
  public RangeExpression withEnd(hydra.util.Opt<hydra.langs.cypher.openCypher.Expression> end) {
    java.util.Objects.requireNonNull((end));
    return new RangeExpression(start, end);
  }
}