// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import hydra.util.Maybe;

import java.io.Serializable;

public class RangeExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.RangeExpression");
  
  public static final hydra.core.Name FIELD_NAME_START = new hydra.core.Name("start");
  
  public static final hydra.core.Name FIELD_NAME_END = new hydra.core.Name("end");
  
  public final Maybe<Expression> start;
  
  public final Maybe<Expression> end;
  
  public RangeExpression (Maybe<Expression> start, Maybe<Expression> end) {
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
  
  public RangeExpression withStart(Maybe<Expression> start) {
    java.util.Objects.requireNonNull((start));
    return new RangeExpression(start, end);
  }
  
  public RangeExpression withEnd(Maybe<Expression> end) {
    java.util.Objects.requireNonNull((end));
    return new RangeExpression(start, end);
  }
}
