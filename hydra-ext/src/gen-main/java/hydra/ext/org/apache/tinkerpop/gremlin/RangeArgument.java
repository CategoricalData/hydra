// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class RangeArgument implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/gremlin.RangeArgument");
  
  public static final hydra.core.Name FIELD_NAME_MIN = new hydra.core.Name("min");
  
  public static final hydra.core.Name FIELD_NAME_MAX = new hydra.core.Name("max");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument min;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument max;
  
  public RangeArgument (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument min, hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument max) {
    java.util.Objects.requireNonNull((min));
    java.util.Objects.requireNonNull((max));
    this.min = min;
    this.max = max;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RangeArgument)) {
      return false;
    }
    RangeArgument o = (RangeArgument) (other);
    return min.equals(o.min) && max.equals(o.max);
  }
  
  @Override
  public int hashCode() {
    return 2 * min.hashCode() + 3 * max.hashCode();
  }
  
  public RangeArgument withMin(hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument min) {
    java.util.Objects.requireNonNull((min));
    return new RangeArgument(min, max);
  }
  
  public RangeArgument withMax(hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument max) {
    java.util.Objects.requireNonNull((max));
    return new RangeArgument(min, max);
  }
}