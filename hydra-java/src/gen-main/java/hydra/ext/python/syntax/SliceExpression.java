// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class SliceExpression implements Serializable, Comparable<SliceExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SliceExpression");
  
  public static final hydra.core.Name FIELD_NAME_START = new hydra.core.Name("start");
  
  public static final hydra.core.Name FIELD_NAME_STOP = new hydra.core.Name("stop");
  
  public static final hydra.core.Name FIELD_NAME_STEP = new hydra.core.Name("step");
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Expression> start;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Expression> stop;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Expression> step;
  
  public SliceExpression (hydra.util.Maybe<hydra.ext.python.syntax.Expression> start, hydra.util.Maybe<hydra.ext.python.syntax.Expression> stop, hydra.util.Maybe<hydra.ext.python.syntax.Expression> step) {
    this.start = start;
    this.stop = stop;
    this.step = step;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SliceExpression)) {
      return false;
    }
    SliceExpression o = (SliceExpression) other;
    return java.util.Objects.equals(
      this.start,
      o.start) && java.util.Objects.equals(
      this.stop,
      o.stop) && java.util.Objects.equals(
      this.step,
      o.step);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(start) + 3 * java.util.Objects.hashCode(stop) + 5 * java.util.Objects.hashCode(step);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SliceExpression other) {
    int cmp = 0;
    cmp = Integer.compare(
      start.hashCode(),
      other.start.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      stop.hashCode(),
      other.stop.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      step.hashCode(),
      other.step.hashCode());
  }
  
  public SliceExpression withStart(hydra.util.Maybe<hydra.ext.python.syntax.Expression> start) {
    return new SliceExpression(start, stop, step);
  }
  
  public SliceExpression withStop(hydra.util.Maybe<hydra.ext.python.syntax.Expression> stop) {
    return new SliceExpression(start, stop, step);
  }
  
  public SliceExpression withStep(hydra.util.Maybe<hydra.ext.python.syntax.Expression> step) {
    return new SliceExpression(start, stop, step);
  }
}
