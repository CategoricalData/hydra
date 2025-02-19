// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class SliceExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SliceExpression");
  
  public static final hydra.core.Name FIELD_NAME_START = new hydra.core.Name("start");
  
  public static final hydra.core.Name FIELD_NAME_STOP = new hydra.core.Name("stop");
  
  public static final hydra.core.Name FIELD_NAME_STEP = new hydra.core.Name("step");
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Expression> start;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Expression> stop;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Expression> step;
  
  public SliceExpression (hydra.util.Opt<hydra.ext.python.syntax.Expression> start, hydra.util.Opt<hydra.ext.python.syntax.Expression> stop, hydra.util.Opt<hydra.ext.python.syntax.Expression> step) {
    java.util.Objects.requireNonNull((start));
    java.util.Objects.requireNonNull((stop));
    java.util.Objects.requireNonNull((step));
    this.start = start;
    this.stop = stop;
    this.step = step;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SliceExpression)) {
      return false;
    }
    SliceExpression o = (SliceExpression) (other);
    return start.equals(o.start) && stop.equals(o.stop) && step.equals(o.step);
  }
  
  @Override
  public int hashCode() {
    return 2 * start.hashCode() + 3 * stop.hashCode() + 5 * step.hashCode();
  }
  
  public SliceExpression withStart(hydra.util.Opt<hydra.ext.python.syntax.Expression> start) {
    java.util.Objects.requireNonNull((start));
    return new SliceExpression(start, stop, step);
  }
  
  public SliceExpression withStop(hydra.util.Opt<hydra.ext.python.syntax.Expression> stop) {
    java.util.Objects.requireNonNull((stop));
    return new SliceExpression(start, stop, step);
  }
  
  public SliceExpression withStep(hydra.util.Opt<hydra.ext.python.syntax.Expression> step) {
    java.util.Objects.requireNonNull((step));
    return new SliceExpression(start, stop, step);
  }
}