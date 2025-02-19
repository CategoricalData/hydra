// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class SwitchBranch implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.SwitchBranch");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name FIELD_NAME_GUARD = new hydra.core.Name("guard");
  
  public final hydra.ext.csharp.syntax.Pattern pattern;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Expression> guard;
  
  public SwitchBranch (hydra.ext.csharp.syntax.Pattern pattern, hydra.util.Opt<hydra.ext.csharp.syntax.Expression> guard) {
    java.util.Objects.requireNonNull((pattern));
    java.util.Objects.requireNonNull((guard));
    this.pattern = pattern;
    this.guard = guard;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SwitchBranch)) {
      return false;
    }
    SwitchBranch o = (SwitchBranch) (other);
    return pattern.equals(o.pattern) && guard.equals(o.guard);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * guard.hashCode();
  }
  
  public SwitchBranch withPattern(hydra.ext.csharp.syntax.Pattern pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new SwitchBranch(pattern, guard);
  }
  
  public SwitchBranch withGuard(hydra.util.Opt<hydra.ext.csharp.syntax.Expression> guard) {
    java.util.Objects.requireNonNull((guard));
    return new SwitchBranch(pattern, guard);
  }
}