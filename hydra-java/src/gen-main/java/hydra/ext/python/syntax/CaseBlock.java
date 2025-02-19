// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class CaseBlock implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.CaseBlock");
  
  public static final hydra.core.Name FIELD_NAME_PATTERNS = new hydra.core.Name("patterns");
  
  public static final hydra.core.Name FIELD_NAME_GUARD = new hydra.core.Name("guard");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.python.syntax.Patterns patterns;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Guard> guard;
  
  public final hydra.ext.python.syntax.Block body;
  
  public CaseBlock (hydra.ext.python.syntax.Patterns patterns, hydra.util.Opt<hydra.ext.python.syntax.Guard> guard, hydra.ext.python.syntax.Block body) {
    java.util.Objects.requireNonNull((patterns));
    java.util.Objects.requireNonNull((guard));
    java.util.Objects.requireNonNull((body));
    this.patterns = patterns;
    this.guard = guard;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseBlock)) {
      return false;
    }
    CaseBlock o = (CaseBlock) (other);
    return patterns.equals(o.patterns) && guard.equals(o.guard) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * patterns.hashCode() + 3 * guard.hashCode() + 5 * body.hashCode();
  }
  
  public CaseBlock withPatterns(hydra.ext.python.syntax.Patterns patterns) {
    java.util.Objects.requireNonNull((patterns));
    return new CaseBlock(patterns, guard, body);
  }
  
  public CaseBlock withGuard(hydra.util.Opt<hydra.ext.python.syntax.Guard> guard) {
    java.util.Objects.requireNonNull((guard));
    return new CaseBlock(patterns, guard, body);
  }
  
  public CaseBlock withBody(hydra.ext.python.syntax.Block body) {
    java.util.Objects.requireNonNull((body));
    return new CaseBlock(patterns, guard, body);
  }
}