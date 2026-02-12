// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class CaseBlock implements Serializable, Comparable<CaseBlock> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.CaseBlock");
  
  public static final hydra.core.Name FIELD_NAME_PATTERNS = new hydra.core.Name("patterns");
  
  public static final hydra.core.Name FIELD_NAME_GUARD = new hydra.core.Name("guard");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.python.syntax.Patterns patterns;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Guard> guard;
  
  public final hydra.ext.python.syntax.Block body;
  
  public CaseBlock (hydra.ext.python.syntax.Patterns patterns, hydra.util.Maybe<hydra.ext.python.syntax.Guard> guard, hydra.ext.python.syntax.Block body) {
    this.patterns = patterns;
    this.guard = guard;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseBlock)) {
      return false;
    }
    CaseBlock o = (CaseBlock) other;
    return java.util.Objects.equals(
      this.patterns,
      o.patterns) && java.util.Objects.equals(
      this.guard,
      o.guard) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(patterns) + 3 * java.util.Objects.hashCode(guard) + 5 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CaseBlock other) {
    int cmp = 0;
    cmp = ((Comparable) patterns).compareTo(other.patterns);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      guard.hashCode(),
      other.guard.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public CaseBlock withPatterns(hydra.ext.python.syntax.Patterns patterns) {
    return new CaseBlock(patterns, guard, body);
  }
  
  public CaseBlock withGuard(hydra.util.Maybe<hydra.ext.python.syntax.Guard> guard) {
    return new CaseBlock(patterns, guard, body);
  }
  
  public CaseBlock withBody(hydra.ext.python.syntax.Block body) {
    return new CaseBlock(patterns, guard, body);
  }
}
