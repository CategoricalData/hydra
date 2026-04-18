// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class CaseBlock implements Serializable, Comparable<CaseBlock> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.CaseBlock");

  public static final hydra.core.Name PATTERNS = new hydra.core.Name("patterns");

  public static final hydra.core.Name GUARD = new hydra.core.Name("guard");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.python.syntax.Patterns patterns;

  public final hydra.util.Maybe<hydra.python.syntax.Guard> guard;

  public final hydra.python.syntax.Block body;

  public CaseBlock (hydra.python.syntax.Patterns patterns, hydra.util.Maybe<hydra.python.syntax.Guard> guard, hydra.python.syntax.Block body) {
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
    cmp = hydra.util.Comparing.compare(
      patterns,
      other.patterns);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      guard,
      other.guard);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public CaseBlock withPatterns(hydra.python.syntax.Patterns patterns) {
    return new CaseBlock(patterns, guard, body);
  }

  public CaseBlock withGuard(hydra.util.Maybe<hydra.python.syntax.Guard> guard) {
    return new CaseBlock(patterns, guard, body);
  }

  public CaseBlock withBody(hydra.python.syntax.Block body) {
    return new CaseBlock(patterns, guard, body);
  }
}
