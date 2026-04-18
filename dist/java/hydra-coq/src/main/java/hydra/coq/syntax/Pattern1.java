// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class Pattern1 implements Serializable, Comparable<Pattern1> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Pattern1");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public static final hydra.core.Name SCOPE = new hydra.core.Name("scope");

  public final hydra.coq.syntax.Pattern0 pattern;

  public final hydra.util.Maybe<hydra.coq.syntax.ScopeKey> scope;

  public Pattern1 (hydra.coq.syntax.Pattern0 pattern, hydra.util.Maybe<hydra.coq.syntax.ScopeKey> scope) {
    this.pattern = pattern;
    this.scope = scope;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern1)) {
      return false;
    }
    Pattern1 o = (Pattern1) other;
    return java.util.Objects.equals(
      this.pattern,
      o.pattern) && java.util.Objects.equals(
      this.scope,
      o.scope);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pattern) + 3 * java.util.Objects.hashCode(scope);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Pattern1 other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pattern,
      other.pattern);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      scope,
      other.scope);
  }

  public Pattern1 withPattern(hydra.coq.syntax.Pattern0 pattern) {
    return new Pattern1(pattern, scope);
  }

  public Pattern1 withScope(hydra.util.Maybe<hydra.coq.syntax.ScopeKey> scope) {
    return new Pattern1(pattern, scope);
  }
}
