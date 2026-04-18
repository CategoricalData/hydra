// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class Pattern10_As implements Serializable, Comparable<Pattern10_As> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Pattern10_As");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public static final hydra.core.Name AS = new hydra.core.Name("as");

  public final hydra.coq.syntax.Pattern1 pattern;

  public final hydra.coq.syntax.Name as;

  public Pattern10_As (hydra.coq.syntax.Pattern1 pattern, hydra.coq.syntax.Name as) {
    this.pattern = pattern;
    this.as = as;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern10_As)) {
      return false;
    }
    Pattern10_As o = (Pattern10_As) other;
    return java.util.Objects.equals(
      this.pattern,
      o.pattern) && java.util.Objects.equals(
      this.as,
      o.as);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pattern) + 3 * java.util.Objects.hashCode(as);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Pattern10_As other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pattern,
      other.pattern);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      as,
      other.as);
  }

  public Pattern10_As withPattern(hydra.coq.syntax.Pattern1 pattern) {
    return new Pattern10_As(pattern, as);
  }

  public Pattern10_As withAs(hydra.coq.syntax.Name as) {
    return new Pattern10_As(pattern, as);
  }
}
