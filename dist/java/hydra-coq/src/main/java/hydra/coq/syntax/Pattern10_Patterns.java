// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class Pattern10_Patterns implements Serializable, Comparable<Pattern10_Patterns> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Pattern10_Patterns");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public static final hydra.core.Name PATTERNS = new hydra.core.Name("patterns");

  public final hydra.coq.syntax.Pattern1 pattern;

  public final java.util.List<hydra.coq.syntax.Pattern1> patterns;

  public Pattern10_Patterns (hydra.coq.syntax.Pattern1 pattern, java.util.List<hydra.coq.syntax.Pattern1> patterns) {
    this.pattern = pattern;
    this.patterns = patterns;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern10_Patterns)) {
      return false;
    }
    Pattern10_Patterns o = (Pattern10_Patterns) other;
    return java.util.Objects.equals(
      this.pattern,
      o.pattern) && java.util.Objects.equals(
      this.patterns,
      o.patterns);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pattern) + 3 * java.util.Objects.hashCode(patterns);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Pattern10_Patterns other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pattern,
      other.pattern);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      patterns,
      other.patterns);
  }

  public Pattern10_Patterns withPattern(hydra.coq.syntax.Pattern1 pattern) {
    return new Pattern10_Patterns(pattern, patterns);
  }

  public Pattern10_Patterns withPatterns(java.util.List<hydra.coq.syntax.Pattern1> patterns) {
    return new Pattern10_Patterns(pattern, patterns);
  }
}
