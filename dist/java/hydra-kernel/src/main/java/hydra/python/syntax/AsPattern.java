// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class AsPattern implements Serializable, Comparable<AsPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.AsPattern");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public static final hydra.core.Name AS = new hydra.core.Name("as");

  public final hydra.python.syntax.OrPattern pattern;

  public final hydra.python.syntax.PatternCaptureTarget as;

  public AsPattern (hydra.python.syntax.OrPattern pattern, hydra.python.syntax.PatternCaptureTarget as) {
    this.pattern = pattern;
    this.as = as;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AsPattern)) {
      return false;
    }
    AsPattern o = (AsPattern) other;
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
  public int compareTo(AsPattern other) {
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

  public AsPattern withPattern(hydra.python.syntax.OrPattern pattern) {
    return new AsPattern(pattern, as);
  }

  public AsPattern withAs(hydra.python.syntax.PatternCaptureTarget as) {
    return new AsPattern(pattern, as);
  }
}
