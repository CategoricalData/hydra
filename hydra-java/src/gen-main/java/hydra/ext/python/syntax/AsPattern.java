// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class AsPattern implements Serializable, Comparable<AsPattern> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.AsPattern");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public final hydra.ext.python.syntax.OrPattern pattern;
  
  public final hydra.ext.python.syntax.PatternCaptureTarget as;
  
  public AsPattern (hydra.ext.python.syntax.OrPattern pattern, hydra.ext.python.syntax.PatternCaptureTarget as) {
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
    cmp = ((Comparable) pattern).compareTo(other.pattern);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) as).compareTo(other.as);
  }
  
  public AsPattern withPattern(hydra.ext.python.syntax.OrPattern pattern) {
    return new AsPattern(pattern, as);
  }
  
  public AsPattern withAs(hydra.ext.python.syntax.PatternCaptureTarget as) {
    return new AsPattern(pattern, as);
  }
}
