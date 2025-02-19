// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class AsPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.AsPattern");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public final hydra.ext.python.syntax.OrPattern pattern;
  
  public final hydra.ext.python.syntax.PatternCaptureTarget as;
  
  public AsPattern (hydra.ext.python.syntax.OrPattern pattern, hydra.ext.python.syntax.PatternCaptureTarget as) {
    java.util.Objects.requireNonNull((pattern));
    java.util.Objects.requireNonNull((as));
    this.pattern = pattern;
    this.as = as;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AsPattern)) {
      return false;
    }
    AsPattern o = (AsPattern) (other);
    return pattern.equals(o.pattern) && as.equals(o.as);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * as.hashCode();
  }
  
  public AsPattern withPattern(hydra.ext.python.syntax.OrPattern pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new AsPattern(pattern, as);
  }
  
  public AsPattern withAs(hydra.ext.python.syntax.PatternCaptureTarget as) {
    java.util.Objects.requireNonNull((as));
    return new AsPattern(pattern, as);
  }
}