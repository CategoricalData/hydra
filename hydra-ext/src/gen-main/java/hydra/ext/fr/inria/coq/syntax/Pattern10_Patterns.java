// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class Pattern10_Patterns implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.Pattern10.Patterns");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name FIELD_NAME_PATTERNS = new hydra.core.Name("patterns");
  
  public final hydra.ext.fr.inria.coq.syntax.Pattern1 pattern;
  
  public final java.util.List<hydra.ext.fr.inria.coq.syntax.Pattern1> patterns;
  
  public Pattern10_Patterns (hydra.ext.fr.inria.coq.syntax.Pattern1 pattern, java.util.List<hydra.ext.fr.inria.coq.syntax.Pattern1> patterns) {
    java.util.Objects.requireNonNull((pattern));
    java.util.Objects.requireNonNull((patterns));
    this.pattern = pattern;
    this.patterns = patterns;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern10_Patterns)) {
      return false;
    }
    Pattern10_Patterns o = (Pattern10_Patterns) (other);
    return pattern.equals(o.pattern) && patterns.equals(o.patterns);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * patterns.hashCode();
  }
  
  public Pattern10_Patterns withPattern(hydra.ext.fr.inria.coq.syntax.Pattern1 pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new Pattern10_Patterns(pattern, patterns);
  }
  
  public Pattern10_Patterns withPatterns(java.util.List<hydra.ext.fr.inria.coq.syntax.Pattern1> patterns) {
    java.util.Objects.requireNonNull((patterns));
    return new Pattern10_Patterns(pattern, patterns);
  }
}