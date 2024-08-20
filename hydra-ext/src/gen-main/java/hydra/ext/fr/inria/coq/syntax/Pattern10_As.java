// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class Pattern10_As implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.Pattern10.As");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public final hydra.ext.fr.inria.coq.syntax.Pattern1 pattern;
  
  public final hydra.ext.fr.inria.coq.syntax.Name as;
  
  public Pattern10_As (hydra.ext.fr.inria.coq.syntax.Pattern1 pattern, hydra.ext.fr.inria.coq.syntax.Name as) {
    java.util.Objects.requireNonNull((pattern));
    java.util.Objects.requireNonNull((as));
    this.pattern = pattern;
    this.as = as;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern10_As)) {
      return false;
    }
    Pattern10_As o = (Pattern10_As) (other);
    return pattern.equals(o.pattern) && as.equals(o.as);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * as.hashCode();
  }
  
  public Pattern10_As withPattern(hydra.ext.fr.inria.coq.syntax.Pattern1 pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new Pattern10_As(pattern, as);
  }
  
  public Pattern10_As withAs(hydra.ext.fr.inria.coq.syntax.Name as) {
    java.util.Objects.requireNonNull((as));
    return new Pattern10_As(pattern, as);
  }
}