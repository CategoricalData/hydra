// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class Pattern10_Qualid implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.Pattern10_Qualid");
  
  public static final hydra.core.Name FIELD_NAME_QUALID = new hydra.core.Name("qualid");
  
  public static final hydra.core.Name FIELD_NAME_PATTERNS = new hydra.core.Name("patterns");
  
  public final hydra.ext.fr.inria.coq.syntax.Qualid qualid;
  
  public final java.util.List<hydra.ext.fr.inria.coq.syntax.Pattern1> patterns;
  
  public Pattern10_Qualid (hydra.ext.fr.inria.coq.syntax.Qualid qualid, java.util.List<hydra.ext.fr.inria.coq.syntax.Pattern1> patterns) {
    java.util.Objects.requireNonNull((qualid));
    java.util.Objects.requireNonNull((patterns));
    this.qualid = qualid;
    this.patterns = patterns;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern10_Qualid)) {
      return false;
    }
    Pattern10_Qualid o = (Pattern10_Qualid) (other);
    return qualid.equals(o.qualid) && patterns.equals(o.patterns);
  }
  
  @Override
  public int hashCode() {
    return 2 * qualid.hashCode() + 3 * patterns.hashCode();
  }
  
  public Pattern10_Qualid withQualid(hydra.ext.fr.inria.coq.syntax.Qualid qualid) {
    java.util.Objects.requireNonNull((qualid));
    return new Pattern10_Qualid(qualid, patterns);
  }
  
  public Pattern10_Qualid withPatterns(java.util.List<hydra.ext.fr.inria.coq.syntax.Pattern1> patterns) {
    java.util.Objects.requireNonNull((patterns));
    return new Pattern10_Qualid(qualid, patterns);
  }
}