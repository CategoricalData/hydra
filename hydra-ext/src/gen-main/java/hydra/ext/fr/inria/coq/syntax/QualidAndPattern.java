// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class QualidAndPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.QualidAndPattern");
  
  public static final hydra.core.Name FIELD_NAME_QUALID = new hydra.core.Name("qualid");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public final hydra.ext.fr.inria.coq.syntax.Qualid qualid;
  
  public final hydra.ext.fr.inria.coq.syntax.Pattern pattern;
  
  public QualidAndPattern (hydra.ext.fr.inria.coq.syntax.Qualid qualid, hydra.ext.fr.inria.coq.syntax.Pattern pattern) {
    java.util.Objects.requireNonNull((qualid));
    java.util.Objects.requireNonNull((pattern));
    this.qualid = qualid;
    this.pattern = pattern;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QualidAndPattern)) {
      return false;
    }
    QualidAndPattern o = (QualidAndPattern) (other);
    return qualid.equals(o.qualid) && pattern.equals(o.pattern);
  }
  
  @Override
  public int hashCode() {
    return 2 * qualid.hashCode() + 3 * pattern.hashCode();
  }
  
  public QualidAndPattern withQualid(hydra.ext.fr.inria.coq.syntax.Qualid qualid) {
    java.util.Objects.requireNonNull((qualid));
    return new QualidAndPattern(qualid, pattern);
  }
  
  public QualidAndPattern withPattern(hydra.ext.fr.inria.coq.syntax.Pattern pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new QualidAndPattern(qualid, pattern);
  }
}