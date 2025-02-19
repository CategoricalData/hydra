// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class QualidAnnotated implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.QualidAnnotated");
  
  public static final hydra.core.Name FIELD_NAME_QUALID = new hydra.core.Name("qualid");
  
  public static final hydra.core.Name FIELD_NAME_UNIV_ANNOT = new hydra.core.Name("univAnnot");
  
  public final hydra.ext.fr.inria.coq.syntax.Qualid qualid;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.UnivAnnot> univAnnot;
  
  public QualidAnnotated (hydra.ext.fr.inria.coq.syntax.Qualid qualid, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.UnivAnnot> univAnnot) {
    java.util.Objects.requireNonNull((qualid));
    java.util.Objects.requireNonNull((univAnnot));
    this.qualid = qualid;
    this.univAnnot = univAnnot;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QualidAnnotated)) {
      return false;
    }
    QualidAnnotated o = (QualidAnnotated) (other);
    return qualid.equals(o.qualid) && univAnnot.equals(o.univAnnot);
  }
  
  @Override
  public int hashCode() {
    return 2 * qualid.hashCode() + 3 * univAnnot.hashCode();
  }
  
  public QualidAnnotated withQualid(hydra.ext.fr.inria.coq.syntax.Qualid qualid) {
    java.util.Objects.requireNonNull((qualid));
    return new QualidAnnotated(qualid, univAnnot);
  }
  
  public QualidAnnotated withUnivAnnot(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.UnivAnnot> univAnnot) {
    java.util.Objects.requireNonNull((univAnnot));
    return new QualidAnnotated(qualid, univAnnot);
  }
}