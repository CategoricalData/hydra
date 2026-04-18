// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class QualidAnnotated implements Serializable, Comparable<QualidAnnotated> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.QualidAnnotated");

  public static final hydra.core.Name QUALID = new hydra.core.Name("qualid");

  public static final hydra.core.Name UNIV_ANNOT = new hydra.core.Name("univAnnot");

  public final hydra.coq.syntax.Qualid qualid;

  public final hydra.util.Maybe<hydra.coq.syntax.UnivAnnot> univAnnot;

  public QualidAnnotated (hydra.coq.syntax.Qualid qualid, hydra.util.Maybe<hydra.coq.syntax.UnivAnnot> univAnnot) {
    this.qualid = qualid;
    this.univAnnot = univAnnot;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QualidAnnotated)) {
      return false;
    }
    QualidAnnotated o = (QualidAnnotated) other;
    return java.util.Objects.equals(
      this.qualid,
      o.qualid) && java.util.Objects.equals(
      this.univAnnot,
      o.univAnnot);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(qualid) + 3 * java.util.Objects.hashCode(univAnnot);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(QualidAnnotated other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      qualid,
      other.qualid);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      univAnnot,
      other.univAnnot);
  }

  public QualidAnnotated withQualid(hydra.coq.syntax.Qualid qualid) {
    return new QualidAnnotated(qualid, univAnnot);
  }

  public QualidAnnotated withUnivAnnot(hydra.util.Maybe<hydra.coq.syntax.UnivAnnot> univAnnot) {
    return new QualidAnnotated(qualid, univAnnot);
  }
}
