// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class QualidAndPattern implements Serializable, Comparable<QualidAndPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.QualidAndPattern");

  public static final hydra.core.Name QUALID = new hydra.core.Name("qualid");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public final hydra.coq.syntax.Qualid qualid;

  public final hydra.coq.syntax.Pattern pattern;

  public QualidAndPattern (hydra.coq.syntax.Qualid qualid, hydra.coq.syntax.Pattern pattern) {
    this.qualid = qualid;
    this.pattern = pattern;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QualidAndPattern)) {
      return false;
    }
    QualidAndPattern o = (QualidAndPattern) other;
    return java.util.Objects.equals(
      this.qualid,
      o.qualid) && java.util.Objects.equals(
      this.pattern,
      o.pattern);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(qualid) + 3 * java.util.Objects.hashCode(pattern);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(QualidAndPattern other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      qualid,
      other.qualid);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      pattern,
      other.pattern);
  }

  public QualidAndPattern withQualid(hydra.coq.syntax.Qualid qualid) {
    return new QualidAndPattern(qualid, pattern);
  }

  public QualidAndPattern withPattern(hydra.coq.syntax.Pattern pattern) {
    return new QualidAndPattern(qualid, pattern);
  }
}
