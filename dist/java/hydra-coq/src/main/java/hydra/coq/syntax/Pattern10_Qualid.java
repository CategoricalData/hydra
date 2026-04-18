// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class Pattern10_Qualid implements Serializable, Comparable<Pattern10_Qualid> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Pattern10_Qualid");

  public static final hydra.core.Name QUALID = new hydra.core.Name("qualid");

  public static final hydra.core.Name PATTERNS = new hydra.core.Name("patterns");

  public final hydra.coq.syntax.Qualid qualid;

  public final java.util.List<hydra.coq.syntax.Pattern1> patterns;

  public Pattern10_Qualid (hydra.coq.syntax.Qualid qualid, java.util.List<hydra.coq.syntax.Pattern1> patterns) {
    this.qualid = qualid;
    this.patterns = patterns;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern10_Qualid)) {
      return false;
    }
    Pattern10_Qualid o = (Pattern10_Qualid) other;
    return java.util.Objects.equals(
      this.qualid,
      o.qualid) && java.util.Objects.equals(
      this.patterns,
      o.patterns);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(qualid) + 3 * java.util.Objects.hashCode(patterns);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Pattern10_Qualid other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      qualid,
      other.qualid);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      patterns,
      other.patterns);
  }

  public Pattern10_Qualid withQualid(hydra.coq.syntax.Qualid qualid) {
    return new Pattern10_Qualid(qualid, patterns);
  }

  public Pattern10_Qualid withPatterns(java.util.List<hydra.coq.syntax.Pattern1> patterns) {
    return new Pattern10_Qualid(qualid, patterns);
  }
}
