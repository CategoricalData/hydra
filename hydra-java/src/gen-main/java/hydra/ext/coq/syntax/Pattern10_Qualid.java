package hydra.ext.coq.syntax;

public class Pattern10_Qualid {
  public final Qualid qualid;
  
  public final java.util.List<Pattern1> patterns;
  
  public Pattern10_Qualid (Qualid qualid, java.util.List<Pattern1> patterns) {
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
  
  public Pattern10_Qualid withQualid(Qualid qualid) {
    return new Pattern10_Qualid(qualid, patterns);
  }
  
  public Pattern10_Qualid withPatterns(java.util.List<Pattern1> patterns) {
    return new Pattern10_Qualid(qualid, patterns);
  }
}