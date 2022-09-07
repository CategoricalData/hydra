package hydra.ext.coq.syntax;

public class Pattern10_Qualid {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Pattern10.Qualid");
  
  public final hydra.ext.coq.syntax.Qualid qualid;
  
  public final java.util.List<hydra.ext.coq.syntax.Pattern1> patterns;
  
  public Pattern10_Qualid (hydra.ext.coq.syntax.Qualid qualid, java.util.List<hydra.ext.coq.syntax.Pattern1> patterns) {
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
  
  public Pattern10_Qualid withQualid(hydra.ext.coq.syntax.Qualid qualid) {
    return new Pattern10_Qualid(qualid, patterns);
  }
  
  public Pattern10_Qualid withPatterns(java.util.List<hydra.ext.coq.syntax.Pattern1> patterns) {
    return new Pattern10_Qualid(qualid, patterns);
  }
}