package hydra.ext.coq.syntax;

public class QualidAndPattern {
  public final hydra.ext.coq.syntax.Qualid qualid;
  
  public final hydra.ext.coq.syntax.Pattern pattern;
  
  public QualidAndPattern (hydra.ext.coq.syntax.Qualid qualid, hydra.ext.coq.syntax.Pattern pattern) {
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
  
  public QualidAndPattern withQualid(hydra.ext.coq.syntax.Qualid qualid) {
    return new QualidAndPattern(qualid, pattern);
  }
  
  public QualidAndPattern withPattern(hydra.ext.coq.syntax.Pattern pattern) {
    return new QualidAndPattern(qualid, pattern);
  }
}