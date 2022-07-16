package hydra.ext.coq.syntax;

public class QualidAndPattern {
  public final Qualid qualid;
  
  public final Pattern pattern;
  
  public QualidAndPattern (Qualid qualid, Pattern pattern) {
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
  
  public QualidAndPattern withQualid(Qualid qualid) {
    return new QualidAndPattern(qualid, pattern);
  }
  
  public QualidAndPattern withPattern(Pattern pattern) {
    return new QualidAndPattern(qualid, pattern);
  }
}