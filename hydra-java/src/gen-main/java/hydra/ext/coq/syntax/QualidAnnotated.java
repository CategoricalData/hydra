package hydra.ext.coq.syntax;

public class QualidAnnotated {
  public final Qualid qualid;
  
  public final java.util.Optional<UnivAnnot> univAnnot;
  
  public QualidAnnotated (Qualid qualid, java.util.Optional<UnivAnnot> univAnnot) {
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
  
  public QualidAnnotated withQualid(Qualid qualid) {
    return new QualidAnnotated(qualid, univAnnot);
  }
  
  public QualidAnnotated withUnivAnnot(java.util.Optional<UnivAnnot> univAnnot) {
    return new QualidAnnotated(qualid, univAnnot);
  }
}