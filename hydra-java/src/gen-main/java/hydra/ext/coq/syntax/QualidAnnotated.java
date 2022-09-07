package hydra.ext.coq.syntax;

public class QualidAnnotated {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.QualidAnnotated");
  
  public final hydra.ext.coq.syntax.Qualid qualid;
  
  public final java.util.Optional<hydra.ext.coq.syntax.UnivAnnot> univAnnot;
  
  public QualidAnnotated (hydra.ext.coq.syntax.Qualid qualid, java.util.Optional<hydra.ext.coq.syntax.UnivAnnot> univAnnot) {
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
  
  public QualidAnnotated withQualid(hydra.ext.coq.syntax.Qualid qualid) {
    return new QualidAnnotated(qualid, univAnnot);
  }
  
  public QualidAnnotated withUnivAnnot(java.util.Optional<hydra.ext.coq.syntax.UnivAnnot> univAnnot) {
    return new QualidAnnotated(qualid, univAnnot);
  }
}