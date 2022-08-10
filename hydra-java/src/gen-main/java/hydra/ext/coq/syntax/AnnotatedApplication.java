package hydra.ext.coq.syntax;

public class AnnotatedApplication {
  public final QualidAnnotated annot;
  
  public final java.util.List<Term1> terms;
  
  public AnnotatedApplication (QualidAnnotated annot, java.util.List<Term1> terms) {
    this.annot = annot;
    this.terms = terms;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotatedApplication)) {
      return false;
    }
    AnnotatedApplication o = (AnnotatedApplication) (other);
    return annot.equals(o.annot) && terms.equals(o.terms);
  }
  
  @Override
  public int hashCode() {
    return 2 * annot.hashCode() + 3 * terms.hashCode();
  }
  
  public AnnotatedApplication withAnnot(QualidAnnotated annot) {
    return new AnnotatedApplication(annot, terms);
  }
  
  public AnnotatedApplication withTerms(java.util.List<Term1> terms) {
    return new AnnotatedApplication(annot, terms);
  }
}