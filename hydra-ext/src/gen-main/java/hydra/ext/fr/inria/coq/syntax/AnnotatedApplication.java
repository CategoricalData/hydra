// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class AnnotatedApplication implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.AnnotatedApplication");
  
  public static final hydra.core.Name FIELD_NAME_ANNOT = new hydra.core.Name("annot");
  
  public static final hydra.core.Name FIELD_NAME_TERMS = new hydra.core.Name("terms");
  
  public final hydra.ext.fr.inria.coq.syntax.QualidAnnotated annot;
  
  public final java.util.List<hydra.ext.fr.inria.coq.syntax.Term1> terms;
  
  public AnnotatedApplication (hydra.ext.fr.inria.coq.syntax.QualidAnnotated annot, java.util.List<hydra.ext.fr.inria.coq.syntax.Term1> terms) {
    java.util.Objects.requireNonNull((annot));
    java.util.Objects.requireNonNull((terms));
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
  
  public AnnotatedApplication withAnnot(hydra.ext.fr.inria.coq.syntax.QualidAnnotated annot) {
    java.util.Objects.requireNonNull((annot));
    return new AnnotatedApplication(annot, terms);
  }
  
  public AnnotatedApplication withTerms(java.util.List<hydra.ext.fr.inria.coq.syntax.Term1> terms) {
    java.util.Objects.requireNonNull((terms));
    return new AnnotatedApplication(annot, terms);
  }
}