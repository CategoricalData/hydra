// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class AnnotatedApplication implements Serializable, Comparable<AnnotatedApplication> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.AnnotatedApplication");

  public static final hydra.core.Name ANNOT = new hydra.core.Name("annot");

  public static final hydra.core.Name TERMS = new hydra.core.Name("terms");

  public final hydra.coq.syntax.QualidAnnotated annot;

  public final java.util.List<hydra.coq.syntax.Term1> terms;

  public AnnotatedApplication (hydra.coq.syntax.QualidAnnotated annot, java.util.List<hydra.coq.syntax.Term1> terms) {
    this.annot = annot;
    this.terms = terms;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotatedApplication)) {
      return false;
    }
    AnnotatedApplication o = (AnnotatedApplication) other;
    return java.util.Objects.equals(
      this.annot,
      o.annot) && java.util.Objects.equals(
      this.terms,
      o.terms);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annot) + 3 * java.util.Objects.hashCode(terms);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AnnotatedApplication other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      annot,
      other.annot);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      terms,
      other.terms);
  }

  public AnnotatedApplication withAnnot(hydra.coq.syntax.QualidAnnotated annot) {
    return new AnnotatedApplication(annot, terms);
  }

  public AnnotatedApplication withTerms(java.util.List<hydra.coq.syntax.Term1> terms) {
    return new AnnotatedApplication(annot, terms);
  }
}
