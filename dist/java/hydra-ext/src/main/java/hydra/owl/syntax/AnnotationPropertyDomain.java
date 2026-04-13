// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

public class AnnotationPropertyDomain implements Serializable, Comparable<AnnotationPropertyDomain> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.AnnotationPropertyDomain");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name IRI = new hydra.core.Name("iri");

  public final java.util.List<hydra.owl.syntax.Annotation> annotations;

  public final hydra.owl.syntax.AnnotationProperty property;

  public final hydra.rdf.syntax.Iri iri;

  public AnnotationPropertyDomain (java.util.List<hydra.owl.syntax.Annotation> annotations, hydra.owl.syntax.AnnotationProperty property, hydra.rdf.syntax.Iri iri) {
    this.annotations = annotations;
    this.property = property;
    this.iri = iri;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotationPropertyDomain)) {
      return false;
    }
    AnnotationPropertyDomain o = (AnnotationPropertyDomain) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.iri,
      o.iri);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(property) + 5 * java.util.Objects.hashCode(iri);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AnnotationPropertyDomain other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      annotations,
      other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      property,
      other.property);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      iri,
      other.iri);
  }

  public AnnotationPropertyDomain withAnnotations(java.util.List<hydra.owl.syntax.Annotation> annotations) {
    return new AnnotationPropertyDomain(annotations, property, iri);
  }

  public AnnotationPropertyDomain withProperty(hydra.owl.syntax.AnnotationProperty property) {
    return new AnnotationPropertyDomain(annotations, property, iri);
  }

  public AnnotationPropertyDomain withIri(hydra.rdf.syntax.Iri iri) {
    return new AnnotationPropertyDomain(annotations, property, iri);
  }
}
