// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class AnnotationPropertyDomain implements Serializable, Comparable<AnnotationPropertyDomain> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name IRI = new hydra.core.Name("iri");

  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations;

  public final hydra.ext.org.w3.owl.syntax.AnnotationProperty property;

  public final hydra.ext.org.w3.rdf.syntax.Iri iri;

  public AnnotationPropertyDomain (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.AnnotationProperty property, hydra.ext.org.w3.rdf.syntax.Iri iri) {
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
    cmp = ((Comparable) annotations).compareTo(other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) property).compareTo(other.property);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) iri).compareTo(other.iri);
  }

  public AnnotationPropertyDomain withAnnotations(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new AnnotationPropertyDomain(annotations, property, iri);
  }

  public AnnotationPropertyDomain withProperty(hydra.ext.org.w3.owl.syntax.AnnotationProperty property) {
    return new AnnotationPropertyDomain(annotations, property, iri);
  }

  public AnnotationPropertyDomain withIri(hydra.ext.org.w3.rdf.syntax.Iri iri) {
    return new AnnotationPropertyDomain(annotations, property, iri);
  }
}
