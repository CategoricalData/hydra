// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class AnnotationPropertyDomain implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/w3/owl/syntax.AnnotationPropertyDomain");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_IRI = new hydra.core.Name("iri");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.org.w3.owl.syntax.AnnotationProperty property;
  
  public final hydra.ext.org.w3.rdf.syntax.Iri iri;
  
  public AnnotationPropertyDomain (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.AnnotationProperty property, hydra.ext.org.w3.rdf.syntax.Iri iri) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((property));
    java.util.Objects.requireNonNull((iri));
    this.annotations = annotations;
    this.property = property;
    this.iri = iri;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotationPropertyDomain)) {
      return false;
    }
    AnnotationPropertyDomain o = (AnnotationPropertyDomain) (other);
    return annotations.equals(o.annotations) && property.equals(o.property) && iri.equals(o.iri);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode() + 5 * iri.hashCode();
  }
  
  public AnnotationPropertyDomain withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new AnnotationPropertyDomain(annotations, property, iri);
  }
  
  public AnnotationPropertyDomain withProperty(hydra.ext.org.w3.owl.syntax.AnnotationProperty property) {
    java.util.Objects.requireNonNull((property));
    return new AnnotationPropertyDomain(annotations, property, iri);
  }
  
  public AnnotationPropertyDomain withIri(hydra.ext.org.w3.rdf.syntax.Iri iri) {
    java.util.Objects.requireNonNull((iri));
    return new AnnotationPropertyDomain(annotations, property, iri);
  }
}