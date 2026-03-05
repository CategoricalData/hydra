// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Object_Property_Domain
 */
public class ObjectPropertyDomain implements Serializable, Comparable<ObjectPropertyDomain> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectPropertyDomain");
  
  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name DOMAIN = new hydra.core.Name("domain");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property;
  
  public final hydra.ext.org.w3.owl.syntax.ClassExpression domain;
  
  public ObjectPropertyDomain (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property, hydra.ext.org.w3.owl.syntax.ClassExpression domain) {
    this.annotations = annotations;
    this.property = property;
    this.domain = domain;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectPropertyDomain)) {
      return false;
    }
    ObjectPropertyDomain o = (ObjectPropertyDomain) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.domain,
      o.domain);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(property) + 5 * java.util.Objects.hashCode(domain);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ObjectPropertyDomain other) {
    int cmp = 0;
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) property).compareTo(other.property);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) domain).compareTo(other.domain);
  }
  
  public ObjectPropertyDomain withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new ObjectPropertyDomain(annotations, property, domain);
  }
  
  public ObjectPropertyDomain withProperty(hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property) {
    return new ObjectPropertyDomain(annotations, property, domain);
  }
  
  public ObjectPropertyDomain withDomain(hydra.ext.org.w3.owl.syntax.ClassExpression domain) {
    return new ObjectPropertyDomain(annotations, property, domain);
  }
}
