// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class DataPropertyDomain implements Serializable, Comparable<DataPropertyDomain> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataPropertyDomain");
  
  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name DOMAIN = new hydra.core.Name("domain");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.org.w3.owl.syntax.DataPropertyExpression property;
  
  public final hydra.ext.org.w3.owl.syntax.ClassExpression domain;
  
  public DataPropertyDomain (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.DataPropertyExpression property, hydra.ext.org.w3.owl.syntax.ClassExpression domain) {
    this.annotations = annotations;
    this.property = property;
    this.domain = domain;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataPropertyDomain)) {
      return false;
    }
    DataPropertyDomain o = (DataPropertyDomain) other;
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
  public int compareTo(DataPropertyDomain other) {
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
  
  public DataPropertyDomain withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new DataPropertyDomain(annotations, property, domain);
  }
  
  public DataPropertyDomain withProperty(hydra.ext.org.w3.owl.syntax.DataPropertyExpression property) {
    return new DataPropertyDomain(annotations, property, domain);
  }
  
  public DataPropertyDomain withDomain(hydra.ext.org.w3.owl.syntax.ClassExpression domain) {
    return new DataPropertyDomain(annotations, property, domain);
  }
}
