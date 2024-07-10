// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class DataPropertyDomain implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.DataPropertyDomain");
  
  public final java.util.List<hydra.langs.owl.syntax.Annotation> annotations;
  
  public final hydra.langs.owl.syntax.DataPropertyExpression property;
  
  public final hydra.langs.owl.syntax.ClassExpression domain;
  
  public DataPropertyDomain (java.util.List<hydra.langs.owl.syntax.Annotation> annotations, hydra.langs.owl.syntax.DataPropertyExpression property, hydra.langs.owl.syntax.ClassExpression domain) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    if (property == null) {
      throw new IllegalArgumentException("null value for 'property' argument");
    }
    if (domain == null) {
      throw new IllegalArgumentException("null value for 'domain' argument");
    }
    this.annotations = annotations;
    this.property = property;
    this.domain = domain;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataPropertyDomain)) {
      return false;
    }
    DataPropertyDomain o = (DataPropertyDomain) (other);
    return annotations.equals(o.annotations) && property.equals(o.property) && domain.equals(o.domain);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode() + 5 * domain.hashCode();
  }
  
  public DataPropertyDomain withAnnotations(java.util.List<hydra.langs.owl.syntax.Annotation> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new DataPropertyDomain(annotations, property, domain);
  }
  
  public DataPropertyDomain withProperty(hydra.langs.owl.syntax.DataPropertyExpression property) {
    if (property == null) {
      throw new IllegalArgumentException("null value for 'property' argument");
    }
    return new DataPropertyDomain(annotations, property, domain);
  }
  
  public DataPropertyDomain withDomain(hydra.langs.owl.syntax.ClassExpression domain) {
    if (domain == null) {
      throw new IllegalArgumentException("null value for 'domain' argument");
    }
    return new DataPropertyDomain(annotations, property, domain);
  }
}