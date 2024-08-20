// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

public class DataPropertyDomain implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.DataPropertyDomain");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_DOMAIN = new hydra.core.Name("domain");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.DataPropertyExpression property;
  
  public final hydra.ext.owl.syntax.ClassExpression domain;
  
  public DataPropertyDomain (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.DataPropertyExpression property, hydra.ext.owl.syntax.ClassExpression domain) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((property));
    java.util.Objects.requireNonNull((domain));
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
  
  public DataPropertyDomain withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new DataPropertyDomain(annotations, property, domain);
  }
  
  public DataPropertyDomain withProperty(hydra.ext.owl.syntax.DataPropertyExpression property) {
    java.util.Objects.requireNonNull((property));
    return new DataPropertyDomain(annotations, property, domain);
  }
  
  public DataPropertyDomain withDomain(hydra.ext.owl.syntax.ClassExpression domain) {
    java.util.Objects.requireNonNull((domain));
    return new DataPropertyDomain(annotations, property, domain);
  }
}