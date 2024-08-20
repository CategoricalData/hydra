// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class DataPropertyRange implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/w3/owl/syntax.DataPropertyRange");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_RANGE = new hydra.core.Name("range");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.org.w3.owl.syntax.DataPropertyExpression property;
  
  public final hydra.ext.org.w3.owl.syntax.ClassExpression range;
  
  public DataPropertyRange (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.DataPropertyExpression property, hydra.ext.org.w3.owl.syntax.ClassExpression range) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((property));
    java.util.Objects.requireNonNull((range));
    this.annotations = annotations;
    this.property = property;
    this.range = range;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataPropertyRange)) {
      return false;
    }
    DataPropertyRange o = (DataPropertyRange) (other);
    return annotations.equals(o.annotations) && property.equals(o.property) && range.equals(o.range);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode() + 5 * range.hashCode();
  }
  
  public DataPropertyRange withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new DataPropertyRange(annotations, property, range);
  }
  
  public DataPropertyRange withProperty(hydra.ext.org.w3.owl.syntax.DataPropertyExpression property) {
    java.util.Objects.requireNonNull((property));
    return new DataPropertyRange(annotations, property, range);
  }
  
  public DataPropertyRange withRange(hydra.ext.org.w3.owl.syntax.ClassExpression range) {
    java.util.Objects.requireNonNull((range));
    return new DataPropertyRange(annotations, property, range);
  }
}