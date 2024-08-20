// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Object_Property_Range
 */
public class ObjectPropertyRange implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/w3/owl/syntax.ObjectPropertyRange");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_RANGE = new hydra.core.Name("range");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property;
  
  public final hydra.ext.org.w3.owl.syntax.ClassExpression range;
  
  public ObjectPropertyRange (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property, hydra.ext.org.w3.owl.syntax.ClassExpression range) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((property));
    java.util.Objects.requireNonNull((range));
    this.annotations = annotations;
    this.property = property;
    this.range = range;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectPropertyRange)) {
      return false;
    }
    ObjectPropertyRange o = (ObjectPropertyRange) (other);
    return annotations.equals(o.annotations) && property.equals(o.property) && range.equals(o.range);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode() + 5 * range.hashCode();
  }
  
  public ObjectPropertyRange withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new ObjectPropertyRange(annotations, property, range);
  }
  
  public ObjectPropertyRange withProperty(hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property) {
    java.util.Objects.requireNonNull((property));
    return new ObjectPropertyRange(annotations, property, range);
  }
  
  public ObjectPropertyRange withRange(hydra.ext.org.w3.owl.syntax.ClassExpression range) {
    java.util.Objects.requireNonNull((range));
    return new ObjectPropertyRange(annotations, property, range);
  }
}