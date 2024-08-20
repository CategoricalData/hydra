// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

public class ObjectSomeValuesFrom implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.ObjectSomeValuesFrom");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  public final hydra.ext.owl.syntax.ObjectPropertyExpression property;
  
  public final hydra.ext.owl.syntax.ClassExpression class_;
  
  public ObjectSomeValuesFrom (hydra.ext.owl.syntax.ObjectPropertyExpression property, hydra.ext.owl.syntax.ClassExpression class_) {
    java.util.Objects.requireNonNull((property));
    java.util.Objects.requireNonNull((class_));
    this.property = property;
    this.class_ = class_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectSomeValuesFrom)) {
      return false;
    }
    ObjectSomeValuesFrom o = (ObjectSomeValuesFrom) (other);
    return property.equals(o.property) && class_.equals(o.class_);
  }
  
  @Override
  public int hashCode() {
    return 2 * property.hashCode() + 3 * class_.hashCode();
  }
  
  public ObjectSomeValuesFrom withProperty(hydra.ext.owl.syntax.ObjectPropertyExpression property) {
    java.util.Objects.requireNonNull((property));
    return new ObjectSomeValuesFrom(property, class_);
  }
  
  public ObjectSomeValuesFrom withClass(hydra.ext.owl.syntax.ClassExpression class_) {
    java.util.Objects.requireNonNull((class_));
    return new ObjectSomeValuesFrom(property, class_);
  }
}
