// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class ObjectSomeValuesFrom implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.ObjectSomeValuesFrom");
  
  public final hydra.langs.owl.syntax.ObjectPropertyExpression property;
  
  public final hydra.langs.owl.syntax.ClassExpression class_;
  
  public ObjectSomeValuesFrom (hydra.langs.owl.syntax.ObjectPropertyExpression property, hydra.langs.owl.syntax.ClassExpression class_) {
    if (property == null) {
      throw new IllegalArgumentException("null value for 'property' argument");
    }
    if (class_ == null) {
      throw new IllegalArgumentException("null value for 'class' argument");
    }
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
  
  public ObjectSomeValuesFrom withProperty(hydra.langs.owl.syntax.ObjectPropertyExpression property) {
    if (property == null) {
      throw new IllegalArgumentException("null value for 'property' argument");
    }
    return new ObjectSomeValuesFrom(property, class_);
  }
  
  public ObjectSomeValuesFrom withClass(hydra.langs.owl.syntax.ClassExpression class_) {
    if (class_ == null) {
      throw new IllegalArgumentException("null value for 'class' argument");
    }
    return new ObjectSomeValuesFrom(property, class_);
  }
}