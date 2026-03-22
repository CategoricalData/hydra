// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class ObjectAllValuesFrom implements Serializable, Comparable<ObjectAllValuesFrom> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectAllValuesFrom");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name CLASS = new hydra.core.Name("class");

  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property;

  public final hydra.ext.org.w3.owl.syntax.ClassExpression class_;

  public ObjectAllValuesFrom (hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property, hydra.ext.org.w3.owl.syntax.ClassExpression class_) {
    this.property = property;
    this.class_ = class_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectAllValuesFrom)) {
      return false;
    }
    ObjectAllValuesFrom o = (ObjectAllValuesFrom) other;
    return java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.class_,
      o.class_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(property) + 3 * java.util.Objects.hashCode(class_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ObjectAllValuesFrom other) {
    int cmp = 0;
    cmp = ((Comparable) property).compareTo(other.property);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) class_).compareTo(other.class_);
  }

  public ObjectAllValuesFrom withProperty(hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property) {
    return new ObjectAllValuesFrom(property, class_);
  }

  public ObjectAllValuesFrom withClass(hydra.ext.org.w3.owl.syntax.ClassExpression class_) {
    return new ObjectAllValuesFrom(property, class_);
  }
}
