// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

public class ObjectAllValuesFrom implements Serializable, Comparable<ObjectAllValuesFrom> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.ObjectAllValuesFrom");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name CLASS = new hydra.core.Name("class");

  public final hydra.owl.syntax.ObjectPropertyExpression property;

  public final hydra.owl.syntax.ClassExpression class_;

  public ObjectAllValuesFrom (hydra.owl.syntax.ObjectPropertyExpression property, hydra.owl.syntax.ClassExpression class_) {
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
    cmp = hydra.util.Comparing.compare(
      property,
      other.property);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      class_,
      other.class_);
  }

  public ObjectAllValuesFrom withProperty(hydra.owl.syntax.ObjectPropertyExpression property) {
    return new ObjectAllValuesFrom(property, class_);
  }

  public ObjectAllValuesFrom withClass(hydra.owl.syntax.ClassExpression class_) {
    return new ObjectAllValuesFrom(property, class_);
  }
}
