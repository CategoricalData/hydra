// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Minimum_Cardinality
 */
public class ObjectMinCardinality implements Serializable, Comparable<ObjectMinCardinality> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectMinCardinality");

  public static final hydra.core.Name BOUND = new hydra.core.Name("bound");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name CLASS = new hydra.core.Name("class");

  public final java.math.BigInteger bound;

  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property;

  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ClassExpression> class_;

  public ObjectMinCardinality (java.math.BigInteger bound, hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property, hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ClassExpression> class_) {
    this.bound = bound;
    this.property = property;
    this.class_ = class_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectMinCardinality)) {
      return false;
    }
    ObjectMinCardinality o = (ObjectMinCardinality) other;
    return this.bound.compareTo(o.bound) == 0 && java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.class_,
      o.class_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(bound) + 3 * java.util.Objects.hashCode(property) + 5 * java.util.Objects.hashCode(class_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ObjectMinCardinality other) {
    int cmp = 0;
    cmp = ((Comparable) bound).compareTo(other.bound);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) property).compareTo(other.property);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) class_).compareTo(other.class_);
  }

  public ObjectMinCardinality withBound(java.math.BigInteger bound) {
    return new ObjectMinCardinality(bound, property, class_);
  }

  public ObjectMinCardinality withProperty(hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property) {
    return new ObjectMinCardinality(bound, property, class_);
  }

  public ObjectMinCardinality withClass(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ClassExpression> class_) {
    return new ObjectMinCardinality(bound, property, class_);
  }
}
