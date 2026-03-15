// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Maximum_Cardinality
 */
public class ObjectMaxCardinality implements Serializable, Comparable<ObjectMaxCardinality> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality");
  
  public static final hydra.core.Name BOUND = new hydra.core.Name("bound");
  
  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name CLASS = new hydra.core.Name("class");
  
  public final java.math.BigInteger bound;
  
  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property;
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ClassExpression> class_;
  
  public ObjectMaxCardinality (java.math.BigInteger bound, hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property, hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ClassExpression> class_) {
    this.bound = bound;
    this.property = property;
    this.class_ = class_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectMaxCardinality)) {
      return false;
    }
    ObjectMaxCardinality o = (ObjectMaxCardinality) other;
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
  public int compareTo(ObjectMaxCardinality other) {
    int cmp = 0;
    cmp = ((Comparable) bound).compareTo(other.bound);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) property).compareTo(other.property);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      class_.hashCode(),
      other.class_.hashCode());
  }
  
  public ObjectMaxCardinality withBound(java.math.BigInteger bound) {
    return new ObjectMaxCardinality(bound, property, class_);
  }
  
  public ObjectMaxCardinality withProperty(hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property) {
    return new ObjectMaxCardinality(bound, property, class_);
  }
  
  public ObjectMaxCardinality withClass(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ClassExpression> class_) {
    return new ObjectMaxCardinality(bound, property, class_);
  }
}
