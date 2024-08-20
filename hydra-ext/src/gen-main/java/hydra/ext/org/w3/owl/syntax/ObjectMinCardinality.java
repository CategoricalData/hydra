// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Minimum_Cardinality
 */
public class ObjectMinCardinality implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/w3/owl/syntax.ObjectMinCardinality");
  
  public static final hydra.core.Name FIELD_NAME_BOUND = new hydra.core.Name("bound");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  public final java.math.BigInteger bound;
  
  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property;
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.ClassExpression> class_;
  
  public ObjectMinCardinality (java.math.BigInteger bound, hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property, java.util.List<hydra.ext.org.w3.owl.syntax.ClassExpression> class_) {
    java.util.Objects.requireNonNull((bound));
    java.util.Objects.requireNonNull((property));
    java.util.Objects.requireNonNull((class_));
    this.bound = bound;
    this.property = property;
    this.class_ = class_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectMinCardinality)) {
      return false;
    }
    ObjectMinCardinality o = (ObjectMinCardinality) (other);
    return bound.equals(o.bound) && property.equals(o.property) && class_.equals(o.class_);
  }
  
  @Override
  public int hashCode() {
    return 2 * bound.hashCode() + 3 * property.hashCode() + 5 * class_.hashCode();
  }
  
  public ObjectMinCardinality withBound(java.math.BigInteger bound) {
    java.util.Objects.requireNonNull((bound));
    return new ObjectMinCardinality(bound, property, class_);
  }
  
  public ObjectMinCardinality withProperty(hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property) {
    java.util.Objects.requireNonNull((property));
    return new ObjectMinCardinality(bound, property, class_);
  }
  
  public ObjectMinCardinality withClass(java.util.List<hydra.ext.org.w3.owl.syntax.ClassExpression> class_) {
    java.util.Objects.requireNonNull((class_));
    return new ObjectMinCardinality(bound, property, class_);
  }
}