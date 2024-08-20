// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Exact_Cardinality
 */
public class ObjectExactCardinality implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.ObjectExactCardinality");
  
  public static final hydra.core.Name FIELD_NAME_BOUND = new hydra.core.Name("bound");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  public final java.math.BigInteger bound;
  
  public final hydra.ext.owl.syntax.ObjectPropertyExpression property;
  
  public final java.util.List<hydra.ext.owl.syntax.ClassExpression> class_;
  
  public ObjectExactCardinality (java.math.BigInteger bound, hydra.ext.owl.syntax.ObjectPropertyExpression property, java.util.List<hydra.ext.owl.syntax.ClassExpression> class_) {
    java.util.Objects.requireNonNull((bound));
    java.util.Objects.requireNonNull((property));
    java.util.Objects.requireNonNull((class_));
    this.bound = bound;
    this.property = property;
    this.class_ = class_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectExactCardinality)) {
      return false;
    }
    ObjectExactCardinality o = (ObjectExactCardinality) (other);
    return bound.equals(o.bound) && property.equals(o.property) && class_.equals(o.class_);
  }
  
  @Override
  public int hashCode() {
    return 2 * bound.hashCode() + 3 * property.hashCode() + 5 * class_.hashCode();
  }
  
  public ObjectExactCardinality withBound(java.math.BigInteger bound) {
    java.util.Objects.requireNonNull((bound));
    return new ObjectExactCardinality(bound, property, class_);
  }
  
  public ObjectExactCardinality withProperty(hydra.ext.owl.syntax.ObjectPropertyExpression property) {
    java.util.Objects.requireNonNull((property));
    return new ObjectExactCardinality(bound, property, class_);
  }
  
  public ObjectExactCardinality withClass(java.util.List<hydra.ext.owl.syntax.ClassExpression> class_) {
    java.util.Objects.requireNonNull((class_));
    return new ObjectExactCardinality(bound, property, class_);
  }
}
