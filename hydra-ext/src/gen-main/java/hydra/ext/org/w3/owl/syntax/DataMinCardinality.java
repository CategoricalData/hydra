// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class DataMinCardinality implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataMinCardinality");
  
  public static final hydra.core.Name FIELD_NAME_BOUND = new hydra.core.Name("bound");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_RANGE = new hydra.core.Name("range");
  
  public final java.math.BigInteger bound;
  
  public final hydra.ext.org.w3.owl.syntax.DataPropertyExpression property;
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.DataRange> range;
  
  public DataMinCardinality (java.math.BigInteger bound, hydra.ext.org.w3.owl.syntax.DataPropertyExpression property, java.util.List<hydra.ext.org.w3.owl.syntax.DataRange> range) {
    java.util.Objects.requireNonNull((bound));
    java.util.Objects.requireNonNull((property));
    java.util.Objects.requireNonNull((range));
    this.bound = bound;
    this.property = property;
    this.range = range;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataMinCardinality)) {
      return false;
    }
    DataMinCardinality o = (DataMinCardinality) (other);
    return bound.equals(o.bound) && property.equals(o.property) && range.equals(o.range);
  }
  
  @Override
  public int hashCode() {
    return 2 * bound.hashCode() + 3 * property.hashCode() + 5 * range.hashCode();
  }
  
  public DataMinCardinality withBound(java.math.BigInteger bound) {
    java.util.Objects.requireNonNull((bound));
    return new DataMinCardinality(bound, property, range);
  }
  
  public DataMinCardinality withProperty(hydra.ext.org.w3.owl.syntax.DataPropertyExpression property) {
    java.util.Objects.requireNonNull((property));
    return new DataMinCardinality(bound, property, range);
  }
  
  public DataMinCardinality withRange(java.util.List<hydra.ext.org.w3.owl.syntax.DataRange> range) {
    java.util.Objects.requireNonNull((range));
    return new DataMinCardinality(bound, property, range);
  }
}