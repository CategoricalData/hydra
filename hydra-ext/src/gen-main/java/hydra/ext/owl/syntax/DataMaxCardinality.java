// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

public class DataMaxCardinality implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.DataMaxCardinality");
  
  public static final hydra.core.Name FIELD_NAME_BOUND = new hydra.core.Name("bound");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_RANGE = new hydra.core.Name("range");
  
  public final java.math.BigInteger bound;
  
  public final hydra.ext.owl.syntax.DataPropertyExpression property;
  
  public final java.util.List<hydra.ext.owl.syntax.DataRange> range;
  
  public DataMaxCardinality (java.math.BigInteger bound, hydra.ext.owl.syntax.DataPropertyExpression property, java.util.List<hydra.ext.owl.syntax.DataRange> range) {
    java.util.Objects.requireNonNull((bound));
    java.util.Objects.requireNonNull((property));
    java.util.Objects.requireNonNull((range));
    this.bound = bound;
    this.property = property;
    this.range = range;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataMaxCardinality)) {
      return false;
    }
    DataMaxCardinality o = (DataMaxCardinality) (other);
    return bound.equals(o.bound) && property.equals(o.property) && range.equals(o.range);
  }
  
  @Override
  public int hashCode() {
    return 2 * bound.hashCode() + 3 * property.hashCode() + 5 * range.hashCode();
  }
  
  public DataMaxCardinality withBound(java.math.BigInteger bound) {
    java.util.Objects.requireNonNull((bound));
    return new DataMaxCardinality(bound, property, range);
  }
  
  public DataMaxCardinality withProperty(hydra.ext.owl.syntax.DataPropertyExpression property) {
    java.util.Objects.requireNonNull((property));
    return new DataMaxCardinality(bound, property, range);
  }
  
  public DataMaxCardinality withRange(java.util.List<hydra.ext.owl.syntax.DataRange> range) {
    java.util.Objects.requireNonNull((range));
    return new DataMaxCardinality(bound, property, range);
  }
}