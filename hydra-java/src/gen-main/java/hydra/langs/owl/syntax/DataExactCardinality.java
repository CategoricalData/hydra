// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class DataExactCardinality implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/owl/syntax.DataExactCardinality");
  
  public static final hydra.core.Name FIELD_NAME_BOUND = new hydra.core.Name("bound");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_RANGE = new hydra.core.Name("range");
  
  public final java.math.BigInteger bound;
  
  public final hydra.langs.owl.syntax.DataPropertyExpression property;
  
  public final java.util.List<hydra.langs.owl.syntax.DataRange> range;
  
  public DataExactCardinality (java.math.BigInteger bound, hydra.langs.owl.syntax.DataPropertyExpression property, java.util.List<hydra.langs.owl.syntax.DataRange> range) {
    java.util.Objects.requireNonNull((bound));
    java.util.Objects.requireNonNull((property));
    java.util.Objects.requireNonNull((range));
    this.bound = bound;
    this.property = property;
    this.range = range;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataExactCardinality)) {
      return false;
    }
    DataExactCardinality o = (DataExactCardinality) (other);
    return bound.equals(o.bound) && property.equals(o.property) && range.equals(o.range);
  }
  
  @Override
  public int hashCode() {
    return 2 * bound.hashCode() + 3 * property.hashCode() + 5 * range.hashCode();
  }
  
  public DataExactCardinality withBound(java.math.BigInteger bound) {
    java.util.Objects.requireNonNull((bound));
    return new DataExactCardinality(bound, property, range);
  }
  
  public DataExactCardinality withProperty(hydra.langs.owl.syntax.DataPropertyExpression property) {
    java.util.Objects.requireNonNull((property));
    return new DataExactCardinality(bound, property, range);
  }
  
  public DataExactCardinality withRange(java.util.List<hydra.langs.owl.syntax.DataRange> range) {
    java.util.Objects.requireNonNull((range));
    return new DataExactCardinality(bound, property, range);
  }
}