// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class CastExpression_RefAndBounds implements Serializable, Comparable<CastExpression_RefAndBounds> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.CastExpression_RefAndBounds");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_BOUNDS = new hydra.core.Name("bounds");
  
  public final hydra.ext.java.syntax.ReferenceType type;
  
  public final java.util.List<hydra.ext.java.syntax.AdditionalBound> bounds;
  
  public CastExpression_RefAndBounds (hydra.ext.java.syntax.ReferenceType type, java.util.List<hydra.ext.java.syntax.AdditionalBound> bounds) {
    this.type = type;
    this.bounds = bounds;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CastExpression_RefAndBounds)) {
      return false;
    }
    CastExpression_RefAndBounds o = (CastExpression_RefAndBounds) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.bounds,
      o.bounds);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(bounds);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CastExpression_RefAndBounds other) {
    int cmp = 0;
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      bounds.hashCode(),
      other.bounds.hashCode());
  }
  
  public CastExpression_RefAndBounds withType(hydra.ext.java.syntax.ReferenceType type) {
    return new CastExpression_RefAndBounds(type, bounds);
  }
  
  public CastExpression_RefAndBounds withBounds(java.util.List<hydra.ext.java.syntax.AdditionalBound> bounds) {
    return new CastExpression_RefAndBounds(type, bounds);
  }
}
