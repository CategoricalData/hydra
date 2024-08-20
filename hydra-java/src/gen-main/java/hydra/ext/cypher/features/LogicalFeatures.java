// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Logical operations
 */
public class LogicalFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.LogicalFeatures");
  
  public static final hydra.core.Name FIELD_NAME_AND = new hydra.core.Name("and");
  
  public static final hydra.core.Name FIELD_NAME_NOT = new hydra.core.Name("not");
  
  public static final hydra.core.Name FIELD_NAME_OR = new hydra.core.Name("or");
  
  public static final hydra.core.Name FIELD_NAME_XOR = new hydra.core.Name("xor");
  
  /**
   * The AND operator
   */
  public final Boolean and;
  
  /**
   * The NOT operator
   */
  public final Boolean not;
  
  /**
   * The OR operator
   */
  public final Boolean or;
  
  /**
   * The XOR operator
   */
  public final Boolean xor;
  
  public LogicalFeatures (Boolean and, Boolean not, Boolean or, Boolean xor) {
    java.util.Objects.requireNonNull((and));
    java.util.Objects.requireNonNull((not));
    java.util.Objects.requireNonNull((or));
    java.util.Objects.requireNonNull((xor));
    this.and = and;
    this.not = not;
    this.or = or;
    this.xor = xor;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LogicalFeatures)) {
      return false;
    }
    LogicalFeatures o = (LogicalFeatures) (other);
    return and.equals(o.and) && not.equals(o.not) && or.equals(o.or) && xor.equals(o.xor);
  }
  
  @Override
  public int hashCode() {
    return 2 * and.hashCode() + 3 * not.hashCode() + 5 * or.hashCode() + 7 * xor.hashCode();
  }
  
  public LogicalFeatures withAnd(Boolean and) {
    java.util.Objects.requireNonNull((and));
    return new LogicalFeatures(and, not, or, xor);
  }
  
  public LogicalFeatures withNot(Boolean not) {
    java.util.Objects.requireNonNull((not));
    return new LogicalFeatures(and, not, or, xor);
  }
  
  public LogicalFeatures withOr(Boolean or) {
    java.util.Objects.requireNonNull((or));
    return new LogicalFeatures(and, not, or, xor);
  }
  
  public LogicalFeatures withXor(Boolean xor) {
    java.util.Objects.requireNonNull((xor));
    return new LogicalFeatures(and, not, or, xor);
  }
}
