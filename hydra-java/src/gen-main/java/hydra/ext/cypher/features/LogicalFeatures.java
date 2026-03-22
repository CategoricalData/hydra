// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Logical operations
 */
public class LogicalFeatures implements Serializable, Comparable<LogicalFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.LogicalFeatures");

  public static final hydra.core.Name AND = new hydra.core.Name("and");

  public static final hydra.core.Name NOT = new hydra.core.Name("not");

  public static final hydra.core.Name OR = new hydra.core.Name("or");

  public static final hydra.core.Name XOR = new hydra.core.Name("xor");

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
    LogicalFeatures o = (LogicalFeatures) other;
    return java.util.Objects.equals(
      this.and,
      o.and) && java.util.Objects.equals(
      this.not,
      o.not) && java.util.Objects.equals(
      this.or,
      o.or) && java.util.Objects.equals(
      this.xor,
      o.xor);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(and) + 3 * java.util.Objects.hashCode(not) + 5 * java.util.Objects.hashCode(or) + 7 * java.util.Objects.hashCode(xor);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LogicalFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) and).compareTo(other.and);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) not).compareTo(other.not);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) or).compareTo(other.or);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) xor).compareTo(other.xor);
  }

  public LogicalFeatures withAnd(Boolean and) {
    return new LogicalFeatures(and, not, or, xor);
  }

  public LogicalFeatures withNot(Boolean not) {
    return new LogicalFeatures(and, not, or, xor);
  }

  public LogicalFeatures withOr(Boolean or) {
    return new LogicalFeatures(and, not, or, xor);
  }

  public LogicalFeatures withXor(Boolean xor) {
    return new LogicalFeatures(and, not, or, xor);
  }
}
