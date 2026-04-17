// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class CastSpecification implements Serializable, Comparable<CastSpecification> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CastSpecification");

  public static final hydra.core.Name OPERAND = new hydra.core.Name("operand");

  public static final hydra.core.Name TARGET = new hydra.core.Name("target");

  public final openGql.grammar.CastOperand operand;

  public final openGql.grammar.ValueType target;

  public CastSpecification (openGql.grammar.CastOperand operand, openGql.grammar.ValueType target) {
    this.operand = operand;
    this.target = target;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CastSpecification)) {
      return false;
    }
    CastSpecification o = (CastSpecification) other;
    return java.util.Objects.equals(
      this.operand,
      o.operand) && java.util.Objects.equals(
      this.target,
      o.target);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(operand) + 3 * java.util.Objects.hashCode(target);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CastSpecification other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      operand,
      other.operand);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      target,
      other.target);
  }

  public CastSpecification withOperand(openGql.grammar.CastOperand operand) {
    return new CastSpecification(operand, target);
  }

  public CastSpecification withTarget(openGql.grammar.ValueType target) {
    return new CastSpecification(operand, target);
  }
}
