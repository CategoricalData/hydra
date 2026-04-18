// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SetOperator implements Serializable, Comparable<SetOperator> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SetOperator");

  public static final hydra.core.Name OPERATOR_TYPE = new hydra.core.Name("operatorType");

  public static final hydra.core.Name QUANTIFIER = new hydra.core.Name("quantifier");

  public final openGql.grammar.SetOperatorType operatorType;

  public final hydra.util.Maybe<openGql.grammar.SetQuantifier> quantifier;

  public SetOperator (openGql.grammar.SetOperatorType operatorType, hydra.util.Maybe<openGql.grammar.SetQuantifier> quantifier) {
    this.operatorType = operatorType;
    this.quantifier = quantifier;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SetOperator)) {
      return false;
    }
    SetOperator o = (SetOperator) other;
    return java.util.Objects.equals(
      this.operatorType,
      o.operatorType) && java.util.Objects.equals(
      this.quantifier,
      o.quantifier);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(operatorType) + 3 * java.util.Objects.hashCode(quantifier);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SetOperator other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      operatorType,
      other.operatorType);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      quantifier,
      other.quantifier);
  }

  public SetOperator withOperatorType(openGql.grammar.SetOperatorType operatorType) {
    return new SetOperator(operatorType, quantifier);
  }

  public SetOperator withQuantifier(hydra.util.Maybe<openGql.grammar.SetQuantifier> quantifier) {
    return new SetOperator(operatorType, quantifier);
  }
}
