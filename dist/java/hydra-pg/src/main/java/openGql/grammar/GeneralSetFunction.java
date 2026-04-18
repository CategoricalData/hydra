// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class GeneralSetFunction implements Serializable, Comparable<GeneralSetFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GeneralSetFunction");

  public static final hydra.core.Name FUNCTION_TYPE = new hydra.core.Name("functionType");

  public static final hydra.core.Name SET_QUANTIFIER = new hydra.core.Name("setQuantifier");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public final openGql.grammar.GeneralSetFunctionType functionType;

  public final hydra.util.Maybe<openGql.grammar.SetQuantifier> setQuantifier;

  public final openGql.grammar.ValueExpression valueExpression;

  public GeneralSetFunction (openGql.grammar.GeneralSetFunctionType functionType, hydra.util.Maybe<openGql.grammar.SetQuantifier> setQuantifier, openGql.grammar.ValueExpression valueExpression) {
    this.functionType = functionType;
    this.setQuantifier = setQuantifier;
    this.valueExpression = valueExpression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GeneralSetFunction)) {
      return false;
    }
    GeneralSetFunction o = (GeneralSetFunction) other;
    return java.util.Objects.equals(
      this.functionType,
      o.functionType) && java.util.Objects.equals(
      this.setQuantifier,
      o.setQuantifier) && java.util.Objects.equals(
      this.valueExpression,
      o.valueExpression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(functionType) + 3 * java.util.Objects.hashCode(setQuantifier) + 5 * java.util.Objects.hashCode(valueExpression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GeneralSetFunction other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      functionType,
      other.functionType);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      setQuantifier,
      other.setQuantifier);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      valueExpression,
      other.valueExpression);
  }

  public GeneralSetFunction withFunctionType(openGql.grammar.GeneralSetFunctionType functionType) {
    return new GeneralSetFunction(functionType, setQuantifier, valueExpression);
  }

  public GeneralSetFunction withSetQuantifier(hydra.util.Maybe<openGql.grammar.SetQuantifier> setQuantifier) {
    return new GeneralSetFunction(functionType, setQuantifier, valueExpression);
  }

  public GeneralSetFunction withValueExpression(openGql.grammar.ValueExpression valueExpression) {
    return new GeneralSetFunction(functionType, setQuantifier, valueExpression);
  }
}
