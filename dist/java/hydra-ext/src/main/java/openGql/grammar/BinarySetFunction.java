// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class BinarySetFunction implements Serializable, Comparable<BinarySetFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.BinarySetFunction");

  public static final hydra.core.Name FUNCTION_TYPE = new hydra.core.Name("functionType");

  public static final hydra.core.Name DEPENDENT_VALUE = new hydra.core.Name("dependentValue");

  public static final hydra.core.Name INDEPENDENT_VALUE = new hydra.core.Name("independentValue");

  public final openGql.grammar.BinarySetFunctionType functionType;

  public final openGql.grammar.DependentValueExpression dependentValue;

  public final openGql.grammar.NumericValueExpression independentValue;

  public BinarySetFunction (openGql.grammar.BinarySetFunctionType functionType, openGql.grammar.DependentValueExpression dependentValue, openGql.grammar.NumericValueExpression independentValue) {
    this.functionType = functionType;
    this.dependentValue = dependentValue;
    this.independentValue = independentValue;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinarySetFunction)) {
      return false;
    }
    BinarySetFunction o = (BinarySetFunction) other;
    return java.util.Objects.equals(
      this.functionType,
      o.functionType) && java.util.Objects.equals(
      this.dependentValue,
      o.dependentValue) && java.util.Objects.equals(
      this.independentValue,
      o.independentValue);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(functionType) + 3 * java.util.Objects.hashCode(dependentValue) + 5 * java.util.Objects.hashCode(independentValue);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(BinarySetFunction other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      functionType,
      other.functionType);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      dependentValue,
      other.dependentValue);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      independentValue,
      other.independentValue);
  }

  public BinarySetFunction withFunctionType(openGql.grammar.BinarySetFunctionType functionType) {
    return new BinarySetFunction(functionType, dependentValue, independentValue);
  }

  public BinarySetFunction withDependentValue(openGql.grammar.DependentValueExpression dependentValue) {
    return new BinarySetFunction(functionType, dependentValue, independentValue);
  }

  public BinarySetFunction withIndependentValue(openGql.grammar.NumericValueExpression independentValue) {
    return new BinarySetFunction(functionType, dependentValue, independentValue);
  }
}
