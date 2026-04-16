// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Property comparison conditions: node(i).prop &gt; v, etc.
 */
public class PropertyComparisonCondition implements Serializable, Comparable<PropertyComparisonCondition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition");

  public static final hydra.core.Name TARGET = new hydra.core.Name("target");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final com.gdblab.pathAlgebra.expressions.PathElement target;

  public final String property;

  public final com.gdblab.pathAlgebra.expressions.ComparisonOperator operator;

  public final com.gdblab.pathAlgebra.expressions.LiteralValue value;

  public PropertyComparisonCondition (com.gdblab.pathAlgebra.expressions.PathElement target, String property, com.gdblab.pathAlgebra.expressions.ComparisonOperator operator, com.gdblab.pathAlgebra.expressions.LiteralValue value) {
    this.target = target;
    this.property = property;
    this.operator = operator;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyComparisonCondition)) {
      return false;
    }
    PropertyComparisonCondition o = (PropertyComparisonCondition) other;
    return java.util.Objects.equals(
      this.target,
      o.target) && java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.operator,
      o.operator) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(target) + 3 * java.util.Objects.hashCode(property) + 5 * java.util.Objects.hashCode(operator) + 7 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PropertyComparisonCondition other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      target,
      other.target);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      property,
      other.property);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      operator,
      other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }

  public PropertyComparisonCondition withTarget(com.gdblab.pathAlgebra.expressions.PathElement target) {
    return new PropertyComparisonCondition(target, property, operator, value);
  }

  public PropertyComparisonCondition withProperty(String property) {
    return new PropertyComparisonCondition(target, property, operator, value);
  }

  public PropertyComparisonCondition withOperator(com.gdblab.pathAlgebra.expressions.ComparisonOperator operator) {
    return new PropertyComparisonCondition(target, property, operator, value);
  }

  public PropertyComparisonCondition withValue(com.gdblab.pathAlgebra.expressions.LiteralValue value) {
    return new PropertyComparisonCondition(target, property, operator, value);
  }
}
