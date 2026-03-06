// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Various kinds of atomic expressions
 */
public class AtomFeatures implements Serializable, Comparable<AtomFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.AtomFeatures");
  
  public static final hydra.core.Name CASE_EXPRESSION = new hydra.core.Name("caseExpression");
  
  public static final hydra.core.Name COUNT = new hydra.core.Name("count");
  
  public static final hydra.core.Name EXISTENTIAL_SUBQUERY = new hydra.core.Name("existentialSubquery");
  
  public static final hydra.core.Name FUNCTION_INVOCATION = new hydra.core.Name("functionInvocation");
  
  public static final hydra.core.Name PARAMETER = new hydra.core.Name("parameter");
  
  public static final hydra.core.Name PATTERN_COMPREHENSION = new hydra.core.Name("patternComprehension");
  
  public static final hydra.core.Name PATTERN_PREDICATE = new hydra.core.Name("patternPredicate");
  
  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");
  
  /**
   * CASE expressions
   */
  public final Boolean caseExpression;
  
  /**
   * The COUNT (*) expression
   */
  public final Boolean count;
  
  /**
   * Existential subqueries
   */
  public final Boolean existentialSubquery;
  
  /**
   * Function invocation
   */
  public final Boolean functionInvocation;
  
  /**
   * Parameter expressions
   */
  public final Boolean parameter;
  
  /**
   * Pattern comprehensions
   */
  public final Boolean patternComprehension;
  
  /**
   * Relationship patterns as subexpressions
   */
  public final Boolean patternPredicate;
  
  /**
   * Variable expressions (note: included by most if not all implementations).
   */
  public final Boolean variable;
  
  public AtomFeatures (Boolean caseExpression, Boolean count, Boolean existentialSubquery, Boolean functionInvocation, Boolean parameter, Boolean patternComprehension, Boolean patternPredicate, Boolean variable) {
    this.caseExpression = caseExpression;
    this.count = count;
    this.existentialSubquery = existentialSubquery;
    this.functionInvocation = functionInvocation;
    this.parameter = parameter;
    this.patternComprehension = patternComprehension;
    this.patternPredicate = patternPredicate;
    this.variable = variable;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AtomFeatures)) {
      return false;
    }
    AtomFeatures o = (AtomFeatures) other;
    return java.util.Objects.equals(
      this.caseExpression,
      o.caseExpression) && java.util.Objects.equals(
      this.count,
      o.count) && java.util.Objects.equals(
      this.existentialSubquery,
      o.existentialSubquery) && java.util.Objects.equals(
      this.functionInvocation,
      o.functionInvocation) && java.util.Objects.equals(
      this.parameter,
      o.parameter) && java.util.Objects.equals(
      this.patternComprehension,
      o.patternComprehension) && java.util.Objects.equals(
      this.patternPredicate,
      o.patternPredicate) && java.util.Objects.equals(
      this.variable,
      o.variable);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(caseExpression) + 3 * java.util.Objects.hashCode(count) + 5 * java.util.Objects.hashCode(existentialSubquery) + 7 * java.util.Objects.hashCode(functionInvocation) + 11 * java.util.Objects.hashCode(parameter) + 13 * java.util.Objects.hashCode(patternComprehension) + 17 * java.util.Objects.hashCode(patternPredicate) + 19 * java.util.Objects.hashCode(variable);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AtomFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) caseExpression).compareTo(other.caseExpression);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) count).compareTo(other.count);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) existentialSubquery).compareTo(other.existentialSubquery);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) functionInvocation).compareTo(other.functionInvocation);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) parameter).compareTo(other.parameter);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) patternComprehension).compareTo(other.patternComprehension);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) patternPredicate).compareTo(other.patternPredicate);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) variable).compareTo(other.variable);
  }
  
  public AtomFeatures withCaseExpression(Boolean caseExpression) {
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, parameter, patternComprehension, patternPredicate, variable);
  }
  
  public AtomFeatures withCount(Boolean count) {
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, parameter, patternComprehension, patternPredicate, variable);
  }
  
  public AtomFeatures withExistentialSubquery(Boolean existentialSubquery) {
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, parameter, patternComprehension, patternPredicate, variable);
  }
  
  public AtomFeatures withFunctionInvocation(Boolean functionInvocation) {
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, parameter, patternComprehension, patternPredicate, variable);
  }
  
  public AtomFeatures withParameter(Boolean parameter) {
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, parameter, patternComprehension, patternPredicate, variable);
  }
  
  public AtomFeatures withPatternComprehension(Boolean patternComprehension) {
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, parameter, patternComprehension, patternPredicate, variable);
  }
  
  public AtomFeatures withPatternPredicate(Boolean patternPredicate) {
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, parameter, patternComprehension, patternPredicate, variable);
  }
  
  public AtomFeatures withVariable(Boolean variable) {
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, parameter, patternComprehension, patternPredicate, variable);
  }
}
