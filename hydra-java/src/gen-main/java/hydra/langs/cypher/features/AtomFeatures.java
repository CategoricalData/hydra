// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * Various kinds of atomic expressions
 */
public class AtomFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.AtomFeatures");
  
  public static final hydra.core.Name FIELD_NAME_CASE_EXPRESSION = new hydra.core.Name("caseExpression");
  
  public static final hydra.core.Name FIELD_NAME_COUNT = new hydra.core.Name("count");
  
  public static final hydra.core.Name FIELD_NAME_EXISTENTIAL_SUBQUERY = new hydra.core.Name("existentialSubquery");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION_INVOCATION = new hydra.core.Name("functionInvocation");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETER = new hydra.core.Name("parameter");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN_COMPREHENSION = new hydra.core.Name("patternComprehension");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN_PREDICATE = new hydra.core.Name("patternPredicate");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE = new hydra.core.Name("variable");
  
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
    java.util.Objects.requireNonNull((caseExpression));
    java.util.Objects.requireNonNull((count));
    java.util.Objects.requireNonNull((existentialSubquery));
    java.util.Objects.requireNonNull((functionInvocation));
    java.util.Objects.requireNonNull((parameter));
    java.util.Objects.requireNonNull((patternComprehension));
    java.util.Objects.requireNonNull((patternPredicate));
    java.util.Objects.requireNonNull((variable));
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
    AtomFeatures o = (AtomFeatures) (other);
    return caseExpression.equals(o.caseExpression) && count.equals(o.count) && existentialSubquery.equals(o.existentialSubquery) && functionInvocation.equals(o.functionInvocation) && parameter.equals(o.parameter) && patternComprehension.equals(o.patternComprehension) && patternPredicate.equals(o.patternPredicate) && variable.equals(o.variable);
  }
  
  @Override
  public int hashCode() {
    return 2 * caseExpression.hashCode() + 3 * count.hashCode() + 5 * existentialSubquery.hashCode() + 7 * functionInvocation.hashCode() + 11 * parameter.hashCode() + 13 * patternComprehension.hashCode() + 17 * patternPredicate.hashCode() + 19 * variable.hashCode();
  }
  
  public AtomFeatures withCaseExpression(Boolean caseExpression) {
    java.util.Objects.requireNonNull((caseExpression));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, parameter, patternComprehension, patternPredicate, variable);
  }
  
  public AtomFeatures withCount(Boolean count) {
    java.util.Objects.requireNonNull((count));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, parameter, patternComprehension, patternPredicate, variable);
  }
  
  public AtomFeatures withExistentialSubquery(Boolean existentialSubquery) {
    java.util.Objects.requireNonNull((existentialSubquery));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, parameter, patternComprehension, patternPredicate, variable);
  }
  
  public AtomFeatures withFunctionInvocation(Boolean functionInvocation) {
    java.util.Objects.requireNonNull((functionInvocation));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, parameter, patternComprehension, patternPredicate, variable);
  }
  
  public AtomFeatures withParameter(Boolean parameter) {
    java.util.Objects.requireNonNull((parameter));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, parameter, patternComprehension, patternPredicate, variable);
  }
  
  public AtomFeatures withPatternComprehension(Boolean patternComprehension) {
    java.util.Objects.requireNonNull((patternComprehension));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, parameter, patternComprehension, patternPredicate, variable);
  }
  
  public AtomFeatures withPatternPredicate(Boolean patternPredicate) {
    java.util.Objects.requireNonNull((patternPredicate));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, parameter, patternComprehension, patternPredicate, variable);
  }
  
  public AtomFeatures withVariable(Boolean variable) {
    java.util.Objects.requireNonNull((variable));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, parameter, patternComprehension, patternPredicate, variable);
  }
}