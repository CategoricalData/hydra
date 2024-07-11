// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for various kinds of atomic expressions.
 */
public class AtomFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.AtomFeatures");
  
  /**
   * Whether to expect CASE expressions.
   */
  public final Boolean caseExpression;
  
  /**
   * Whether to expect the COUNT (*) expression.
   */
  public final Boolean count;
  
  /**
   * Whether to expect existential subqueries.
   */
  public final Boolean existentialSubquery;
  
  /**
   * Whether to expect function invocation.
   */
  public final Boolean functionInvocation;
  
  /**
   * Whether to expect lists, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.ListFeatures> list;
  
  /**
   * Whether to expect literal values, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.LiteralFeatures> literal;
  
  /**
   * Whether to expect parameter expressions.
   */
  public final Boolean parameter;
  
  /**
   * Whether to expect pattern comprehensions.
   */
  public final Boolean patternComprehension;
  
  /**
   * Whether to expect relationship patterns as subexpressions.
   */
  public final Boolean patternPredicate;
  
  /**
   * Whether to expect quantifier expressions, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.QuantifierFeatures> quantifier;
  
  /**
   * Whether to expect variable expressions (note: included by most if not all implementations).
   */
  public final Boolean variable;
  
  public AtomFeatures (Boolean caseExpression, Boolean count, Boolean existentialSubquery, Boolean functionInvocation, hydra.util.Opt<hydra.langs.cypher.features.ListFeatures> list, hydra.util.Opt<hydra.langs.cypher.features.LiteralFeatures> literal, Boolean parameter, Boolean patternComprehension, Boolean patternPredicate, hydra.util.Opt<hydra.langs.cypher.features.QuantifierFeatures> quantifier, Boolean variable) {
    java.util.Objects.requireNonNull((caseExpression));
    java.util.Objects.requireNonNull((count));
    java.util.Objects.requireNonNull((existentialSubquery));
    java.util.Objects.requireNonNull((functionInvocation));
    java.util.Objects.requireNonNull((list));
    java.util.Objects.requireNonNull((literal));
    java.util.Objects.requireNonNull((parameter));
    java.util.Objects.requireNonNull((patternComprehension));
    java.util.Objects.requireNonNull((patternPredicate));
    java.util.Objects.requireNonNull((quantifier));
    java.util.Objects.requireNonNull((variable));
    this.caseExpression = caseExpression;
    this.count = count;
    this.existentialSubquery = existentialSubquery;
    this.functionInvocation = functionInvocation;
    this.list = list;
    this.literal = literal;
    this.parameter = parameter;
    this.patternComprehension = patternComprehension;
    this.patternPredicate = patternPredicate;
    this.quantifier = quantifier;
    this.variable = variable;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AtomFeatures)) {
      return false;
    }
    AtomFeatures o = (AtomFeatures) (other);
    return caseExpression.equals(o.caseExpression) && count.equals(o.count) && existentialSubquery.equals(o.existentialSubquery) && functionInvocation.equals(o.functionInvocation) && list.equals(o.list) && literal.equals(o.literal) && parameter.equals(o.parameter) && patternComprehension.equals(o.patternComprehension) && patternPredicate.equals(o.patternPredicate) && quantifier.equals(o.quantifier) && variable.equals(o.variable);
  }
  
  @Override
  public int hashCode() {
    return 2 * caseExpression.hashCode() + 3 * count.hashCode() + 5 * existentialSubquery.hashCode() + 7 * functionInvocation.hashCode() + 11 * list.hashCode() + 13 * literal.hashCode() + 17 * parameter.hashCode() + 19 * patternComprehension.hashCode() + 23 * patternPredicate.hashCode() + 29 * quantifier.hashCode() + 31 * variable.hashCode();
  }
  
  public AtomFeatures withCaseExpression(Boolean caseExpression) {
    java.util.Objects.requireNonNull((caseExpression));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, list, literal, parameter, patternComprehension, patternPredicate, quantifier, variable);
  }
  
  public AtomFeatures withCount(Boolean count) {
    java.util.Objects.requireNonNull((count));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, list, literal, parameter, patternComprehension, patternPredicate, quantifier, variable);
  }
  
  public AtomFeatures withExistentialSubquery(Boolean existentialSubquery) {
    java.util.Objects.requireNonNull((existentialSubquery));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, list, literal, parameter, patternComprehension, patternPredicate, quantifier, variable);
  }
  
  public AtomFeatures withFunctionInvocation(Boolean functionInvocation) {
    java.util.Objects.requireNonNull((functionInvocation));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, list, literal, parameter, patternComprehension, patternPredicate, quantifier, variable);
  }
  
  public AtomFeatures withList(hydra.util.Opt<hydra.langs.cypher.features.ListFeatures> list) {
    java.util.Objects.requireNonNull((list));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, list, literal, parameter, patternComprehension, patternPredicate, quantifier, variable);
  }
  
  public AtomFeatures withLiteral(hydra.util.Opt<hydra.langs.cypher.features.LiteralFeatures> literal) {
    java.util.Objects.requireNonNull((literal));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, list, literal, parameter, patternComprehension, patternPredicate, quantifier, variable);
  }
  
  public AtomFeatures withParameter(Boolean parameter) {
    java.util.Objects.requireNonNull((parameter));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, list, literal, parameter, patternComprehension, patternPredicate, quantifier, variable);
  }
  
  public AtomFeatures withPatternComprehension(Boolean patternComprehension) {
    java.util.Objects.requireNonNull((patternComprehension));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, list, literal, parameter, patternComprehension, patternPredicate, quantifier, variable);
  }
  
  public AtomFeatures withPatternPredicate(Boolean patternPredicate) {
    java.util.Objects.requireNonNull((patternPredicate));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, list, literal, parameter, patternComprehension, patternPredicate, quantifier, variable);
  }
  
  public AtomFeatures withQuantifier(hydra.util.Opt<hydra.langs.cypher.features.QuantifierFeatures> quantifier) {
    java.util.Objects.requireNonNull((quantifier));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, list, literal, parameter, patternComprehension, patternPredicate, quantifier, variable);
  }
  
  public AtomFeatures withVariable(Boolean variable) {
    java.util.Objects.requireNonNull((variable));
    return new AtomFeatures(caseExpression, count, existentialSubquery, functionInvocation, list, literal, parameter, patternComprehension, patternPredicate, quantifier, variable);
  }
}