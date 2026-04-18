// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ParenthesizedPathPatternExpression implements Serializable, Comparable<ParenthesizedPathPatternExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ParenthesizedPathPatternExpression");

  public static final hydra.core.Name SUBPATH_DECLARATION = new hydra.core.Name("subpathDeclaration");

  public static final hydra.core.Name PATH_MODE = new hydra.core.Name("pathMode");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name WHERE_CLAUSE = new hydra.core.Name("whereClause");

  public final hydra.util.Maybe<String> subpathDeclaration;

  public final hydra.util.Maybe<openGql.grammar.PathModePrefix> pathMode;

  public final openGql.grammar.PathPatternExpression expression;

  public final hydra.util.Maybe<openGql.grammar.ValueExpression> whereClause;

  public ParenthesizedPathPatternExpression (hydra.util.Maybe<String> subpathDeclaration, hydra.util.Maybe<openGql.grammar.PathModePrefix> pathMode, openGql.grammar.PathPatternExpression expression, hydra.util.Maybe<openGql.grammar.ValueExpression> whereClause) {
    this.subpathDeclaration = subpathDeclaration;
    this.pathMode = pathMode;
    this.expression = expression;
    this.whereClause = whereClause;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParenthesizedPathPatternExpression)) {
      return false;
    }
    ParenthesizedPathPatternExpression o = (ParenthesizedPathPatternExpression) other;
    return java.util.Objects.equals(
      this.subpathDeclaration,
      o.subpathDeclaration) && java.util.Objects.equals(
      this.pathMode,
      o.pathMode) && java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.whereClause,
      o.whereClause);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(subpathDeclaration) + 3 * java.util.Objects.hashCode(pathMode) + 5 * java.util.Objects.hashCode(expression) + 7 * java.util.Objects.hashCode(whereClause);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ParenthesizedPathPatternExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      subpathDeclaration,
      other.subpathDeclaration);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      pathMode,
      other.pathMode);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      expression,
      other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      whereClause,
      other.whereClause);
  }

  public ParenthesizedPathPatternExpression withSubpathDeclaration(hydra.util.Maybe<String> subpathDeclaration) {
    return new ParenthesizedPathPatternExpression(subpathDeclaration, pathMode, expression, whereClause);
  }

  public ParenthesizedPathPatternExpression withPathMode(hydra.util.Maybe<openGql.grammar.PathModePrefix> pathMode) {
    return new ParenthesizedPathPatternExpression(subpathDeclaration, pathMode, expression, whereClause);
  }

  public ParenthesizedPathPatternExpression withExpression(openGql.grammar.PathPatternExpression expression) {
    return new ParenthesizedPathPatternExpression(subpathDeclaration, pathMode, expression, whereClause);
  }

  public ParenthesizedPathPatternExpression withWhereClause(hydra.util.Maybe<openGql.grammar.ValueExpression> whereClause) {
    return new ParenthesizedPathPatternExpression(subpathDeclaration, pathMode, expression, whereClause);
  }
}
