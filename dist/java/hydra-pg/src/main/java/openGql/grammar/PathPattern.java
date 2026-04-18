// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class PathPattern implements Serializable, Comparable<PathPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PathPattern");

  public static final hydra.core.Name VARIABLE_DECLARATION = new hydra.core.Name("variableDeclaration");

  public static final hydra.core.Name PREFIX = new hydra.core.Name("prefix");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final hydra.util.Maybe<String> variableDeclaration;

  public final hydra.util.Maybe<openGql.grammar.PathPatternPrefix> prefix;

  public final openGql.grammar.PathPatternExpression expression;

  public PathPattern (hydra.util.Maybe<String> variableDeclaration, hydra.util.Maybe<openGql.grammar.PathPatternPrefix> prefix, openGql.grammar.PathPatternExpression expression) {
    this.variableDeclaration = variableDeclaration;
    this.prefix = prefix;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PathPattern)) {
      return false;
    }
    PathPattern o = (PathPattern) other;
    return java.util.Objects.equals(
      this.variableDeclaration,
      o.variableDeclaration) && java.util.Objects.equals(
      this.prefix,
      o.prefix) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variableDeclaration) + 3 * java.util.Objects.hashCode(prefix) + 5 * java.util.Objects.hashCode(expression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PathPattern other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      variableDeclaration,
      other.variableDeclaration);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      prefix,
      other.prefix);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expression,
      other.expression);
  }

  public PathPattern withVariableDeclaration(hydra.util.Maybe<String> variableDeclaration) {
    return new PathPattern(variableDeclaration, prefix, expression);
  }

  public PathPattern withPrefix(hydra.util.Maybe<openGql.grammar.PathPatternPrefix> prefix) {
    return new PathPattern(variableDeclaration, prefix, expression);
  }

  public PathPattern withExpression(openGql.grammar.PathPatternExpression expression) {
    return new PathPattern(variableDeclaration, prefix, expression);
  }
}
