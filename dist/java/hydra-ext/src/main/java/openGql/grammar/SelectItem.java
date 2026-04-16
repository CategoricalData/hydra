// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SelectItem implements Serializable, Comparable<SelectItem> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SelectItem");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name ALIAS = new hydra.core.Name("alias");

  public final openGql.grammar.ValueExpression expression;

  public final hydra.util.Maybe<String> alias;

  public SelectItem (openGql.grammar.ValueExpression expression, hydra.util.Maybe<String> alias) {
    this.expression = expression;
    this.alias = alias;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SelectItem)) {
      return false;
    }
    SelectItem o = (SelectItem) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.alias,
      o.alias);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression) + 3 * java.util.Objects.hashCode(alias);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SelectItem other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      expression,
      other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      alias,
      other.alias);
  }

  public SelectItem withExpression(openGql.grammar.ValueExpression expression) {
    return new SelectItem(expression, alias);
  }

  public SelectItem withAlias(hydra.util.Maybe<String> alias) {
    return new SelectItem(expression, alias);
  }
}
