// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ForItem implements Serializable, Comparable<ForItem> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ForItem");

  public static final hydra.core.Name ALIAS = new hydra.core.Name("alias");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public final String alias;

  public final openGql.grammar.ValueExpression source;

  public ForItem (String alias, openGql.grammar.ValueExpression source) {
    this.alias = alias;
    this.source = source;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForItem)) {
      return false;
    }
    ForItem o = (ForItem) other;
    return java.util.Objects.equals(
      this.alias,
      o.alias) && java.util.Objects.equals(
      this.source,
      o.source);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(alias) + 3 * java.util.Objects.hashCode(source);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ForItem other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      alias,
      other.alias);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      source,
      other.source);
  }

  public ForItem withAlias(String alias) {
    return new ForItem(alias, source);
  }

  public ForItem withSource(openGql.grammar.ValueExpression source) {
    return new ForItem(alias, source);
  }
}
