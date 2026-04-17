// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SelectStatement implements Serializable, Comparable<SelectStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SelectStatement");

  public static final hydra.core.Name QUANTIFIER = new hydra.core.Name("quantifier");

  public static final hydra.core.Name ITEMS = new hydra.core.Name("items");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.util.Maybe<openGql.grammar.SetQuantifier> quantifier;

  public final openGql.grammar.SelectItems items;

  public final hydra.util.Maybe<openGql.grammar.SelectStatementBodyAndClauses> body;

  public SelectStatement (hydra.util.Maybe<openGql.grammar.SetQuantifier> quantifier, openGql.grammar.SelectItems items, hydra.util.Maybe<openGql.grammar.SelectStatementBodyAndClauses> body) {
    this.quantifier = quantifier;
    this.items = items;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SelectStatement)) {
      return false;
    }
    SelectStatement o = (SelectStatement) other;
    return java.util.Objects.equals(
      this.quantifier,
      o.quantifier) && java.util.Objects.equals(
      this.items,
      o.items) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(quantifier) + 3 * java.util.Objects.hashCode(items) + 5 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SelectStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      quantifier,
      other.quantifier);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      items,
      other.items);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public SelectStatement withQuantifier(hydra.util.Maybe<openGql.grammar.SetQuantifier> quantifier) {
    return new SelectStatement(quantifier, items, body);
  }

  public SelectStatement withItems(openGql.grammar.SelectItems items) {
    return new SelectStatement(quantifier, items, body);
  }

  public SelectStatement withBody(hydra.util.Maybe<openGql.grammar.SelectStatementBodyAndClauses> body) {
    return new SelectStatement(quantifier, items, body);
  }
}
