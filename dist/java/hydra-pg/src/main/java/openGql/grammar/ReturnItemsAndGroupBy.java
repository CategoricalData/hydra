// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ReturnItemsAndGroupBy implements Serializable, Comparable<ReturnItemsAndGroupBy> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ReturnItemsAndGroupBy");

  public static final hydra.core.Name QUANTIFIER = new hydra.core.Name("quantifier");

  public static final hydra.core.Name ITEMS = new hydra.core.Name("items");

  public static final hydra.core.Name GROUP_BY = new hydra.core.Name("groupBy");

  public final hydra.util.Maybe<openGql.grammar.SetQuantifier> quantifier;

  public final openGql.grammar.ReturnItems items;

  public final hydra.util.Maybe<openGql.grammar.GroupingElementList> groupBy;

  public ReturnItemsAndGroupBy (hydra.util.Maybe<openGql.grammar.SetQuantifier> quantifier, openGql.grammar.ReturnItems items, hydra.util.Maybe<openGql.grammar.GroupingElementList> groupBy) {
    this.quantifier = quantifier;
    this.items = items;
    this.groupBy = groupBy;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReturnItemsAndGroupBy)) {
      return false;
    }
    ReturnItemsAndGroupBy o = (ReturnItemsAndGroupBy) other;
    return java.util.Objects.equals(
      this.quantifier,
      o.quantifier) && java.util.Objects.equals(
      this.items,
      o.items) && java.util.Objects.equals(
      this.groupBy,
      o.groupBy);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(quantifier) + 3 * java.util.Objects.hashCode(items) + 5 * java.util.Objects.hashCode(groupBy);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ReturnItemsAndGroupBy other) {
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
      groupBy,
      other.groupBy);
  }

  public ReturnItemsAndGroupBy withQuantifier(hydra.util.Maybe<openGql.grammar.SetQuantifier> quantifier) {
    return new ReturnItemsAndGroupBy(quantifier, items, groupBy);
  }

  public ReturnItemsAndGroupBy withItems(openGql.grammar.ReturnItems items) {
    return new ReturnItemsAndGroupBy(quantifier, items, groupBy);
  }

  public ReturnItemsAndGroupBy withGroupBy(hydra.util.Maybe<openGql.grammar.GroupingElementList> groupBy) {
    return new ReturnItemsAndGroupBy(quantifier, items, groupBy);
  }
}
