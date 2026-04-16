// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ForStatement implements Serializable, Comparable<ForStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ForStatement");

  public static final hydra.core.Name ITEM = new hydra.core.Name("item");

  public static final hydra.core.Name ORDINALITY_OR_OFFSET = new hydra.core.Name("ordinalityOrOffset");

  public final openGql.grammar.ForItem item;

  public final hydra.util.Maybe<openGql.grammar.ForOrdinalityOrOffset> ordinalityOrOffset;

  public ForStatement (openGql.grammar.ForItem item, hydra.util.Maybe<openGql.grammar.ForOrdinalityOrOffset> ordinalityOrOffset) {
    this.item = item;
    this.ordinalityOrOffset = ordinalityOrOffset;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForStatement)) {
      return false;
    }
    ForStatement o = (ForStatement) other;
    return java.util.Objects.equals(
      this.item,
      o.item) && java.util.Objects.equals(
      this.ordinalityOrOffset,
      o.ordinalityOrOffset);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(item) + 3 * java.util.Objects.hashCode(ordinalityOrOffset);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ForStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      item,
      other.item);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      ordinalityOrOffset,
      other.ordinalityOrOffset);
  }

  public ForStatement withItem(openGql.grammar.ForItem item) {
    return new ForStatement(item, ordinalityOrOffset);
  }

  public ForStatement withOrdinalityOrOffset(hydra.util.Maybe<openGql.grammar.ForOrdinalityOrOffset> ordinalityOrOffset) {
    return new ForStatement(item, ordinalityOrOffset);
  }
}
