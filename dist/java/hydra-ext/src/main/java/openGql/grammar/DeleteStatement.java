// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class DeleteStatement implements Serializable, Comparable<DeleteStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DeleteStatement");

  public static final hydra.core.Name DETACH = new hydra.core.Name("detach");

  public static final hydra.core.Name ITEMS = new hydra.core.Name("items");

  public final hydra.util.Maybe<openGql.grammar.DetachOption> detach;

  public final java.util.List<openGql.grammar.ValueExpression> items;

  public DeleteStatement (hydra.util.Maybe<openGql.grammar.DetachOption> detach, java.util.List<openGql.grammar.ValueExpression> items) {
    this.detach = detach;
    this.items = items;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DeleteStatement)) {
      return false;
    }
    DeleteStatement o = (DeleteStatement) other;
    return java.util.Objects.equals(
      this.detach,
      o.detach) && java.util.Objects.equals(
      this.items,
      o.items);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(detach) + 3 * java.util.Objects.hashCode(items);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DeleteStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      detach,
      other.detach);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      items,
      other.items);
  }

  public DeleteStatement withDetach(hydra.util.Maybe<openGql.grammar.DetachOption> detach) {
    return new DeleteStatement(detach, items);
  }

  public DeleteStatement withItems(java.util.List<openGql.grammar.ValueExpression> items) {
    return new DeleteStatement(detach, items);
  }
}
