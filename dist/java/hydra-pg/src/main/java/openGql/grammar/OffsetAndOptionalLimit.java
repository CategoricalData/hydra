// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class OffsetAndOptionalLimit implements Serializable, Comparable<OffsetAndOptionalLimit> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.OffsetAndOptionalLimit");

  public static final hydra.core.Name OFFSET = new hydra.core.Name("offset");

  public static final hydra.core.Name LIMIT = new hydra.core.Name("limit");

  public final openGql.grammar.OffsetClause offset;

  public final hydra.util.Maybe<openGql.grammar.NonNegativeIntegerSpecification> limit;

  public OffsetAndOptionalLimit (openGql.grammar.OffsetClause offset, hydra.util.Maybe<openGql.grammar.NonNegativeIntegerSpecification> limit) {
    this.offset = offset;
    this.limit = limit;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OffsetAndOptionalLimit)) {
      return false;
    }
    OffsetAndOptionalLimit o = (OffsetAndOptionalLimit) other;
    return java.util.Objects.equals(
      this.offset,
      o.offset) && java.util.Objects.equals(
      this.limit,
      o.limit);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(offset) + 3 * java.util.Objects.hashCode(limit);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OffsetAndOptionalLimit other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      offset,
      other.offset);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      limit,
      other.limit);
  }

  public OffsetAndOptionalLimit withOffset(openGql.grammar.OffsetClause offset) {
    return new OffsetAndOptionalLimit(offset, limit);
  }

  public OffsetAndOptionalLimit withLimit(hydra.util.Maybe<openGql.grammar.NonNegativeIntegerSpecification> limit) {
    return new OffsetAndOptionalLimit(offset, limit);
  }
}
