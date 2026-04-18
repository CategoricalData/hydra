// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class FocusedLinearQueryStatementPartsAndResult implements Serializable, Comparable<FocusedLinearQueryStatementPartsAndResult> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.FocusedLinearQueryStatementPartsAndResult");

  public static final hydra.core.Name PARTS = new hydra.core.Name("parts");

  public static final hydra.core.Name RESULT = new hydra.core.Name("result");

  public final java.util.List<openGql.grammar.FocusedLinearQueryStatementPart> parts;

  public final openGql.grammar.FocusedLinearQueryAndPrimitiveResultStatementPart result;

  public FocusedLinearQueryStatementPartsAndResult (java.util.List<openGql.grammar.FocusedLinearQueryStatementPart> parts, openGql.grammar.FocusedLinearQueryAndPrimitiveResultStatementPart result) {
    this.parts = parts;
    this.result = result;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FocusedLinearQueryStatementPartsAndResult)) {
      return false;
    }
    FocusedLinearQueryStatementPartsAndResult o = (FocusedLinearQueryStatementPartsAndResult) other;
    return java.util.Objects.equals(
      this.parts,
      o.parts) && java.util.Objects.equals(
      this.result,
      o.result);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(parts) + 3 * java.util.Objects.hashCode(result);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FocusedLinearQueryStatementPartsAndResult other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      parts,
      other.parts);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      result,
      other.result);
  }

  public FocusedLinearQueryStatementPartsAndResult withParts(java.util.List<openGql.grammar.FocusedLinearQueryStatementPart> parts) {
    return new FocusedLinearQueryStatementPartsAndResult(parts, result);
  }

  public FocusedLinearQueryStatementPartsAndResult withResult(openGql.grammar.FocusedLinearQueryAndPrimitiveResultStatementPart result) {
    return new FocusedLinearQueryStatementPartsAndResult(parts, result);
  }
}
