// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public class UnaryTripleExpr_Sequence implements Serializable, Comparable<UnaryTripleExpr_Sequence> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.UnaryTripleExpr_Sequence");

  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("Sequence");

  public static final hydra.core.Name ALTS = new hydra.core.Name("alts");

  public final hydra.util.Maybe<hydra.shex.syntax.TripleExprLabel> Sequence;

  public final hydra.shex.syntax.UnaryTripleExpr_Sequence_Alts alts;

  public UnaryTripleExpr_Sequence (hydra.util.Maybe<hydra.shex.syntax.TripleExprLabel> Sequence, hydra.shex.syntax.UnaryTripleExpr_Sequence_Alts alts) {
    this.Sequence = Sequence;
    this.alts = alts;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnaryTripleExpr_Sequence)) {
      return false;
    }
    UnaryTripleExpr_Sequence o = (UnaryTripleExpr_Sequence) other;
    return java.util.Objects.equals(
      this.Sequence,
      o.Sequence) && java.util.Objects.equals(
      this.alts,
      o.alts);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(Sequence) + 3 * java.util.Objects.hashCode(alts);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnaryTripleExpr_Sequence other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      Sequence,
      other.Sequence);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      alts,
      other.alts);
  }

  public UnaryTripleExpr_Sequence withSequence(hydra.util.Maybe<hydra.shex.syntax.TripleExprLabel> Sequence) {
    return new UnaryTripleExpr_Sequence(Sequence, alts);
  }

  public UnaryTripleExpr_Sequence withAlts(hydra.shex.syntax.UnaryTripleExpr_Sequence_Alts alts) {
    return new UnaryTripleExpr_Sequence(Sequence, alts);
  }
}
