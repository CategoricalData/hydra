// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class PnPrefix_Sequence_Option implements Serializable, Comparable<PnPrefix_Sequence_Option> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option");

  public static final hydra.core.Name ALTS = new hydra.core.Name("alts");

  public static final hydra.core.Name PN_CHARS = new hydra.core.Name("PnChars");

  public final hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option_Alts alts;

  public final hydra.ext.io.shex.syntax.PnChars PnChars;

  public PnPrefix_Sequence_Option (hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option_Alts alts, hydra.ext.io.shex.syntax.PnChars PnChars) {
    this.alts = alts;
    this.PnChars = PnChars;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PnPrefix_Sequence_Option)) {
      return false;
    }
    PnPrefix_Sequence_Option o = (PnPrefix_Sequence_Option) other;
    return java.util.Objects.equals(
      this.alts,
      o.alts) && java.util.Objects.equals(
      this.PnChars,
      o.PnChars);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(alts) + 3 * java.util.Objects.hashCode(PnChars);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PnPrefix_Sequence_Option other) {
    int cmp = 0;
    cmp = ((Comparable) alts).compareTo(other.alts);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) PnChars).compareTo(other.PnChars);
  }

  public PnPrefix_Sequence_Option withAlts(hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option_Alts alts) {
    return new PnPrefix_Sequence_Option(alts, PnChars);
  }

  public PnPrefix_Sequence_Option withPnChars(hydra.ext.io.shex.syntax.PnChars PnChars) {
    return new PnPrefix_Sequence_Option(alts, PnChars);
  }
}
