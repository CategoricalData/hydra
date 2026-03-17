// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class PnLocal_Sequence_Option implements Serializable, Comparable<PnLocal_Sequence_Option> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.PnLocal_Sequence_Option");

  public static final hydra.core.Name LIST_OF_ALTS = new hydra.core.Name("listOfAlts");

  public static final hydra.core.Name ALTS = new hydra.core.Name("alts");

  public final hydra.util.ConsList<hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt> listOfAlts;

  public final hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_Alts alts;

  public PnLocal_Sequence_Option (hydra.util.ConsList<hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt> listOfAlts, hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_Alts alts) {
    this.listOfAlts = listOfAlts;
    this.alts = alts;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PnLocal_Sequence_Option)) {
      return false;
    }
    PnLocal_Sequence_Option o = (PnLocal_Sequence_Option) other;
    return java.util.Objects.equals(
      this.listOfAlts,
      o.listOfAlts) && java.util.Objects.equals(
      this.alts,
      o.alts);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(listOfAlts) + 3 * java.util.Objects.hashCode(alts);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PnLocal_Sequence_Option other) {
    int cmp = 0;
    cmp = ((Comparable) listOfAlts).compareTo(other.listOfAlts);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) alts).compareTo(other.alts);
  }

  public PnLocal_Sequence_Option withListOfAlts(hydra.util.ConsList<hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt> listOfAlts) {
    return new PnLocal_Sequence_Option(listOfAlts, alts);
  }

  public PnLocal_Sequence_Option withAlts(hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_Alts alts) {
    return new PnLocal_Sequence_Option(listOfAlts, alts);
  }
}
