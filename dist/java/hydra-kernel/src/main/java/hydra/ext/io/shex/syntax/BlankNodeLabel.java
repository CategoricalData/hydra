// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class BlankNodeLabel implements Serializable, Comparable<BlankNodeLabel> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.BlankNodeLabel");

  public static final hydra.core.Name ALTS = new hydra.core.Name("alts");

  public static final hydra.core.Name LIST_OF_ALTS = new hydra.core.Name("ListOfAlts");

  public static final hydra.core.Name PN_CHARS = new hydra.core.Name("PnChars");

  public final hydra.ext.io.shex.syntax.BlankNodeLabel_Alts alts;

  public final hydra.util.Maybe<java.util.List<hydra.ext.io.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt>> ListOfAlts;

  public final hydra.ext.io.shex.syntax.PnChars PnChars;

  public BlankNodeLabel (hydra.ext.io.shex.syntax.BlankNodeLabel_Alts alts, hydra.util.Maybe<java.util.List<hydra.ext.io.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt>> ListOfAlts, hydra.ext.io.shex.syntax.PnChars PnChars) {
    this.alts = alts;
    this.ListOfAlts = ListOfAlts;
    this.PnChars = PnChars;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BlankNodeLabel)) {
      return false;
    }
    BlankNodeLabel o = (BlankNodeLabel) other;
    return java.util.Objects.equals(
      this.alts,
      o.alts) && java.util.Objects.equals(
      this.ListOfAlts,
      o.ListOfAlts) && java.util.Objects.equals(
      this.PnChars,
      o.PnChars);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(alts) + 3 * java.util.Objects.hashCode(ListOfAlts) + 5 * java.util.Objects.hashCode(PnChars);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(BlankNodeLabel other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      alts,
      other.alts);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      ListOfAlts,
      other.ListOfAlts);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      PnChars,
      other.PnChars);
  }

  public BlankNodeLabel withAlts(hydra.ext.io.shex.syntax.BlankNodeLabel_Alts alts) {
    return new BlankNodeLabel(alts, ListOfAlts, PnChars);
  }

  public BlankNodeLabel withListOfAlts(hydra.util.Maybe<java.util.List<hydra.ext.io.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt>> ListOfAlts) {
    return new BlankNodeLabel(alts, ListOfAlts, PnChars);
  }

  public BlankNodeLabel withPnChars(hydra.ext.io.shex.syntax.PnChars PnChars) {
    return new BlankNodeLabel(alts, ListOfAlts, PnChars);
  }
}
