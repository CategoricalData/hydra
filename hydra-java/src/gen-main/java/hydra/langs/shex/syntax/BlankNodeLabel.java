// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class BlankNodeLabel implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.BlankNodeLabel");
  
  public final hydra.langs.shex.syntax.BlankNodeLabel_Alts alts;
  
  public final hydra.util.Opt<java.util.List<hydra.langs.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt>> listOfAlts;
  
  public final hydra.langs.shex.syntax.PnChars pnChars;
  
  public BlankNodeLabel (hydra.langs.shex.syntax.BlankNodeLabel_Alts alts, hydra.util.Opt<java.util.List<hydra.langs.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt>> listOfAlts, hydra.langs.shex.syntax.PnChars pnChars) {
    if (alts == null) {
      throw new IllegalArgumentException("null value for 'alts' argument");
    }
    if (listOfAlts == null) {
      throw new IllegalArgumentException("null value for 'listOfAlts' argument");
    }
    if (pnChars == null) {
      throw new IllegalArgumentException("null value for 'pnChars' argument");
    }
    this.alts = alts;
    this.listOfAlts = listOfAlts;
    this.pnChars = pnChars;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BlankNodeLabel)) {
      return false;
    }
    BlankNodeLabel o = (BlankNodeLabel) (other);
    return alts.equals(o.alts) && listOfAlts.equals(o.listOfAlts) && pnChars.equals(o.pnChars);
  }
  
  @Override
  public int hashCode() {
    return 2 * alts.hashCode() + 3 * listOfAlts.hashCode() + 5 * pnChars.hashCode();
  }
  
  public BlankNodeLabel withAlts(hydra.langs.shex.syntax.BlankNodeLabel_Alts alts) {
    if (alts == null) {
      throw new IllegalArgumentException("null value for 'alts' argument");
    }
    return new BlankNodeLabel(alts, listOfAlts, pnChars);
  }
  
  public BlankNodeLabel withListOfAlts(hydra.util.Opt<java.util.List<hydra.langs.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt>> listOfAlts) {
    if (listOfAlts == null) {
      throw new IllegalArgumentException("null value for 'listOfAlts' argument");
    }
    return new BlankNodeLabel(alts, listOfAlts, pnChars);
  }
  
  public BlankNodeLabel withPnChars(hydra.langs.shex.syntax.PnChars pnChars) {
    if (pnChars == null) {
      throw new IllegalArgumentException("null value for 'pnChars' argument");
    }
    return new BlankNodeLabel(alts, listOfAlts, pnChars);
  }
}