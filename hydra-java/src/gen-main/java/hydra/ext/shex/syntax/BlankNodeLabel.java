package hydra.ext.shex.syntax;

public class BlankNodeLabel {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.BlankNodeLabel");
  
  public final hydra.ext.shex.syntax.BlankNodeLabel_Alts alts;
  
  public final java.util.Optional<java.util.List<hydra.ext.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt>> listOfAlts;
  
  public final hydra.ext.shex.syntax.PnChars pnChars;
  
  public BlankNodeLabel (hydra.ext.shex.syntax.BlankNodeLabel_Alts alts, java.util.Optional<java.util.List<hydra.ext.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt>> listOfAlts, hydra.ext.shex.syntax.PnChars pnChars) {
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
  
  public BlankNodeLabel withAlts(hydra.ext.shex.syntax.BlankNodeLabel_Alts alts) {
    return new BlankNodeLabel(alts, listOfAlts, pnChars);
  }
  
  public BlankNodeLabel withListOfAlts(java.util.Optional<java.util.List<hydra.ext.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt>> listOfAlts) {
    return new BlankNodeLabel(alts, listOfAlts, pnChars);
  }
  
  public BlankNodeLabel withPnChars(hydra.ext.shex.syntax.PnChars pnChars) {
    return new BlankNodeLabel(alts, listOfAlts, pnChars);
  }
}