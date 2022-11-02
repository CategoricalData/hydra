package hydra.ext.shex.syntax;

public class PnLocal_Sequence_Option {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.PnLocal.Sequence.Option");
  
  public final java.util.List<hydra.ext.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt> listOfAlts;
  
  public final hydra.ext.shex.syntax.PnLocal_Sequence_Option_Alts alts;
  
  public PnLocal_Sequence_Option (java.util.List<hydra.ext.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt> listOfAlts, hydra.ext.shex.syntax.PnLocal_Sequence_Option_Alts alts) {
    this.listOfAlts = listOfAlts;
    this.alts = alts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PnLocal_Sequence_Option)) {
      return false;
    }
    PnLocal_Sequence_Option o = (PnLocal_Sequence_Option) (other);
    return listOfAlts.equals(o.listOfAlts) && alts.equals(o.alts);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfAlts.hashCode() + 3 * alts.hashCode();
  }
  
  public PnLocal_Sequence_Option withListOfAlts(java.util.List<hydra.ext.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt> listOfAlts) {
    return new PnLocal_Sequence_Option(listOfAlts, alts);
  }
  
  public PnLocal_Sequence_Option withAlts(hydra.ext.shex.syntax.PnLocal_Sequence_Option_Alts alts) {
    return new PnLocal_Sequence_Option(listOfAlts, alts);
  }
}