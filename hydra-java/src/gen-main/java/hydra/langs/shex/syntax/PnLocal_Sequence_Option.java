// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class PnLocal_Sequence_Option implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/shex/syntax.PnLocal.Sequence.Option");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_ALTS = new hydra.core.Name("listOfAlts");
  
  public static final hydra.core.Name FIELD_NAME_ALTS = new hydra.core.Name("alts");
  
  public final java.util.List<hydra.langs.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt> listOfAlts;
  
  public final hydra.langs.shex.syntax.PnLocal_Sequence_Option_Alts alts;
  
  public PnLocal_Sequence_Option (java.util.List<hydra.langs.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt> listOfAlts, hydra.langs.shex.syntax.PnLocal_Sequence_Option_Alts alts) {
    java.util.Objects.requireNonNull((listOfAlts));
    java.util.Objects.requireNonNull((alts));
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
  
  public PnLocal_Sequence_Option withListOfAlts(java.util.List<hydra.langs.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt> listOfAlts) {
    java.util.Objects.requireNonNull((listOfAlts));
    return new PnLocal_Sequence_Option(listOfAlts, alts);
  }
  
  public PnLocal_Sequence_Option withAlts(hydra.langs.shex.syntax.PnLocal_Sequence_Option_Alts alts) {
    java.util.Objects.requireNonNull((alts));
    return new PnLocal_Sequence_Option(listOfAlts, alts);
  }
}