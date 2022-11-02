package hydra.ext.shex.syntax;

public class Code_ListOfAlts_Elmt_Sequence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.Code.ListOfAlts.Elmt.Sequence");
  
  public final String regex;
  
  public Code_ListOfAlts_Elmt_Sequence (String regex) {
    this.regex = regex;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Code_ListOfAlts_Elmt_Sequence)) {
      return false;
    }
    Code_ListOfAlts_Elmt_Sequence o = (Code_ListOfAlts_Elmt_Sequence) (other);
    return regex.equals(o.regex);
  }
  
  @Override
  public int hashCode() {
    return 2 * regex.hashCode();
  }
}