package hydra.ext.shex.syntax;

public class Regexp_ListOfAlts_Elmt_Sequence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.Regexp.ListOfAlts.Elmt.Sequence");
  
  public final String regex;
  
  public Regexp_ListOfAlts_Elmt_Sequence (String regex) {
    this.regex = regex;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Regexp_ListOfAlts_Elmt_Sequence)) {
      return false;
    }
    Regexp_ListOfAlts_Elmt_Sequence o = (Regexp_ListOfAlts_Elmt_Sequence) (other);
    return regex.equals(o.regex);
  }
  
  @Override
  public int hashCode() {
    return 2 * regex.hashCode();
  }
}