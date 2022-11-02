package hydra.ext.shex.syntax;

public class Code {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.Code");
  
  public final java.util.List<hydra.ext.shex.syntax.Code_ListOfAlts_Elmt> listOfAlts;
  
  public Code (java.util.List<hydra.ext.shex.syntax.Code_ListOfAlts_Elmt> listOfAlts) {
    this.listOfAlts = listOfAlts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Code)) {
      return false;
    }
    Code o = (Code) (other);
    return listOfAlts.equals(o.listOfAlts);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfAlts.hashCode();
  }
}