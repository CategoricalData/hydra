package hydra.ext.shex.syntax;

public class StringLiteral2 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.StringLiteral2");
  
  public final java.util.List<hydra.ext.shex.syntax.StringLiteral2_ListOfAlts_Elmt> listOfAlts;
  
  public StringLiteral2 (java.util.List<hydra.ext.shex.syntax.StringLiteral2_ListOfAlts_Elmt> listOfAlts) {
    this.listOfAlts = listOfAlts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringLiteral2)) {
      return false;
    }
    StringLiteral2 o = (StringLiteral2) (other);
    return listOfAlts.equals(o.listOfAlts);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfAlts.hashCode();
  }
}