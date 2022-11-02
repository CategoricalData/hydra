package hydra.ext.shex.syntax;

public class StringLiteral1 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.StringLiteral1");
  
  public final java.util.List<hydra.ext.shex.syntax.StringLiteral1_ListOfAlts_Elmt> listOfAlts;
  
  public StringLiteral1 (java.util.List<hydra.ext.shex.syntax.StringLiteral1_ListOfAlts_Elmt> listOfAlts) {
    this.listOfAlts = listOfAlts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringLiteral1)) {
      return false;
    }
    StringLiteral1 o = (StringLiteral1) (other);
    return listOfAlts.equals(o.listOfAlts);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfAlts.hashCode();
  }
}