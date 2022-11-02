package hydra.ext.shex.syntax;

public class StringLiteralLong1 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.StringLiteralLong1");
  
  public final java.util.List<hydra.ext.shex.syntax.StringLiteralLong1_ListOfAlts_Elmt> listOfAlts;
  
  public StringLiteralLong1 (java.util.List<hydra.ext.shex.syntax.StringLiteralLong1_ListOfAlts_Elmt> listOfAlts) {
    this.listOfAlts = listOfAlts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringLiteralLong1)) {
      return false;
    }
    StringLiteralLong1 o = (StringLiteralLong1) (other);
    return listOfAlts.equals(o.listOfAlts);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfAlts.hashCode();
  }
}