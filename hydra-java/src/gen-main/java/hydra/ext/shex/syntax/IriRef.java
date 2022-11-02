package hydra.ext.shex.syntax;

public class IriRef {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.IriRef");
  
  public final java.util.List<hydra.ext.shex.syntax.IriRef_ListOfAlts_Elmt> listOfAlts;
  
  public IriRef (java.util.List<hydra.ext.shex.syntax.IriRef_ListOfAlts_Elmt> listOfAlts) {
    this.listOfAlts = listOfAlts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IriRef)) {
      return false;
    }
    IriRef o = (IriRef) (other);
    return listOfAlts.equals(o.listOfAlts);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfAlts.hashCode();
  }
}