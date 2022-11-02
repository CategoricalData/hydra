package hydra.ext.shex.syntax;

public class IriRange_Sequence2 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.IriRange.Sequence2");
  
  public final java.util.List<hydra.ext.shex.syntax.Exclusion> listOfExclusion;
  
  public IriRange_Sequence2 (java.util.List<hydra.ext.shex.syntax.Exclusion> listOfExclusion) {
    this.listOfExclusion = listOfExclusion;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IriRange_Sequence2)) {
      return false;
    }
    IriRange_Sequence2 o = (IriRange_Sequence2) (other);
    return listOfExclusion.equals(o.listOfExclusion);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfExclusion.hashCode();
  }
}