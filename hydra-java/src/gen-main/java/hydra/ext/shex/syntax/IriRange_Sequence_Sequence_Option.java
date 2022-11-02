package hydra.ext.shex.syntax;

public class IriRange_Sequence_Sequence_Option {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.IriRange.Sequence.Sequence.Option");
  
  public final java.util.List<hydra.ext.shex.syntax.Exclusion> listOfExclusion;
  
  public IriRange_Sequence_Sequence_Option (java.util.List<hydra.ext.shex.syntax.Exclusion> listOfExclusion) {
    this.listOfExclusion = listOfExclusion;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IriRange_Sequence_Sequence_Option)) {
      return false;
    }
    IriRange_Sequence_Sequence_Option o = (IriRange_Sequence_Sequence_Option) (other);
    return listOfExclusion.equals(o.listOfExclusion);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfExclusion.hashCode();
  }
}