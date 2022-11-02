package hydra.ext.shex.syntax;

public class ExtraPropertySet {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.ExtraPropertySet");
  
  public final java.util.List<hydra.ext.shex.syntax.Predicate> listOfPredicate;
  
  public ExtraPropertySet (java.util.List<hydra.ext.shex.syntax.Predicate> listOfPredicate) {
    this.listOfPredicate = listOfPredicate;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExtraPropertySet)) {
      return false;
    }
    ExtraPropertySet o = (ExtraPropertySet) (other);
    return listOfPredicate.equals(o.listOfPredicate);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfPredicate.hashCode();
  }
}