package hydra.ext.graphql.syntax;

public class ListValue_Sequence2 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.ListValue.Sequence2");
  
  public final java.util.List<hydra.ext.graphql.syntax.Value> listOfValue;
  
  public ListValue_Sequence2 (java.util.List<hydra.ext.graphql.syntax.Value> listOfValue) {
    this.listOfValue = listOfValue;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListValue_Sequence2)) {
      return false;
    }
    ListValue_Sequence2 o = (ListValue_Sequence2) (other);
    return listOfValue.equals(o.listOfValue);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfValue.hashCode();
  }
}