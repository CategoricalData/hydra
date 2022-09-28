package hydra.ext.graphql.syntax;

public class SelectionSet {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.SelectionSet");
  
  public final java.util.List<hydra.ext.graphql.syntax.Selection> listOfSelection;
  
  public SelectionSet (java.util.List<hydra.ext.graphql.syntax.Selection> listOfSelection) {
    this.listOfSelection = listOfSelection;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SelectionSet)) {
      return false;
    }
    SelectionSet o = (SelectionSet) (other);
    return listOfSelection.equals(o.listOfSelection);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfSelection.hashCode();
  }
}