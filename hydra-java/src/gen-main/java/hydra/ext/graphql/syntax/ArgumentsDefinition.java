package hydra.ext.graphql.syntax;

public class ArgumentsDefinition {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.ArgumentsDefinition");
  
  public final java.util.List<hydra.ext.graphql.syntax.InputValueDefinition> listOfInputValueDefinition;
  
  public ArgumentsDefinition (java.util.List<hydra.ext.graphql.syntax.InputValueDefinition> listOfInputValueDefinition) {
    this.listOfInputValueDefinition = listOfInputValueDefinition;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArgumentsDefinition)) {
      return false;
    }
    ArgumentsDefinition o = (ArgumentsDefinition) (other);
    return listOfInputValueDefinition.equals(o.listOfInputValueDefinition);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfInputValueDefinition.hashCode();
  }
}