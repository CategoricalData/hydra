package hydra.ext.graphql.syntax;

public class InputFieldsDefinition {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.InputFieldsDefinition");
  
  public final java.util.List<hydra.ext.graphql.syntax.InputValueDefinition> listOfInputValueDefinition;
  
  public InputFieldsDefinition (java.util.List<hydra.ext.graphql.syntax.InputValueDefinition> listOfInputValueDefinition) {
    this.listOfInputValueDefinition = listOfInputValueDefinition;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InputFieldsDefinition)) {
      return false;
    }
    InputFieldsDefinition o = (InputFieldsDefinition) (other);
    return listOfInputValueDefinition.equals(o.listOfInputValueDefinition);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfInputValueDefinition.hashCode();
  }
}