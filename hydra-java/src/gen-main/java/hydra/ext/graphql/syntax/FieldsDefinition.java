package hydra.ext.graphql.syntax;

public class FieldsDefinition {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.FieldsDefinition");
  
  public final java.util.List<hydra.ext.graphql.syntax.FieldDefinition> listOfFieldDefinition;
  
  public FieldsDefinition (java.util.List<hydra.ext.graphql.syntax.FieldDefinition> listOfFieldDefinition) {
    this.listOfFieldDefinition = listOfFieldDefinition;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldsDefinition)) {
      return false;
    }
    FieldsDefinition o = (FieldsDefinition) (other);
    return listOfFieldDefinition.equals(o.listOfFieldDefinition);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfFieldDefinition.hashCode();
  }
}