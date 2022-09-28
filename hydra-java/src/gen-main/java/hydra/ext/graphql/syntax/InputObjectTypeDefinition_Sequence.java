package hydra.ext.graphql.syntax;

public class InputObjectTypeDefinition_Sequence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.InputObjectTypeDefinition.Sequence");
  
  public final java.util.Optional<hydra.ext.graphql.syntax.Description> description;
  
  public final hydra.ext.graphql.syntax.Name name;
  
  public final java.util.Optional<hydra.ext.graphql.syntax.Directives> directives;
  
  public final hydra.ext.graphql.syntax.InputFieldsDefinition inputFieldsDefinition;
  
  public InputObjectTypeDefinition_Sequence (java.util.Optional<hydra.ext.graphql.syntax.Description> description, hydra.ext.graphql.syntax.Name name, java.util.Optional<hydra.ext.graphql.syntax.Directives> directives, hydra.ext.graphql.syntax.InputFieldsDefinition inputFieldsDefinition) {
    this.description = description;
    this.name = name;
    this.directives = directives;
    this.inputFieldsDefinition = inputFieldsDefinition;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InputObjectTypeDefinition_Sequence)) {
      return false;
    }
    InputObjectTypeDefinition_Sequence o = (InputObjectTypeDefinition_Sequence) (other);
    return description.equals(o.description) && name.equals(o.name) && directives.equals(o.directives) && inputFieldsDefinition.equals(o.inputFieldsDefinition);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * name.hashCode() + 5 * directives.hashCode() + 7 * inputFieldsDefinition.hashCode();
  }
  
  public InputObjectTypeDefinition_Sequence withDescription(java.util.Optional<hydra.ext.graphql.syntax.Description> description) {
    return new InputObjectTypeDefinition_Sequence(description, name, directives, inputFieldsDefinition);
  }
  
  public InputObjectTypeDefinition_Sequence withName(hydra.ext.graphql.syntax.Name name) {
    return new InputObjectTypeDefinition_Sequence(description, name, directives, inputFieldsDefinition);
  }
  
  public InputObjectTypeDefinition_Sequence withDirectives(java.util.Optional<hydra.ext.graphql.syntax.Directives> directives) {
    return new InputObjectTypeDefinition_Sequence(description, name, directives, inputFieldsDefinition);
  }
  
  public InputObjectTypeDefinition_Sequence withInputFieldsDefinition(hydra.ext.graphql.syntax.InputFieldsDefinition inputFieldsDefinition) {
    return new InputObjectTypeDefinition_Sequence(description, name, directives, inputFieldsDefinition);
  }
}