// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class InputObjectTypeDefinition_Sequence implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.InputObjectTypeDefinition.Sequence");
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Description> description;
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public final hydra.langs.graphql.syntax.InputFieldsDefinition inputFieldsDefinition;
  
  public InputObjectTypeDefinition_Sequence (hydra.util.Opt<hydra.langs.graphql.syntax.Description> description, hydra.langs.graphql.syntax.Name name, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives, hydra.langs.graphql.syntax.InputFieldsDefinition inputFieldsDefinition) {
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    if (inputFieldsDefinition == null) {
      throw new IllegalArgumentException("null value for 'inputFieldsDefinition' argument");
    }
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
  
  public InputObjectTypeDefinition_Sequence withDescription(hydra.util.Opt<hydra.langs.graphql.syntax.Description> description) {
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    return new InputObjectTypeDefinition_Sequence(description, name, directives, inputFieldsDefinition);
  }
  
  public InputObjectTypeDefinition_Sequence withName(hydra.langs.graphql.syntax.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new InputObjectTypeDefinition_Sequence(description, name, directives, inputFieldsDefinition);
  }
  
  public InputObjectTypeDefinition_Sequence withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new InputObjectTypeDefinition_Sequence(description, name, directives, inputFieldsDefinition);
  }
  
  public InputObjectTypeDefinition_Sequence withInputFieldsDefinition(hydra.langs.graphql.syntax.InputFieldsDefinition inputFieldsDefinition) {
    if (inputFieldsDefinition == null) {
      throw new IllegalArgumentException("null value for 'inputFieldsDefinition' argument");
    }
    return new InputObjectTypeDefinition_Sequence(description, name, directives, inputFieldsDefinition);
  }
}