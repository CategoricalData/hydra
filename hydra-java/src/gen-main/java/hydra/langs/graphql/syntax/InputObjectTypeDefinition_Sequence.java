// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class InputObjectTypeDefinition_Sequence implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/graphql/syntax.InputObjectTypeDefinition.Sequence");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public static final hydra.core.Name FIELD_NAME_INPUT_FIELDS_DEFINITION = new hydra.core.Name("inputFieldsDefinition");
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Description> description;
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public final hydra.langs.graphql.syntax.InputFieldsDefinition inputFieldsDefinition;
  
  public InputObjectTypeDefinition_Sequence (hydra.util.Opt<hydra.langs.graphql.syntax.Description> description, hydra.langs.graphql.syntax.Name name, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives, hydra.langs.graphql.syntax.InputFieldsDefinition inputFieldsDefinition) {
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((directives));
    java.util.Objects.requireNonNull((inputFieldsDefinition));
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
    java.util.Objects.requireNonNull((description));
    return new InputObjectTypeDefinition_Sequence(description, name, directives, inputFieldsDefinition);
  }
  
  public InputObjectTypeDefinition_Sequence withName(hydra.langs.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new InputObjectTypeDefinition_Sequence(description, name, directives, inputFieldsDefinition);
  }
  
  public InputObjectTypeDefinition_Sequence withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new InputObjectTypeDefinition_Sequence(description, name, directives, inputFieldsDefinition);
  }
  
  public InputObjectTypeDefinition_Sequence withInputFieldsDefinition(hydra.langs.graphql.syntax.InputFieldsDefinition inputFieldsDefinition) {
    java.util.Objects.requireNonNull((inputFieldsDefinition));
    return new InputObjectTypeDefinition_Sequence(description, name, directives, inputFieldsDefinition);
  }
}