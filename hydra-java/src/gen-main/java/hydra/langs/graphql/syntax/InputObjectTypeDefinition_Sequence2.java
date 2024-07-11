// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class InputObjectTypeDefinition_Sequence2 implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.InputObjectTypeDefinition.Sequence2");
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Description> description;
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public InputObjectTypeDefinition_Sequence2 (hydra.util.Opt<hydra.langs.graphql.syntax.Description> description, hydra.langs.graphql.syntax.Name name, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    this.description = description;
    this.name = name;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InputObjectTypeDefinition_Sequence2)) {
      return false;
    }
    InputObjectTypeDefinition_Sequence2 o = (InputObjectTypeDefinition_Sequence2) (other);
    return description.equals(o.description) && name.equals(o.name) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * name.hashCode() + 5 * directives.hashCode();
  }
  
  public InputObjectTypeDefinition_Sequence2 withDescription(hydra.util.Opt<hydra.langs.graphql.syntax.Description> description) {
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    return new InputObjectTypeDefinition_Sequence2(description, name, directives);
  }
  
  public InputObjectTypeDefinition_Sequence2 withName(hydra.langs.graphql.syntax.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new InputObjectTypeDefinition_Sequence2(description, name, directives);
  }
  
  public InputObjectTypeDefinition_Sequence2 withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new InputObjectTypeDefinition_Sequence2(description, name, directives);
  }
}