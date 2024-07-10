// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class InputValueDefinition implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.InputValueDefinition");
  
  public final java.util.Optional<hydra.langs.graphql.syntax.Description> description;
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.langs.graphql.syntax.Type type;
  
  public final java.util.Optional<hydra.langs.graphql.syntax.DefaultValue> defaultValue;
  
  public final java.util.Optional<hydra.langs.graphql.syntax.Directives> directives;
  
  public InputValueDefinition (java.util.Optional<hydra.langs.graphql.syntax.Description> description, hydra.langs.graphql.syntax.Name name, hydra.langs.graphql.syntax.Type type, java.util.Optional<hydra.langs.graphql.syntax.DefaultValue> defaultValue, java.util.Optional<hydra.langs.graphql.syntax.Directives> directives) {
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    if (defaultValue == null) {
      throw new IllegalArgumentException("null value for 'defaultValue' argument");
    }
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    this.description = description;
    this.name = name;
    this.type = type;
    this.defaultValue = defaultValue;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InputValueDefinition)) {
      return false;
    }
    InputValueDefinition o = (InputValueDefinition) (other);
    return description.equals(o.description) && name.equals(o.name) && type.equals(o.type) && defaultValue.equals(o.defaultValue) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * name.hashCode() + 5 * type.hashCode() + 7 * defaultValue.hashCode() + 11 * directives.hashCode();
  }
  
  public InputValueDefinition withDescription(java.util.Optional<hydra.langs.graphql.syntax.Description> description) {
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    return new InputValueDefinition(description, name, type, defaultValue, directives);
  }
  
  public InputValueDefinition withName(hydra.langs.graphql.syntax.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new InputValueDefinition(description, name, type, defaultValue, directives);
  }
  
  public InputValueDefinition withType(hydra.langs.graphql.syntax.Type type) {
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    return new InputValueDefinition(description, name, type, defaultValue, directives);
  }
  
  public InputValueDefinition withDefaultValue(java.util.Optional<hydra.langs.graphql.syntax.DefaultValue> defaultValue) {
    if (defaultValue == null) {
      throw new IllegalArgumentException("null value for 'defaultValue' argument");
    }
    return new InputValueDefinition(description, name, type, defaultValue, directives);
  }
  
  public InputValueDefinition withDirectives(java.util.Optional<hydra.langs.graphql.syntax.Directives> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new InputValueDefinition(description, name, type, defaultValue, directives);
  }
}