// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class FieldDefinition implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.FieldDefinition");
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Description> description;
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.ArgumentsDefinition> argumentsDefinition;
  
  public final hydra.langs.graphql.syntax.Type type;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public FieldDefinition (hydra.util.Opt<hydra.langs.graphql.syntax.Description> description, hydra.langs.graphql.syntax.Name name, hydra.util.Opt<hydra.langs.graphql.syntax.ArgumentsDefinition> argumentsDefinition, hydra.langs.graphql.syntax.Type type, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (argumentsDefinition == null) {
      throw new IllegalArgumentException("null value for 'argumentsDefinition' argument");
    }
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    this.description = description;
    this.name = name;
    this.argumentsDefinition = argumentsDefinition;
    this.type = type;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldDefinition)) {
      return false;
    }
    FieldDefinition o = (FieldDefinition) (other);
    return description.equals(o.description) && name.equals(o.name) && argumentsDefinition.equals(o.argumentsDefinition) && type.equals(o.type) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * name.hashCode() + 5 * argumentsDefinition.hashCode() + 7 * type.hashCode() + 11 * directives.hashCode();
  }
  
  public FieldDefinition withDescription(hydra.util.Opt<hydra.langs.graphql.syntax.Description> description) {
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    return new FieldDefinition(description, name, argumentsDefinition, type, directives);
  }
  
  public FieldDefinition withName(hydra.langs.graphql.syntax.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new FieldDefinition(description, name, argumentsDefinition, type, directives);
  }
  
  public FieldDefinition withArgumentsDefinition(hydra.util.Opt<hydra.langs.graphql.syntax.ArgumentsDefinition> argumentsDefinition) {
    if (argumentsDefinition == null) {
      throw new IllegalArgumentException("null value for 'argumentsDefinition' argument");
    }
    return new FieldDefinition(description, name, argumentsDefinition, type, directives);
  }
  
  public FieldDefinition withType(hydra.langs.graphql.syntax.Type type) {
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    return new FieldDefinition(description, name, argumentsDefinition, type, directives);
  }
  
  public FieldDefinition withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new FieldDefinition(description, name, argumentsDefinition, type, directives);
  }
}