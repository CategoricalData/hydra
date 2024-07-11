// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class DirectiveDefinition implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.DirectiveDefinition");
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Description> description;
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.ArgumentsDefinition> argumentsDefinition;
  
  public final hydra.util.Opt<java.lang.Void> repeatable;
  
  public final hydra.langs.graphql.syntax.DirectiveLocations directiveLocations;
  
  public DirectiveDefinition (hydra.util.Opt<hydra.langs.graphql.syntax.Description> description, hydra.langs.graphql.syntax.Name name, hydra.util.Opt<hydra.langs.graphql.syntax.ArgumentsDefinition> argumentsDefinition, hydra.util.Opt<java.lang.Void> repeatable, hydra.langs.graphql.syntax.DirectiveLocations directiveLocations) {
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (argumentsDefinition == null) {
      throw new IllegalArgumentException("null value for 'argumentsDefinition' argument");
    }
    if (repeatable == null) {
      throw new IllegalArgumentException("null value for 'repeatable' argument");
    }
    if (directiveLocations == null) {
      throw new IllegalArgumentException("null value for 'directiveLocations' argument");
    }
    this.description = description;
    this.name = name;
    this.argumentsDefinition = argumentsDefinition;
    this.repeatable = repeatable;
    this.directiveLocations = directiveLocations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DirectiveDefinition)) {
      return false;
    }
    DirectiveDefinition o = (DirectiveDefinition) (other);
    return description.equals(o.description) && name.equals(o.name) && argumentsDefinition.equals(o.argumentsDefinition) && repeatable.equals(o.repeatable) && directiveLocations.equals(o.directiveLocations);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * name.hashCode() + 5 * argumentsDefinition.hashCode() + 7 * repeatable.hashCode() + 11 * directiveLocations.hashCode();
  }
  
  public DirectiveDefinition withDescription(hydra.util.Opt<hydra.langs.graphql.syntax.Description> description) {
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    return new DirectiveDefinition(description, name, argumentsDefinition, repeatable, directiveLocations);
  }
  
  public DirectiveDefinition withName(hydra.langs.graphql.syntax.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new DirectiveDefinition(description, name, argumentsDefinition, repeatable, directiveLocations);
  }
  
  public DirectiveDefinition withArgumentsDefinition(hydra.util.Opt<hydra.langs.graphql.syntax.ArgumentsDefinition> argumentsDefinition) {
    if (argumentsDefinition == null) {
      throw new IllegalArgumentException("null value for 'argumentsDefinition' argument");
    }
    return new DirectiveDefinition(description, name, argumentsDefinition, repeatable, directiveLocations);
  }
  
  public DirectiveDefinition withRepeatable(hydra.util.Opt<java.lang.Void> repeatable) {
    if (repeatable == null) {
      throw new IllegalArgumentException("null value for 'repeatable' argument");
    }
    return new DirectiveDefinition(description, name, argumentsDefinition, repeatable, directiveLocations);
  }
  
  public DirectiveDefinition withDirectiveLocations(hydra.langs.graphql.syntax.DirectiveLocations directiveLocations) {
    if (directiveLocations == null) {
      throw new IllegalArgumentException("null value for 'directiveLocations' argument");
    }
    return new DirectiveDefinition(description, name, argumentsDefinition, repeatable, directiveLocations);
  }
}