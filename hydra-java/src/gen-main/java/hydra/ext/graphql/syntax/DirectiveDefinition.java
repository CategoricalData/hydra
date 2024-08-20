// Note: this is an automatically generated file. Do not edit.

package hydra.ext.graphql.syntax;

import java.io.Serializable;

public class DirectiveDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/graphql/syntax.DirectiveDefinition");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS_DEFINITION = new hydra.core.Name("argumentsDefinition");
  
  public static final hydra.core.Name FIELD_NAME_REPEATABLE = new hydra.core.Name("repeatable");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVE_LOCATIONS = new hydra.core.Name("directiveLocations");
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.Description> description;
  
  public final hydra.ext.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.ArgumentsDefinition> argumentsDefinition;
  
  public final hydra.util.Opt<java.lang.Void> repeatable;
  
  public final hydra.ext.graphql.syntax.DirectiveLocations directiveLocations;
  
  public DirectiveDefinition (hydra.util.Opt<hydra.ext.graphql.syntax.Description> description, hydra.ext.graphql.syntax.Name name, hydra.util.Opt<hydra.ext.graphql.syntax.ArgumentsDefinition> argumentsDefinition, hydra.util.Opt<java.lang.Void> repeatable, hydra.ext.graphql.syntax.DirectiveLocations directiveLocations) {
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((argumentsDefinition));
    java.util.Objects.requireNonNull((repeatable));
    java.util.Objects.requireNonNull((directiveLocations));
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
  
  public DirectiveDefinition withDescription(hydra.util.Opt<hydra.ext.graphql.syntax.Description> description) {
    java.util.Objects.requireNonNull((description));
    return new DirectiveDefinition(description, name, argumentsDefinition, repeatable, directiveLocations);
  }
  
  public DirectiveDefinition withName(hydra.ext.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new DirectiveDefinition(description, name, argumentsDefinition, repeatable, directiveLocations);
  }
  
  public DirectiveDefinition withArgumentsDefinition(hydra.util.Opt<hydra.ext.graphql.syntax.ArgumentsDefinition> argumentsDefinition) {
    java.util.Objects.requireNonNull((argumentsDefinition));
    return new DirectiveDefinition(description, name, argumentsDefinition, repeatable, directiveLocations);
  }
  
  public DirectiveDefinition withRepeatable(hydra.util.Opt<java.lang.Void> repeatable) {
    java.util.Objects.requireNonNull((repeatable));
    return new DirectiveDefinition(description, name, argumentsDefinition, repeatable, directiveLocations);
  }
  
  public DirectiveDefinition withDirectiveLocations(hydra.ext.graphql.syntax.DirectiveLocations directiveLocations) {
    java.util.Objects.requireNonNull((directiveLocations));
    return new DirectiveDefinition(description, name, argumentsDefinition, repeatable, directiveLocations);
  }
}
