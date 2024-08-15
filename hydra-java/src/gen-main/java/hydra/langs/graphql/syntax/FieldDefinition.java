// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class FieldDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/graphql/syntax.FieldDefinition");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS_DEFINITION = new hydra.core.Name("argumentsDefinition");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Description> description;
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.ArgumentsDefinition> argumentsDefinition;
  
  public final hydra.langs.graphql.syntax.Type type;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public FieldDefinition (hydra.util.Opt<hydra.langs.graphql.syntax.Description> description, hydra.langs.graphql.syntax.Name name, hydra.util.Opt<hydra.langs.graphql.syntax.ArgumentsDefinition> argumentsDefinition, hydra.langs.graphql.syntax.Type type, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((argumentsDefinition));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((directives));
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
    java.util.Objects.requireNonNull((description));
    return new FieldDefinition(description, name, argumentsDefinition, type, directives);
  }
  
  public FieldDefinition withName(hydra.langs.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new FieldDefinition(description, name, argumentsDefinition, type, directives);
  }
  
  public FieldDefinition withArgumentsDefinition(hydra.util.Opt<hydra.langs.graphql.syntax.ArgumentsDefinition> argumentsDefinition) {
    java.util.Objects.requireNonNull((argumentsDefinition));
    return new FieldDefinition(description, name, argumentsDefinition, type, directives);
  }
  
  public FieldDefinition withType(hydra.langs.graphql.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new FieldDefinition(description, name, argumentsDefinition, type, directives);
  }
  
  public FieldDefinition withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new FieldDefinition(description, name, argumentsDefinition, type, directives);
  }
}