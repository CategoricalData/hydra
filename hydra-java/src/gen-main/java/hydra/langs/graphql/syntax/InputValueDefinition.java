// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class InputValueDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/graphql/syntax.InputValueDefinition");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT_VALUE = new hydra.core.Name("defaultValue");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Description> description;
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.langs.graphql.syntax.Type type;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.DefaultValue> defaultValue;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public InputValueDefinition (hydra.util.Opt<hydra.langs.graphql.syntax.Description> description, hydra.langs.graphql.syntax.Name name, hydra.langs.graphql.syntax.Type type, hydra.util.Opt<hydra.langs.graphql.syntax.DefaultValue> defaultValue, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((defaultValue));
    java.util.Objects.requireNonNull((directives));
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
  
  public InputValueDefinition withDescription(hydra.util.Opt<hydra.langs.graphql.syntax.Description> description) {
    java.util.Objects.requireNonNull((description));
    return new InputValueDefinition(description, name, type, defaultValue, directives);
  }
  
  public InputValueDefinition withName(hydra.langs.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new InputValueDefinition(description, name, type, defaultValue, directives);
  }
  
  public InputValueDefinition withType(hydra.langs.graphql.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new InputValueDefinition(description, name, type, defaultValue, directives);
  }
  
  public InputValueDefinition withDefaultValue(hydra.util.Opt<hydra.langs.graphql.syntax.DefaultValue> defaultValue) {
    java.util.Objects.requireNonNull((defaultValue));
    return new InputValueDefinition(description, name, type, defaultValue, directives);
  }
  
  public InputValueDefinition withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new InputValueDefinition(description, name, type, defaultValue, directives);
  }
}