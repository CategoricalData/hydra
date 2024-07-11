// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class EnumTypeDefinition implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.EnumTypeDefinition");
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Description> description;
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.EnumValuesDefinition> enumValuesDefinition;
  
  public EnumTypeDefinition (hydra.util.Opt<hydra.langs.graphql.syntax.Description> description, hydra.langs.graphql.syntax.Name name, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives, hydra.util.Opt<hydra.langs.graphql.syntax.EnumValuesDefinition> enumValuesDefinition) {
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((directives));
    java.util.Objects.requireNonNull((enumValuesDefinition));
    this.description = description;
    this.name = name;
    this.directives = directives;
    this.enumValuesDefinition = enumValuesDefinition;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumTypeDefinition)) {
      return false;
    }
    EnumTypeDefinition o = (EnumTypeDefinition) (other);
    return description.equals(o.description) && name.equals(o.name) && directives.equals(o.directives) && enumValuesDefinition.equals(o.enumValuesDefinition);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * name.hashCode() + 5 * directives.hashCode() + 7 * enumValuesDefinition.hashCode();
  }
  
  public EnumTypeDefinition withDescription(hydra.util.Opt<hydra.langs.graphql.syntax.Description> description) {
    java.util.Objects.requireNonNull((description));
    return new EnumTypeDefinition(description, name, directives, enumValuesDefinition);
  }
  
  public EnumTypeDefinition withName(hydra.langs.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new EnumTypeDefinition(description, name, directives, enumValuesDefinition);
  }
  
  public EnumTypeDefinition withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new EnumTypeDefinition(description, name, directives, enumValuesDefinition);
  }
  
  public EnumTypeDefinition withEnumValuesDefinition(hydra.util.Opt<hydra.langs.graphql.syntax.EnumValuesDefinition> enumValuesDefinition) {
    java.util.Objects.requireNonNull((enumValuesDefinition));
    return new EnumTypeDefinition(description, name, directives, enumValuesDefinition);
  }
}