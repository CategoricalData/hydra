// Note: this is an automatically generated file. Do not edit.

package hydra.ext.graphql.syntax;

import java.io.Serializable;

public class EnumTypeDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/graphql/syntax.EnumTypeDefinition");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public static final hydra.core.Name FIELD_NAME_ENUM_VALUES_DEFINITION = new hydra.core.Name("enumValuesDefinition");
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.Description> description;
  
  public final hydra.ext.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives;
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.EnumValuesDefinition> enumValuesDefinition;
  
  public EnumTypeDefinition (hydra.util.Opt<hydra.ext.graphql.syntax.Description> description, hydra.ext.graphql.syntax.Name name, hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives, hydra.util.Opt<hydra.ext.graphql.syntax.EnumValuesDefinition> enumValuesDefinition) {
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
  
  public EnumTypeDefinition withDescription(hydra.util.Opt<hydra.ext.graphql.syntax.Description> description) {
    java.util.Objects.requireNonNull((description));
    return new EnumTypeDefinition(description, name, directives, enumValuesDefinition);
  }
  
  public EnumTypeDefinition withName(hydra.ext.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new EnumTypeDefinition(description, name, directives, enumValuesDefinition);
  }
  
  public EnumTypeDefinition withDirectives(hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new EnumTypeDefinition(description, name, directives, enumValuesDefinition);
  }
  
  public EnumTypeDefinition withEnumValuesDefinition(hydra.util.Opt<hydra.ext.graphql.syntax.EnumValuesDefinition> enumValuesDefinition) {
    java.util.Objects.requireNonNull((enumValuesDefinition));
    return new EnumTypeDefinition(description, name, directives, enumValuesDefinition);
  }
}
