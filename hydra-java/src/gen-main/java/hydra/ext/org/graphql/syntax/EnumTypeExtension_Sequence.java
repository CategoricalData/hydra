// Note: this is an automatically generated file. Do not edit.

package hydra.ext.graphql.syntax;

import java.io.Serializable;

public class EnumTypeExtension_Sequence implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/graphql/syntax.EnumTypeExtension.Sequence");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public static final hydra.core.Name FIELD_NAME_ENUM_VALUES_DEFINITION = new hydra.core.Name("enumValuesDefinition");
  
  public final hydra.ext.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives;
  
  public final hydra.ext.graphql.syntax.EnumValuesDefinition enumValuesDefinition;
  
  public EnumTypeExtension_Sequence (hydra.ext.graphql.syntax.Name name, hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives, hydra.ext.graphql.syntax.EnumValuesDefinition enumValuesDefinition) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((directives));
    java.util.Objects.requireNonNull((enumValuesDefinition));
    this.name = name;
    this.directives = directives;
    this.enumValuesDefinition = enumValuesDefinition;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumTypeExtension_Sequence)) {
      return false;
    }
    EnumTypeExtension_Sequence o = (EnumTypeExtension_Sequence) (other);
    return name.equals(o.name) && directives.equals(o.directives) && enumValuesDefinition.equals(o.enumValuesDefinition);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * directives.hashCode() + 5 * enumValuesDefinition.hashCode();
  }
  
  public EnumTypeExtension_Sequence withName(hydra.ext.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new EnumTypeExtension_Sequence(name, directives, enumValuesDefinition);
  }
  
  public EnumTypeExtension_Sequence withDirectives(hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new EnumTypeExtension_Sequence(name, directives, enumValuesDefinition);
  }
  
  public EnumTypeExtension_Sequence withEnumValuesDefinition(hydra.ext.graphql.syntax.EnumValuesDefinition enumValuesDefinition) {
    java.util.Objects.requireNonNull((enumValuesDefinition));
    return new EnumTypeExtension_Sequence(name, directives, enumValuesDefinition);
  }
}
