// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class EnumTypeExtension_Sequence implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.EnumTypeExtension.Sequence");
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public final hydra.langs.graphql.syntax.EnumValuesDefinition enumValuesDefinition;
  
  public EnumTypeExtension_Sequence (hydra.langs.graphql.syntax.Name name, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives, hydra.langs.graphql.syntax.EnumValuesDefinition enumValuesDefinition) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    if (enumValuesDefinition == null) {
      throw new IllegalArgumentException("null value for 'enumValuesDefinition' argument");
    }
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
  
  public EnumTypeExtension_Sequence withName(hydra.langs.graphql.syntax.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new EnumTypeExtension_Sequence(name, directives, enumValuesDefinition);
  }
  
  public EnumTypeExtension_Sequence withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new EnumTypeExtension_Sequence(name, directives, enumValuesDefinition);
  }
  
  public EnumTypeExtension_Sequence withEnumValuesDefinition(hydra.langs.graphql.syntax.EnumValuesDefinition enumValuesDefinition) {
    if (enumValuesDefinition == null) {
      throw new IllegalArgumentException("null value for 'enumValuesDefinition' argument");
    }
    return new EnumTypeExtension_Sequence(name, directives, enumValuesDefinition);
  }
}