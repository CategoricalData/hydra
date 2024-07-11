// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class EnumValueDefinition implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.EnumValueDefinition");
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Description> description;
  
  public final hydra.langs.graphql.syntax.EnumValue enumValue;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public EnumValueDefinition (hydra.util.Opt<hydra.langs.graphql.syntax.Description> description, hydra.langs.graphql.syntax.EnumValue enumValue, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    if (enumValue == null) {
      throw new IllegalArgumentException("null value for 'enumValue' argument");
    }
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    this.description = description;
    this.enumValue = enumValue;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumValueDefinition)) {
      return false;
    }
    EnumValueDefinition o = (EnumValueDefinition) (other);
    return description.equals(o.description) && enumValue.equals(o.enumValue) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * enumValue.hashCode() + 5 * directives.hashCode();
  }
  
  public EnumValueDefinition withDescription(hydra.util.Opt<hydra.langs.graphql.syntax.Description> description) {
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    return new EnumValueDefinition(description, enumValue, directives);
  }
  
  public EnumValueDefinition withEnumValue(hydra.langs.graphql.syntax.EnumValue enumValue) {
    if (enumValue == null) {
      throw new IllegalArgumentException("null value for 'enumValue' argument");
    }
    return new EnumValueDefinition(description, enumValue, directives);
  }
  
  public EnumValueDefinition withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new EnumValueDefinition(description, enumValue, directives);
  }
}