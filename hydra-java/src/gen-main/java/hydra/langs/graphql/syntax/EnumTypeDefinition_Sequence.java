package hydra.langs.graphql.syntax;

public class EnumTypeDefinition_Sequence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.EnumTypeDefinition.Sequence");
  
  public final java.util.Optional<hydra.langs.graphql.syntax.Description> description;
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final java.util.Optional<hydra.langs.graphql.syntax.Directives> directives;
  
  public final hydra.langs.graphql.syntax.EnumValuesDefinition enumValuesDefinition;
  
  public EnumTypeDefinition_Sequence (java.util.Optional<hydra.langs.graphql.syntax.Description> description, hydra.langs.graphql.syntax.Name name, java.util.Optional<hydra.langs.graphql.syntax.Directives> directives, hydra.langs.graphql.syntax.EnumValuesDefinition enumValuesDefinition) {
    this.description = description;
    this.name = name;
    this.directives = directives;
    this.enumValuesDefinition = enumValuesDefinition;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumTypeDefinition_Sequence)) {
      return false;
    }
    EnumTypeDefinition_Sequence o = (EnumTypeDefinition_Sequence) (other);
    return description.equals(o.description) && name.equals(o.name) && directives.equals(o.directives) && enumValuesDefinition.equals(o.enumValuesDefinition);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * name.hashCode() + 5 * directives.hashCode() + 7 * enumValuesDefinition.hashCode();
  }
  
  public EnumTypeDefinition_Sequence withDescription(java.util.Optional<hydra.langs.graphql.syntax.Description> description) {
    return new EnumTypeDefinition_Sequence(description, name, directives, enumValuesDefinition);
  }
  
  public EnumTypeDefinition_Sequence withName(hydra.langs.graphql.syntax.Name name) {
    return new EnumTypeDefinition_Sequence(description, name, directives, enumValuesDefinition);
  }
  
  public EnumTypeDefinition_Sequence withDirectives(java.util.Optional<hydra.langs.graphql.syntax.Directives> directives) {
    return new EnumTypeDefinition_Sequence(description, name, directives, enumValuesDefinition);
  }
  
  public EnumTypeDefinition_Sequence withEnumValuesDefinition(hydra.langs.graphql.syntax.EnumValuesDefinition enumValuesDefinition) {
    return new EnumTypeDefinition_Sequence(description, name, directives, enumValuesDefinition);
  }
}