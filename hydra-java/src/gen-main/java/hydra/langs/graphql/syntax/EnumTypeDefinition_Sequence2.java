package hydra.langs.graphql.syntax;

public class EnumTypeDefinition_Sequence2 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.EnumTypeDefinition.Sequence2");
  
  public final java.util.Optional<hydra.langs.graphql.syntax.Description> description;
  
  public final java.util.Optional<hydra.langs.graphql.syntax.Directives> directives;
  
  public EnumTypeDefinition_Sequence2 (java.util.Optional<hydra.langs.graphql.syntax.Description> description, java.util.Optional<hydra.langs.graphql.syntax.Directives> directives) {
    this.description = description;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumTypeDefinition_Sequence2)) {
      return false;
    }
    EnumTypeDefinition_Sequence2 o = (EnumTypeDefinition_Sequence2) (other);
    return description.equals(o.description) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * directives.hashCode();
  }
  
  public EnumTypeDefinition_Sequence2 withDescription(java.util.Optional<hydra.langs.graphql.syntax.Description> description) {
    return new EnumTypeDefinition_Sequence2(description, directives);
  }
  
  public EnumTypeDefinition_Sequence2 withDirectives(java.util.Optional<hydra.langs.graphql.syntax.Directives> directives) {
    return new EnumTypeDefinition_Sequence2(description, directives);
  }
}