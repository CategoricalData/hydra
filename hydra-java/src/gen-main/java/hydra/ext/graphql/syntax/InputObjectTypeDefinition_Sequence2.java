package hydra.ext.graphql.syntax;

public class InputObjectTypeDefinition_Sequence2 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.InputObjectTypeDefinition.Sequence2");
  
  public final java.util.Optional<hydra.ext.graphql.syntax.Description> description;
  
  public final hydra.ext.graphql.syntax.Name name;
  
  public final java.util.Optional<hydra.ext.graphql.syntax.Directives> directives;
  
  public InputObjectTypeDefinition_Sequence2 (java.util.Optional<hydra.ext.graphql.syntax.Description> description, hydra.ext.graphql.syntax.Name name, java.util.Optional<hydra.ext.graphql.syntax.Directives> directives) {
    this.description = description;
    this.name = name;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InputObjectTypeDefinition_Sequence2)) {
      return false;
    }
    InputObjectTypeDefinition_Sequence2 o = (InputObjectTypeDefinition_Sequence2) (other);
    return description.equals(o.description) && name.equals(o.name) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * name.hashCode() + 5 * directives.hashCode();
  }
  
  public InputObjectTypeDefinition_Sequence2 withDescription(java.util.Optional<hydra.ext.graphql.syntax.Description> description) {
    return new InputObjectTypeDefinition_Sequence2(description, name, directives);
  }
  
  public InputObjectTypeDefinition_Sequence2 withName(hydra.ext.graphql.syntax.Name name) {
    return new InputObjectTypeDefinition_Sequence2(description, name, directives);
  }
  
  public InputObjectTypeDefinition_Sequence2 withDirectives(java.util.Optional<hydra.ext.graphql.syntax.Directives> directives) {
    return new InputObjectTypeDefinition_Sequence2(description, name, directives);
  }
}