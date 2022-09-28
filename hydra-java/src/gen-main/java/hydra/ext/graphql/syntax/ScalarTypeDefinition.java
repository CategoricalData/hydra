package hydra.ext.graphql.syntax;

public class ScalarTypeDefinition {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.ScalarTypeDefinition");
  
  public final java.util.Optional<hydra.ext.graphql.syntax.Description> description;
  
  public final hydra.ext.graphql.syntax.Name name;
  
  public final java.util.Optional<hydra.ext.graphql.syntax.Directives> directives;
  
  public ScalarTypeDefinition (java.util.Optional<hydra.ext.graphql.syntax.Description> description, hydra.ext.graphql.syntax.Name name, java.util.Optional<hydra.ext.graphql.syntax.Directives> directives) {
    this.description = description;
    this.name = name;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ScalarTypeDefinition)) {
      return false;
    }
    ScalarTypeDefinition o = (ScalarTypeDefinition) (other);
    return description.equals(o.description) && name.equals(o.name) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * name.hashCode() + 5 * directives.hashCode();
  }
  
  public ScalarTypeDefinition withDescription(java.util.Optional<hydra.ext.graphql.syntax.Description> description) {
    return new ScalarTypeDefinition(description, name, directives);
  }
  
  public ScalarTypeDefinition withName(hydra.ext.graphql.syntax.Name name) {
    return new ScalarTypeDefinition(description, name, directives);
  }
  
  public ScalarTypeDefinition withDirectives(java.util.Optional<hydra.ext.graphql.syntax.Directives> directives) {
    return new ScalarTypeDefinition(description, name, directives);
  }
}