package hydra.ext.graphql.syntax;

public class UnionTypeDefinition {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.UnionTypeDefinition");
  
  public final java.util.Optional<hydra.ext.graphql.syntax.Description> description;
  
  public final hydra.ext.graphql.syntax.Name name;
  
  public final java.util.Optional<hydra.ext.graphql.syntax.Directives> directives;
  
  public final java.util.Optional<hydra.ext.graphql.syntax.UnionMemberTypes> unionMemberTypes;
  
  public UnionTypeDefinition (java.util.Optional<hydra.ext.graphql.syntax.Description> description, hydra.ext.graphql.syntax.Name name, java.util.Optional<hydra.ext.graphql.syntax.Directives> directives, java.util.Optional<hydra.ext.graphql.syntax.UnionMemberTypes> unionMemberTypes) {
    this.description = description;
    this.name = name;
    this.directives = directives;
    this.unionMemberTypes = unionMemberTypes;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnionTypeDefinition)) {
      return false;
    }
    UnionTypeDefinition o = (UnionTypeDefinition) (other);
    return description.equals(o.description) && name.equals(o.name) && directives.equals(o.directives) && unionMemberTypes.equals(o.unionMemberTypes);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * name.hashCode() + 5 * directives.hashCode() + 7 * unionMemberTypes.hashCode();
  }
  
  public UnionTypeDefinition withDescription(java.util.Optional<hydra.ext.graphql.syntax.Description> description) {
    return new UnionTypeDefinition(description, name, directives, unionMemberTypes);
  }
  
  public UnionTypeDefinition withName(hydra.ext.graphql.syntax.Name name) {
    return new UnionTypeDefinition(description, name, directives, unionMemberTypes);
  }
  
  public UnionTypeDefinition withDirectives(java.util.Optional<hydra.ext.graphql.syntax.Directives> directives) {
    return new UnionTypeDefinition(description, name, directives, unionMemberTypes);
  }
  
  public UnionTypeDefinition withUnionMemberTypes(java.util.Optional<hydra.ext.graphql.syntax.UnionMemberTypes> unionMemberTypes) {
    return new UnionTypeDefinition(description, name, directives, unionMemberTypes);
  }
}