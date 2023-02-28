package hydra.langs.graphql.syntax;

public class ObjectTypeDefinition_Sequence2 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.ObjectTypeDefinition.Sequence2");
  
  public final java.util.Optional<hydra.langs.graphql.syntax.Description> description;
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final java.util.Optional<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces;
  
  public final java.util.Optional<hydra.langs.graphql.syntax.Directives> directives;
  
  public ObjectTypeDefinition_Sequence2 (java.util.Optional<hydra.langs.graphql.syntax.Description> description, hydra.langs.graphql.syntax.Name name, java.util.Optional<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces, java.util.Optional<hydra.langs.graphql.syntax.Directives> directives) {
    this.description = description;
    this.name = name;
    this.implementsInterfaces = implementsInterfaces;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectTypeDefinition_Sequence2)) {
      return false;
    }
    ObjectTypeDefinition_Sequence2 o = (ObjectTypeDefinition_Sequence2) (other);
    return description.equals(o.description) && name.equals(o.name) && implementsInterfaces.equals(o.implementsInterfaces) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * name.hashCode() + 5 * implementsInterfaces.hashCode() + 7 * directives.hashCode();
  }
  
  public ObjectTypeDefinition_Sequence2 withDescription(java.util.Optional<hydra.langs.graphql.syntax.Description> description) {
    return new ObjectTypeDefinition_Sequence2(description, name, implementsInterfaces, directives);
  }
  
  public ObjectTypeDefinition_Sequence2 withName(hydra.langs.graphql.syntax.Name name) {
    return new ObjectTypeDefinition_Sequence2(description, name, implementsInterfaces, directives);
  }
  
  public ObjectTypeDefinition_Sequence2 withImplementsInterfaces(java.util.Optional<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces) {
    return new ObjectTypeDefinition_Sequence2(description, name, implementsInterfaces, directives);
  }
  
  public ObjectTypeDefinition_Sequence2 withDirectives(java.util.Optional<hydra.langs.graphql.syntax.Directives> directives) {
    return new ObjectTypeDefinition_Sequence2(description, name, implementsInterfaces, directives);
  }
}