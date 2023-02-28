package hydra.langs.graphql.syntax;

public class ObjectTypeDefinition_Sequence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.ObjectTypeDefinition.Sequence");
  
  public final java.util.Optional<hydra.langs.graphql.syntax.Description> description;
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final java.util.Optional<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces;
  
  public final java.util.Optional<hydra.langs.graphql.syntax.Directives> directives;
  
  public final hydra.langs.graphql.syntax.FieldsDefinition fieldsDefinition;
  
  public ObjectTypeDefinition_Sequence (java.util.Optional<hydra.langs.graphql.syntax.Description> description, hydra.langs.graphql.syntax.Name name, java.util.Optional<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces, java.util.Optional<hydra.langs.graphql.syntax.Directives> directives, hydra.langs.graphql.syntax.FieldsDefinition fieldsDefinition) {
    this.description = description;
    this.name = name;
    this.implementsInterfaces = implementsInterfaces;
    this.directives = directives;
    this.fieldsDefinition = fieldsDefinition;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectTypeDefinition_Sequence)) {
      return false;
    }
    ObjectTypeDefinition_Sequence o = (ObjectTypeDefinition_Sequence) (other);
    return description.equals(o.description) && name.equals(o.name) && implementsInterfaces.equals(o.implementsInterfaces) && directives.equals(o.directives) && fieldsDefinition.equals(o.fieldsDefinition);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * name.hashCode() + 5 * implementsInterfaces.hashCode() + 7 * directives.hashCode() + 11 * fieldsDefinition.hashCode();
  }
  
  public ObjectTypeDefinition_Sequence withDescription(java.util.Optional<hydra.langs.graphql.syntax.Description> description) {
    return new ObjectTypeDefinition_Sequence(description, name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public ObjectTypeDefinition_Sequence withName(hydra.langs.graphql.syntax.Name name) {
    return new ObjectTypeDefinition_Sequence(description, name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public ObjectTypeDefinition_Sequence withImplementsInterfaces(java.util.Optional<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces) {
    return new ObjectTypeDefinition_Sequence(description, name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public ObjectTypeDefinition_Sequence withDirectives(java.util.Optional<hydra.langs.graphql.syntax.Directives> directives) {
    return new ObjectTypeDefinition_Sequence(description, name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public ObjectTypeDefinition_Sequence withFieldsDefinition(hydra.langs.graphql.syntax.FieldsDefinition fieldsDefinition) {
    return new ObjectTypeDefinition_Sequence(description, name, implementsInterfaces, directives, fieldsDefinition);
  }
}