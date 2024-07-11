// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class InterfaceTypeDefinition_Sequence implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.InterfaceTypeDefinition.Sequence");
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Description> description;
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public final hydra.langs.graphql.syntax.FieldsDefinition fieldsDefinition;
  
  public InterfaceTypeDefinition_Sequence (hydra.util.Opt<hydra.langs.graphql.syntax.Description> description, hydra.langs.graphql.syntax.Name name, hydra.util.Opt<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives, hydra.langs.graphql.syntax.FieldsDefinition fieldsDefinition) {
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((implementsInterfaces));
    java.util.Objects.requireNonNull((directives));
    java.util.Objects.requireNonNull((fieldsDefinition));
    this.description = description;
    this.name = name;
    this.implementsInterfaces = implementsInterfaces;
    this.directives = directives;
    this.fieldsDefinition = fieldsDefinition;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InterfaceTypeDefinition_Sequence)) {
      return false;
    }
    InterfaceTypeDefinition_Sequence o = (InterfaceTypeDefinition_Sequence) (other);
    return description.equals(o.description) && name.equals(o.name) && implementsInterfaces.equals(o.implementsInterfaces) && directives.equals(o.directives) && fieldsDefinition.equals(o.fieldsDefinition);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * name.hashCode() + 5 * implementsInterfaces.hashCode() + 7 * directives.hashCode() + 11 * fieldsDefinition.hashCode();
  }
  
  public InterfaceTypeDefinition_Sequence withDescription(hydra.util.Opt<hydra.langs.graphql.syntax.Description> description) {
    java.util.Objects.requireNonNull((description));
    return new InterfaceTypeDefinition_Sequence(description, name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public InterfaceTypeDefinition_Sequence withName(hydra.langs.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new InterfaceTypeDefinition_Sequence(description, name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public InterfaceTypeDefinition_Sequence withImplementsInterfaces(hydra.util.Opt<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces) {
    java.util.Objects.requireNonNull((implementsInterfaces));
    return new InterfaceTypeDefinition_Sequence(description, name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public InterfaceTypeDefinition_Sequence withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new InterfaceTypeDefinition_Sequence(description, name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public InterfaceTypeDefinition_Sequence withFieldsDefinition(hydra.langs.graphql.syntax.FieldsDefinition fieldsDefinition) {
    java.util.Objects.requireNonNull((fieldsDefinition));
    return new InterfaceTypeDefinition_Sequence(description, name, implementsInterfaces, directives, fieldsDefinition);
  }
}