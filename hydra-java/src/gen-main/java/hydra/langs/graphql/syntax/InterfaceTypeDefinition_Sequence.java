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
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (implementsInterfaces == null) {
      throw new IllegalArgumentException("null value for 'implementsInterfaces' argument");
    }
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    if (fieldsDefinition == null) {
      throw new IllegalArgumentException("null value for 'fieldsDefinition' argument");
    }
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
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    return new InterfaceTypeDefinition_Sequence(description, name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public InterfaceTypeDefinition_Sequence withName(hydra.langs.graphql.syntax.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new InterfaceTypeDefinition_Sequence(description, name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public InterfaceTypeDefinition_Sequence withImplementsInterfaces(hydra.util.Opt<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces) {
    if (implementsInterfaces == null) {
      throw new IllegalArgumentException("null value for 'implementsInterfaces' argument");
    }
    return new InterfaceTypeDefinition_Sequence(description, name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public InterfaceTypeDefinition_Sequence withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new InterfaceTypeDefinition_Sequence(description, name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public InterfaceTypeDefinition_Sequence withFieldsDefinition(hydra.langs.graphql.syntax.FieldsDefinition fieldsDefinition) {
    if (fieldsDefinition == null) {
      throw new IllegalArgumentException("null value for 'fieldsDefinition' argument");
    }
    return new InterfaceTypeDefinition_Sequence(description, name, implementsInterfaces, directives, fieldsDefinition);
  }
}