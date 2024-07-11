// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class InterfaceTypeExtension_Sequence implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.InterfaceTypeExtension.Sequence");
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public final hydra.langs.graphql.syntax.FieldsDefinition fieldsDefinition;
  
  public InterfaceTypeExtension_Sequence (hydra.langs.graphql.syntax.Name name, hydra.util.Opt<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives, hydra.langs.graphql.syntax.FieldsDefinition fieldsDefinition) {
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
    this.name = name;
    this.implementsInterfaces = implementsInterfaces;
    this.directives = directives;
    this.fieldsDefinition = fieldsDefinition;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InterfaceTypeExtension_Sequence)) {
      return false;
    }
    InterfaceTypeExtension_Sequence o = (InterfaceTypeExtension_Sequence) (other);
    return name.equals(o.name) && implementsInterfaces.equals(o.implementsInterfaces) && directives.equals(o.directives) && fieldsDefinition.equals(o.fieldsDefinition);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * implementsInterfaces.hashCode() + 5 * directives.hashCode() + 7 * fieldsDefinition.hashCode();
  }
  
  public InterfaceTypeExtension_Sequence withName(hydra.langs.graphql.syntax.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new InterfaceTypeExtension_Sequence(name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public InterfaceTypeExtension_Sequence withImplementsInterfaces(hydra.util.Opt<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces) {
    if (implementsInterfaces == null) {
      throw new IllegalArgumentException("null value for 'implementsInterfaces' argument");
    }
    return new InterfaceTypeExtension_Sequence(name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public InterfaceTypeExtension_Sequence withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new InterfaceTypeExtension_Sequence(name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public InterfaceTypeExtension_Sequence withFieldsDefinition(hydra.langs.graphql.syntax.FieldsDefinition fieldsDefinition) {
    if (fieldsDefinition == null) {
      throw new IllegalArgumentException("null value for 'fieldsDefinition' argument");
    }
    return new InterfaceTypeExtension_Sequence(name, implementsInterfaces, directives, fieldsDefinition);
  }
}