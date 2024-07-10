// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class ObjectTypeExtension_Sequence implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.ObjectTypeExtension.Sequence");
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final java.util.Optional<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces;
  
  public final java.util.Optional<hydra.langs.graphql.syntax.Directives> directives;
  
  public final hydra.langs.graphql.syntax.FieldsDefinition fieldsDefinition;
  
  public ObjectTypeExtension_Sequence (hydra.langs.graphql.syntax.Name name, java.util.Optional<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces, java.util.Optional<hydra.langs.graphql.syntax.Directives> directives, hydra.langs.graphql.syntax.FieldsDefinition fieldsDefinition) {
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
    if (!(other instanceof ObjectTypeExtension_Sequence)) {
      return false;
    }
    ObjectTypeExtension_Sequence o = (ObjectTypeExtension_Sequence) (other);
    return name.equals(o.name) && implementsInterfaces.equals(o.implementsInterfaces) && directives.equals(o.directives) && fieldsDefinition.equals(o.fieldsDefinition);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * implementsInterfaces.hashCode() + 5 * directives.hashCode() + 7 * fieldsDefinition.hashCode();
  }
  
  public ObjectTypeExtension_Sequence withName(hydra.langs.graphql.syntax.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new ObjectTypeExtension_Sequence(name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public ObjectTypeExtension_Sequence withImplementsInterfaces(java.util.Optional<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces) {
    if (implementsInterfaces == null) {
      throw new IllegalArgumentException("null value for 'implementsInterfaces' argument");
    }
    return new ObjectTypeExtension_Sequence(name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public ObjectTypeExtension_Sequence withDirectives(java.util.Optional<hydra.langs.graphql.syntax.Directives> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new ObjectTypeExtension_Sequence(name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public ObjectTypeExtension_Sequence withFieldsDefinition(hydra.langs.graphql.syntax.FieldsDefinition fieldsDefinition) {
    if (fieldsDefinition == null) {
      throw new IllegalArgumentException("null value for 'fieldsDefinition' argument");
    }
    return new ObjectTypeExtension_Sequence(name, implementsInterfaces, directives, fieldsDefinition);
  }
}