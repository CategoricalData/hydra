// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public class InterfaceTypeExtension_Sequence implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.InterfaceTypeExtension_Sequence");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_IMPLEMENTS_INTERFACES = new hydra.core.Name("implementsInterfaces");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS_DEFINITION = new hydra.core.Name("fieldsDefinition");
  
  public final hydra.ext.org.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.ext.org.graphql.syntax.ImplementsInterfaces> implementsInterfaces;
  
  public final hydra.util.Opt<hydra.ext.org.graphql.syntax.Directives> directives;
  
  public final hydra.ext.org.graphql.syntax.FieldsDefinition fieldsDefinition;
  
  public InterfaceTypeExtension_Sequence (hydra.ext.org.graphql.syntax.Name name, hydra.util.Opt<hydra.ext.org.graphql.syntax.ImplementsInterfaces> implementsInterfaces, hydra.util.Opt<hydra.ext.org.graphql.syntax.Directives> directives, hydra.ext.org.graphql.syntax.FieldsDefinition fieldsDefinition) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((implementsInterfaces));
    java.util.Objects.requireNonNull((directives));
    java.util.Objects.requireNonNull((fieldsDefinition));
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
  
  public InterfaceTypeExtension_Sequence withName(hydra.ext.org.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new InterfaceTypeExtension_Sequence(name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public InterfaceTypeExtension_Sequence withImplementsInterfaces(hydra.util.Opt<hydra.ext.org.graphql.syntax.ImplementsInterfaces> implementsInterfaces) {
    java.util.Objects.requireNonNull((implementsInterfaces));
    return new InterfaceTypeExtension_Sequence(name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public InterfaceTypeExtension_Sequence withDirectives(hydra.util.Opt<hydra.ext.org.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new InterfaceTypeExtension_Sequence(name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public InterfaceTypeExtension_Sequence withFieldsDefinition(hydra.ext.org.graphql.syntax.FieldsDefinition fieldsDefinition) {
    java.util.Objects.requireNonNull((fieldsDefinition));
    return new InterfaceTypeExtension_Sequence(name, implementsInterfaces, directives, fieldsDefinition);
  }
}