// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class ObjectTypeExtension_Sequence implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.ObjectTypeExtension.Sequence");
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public final hydra.langs.graphql.syntax.FieldsDefinition fieldsDefinition;
  
  public ObjectTypeExtension_Sequence (hydra.langs.graphql.syntax.Name name, hydra.util.Opt<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives, hydra.langs.graphql.syntax.FieldsDefinition fieldsDefinition) {
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
    java.util.Objects.requireNonNull((name));
    return new ObjectTypeExtension_Sequence(name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public ObjectTypeExtension_Sequence withImplementsInterfaces(hydra.util.Opt<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces) {
    java.util.Objects.requireNonNull((implementsInterfaces));
    return new ObjectTypeExtension_Sequence(name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public ObjectTypeExtension_Sequence withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new ObjectTypeExtension_Sequence(name, implementsInterfaces, directives, fieldsDefinition);
  }
  
  public ObjectTypeExtension_Sequence withFieldsDefinition(hydra.langs.graphql.syntax.FieldsDefinition fieldsDefinition) {
    java.util.Objects.requireNonNull((fieldsDefinition));
    return new ObjectTypeExtension_Sequence(name, implementsInterfaces, directives, fieldsDefinition);
  }
}