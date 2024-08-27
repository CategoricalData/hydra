// Note: this is an automatically generated file. Do not edit.

package hydra.ext.graphql.syntax;

import java.io.Serializable;

public class ObjectTypeExtension_Sequence3 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/graphql/syntax.ObjectTypeExtension.Sequence3");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_IMPLEMENTS_INTERFACES = new hydra.core.Name("implementsInterfaces");
  
  public final hydra.ext.graphql.syntax.Name name;
  
  public final hydra.ext.graphql.syntax.ImplementsInterfaces implementsInterfaces;
  
  public ObjectTypeExtension_Sequence3 (hydra.ext.graphql.syntax.Name name, hydra.ext.graphql.syntax.ImplementsInterfaces implementsInterfaces) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((implementsInterfaces));
    this.name = name;
    this.implementsInterfaces = implementsInterfaces;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectTypeExtension_Sequence3)) {
      return false;
    }
    ObjectTypeExtension_Sequence3 o = (ObjectTypeExtension_Sequence3) (other);
    return name.equals(o.name) && implementsInterfaces.equals(o.implementsInterfaces);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * implementsInterfaces.hashCode();
  }
  
  public ObjectTypeExtension_Sequence3 withName(hydra.ext.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new ObjectTypeExtension_Sequence3(name, implementsInterfaces);
  }
  
  public ObjectTypeExtension_Sequence3 withImplementsInterfaces(hydra.ext.graphql.syntax.ImplementsInterfaces implementsInterfaces) {
    java.util.Objects.requireNonNull((implementsInterfaces));
    return new ObjectTypeExtension_Sequence3(name, implementsInterfaces);
  }
}