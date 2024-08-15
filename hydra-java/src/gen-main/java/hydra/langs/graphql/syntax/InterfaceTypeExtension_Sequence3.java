// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class InterfaceTypeExtension_Sequence3 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/graphql/syntax.InterfaceTypeExtension.Sequence3");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_IMPLEMENTS_INTERFACES = new hydra.core.Name("implementsInterfaces");
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.langs.graphql.syntax.ImplementsInterfaces implementsInterfaces;
  
  public InterfaceTypeExtension_Sequence3 (hydra.langs.graphql.syntax.Name name, hydra.langs.graphql.syntax.ImplementsInterfaces implementsInterfaces) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((implementsInterfaces));
    this.name = name;
    this.implementsInterfaces = implementsInterfaces;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InterfaceTypeExtension_Sequence3)) {
      return false;
    }
    InterfaceTypeExtension_Sequence3 o = (InterfaceTypeExtension_Sequence3) (other);
    return name.equals(o.name) && implementsInterfaces.equals(o.implementsInterfaces);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * implementsInterfaces.hashCode();
  }
  
  public InterfaceTypeExtension_Sequence3 withName(hydra.langs.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new InterfaceTypeExtension_Sequence3(name, implementsInterfaces);
  }
  
  public InterfaceTypeExtension_Sequence3 withImplementsInterfaces(hydra.langs.graphql.syntax.ImplementsInterfaces implementsInterfaces) {
    java.util.Objects.requireNonNull((implementsInterfaces));
    return new InterfaceTypeExtension_Sequence3(name, implementsInterfaces);
  }
}