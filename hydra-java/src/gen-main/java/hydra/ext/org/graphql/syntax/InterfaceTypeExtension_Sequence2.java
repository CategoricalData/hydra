// Note: this is an automatically generated file. Do not edit.

package hydra.ext.graphql.syntax;

import java.io.Serializable;

public class InterfaceTypeExtension_Sequence2 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/graphql/syntax.InterfaceTypeExtension.Sequence2");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_IMPLEMENTS_INTERFACES = new hydra.core.Name("implementsInterfaces");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public final hydra.ext.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.ImplementsInterfaces> implementsInterfaces;
  
  public final hydra.ext.graphql.syntax.Directives directives;
  
  public InterfaceTypeExtension_Sequence2 (hydra.ext.graphql.syntax.Name name, hydra.util.Opt<hydra.ext.graphql.syntax.ImplementsInterfaces> implementsInterfaces, hydra.ext.graphql.syntax.Directives directives) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((implementsInterfaces));
    java.util.Objects.requireNonNull((directives));
    this.name = name;
    this.implementsInterfaces = implementsInterfaces;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InterfaceTypeExtension_Sequence2)) {
      return false;
    }
    InterfaceTypeExtension_Sequence2 o = (InterfaceTypeExtension_Sequence2) (other);
    return name.equals(o.name) && implementsInterfaces.equals(o.implementsInterfaces) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * implementsInterfaces.hashCode() + 5 * directives.hashCode();
  }
  
  public InterfaceTypeExtension_Sequence2 withName(hydra.ext.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new InterfaceTypeExtension_Sequence2(name, implementsInterfaces, directives);
  }
  
  public InterfaceTypeExtension_Sequence2 withImplementsInterfaces(hydra.util.Opt<hydra.ext.graphql.syntax.ImplementsInterfaces> implementsInterfaces) {
    java.util.Objects.requireNonNull((implementsInterfaces));
    return new InterfaceTypeExtension_Sequence2(name, implementsInterfaces, directives);
  }
  
  public InterfaceTypeExtension_Sequence2 withDirectives(hydra.ext.graphql.syntax.Directives directives) {
    java.util.Objects.requireNonNull((directives));
    return new InterfaceTypeExtension_Sequence2(name, implementsInterfaces, directives);
  }
}
