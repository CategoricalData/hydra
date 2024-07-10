// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class InterfaceTypeExtension_Sequence2 implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.InterfaceTypeExtension.Sequence2");
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final java.util.Optional<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces;
  
  public final hydra.langs.graphql.syntax.Directives directives;
  
  public InterfaceTypeExtension_Sequence2 (hydra.langs.graphql.syntax.Name name, java.util.Optional<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces, hydra.langs.graphql.syntax.Directives directives) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (implementsInterfaces == null) {
      throw new IllegalArgumentException("null value for 'implementsInterfaces' argument");
    }
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
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
  
  public InterfaceTypeExtension_Sequence2 withName(hydra.langs.graphql.syntax.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new InterfaceTypeExtension_Sequence2(name, implementsInterfaces, directives);
  }
  
  public InterfaceTypeExtension_Sequence2 withImplementsInterfaces(java.util.Optional<hydra.langs.graphql.syntax.ImplementsInterfaces> implementsInterfaces) {
    if (implementsInterfaces == null) {
      throw new IllegalArgumentException("null value for 'implementsInterfaces' argument");
    }
    return new InterfaceTypeExtension_Sequence2(name, implementsInterfaces, directives);
  }
  
  public InterfaceTypeExtension_Sequence2 withDirectives(hydra.langs.graphql.syntax.Directives directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new InterfaceTypeExtension_Sequence2(name, implementsInterfaces, directives);
  }
}