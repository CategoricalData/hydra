// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public class ObjectTypeExtension_Sequence2 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.ObjectTypeExtension_Sequence2");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_IMPLEMENTS_INTERFACES = new hydra.core.Name("implementsInterfaces");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public final hydra.ext.org.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.ext.org.graphql.syntax.ImplementsInterfaces> implementsInterfaces;
  
  public final hydra.util.Opt<hydra.ext.org.graphql.syntax.Directives> directives;
  
  public ObjectTypeExtension_Sequence2 (hydra.ext.org.graphql.syntax.Name name, hydra.util.Opt<hydra.ext.org.graphql.syntax.ImplementsInterfaces> implementsInterfaces, hydra.util.Opt<hydra.ext.org.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((implementsInterfaces));
    java.util.Objects.requireNonNull((directives));
    this.name = name;
    this.implementsInterfaces = implementsInterfaces;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectTypeExtension_Sequence2)) {
      return false;
    }
    ObjectTypeExtension_Sequence2 o = (ObjectTypeExtension_Sequence2) (other);
    return name.equals(o.name) && implementsInterfaces.equals(o.implementsInterfaces) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * implementsInterfaces.hashCode() + 5 * directives.hashCode();
  }
  
  public ObjectTypeExtension_Sequence2 withName(hydra.ext.org.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new ObjectTypeExtension_Sequence2(name, implementsInterfaces, directives);
  }
  
  public ObjectTypeExtension_Sequence2 withImplementsInterfaces(hydra.util.Opt<hydra.ext.org.graphql.syntax.ImplementsInterfaces> implementsInterfaces) {
    java.util.Objects.requireNonNull((implementsInterfaces));
    return new ObjectTypeExtension_Sequence2(name, implementsInterfaces, directives);
  }
  
  public ObjectTypeExtension_Sequence2 withDirectives(hydra.util.Opt<hydra.ext.org.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new ObjectTypeExtension_Sequence2(name, implementsInterfaces, directives);
  }
}