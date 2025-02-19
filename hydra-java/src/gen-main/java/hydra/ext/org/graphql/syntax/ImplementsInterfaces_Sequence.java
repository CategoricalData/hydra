// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public class ImplementsInterfaces_Sequence implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.ImplementsInterfaces_Sequence");
  
  public static final hydra.core.Name FIELD_NAME_IMPLEMENTS_INTERFACES = new hydra.core.Name("implementsInterfaces");
  
  public static final hydra.core.Name FIELD_NAME_NAMED_TYPE = new hydra.core.Name("namedType");
  
  public final hydra.ext.org.graphql.syntax.ImplementsInterfaces implementsInterfaces;
  
  public final hydra.ext.org.graphql.syntax.NamedType namedType;
  
  public ImplementsInterfaces_Sequence (hydra.ext.org.graphql.syntax.ImplementsInterfaces implementsInterfaces, hydra.ext.org.graphql.syntax.NamedType namedType) {
    java.util.Objects.requireNonNull((implementsInterfaces));
    java.util.Objects.requireNonNull((namedType));
    this.implementsInterfaces = implementsInterfaces;
    this.namedType = namedType;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ImplementsInterfaces_Sequence)) {
      return false;
    }
    ImplementsInterfaces_Sequence o = (ImplementsInterfaces_Sequence) (other);
    return implementsInterfaces.equals(o.implementsInterfaces) && namedType.equals(o.namedType);
  }
  
  @Override
  public int hashCode() {
    return 2 * implementsInterfaces.hashCode() + 3 * namedType.hashCode();
  }
  
  public ImplementsInterfaces_Sequence withImplementsInterfaces(hydra.ext.org.graphql.syntax.ImplementsInterfaces implementsInterfaces) {
    java.util.Objects.requireNonNull((implementsInterfaces));
    return new ImplementsInterfaces_Sequence(implementsInterfaces, namedType);
  }
  
  public ImplementsInterfaces_Sequence withNamedType(hydra.ext.org.graphql.syntax.NamedType namedType) {
    java.util.Objects.requireNonNull((namedType));
    return new ImplementsInterfaces_Sequence(implementsInterfaces, namedType);
  }
}