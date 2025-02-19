// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class MemberName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.MemberName");
  
  public static final hydra.core.Name FIELD_NAME_INTERFACE_TYPE = new hydra.core.Name("interfaceType");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.TypeName> interfaceType;
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public MemberName (hydra.util.Opt<hydra.ext.csharp.syntax.TypeName> interfaceType, hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((interfaceType));
    java.util.Objects.requireNonNull((identifier));
    this.interfaceType = interfaceType;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MemberName)) {
      return false;
    }
    MemberName o = (MemberName) (other);
    return interfaceType.equals(o.interfaceType) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * interfaceType.hashCode() + 3 * identifier.hashCode();
  }
  
  public MemberName withInterfaceType(hydra.util.Opt<hydra.ext.csharp.syntax.TypeName> interfaceType) {
    java.util.Objects.requireNonNull((interfaceType));
    return new MemberName(interfaceType, identifier);
  }
  
  public MemberName withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new MemberName(interfaceType, identifier);
  }
}