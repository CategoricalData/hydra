// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class MemberAccess implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.MemberAccess");
  
  public static final hydra.core.Name FIELD_NAME_HEAD = new hydra.core.Name("head");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public final hydra.ext.csharp.syntax.MemberAccessHead head;
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments;
  
  public MemberAccess (hydra.ext.csharp.syntax.MemberAccessHead head, hydra.ext.csharp.syntax.Identifier identifier, hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments) {
    java.util.Objects.requireNonNull((head));
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((typeArguments));
    this.head = head;
    this.identifier = identifier;
    this.typeArguments = typeArguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MemberAccess)) {
      return false;
    }
    MemberAccess o = (MemberAccess) (other);
    return head.equals(o.head) && identifier.equals(o.identifier) && typeArguments.equals(o.typeArguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * head.hashCode() + 3 * identifier.hashCode() + 5 * typeArguments.hashCode();
  }
  
  public MemberAccess withHead(hydra.ext.csharp.syntax.MemberAccessHead head) {
    java.util.Objects.requireNonNull((head));
    return new MemberAccess(head, identifier, typeArguments);
  }
  
  public MemberAccess withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new MemberAccess(head, identifier, typeArguments);
  }
  
  public MemberAccess withTypeArguments(hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments) {
    java.util.Objects.requireNonNull((typeArguments));
    return new MemberAccess(head, identifier, typeArguments);
  }
}