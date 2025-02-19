// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class PointerMemberAccess implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.PointerMemberAccess");
  
  public static final hydra.core.Name FIELD_NAME_POINTER = new hydra.core.Name("pointer");
  
  public static final hydra.core.Name FIELD_NAME_MEMBER = new hydra.core.Name("member");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public final hydra.ext.csharp.syntax.PrimaryExpression pointer;
  
  public final hydra.ext.csharp.syntax.Identifier member;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments;
  
  public PointerMemberAccess (hydra.ext.csharp.syntax.PrimaryExpression pointer, hydra.ext.csharp.syntax.Identifier member, hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments) {
    java.util.Objects.requireNonNull((pointer));
    java.util.Objects.requireNonNull((member));
    java.util.Objects.requireNonNull((typeArguments));
    this.pointer = pointer;
    this.member = member;
    this.typeArguments = typeArguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PointerMemberAccess)) {
      return false;
    }
    PointerMemberAccess o = (PointerMemberAccess) (other);
    return pointer.equals(o.pointer) && member.equals(o.member) && typeArguments.equals(o.typeArguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * pointer.hashCode() + 3 * member.hashCode() + 5 * typeArguments.hashCode();
  }
  
  public PointerMemberAccess withPointer(hydra.ext.csharp.syntax.PrimaryExpression pointer) {
    java.util.Objects.requireNonNull((pointer));
    return new PointerMemberAccess(pointer, member, typeArguments);
  }
  
  public PointerMemberAccess withMember(hydra.ext.csharp.syntax.Identifier member) {
    java.util.Objects.requireNonNull((member));
    return new PointerMemberAccess(pointer, member, typeArguments);
  }
  
  public PointerMemberAccess withTypeArguments(hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments) {
    java.util.Objects.requireNonNull((typeArguments));
    return new PointerMemberAccess(pointer, member, typeArguments);
  }
}