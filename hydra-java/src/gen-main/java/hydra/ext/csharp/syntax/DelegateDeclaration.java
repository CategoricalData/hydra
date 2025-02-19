// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class DelegateDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.DelegateDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_RETURN_TYPE = new hydra.core.Name("returnType");
  
  public static final hydra.core.Name FIELD_NAME_REF_KIND = new hydra.core.Name("refKind");
  
  public static final hydra.core.Name FIELD_NAME_REF_RETURN_TYPE = new hydra.core.Name("refReturnType");
  
  public static final hydra.core.Name FIELD_NAME_HEADER = new hydra.core.Name("header");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final java.util.List<hydra.ext.csharp.syntax.DelegateModifier> modifiers;
  
  public final hydra.ext.csharp.syntax.ReturnType returnType;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.RefKind> refKind;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Type> refReturnType;
  
  public final hydra.ext.csharp.syntax.DelegateHeader header;
  
  public DelegateDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, java.util.List<hydra.ext.csharp.syntax.DelegateModifier> modifiers, hydra.ext.csharp.syntax.ReturnType returnType, hydra.util.Opt<hydra.ext.csharp.syntax.RefKind> refKind, hydra.util.Opt<hydra.ext.csharp.syntax.Type> refReturnType, hydra.ext.csharp.syntax.DelegateHeader header) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((returnType));
    java.util.Objects.requireNonNull((refKind));
    java.util.Objects.requireNonNull((refReturnType));
    java.util.Objects.requireNonNull((header));
    this.attributes = attributes;
    this.modifiers = modifiers;
    this.returnType = returnType;
    this.refKind = refKind;
    this.refReturnType = refReturnType;
    this.header = header;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DelegateDeclaration)) {
      return false;
    }
    DelegateDeclaration o = (DelegateDeclaration) (other);
    return attributes.equals(o.attributes) && modifiers.equals(o.modifiers) && returnType.equals(o.returnType) && refKind.equals(o.refKind) && refReturnType.equals(o.refReturnType) && header.equals(o.header);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifiers.hashCode() + 5 * returnType.hashCode() + 7 * refKind.hashCode() + 11 * refReturnType.hashCode() + 13 * header.hashCode();
  }
  
  public DelegateDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new DelegateDeclaration(attributes, modifiers, returnType, refKind, refReturnType, header);
  }
  
  public DelegateDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.DelegateModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new DelegateDeclaration(attributes, modifiers, returnType, refKind, refReturnType, header);
  }
  
  public DelegateDeclaration withReturnType(hydra.ext.csharp.syntax.ReturnType returnType) {
    java.util.Objects.requireNonNull((returnType));
    return new DelegateDeclaration(attributes, modifiers, returnType, refKind, refReturnType, header);
  }
  
  public DelegateDeclaration withRefKind(hydra.util.Opt<hydra.ext.csharp.syntax.RefKind> refKind) {
    java.util.Objects.requireNonNull((refKind));
    return new DelegateDeclaration(attributes, modifiers, returnType, refKind, refReturnType, header);
  }
  
  public DelegateDeclaration withRefReturnType(hydra.util.Opt<hydra.ext.csharp.syntax.Type> refReturnType) {
    java.util.Objects.requireNonNull((refReturnType));
    return new DelegateDeclaration(attributes, modifiers, returnType, refKind, refReturnType, header);
  }
  
  public DelegateDeclaration withHeader(hydra.ext.csharp.syntax.DelegateHeader header) {
    java.util.Objects.requireNonNull((header));
    return new DelegateDeclaration(attributes, modifiers, returnType, refKind, refReturnType, header);
  }
}